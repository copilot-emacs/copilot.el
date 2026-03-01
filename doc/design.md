# Design

## Overview

copilot.el is an Emacs client for [GitHub Copilot](https://github.com/features/copilot). It
communicates with `@github/copilot-language-server` — a Node.js process that
speaks a superset of the Language Server Protocol (LSP) over JSON-RPC. The
server handles authentication, model inference, telemetry, and all
Copilot-specific logic; copilot.el is responsible for editor integration:
document synchronization, displaying completions as overlays, and exposing
interactive commands.

## Architecture

The package is split into four files, each with a clear responsibility:

| File | Role |
|---|---|
| `copilot.el` | Core — server lifecycle, document sync, inline completions, authentication, handler registries |
| `copilot-chat.el` | Chat UI via `conversation/*` JSON-RPC methods |
| `copilot-nes.el` | Next Edit Suggestions via `textDocument/copilotInlineEdit` |
| `copilot-balancer.el` | Lisp parentheses post-processor for completions |

`copilot-chat.el` and `copilot-nes.el` depend on the core for the server
connection and the handler registries, but they are otherwise self-contained
modules that can be enabled or disabled independently. `copilot-balancer.el` is
fully standalone — it uses only Emacs built-ins (`parse-partial-sexp`, syntax
tables) and has no dependency on JSON-RPC or any other copilot infrastructure.

## Single global server

copilot.el maintains a single `jsonrpc-process-connection` instance
(`copilot--connection`) for the entire Emacs session, shared across all buffers
and projects. This mirrors the Copilot language server's own design, which
expects one connection per editor instance.

When a buffer with `copilot-mode` enabled gains focus, copilot.el sends
`workspace/didChangeWorkspaceFolders` to register the buffer's project root if
it hasn't been registered yet. This lets the server track multiple workspace
roots over the lifetime of a single connection.

## Why not eglot (or lsp-mode)?

The most common question about copilot.el's architecture is why it talks to the
server directly via `jsonrpc` instead of going through an existing LSP client.
There are several reasons:

**Copilot is mostly non-standard LSP.** The server uses standard LSP for
document synchronization (`textDocument/didOpen`, `didChange`, `didClose`) and a
few housekeeping messages, but the features that matter — authentication
(`signInInitiate`, `signInConfirm`, `checkStatus`), inline completions
(`textDocument/inlineCompletion`), chat (`conversation/create`,
`conversation/turn`), Next Edit Suggestions (`textDocument/copilotInlineEdit`),
panel completions (`PanelSolution`, `PanelSolutionsDone`), status tracking
(`didChangeStatus`) — are all Copilot-specific extensions. An LSP client that
doesn't know about these methods provides no value for them.

**Different server topology.** Standard LSP clients like eglot create one server
per project (per language). Copilot uses a single global server for all projects,
which is fundamentally at odds with eglot's lifecycle model. Working around this
would require significant hacks.

**Different completion model.** LSP `textDocument/completion` returns structured
items that integrate with `completion-at-point`. Copilot's inline completions
return raw text meant to be displayed as ghost-text overlays, with support for
partial acceptance (by word, by line, etc.) and completion cycling. Forcing this
into `completion-at-point` would lose most of the UX.

**Potential conflicts.** Many users already run eglot or lsp-mode for their
language servers. Having Copilot managed by the same client would create
interference — conflicting capabilities, race conditions in document
synchronization, or unexpected interactions with other servers.

**`jsonrpc` is enough.** Emacs ships `jsonrpc.el` in core. Creating a
`jsonrpc-process-connection`, sending requests and notifications, and dispatching
incoming messages takes roughly 50 lines of code. The overhead of a full LSP
client is not justified when most of the protocol surface is custom.

## Document synchronization

copilot.el uses `track-changes` (available since Emacs 30) to feed incremental
`textDocument/didChange` notifications to the server.

When `copilot-mode` is enabled in a buffer, it registers a tracker via
`track-changes-register`. On each change, the tracker callback receives the
changed region (`beg`, `end`, `before`) and:

1. Computes the old range end in LSP coordinates using UTF-16 offsets
   (`copilot--lsp-range-end-from-oldtext`).
2. Increments the buffer-local `copilot--doc-version`.
3. Sends a `textDocument/didChange` notification with the changed range and new
   text.

All positions use **UTF-16 code-unit offsets** for the character position on a
line, as required by the LSP specification. `copilot--utf16-offset` handles the
conversion (characters outside the Basic Multilingual Plane count as two UTF-16
code units).

Buffer lifecycle is managed through three notifications:

- `textDocument/didOpen` — sent when a buffer first gains focus with
  `copilot-mode` active, includes the full buffer text.
- `textDocument/didFocus` — sent on subsequent focus changes.
- `textDocument/didClose` — sent when the buffer is killed or `copilot-mode` is
  disabled.

For very large buffers, `copilot--get-source` truncates the text sent to the
server based on `copilot-max-char` (default 100,000 characters), using a
windowing strategy centered around point.

## Completion pipeline

The inline completion flow has several stages:

1. **Trigger.** After each command, `copilot--post-command` checks a set of
   enable/disable predicates. If they pass, it schedules `copilot-complete` via
   an idle timer (configurable with `copilot-idle-delay`).

2. **Request.** `copilot--get-completion` sends a
   `textDocument/inlineCompletion` request with the document URI, cursor
   position (UTF-16), trigger kind (automatic or manual), and formatting options.
   Any pending request is cancelled first via `$/cancelRequest`.

3. **Response.** The server returns a list of completion items. These are
   normalized and cached in the buffer-local `copilot--completion-cache` for
   cycling.

4. **Post-processing.** In Lisp modes, `copilot-balancer-fix-completion` trims
   excess closing delimiters and rebalances parentheses using
   `parse-partial-sexp`.

5. **Display.** `copilot--display-overlay-completion` creates an overlay with
   the completion text styled as ghost text. The overlay carries a keymap
   (`copilot-completion-map`) for acceptance bindings.

6. **Acceptance.** `copilot-accept-completion` inserts the text and sends
   `workspace/executeCommand` for telemetry. Partial acceptance (by word, line,
   sentence, paragraph, or up to a character) inserts a prefix and sends
   `textDocument/didPartiallyAcceptCompletion` instead. The remaining text
   stays visible as a shorter overlay.

Users can cycle through alternatives with `copilot-next-completion` /
`copilot-previous-completion`, which index into the cached response without
making a new server request.

## Extensibility

The core provides two handler registries that extension modules (and user
code) can use to react to server messages:

- **`copilot-on-request`** — registers a handler for an incoming JSON-RPC
  request method. One handler per method. The handler receives the message
  params and must return a response.

- **`copilot-on-notification`** — registers a handler for an incoming JSON-RPC
  notification method. Multiple handlers per method are allowed. Handlers
  receive the message params and return nothing.

Both `copilot-chat.el` and `copilot-nes.el` use these registries to hook into
the server's message stream without modifying the core dispatcher.

Built-in handlers cover `window/showMessageRequest`,
`window/showDocument`, `window/logMessage`, `didChangeStatus`,
`$/progress`, `PanelSolution`, and `PanelSolutionsDone`.

## Chat and Next Edit Suggestions

Chat and NES are self-contained modules that layer on top of the core's server
connection and handler infrastructure.

**Chat** (`copilot-chat.el`) manages conversations through three JSON-RPC
methods: `conversation/create`, `conversation/turn`, and
`conversation/destroy`. Responses stream in via `$/progress` notifications
keyed by a work-done token. The module registers a `conversation/context`
request handler so the server can pull editor context (current file, cursor
position, source text) during a conversation. The chat UI lives in a dedicated
`*copilot-chat*` buffer using a custom major mode.

**Next Edit Suggestions** (`copilot-nes.el`) predicts edits based on recent
editing history. It sends `textDocument/copilotInlineEdit` requests on an idle
timer and displays the suggested edit as a pair of overlays — one showing
deleted text with strikethrough, another showing inserted text. Unlike inline
completions (which only insert at point), NES edits can modify or delete text
anywhere in the file. Acceptance is a two-step process: the first TAB press
jumps to the edit location, the second applies it.

Both modules can be enabled or disabled independently via their respective minor
modes (`copilot-chat` for ad-hoc use, `copilot-nes-mode` as a persistent minor
mode), and neither interferes with the core inline completion flow.
