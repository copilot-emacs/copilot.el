# Changelog

## main (unreleased)

### Changes

- `copilot-login` waits longer for the language server to authenticate with GitHub, and when it times out it points you at proxy/TLS setup (`copilot-network-proxy`, `NODE_EXTRA_CA_CERTS`) instead of only reporting `Authentication failure: Timed out`, which reads like a credential problem when it is usually a TLS-inspecting proxy or firewall.
- `copilot-login` now stops early with a clear message when the language server returns an empty device code (a sign the request to GitHub was blocked by a proxy or firewall), rather than copying an empty code and timing out later.

## 0.9.0 (2026-07-06)

### New Features

- Add `copilot-chat-compose` (bound to `C-c C-e` in the chat buffer) to draft a multi-line message in a dedicated writable buffer, sending it with `C-c C-c` (as a new conversation or a follow-up, just like `copilot-chat`) or cancelling with `C-c C-k`.
- Add `copilot-chat-display` (bound to `C-c C-d` in the chat buffer) to show the existing chat buffer without sending a message.
- Add commands for acting on a chat response, bound in the `*copilot-chat*` buffer:
  - `copilot-chat-copy-response` (`C-c C-w`) copies the most recent reply to the kill ring.
  - `copilot-chat-next-turn` (`C-c C-n`) and `copilot-chat-previous-turn` (`C-c C-p`) move between exchanges.
  - `copilot-chat-retry` (`C-c C-r`) regenerates the answer to the last request, or with a prefix argument pre-fills it for editing before re-sending.
  - `copilot-chat-rate-response` (`C-c C-t`), along with `copilot-chat-thumbs-up` and `copilot-chat-thumbs-down`, sends good/bad feedback about the current response to GitHub.
- Show an animated thinking indicator under the `Copilot:` label while a reply is being prepared, so the gap before the first streamed chunk no longer looks dead; it disappears the moment the reply starts (or the turn ends, is cancelled, or errors) and is disabled with `copilot-chat-show-thinking-indicator`.
- Keep manual scrollback sticky while a reply streams: a chat window is only auto-scrolled to follow new output when it is already at the bottom, so scrolling up to re-read an earlier part of a long response no longer fights the stream.
- Add `copilot-panel-accept-completion` to insert a solution from the `*copilot-panel*` buffer back into the buffer that requested it. Move to a suggestion and press `RET` or `C-c C-c`; previously the panel could only be read, never accepted from.
- Add `copilot-chat-compact` (in the `copilot-menu` chat section) to compact the current chat conversation on demand: it asks the server to summarize the discussion so far, reclaiming token budget while the conversation continues. The visible transcript is left as-is, and it reports how many turns were compacted along with the resulting context usage.
- `copilot-chat-select-model` now flags premium and policy-locked models in the completion list, and selecting a policy-locked one prompts to accept its terms and enables it on the server (via `copilot/setModelPolicy`) before switching, instead of leaving you to hit an error on the next message.
- Add Bring Your Own Key (BYOK) support, letting you use your own model-provider API keys (Anthropic, OpenAI, Gemini, Groq, OpenRouter, Azure) in chat:
  - `copilot-chat-byok-add-key` saves a provider API key (read without echoing and held by the language server, never stored or shown by copilot.el), and `copilot-chat-byok-add-model` registers a model to use with it.
  - `copilot-chat-byok-add-custom-provider` registers a custom OpenAI-compatible endpoint (any provider name that is not built in, with its API type and key); `copilot-chat-byok-add-model` then registers models for it (each with its deployment URL), and `copilot-chat-byok-list-custom-providers` shows the ones you have configured.
  - `copilot-chat-byok-list`, `copilot-chat-byok-remove-model`, and `copilot-chat-byok-remove-key` manage what you have registered.
  - `copilot-chat-select-model` lists your registered BYOK models (tagged `[BYOK: PROVIDER]`) alongside Copilot's own, and selecting one routes chat turns to that provider using your key.
  - Using a BYOK model needs a BYOK-eligible Copilot account; otherwise the server rejects the turn.

### Changes

- `copilot-mode` now binds `TAB` (accept the suggestion), `C-TAB` (accept by word), and `M-n`/`M-p` (cycle suggestions) in `copilot-completion-map`, so inline completion works out of the box. The bindings act only while a suggestion overlay is visible; rebind or clear the map to change them.
- `copilot-chat-insert-commit-message` now uses the language server's native commit-message generator, which also sees your recent commit subjects (so the result matches your repository's style) and the repository's commit instructions. On a server too old to provide it, it falls back to the previous one-shot chat, so `copilot-chat-commit-message-prompt` still applies there.

## 0.8.0 (2026-07-04)

### New Features

- Add `copilot-chat-select-mode` to choose the chat mode reported by the server (`Ask`, `Agent`, `InlineAgent`, and any custom project modes) instead of only the agent on/off toggle. `InlineAgent` is an agent-kind mode with a restricted tool set aimed at inline editing; the tool-call confirmation, tool registration, and status header apply to it as they do to `Agent`. The selection takes effect on the next new conversation, and without one the existing `copilot-chat-use-agent-mode` toggle still decides between Agent and Ask.
- Add `copilot-chat-presets`, named bundles of chat settings (model, agent mode, auto-approved tools) that `copilot-chat-apply-preset` switches between in one step. A preset only touches the settings it lists; its model takes effect on your next message and its agent mode on the next new conversation.
- Show a status header line in the chat buffer with the chat mode (Agent or Ask), the active model, and in agent mode the number of available tools; disable it with `copilot-chat-show-status-header`. ([#509](https://github.com/copilot-emacs/copilot.el/discussions/509))
- Add one-shot chat task commands that send the active region (or the defun at point) with a canned prompt, with the answer streaming into the regular chat buffer. The prompts are customizable via `copilot-chat-task-prompts`, and `copilot-chat-task` picks a task with completion:
  - `copilot-chat-review`
  - `copilot-chat-fix`
  - `copilot-chat-doc`
  - `copilot-chat-optimize`
  - `copilot-chat-write-tests`
- Add `copilot-chat-rewrite` to rewrite the active region according to a free-form instruction, with the proposed change previewed as a diff and applied only after confirmation. The instruction preamble is customizable via `copilot-chat-rewrite-prompt`, re-indentation of the result can be disabled with `copilot-chat-rewrite-indent`, and a pending rewrite is cancellable with `copilot-chat-stop`.
- Add native Copilot Code Review support (the same reviewer behind GitHub's code review, via the language server's `copilot/codeReview` methods), with the resulting comments rendered in the chat buffer with file, line, and suggested changes:
  - `copilot-chat-review-changes` reviews the uncommitted changes (staged and unstaged)
  - `copilot-chat-review-region` reviews the selected code
- Add `copilot-chat-insert-commit-message` to generate a commit message from the staged changes and insert it at point (handy in a Magit or `git-commit` buffer), with the instruction sent to Copilot customizable via `copilot-chat-commit-message-prompt` and a pending generation cancellable with `copilot-chat-stop`.
- Optionally persist chat sessions across Emacs restarts: with `copilot-chat-save-history` enabled (off by default) the transcript is saved after each turn to a per-workspace file under `copilot-chat-history-directory`, `copilot-chat-restore` brings the saved conversation back with its full context, and `copilot-chat-clear-history` deletes the saved file.
- Handle the server's `copilot/codingAgentMessage` request, so updates from GitHub's cloud coding agent (e.g. the pull request it opens to continue delegated work) are echoed and recorded in the `*copilot-coding-agent*` buffer instead of being dropped.
- Add `copilot-menu`, a transient (magit-style) menu that gathers the most common Copilot commands (completions, chat, agent mode, account and usage info, and server management) and shows the current state of `copilot-mode`, agent mode, and the selected chat model. Requires the `transient` package (bundled with Emacs 28.1 and newer) but only when the menu itself is used.
- Add an `edit` choice at the agent-mode tool-confirmation prompt for the tools copilot.el runs itself, letting you tweak the input before it runs: the shell command of `run_in_terminal`, the content of `create_file` (in a temporary buffer), or the URLs of `fetch_web_page`. Server-executed and MCP tools keep the plain yes/no/always prompt, since the server never reads a modified input back.
- Raise a desktop notification when a chat turn finishes, if it ran at least `copilot-chat-notify-after-seconds` (10 by default; nil disables) and the chat buffer is not the one you are looking at. The backend prefers D-Bus, falls back to `osascript` on macOS and to `message` elsewhere, and is overridable via `copilot-chat-notify-function`.
- Add `copilot-chat-frontend`, letting you render the `*copilot-chat*` buffer as Org instead of the default GitHub-flavored Markdown. Set it to `org` to write each exchange as foldable Org headings (`* You` / `** Copilot`) with Org highlighting; this needs the `org` library. The choice applies to new chat buffers, and the streamed assistant body stays markdown-ish (fenced code blocks and all) in either frontend.

### Bug Fixes

- Send the selected chat model on every conversation turn, not just the first one, so follow-up messages no longer fail with "A model id is required" on recent language-server versions. The model is now sent via the modern `modelInfo` field, with the deprecated `model` field kept as a fallback for older servers. ([#508](https://github.com/copilot-emacs/copilot.el/issues/508))

### Changes

- Rename the public-code matching feature to use "references" consistently (matching GitHub's own "code references" terminology): `copilot-show-code-citations` is now `copilot-show-code-references` and `copilot-list-code-citations` is now `copilot-list-code-references`. The old names from 0.7.0 still work as obsolete aliases. ([#496](https://github.com/copilot-emacs/copilot.el/pull/496))
- Give user-facing messages a consistent `Copilot:`/`Copilot Chat:` prefix, and describe attached chat context as "context" throughout (the `copilot-chat-add-file-reference` family keeps its names, which match the server's `references` field).
- Warn when starting an agent-mode chat with a model that can't call tools, so it's clear why Copilot describes commands to run instead of running them itself. ([#509](https://github.com/copilot-emacs/copilot.el/discussions/509))

## 0.7.0 (2026-06-26)

### New Features

Agent mode tooling:

- Add MCP (Model Context Protocol) server support for agent mode via the new `copilot-mcp-servers` option. Configured servers are forwarded to the language server and their tools become available in `copilot-chat`, each prompting for confirmation like the built-in tools.
- Add `copilot-chat-list-mcp-tools` to see the connected MCP servers, their status, and their tools, and warn when an MCP server fails to start (previously silent).
- Preview file changes in a temporary buffer before confirming an agent-mode edit tool (`create_file`, `insert_edit_into_file`, `replace_string_in_file`), so you can see what will be written before approving. Controlled by `copilot-chat-preview-tool-edits` (default on).
- Offer an `always` choice at agent-mode tool confirmation prompts, alongside yes and no, to approve a tool for the rest of the conversation instead of confirming each call. The choice is remembered until a new conversation starts.
- Run the agent-mode `run_in_terminal` tool asynchronously instead of blocking Emacs for the whole command. The editor stays responsive, the language-server connection keeps flowing, `C-g` aborts a running command, and `copilot-chat-terminal-timeout` (default 30s) kills runaways. The command's exit status is now reported back to the model.
- Show a status line in the chat buffer when Copilot runs one of its own built-in or MCP tools that asks for confirmation (`read_file`, the edit tools, etc.), so those tool calls leave a trace, not just the ones copilot.el executes locally.

Workspace access for agent mode:

- Answer the server's `workspace/findFiles` and `workspace/findTextInFiles` requests so agent mode can search the project by file glob and by text/regex (via ripgrep), a step toward whole-workspace questions.
- Answer the server's `workspace/readFile` and `workspace/readDirectory` requests so agent mode can read files and list directories across the project.
- Add opt-in semantic search (`copilot-chat-enable-semantic-search`): declare the `watchedFiles` capability and answer the server's `copilot/watchedFiles` request with the project's file list, so the server can build a workspace index for whole-codebase questions.

Chat usability:

- Add `copilot-chat-slash-command` (`C-c /`) to pick and send a chat slash command (`/explain`, `/fix`, `/tests`, `/doc`, etc.) fetched from the server, with optional arguments.
- Act on chat code blocks: `copilot-chat-insert-code-block` (`C-c C-i`) inserts the block at point into the source buffer, and `copilot-chat-copy-code-block` (`C-c M-w`) copies it to the kill ring, instead of leaving chat output as read-only text.
- Attach extra context to a chat message: `copilot-chat-add-file-reference` (`C-c C-f`) and `copilot-chat-add-region-reference` send specific files or selections along with the next message, and `copilot-chat-clear-references` drops anything pending.

Account and usage:

- Track Copilot usage quota: surface the server's quota warnings and add `copilot-quota` to show how much of your chat, completion, and premium-request allowance is left.
- Announce when a suggestion matches public code, and collect the matches (with licenses and reference URLs) in a buffer shown by `copilot-list-code-citations`. Controlled by `copilot-show-code-citations` (default on). ([#471](https://github.com/copilot-emacs/copilot.el/issues/471))

### Changes

- Show readable agent-mode confirmation prompts for the server's own tools (`read_file`, `insert_edit_into_file`, `replace_string_in_file`, etc.) instead of a raw plist dump, falling back to the server-provided message, then the raw input, for any tool we don't recognize. `copilot-chat-auto-approve-tools` is matched exactly so it never auto-approves a same-named tool from a different namespace.

### Bug Fixes

- Fix agent-mode tool confirmations always failing (so Copilot Chat couldn't read files, edit, etc.) by returning the `(:result "accept")`/`(:result "dismiss")` shape the server expects instead of a bare string. ([#483](https://github.com/copilot-emacs/copilot.el/issues/483))
- Resolve a concrete default chat model from the server when `copilot-chat-model` is nil (preferring the server's designated chat default, then an `auto` model) instead of leaving the model unset, which some servers answer with an empty reply. The lookup runs once per session, only against an already-running server. ([#473](https://github.com/copilot-emacs/copilot.el/issues/473))
- Surface a turn-level error reported by the server at the end of a chat turn, instead of leaving only an empty reply. ([#473](https://github.com/copilot-emacs/copilot.el/issues/473))
- Stop the `copilot--infer-indentation-offset` warning from firing while generating chat context, so chatting from a buffer whose mode has no configured indentation offset no longer nags. ([#473](https://github.com/copilot-emacs/copilot.el/issues/473))
- Report Flycheck diagnostics for the agent-mode `get_errors` tool, not just Flymake, so Flycheck users get real diagnostics instead of "no diagnostics available".

## 0.6.0 (2026-06-22)

### New Features

- Hide regular `copilot-mode` completions while a NES suggestion is pending, so the two no longer overlap on screen. ([#477](https://github.com/copilot-emacs/copilot.el/issues/477))
- Add experimental Agent mode for Copilot Chat (`copilot-chat-use-agent-mode`). When enabled, Copilot can run client-side tools (`run_in_terminal`, `create_file`, `get_errors`, `fetch_web_page`); each invocation prompts for confirmation unless listed in `copilot-chat-auto-approve-tools`. ([#441](https://github.com/copilot-emacs/copilot.el/issues/441))
- Add native binary installation support. `copilot-install-server` now falls back to downloading precompiled binaries when npm is unavailable, removing the Node.js requirement on supported platforms. A new `copilot-install-server-native` command is also available for explicit native installation.
- Add `copilot-chat-select-model` for interactively choosing a chat model, since many more models are available for chat than for completions. ([#465](https://github.com/copilot-emacs/copilot.el/discussions/465))

### Changes

- NES now detects buffer edits via `track-changes` instead of a hardcoded list of editing commands, so it reacts to edits from any command (including `evil` and other non-listed commands). ([#477](https://github.com/copilot-emacs/copilot.el/issues/477))
- `copilot-chat` now only attaches the originating buffer as context when it is a file-visiting buffer, so invoking chat from an unrelated buffer (dired, `*scratch*`, etc.) no longer sends it as context. ([#470](https://github.com/copilot-emacs/copilot.el/issues/470))

### Bug Fixes

- Dismiss a pending NES suggestion immediately when the buffer is edited (e.g. after accepting a `copilot-mode` completion), instead of leaving a misplaced overlay behind. ([#477](https://github.com/copilot-emacs/copilot.el/issues/477))
- Clear NES overlays before applying an accepted suggestion so the ghost text no longer lingers over the edit until the next command. ([#477](https://github.com/copilot-emacs/copilot.el/issues/477))
- Tell the user to enable `copilot-mode` when `copilot-nes-mode` is turned on without it, since NES relies on `copilot-mode` to start and sync the server. ([#477](https://github.com/copilot-emacs/copilot.el/issues/477))
- Fix Emacs hanging on exit when shutting down the Copilot server (the blocking `jsonrpc-shutdown` is now skipped at `kill-emacs` time). ([#469](https://github.com/copilot-emacs/copilot.el/issues/469))
- Fix NES insertion text never rendering because its zero-width overlay was marked `evaporate` and got deleted immediately. ([#451](https://github.com/copilot-emacs/copilot.el/issues/451))
- Suppress "Request was canceled" error messages in the echo area. ([#464](https://github.com/copilot-emacs/copilot.el/pull/464))

## 0.5.0 (2026-03-16)

### New Features

- Add Copilot Chat support (`copilot-chat`, `copilot-chat-send`, `copilot-chat-send-region`, `copilot-chat-reset`) using the `conversation/*` LSP methods. ([#446](https://github.com/copilot-emacs/copilot.el/pull/446))
- Add Next Edit Suggestions (`copilot-nes-mode`) via `textDocument/copilotInlineEdit`. NES predicts edits anywhere in the file based on recent editing patterns. ([#447](https://github.com/copilot-emacs/copilot.el/pull/447))
- Add `copilot-chat-stop` command to cancel in-flight streaming.
- Show mode-line streaming status indicator for chat.
- Display follow-up suggestions after chat responses.
- Show errors inline in the chat buffer.
- Use `markdown-mode` GFM font-lock in the chat buffer when available.
- Add Chat, NES, Panel Complete, Clear Overlay, Select Completion Model, and Reinstall Server entries to the Copilot mode menu.
- Add GitHub issue templates for bug reports and feature requests.

### Changes

- Add a default error handler to `copilot--async-request` so async failures (e.g. cancelled completions) are logged to `*Messages*` instead of being silently swallowed.
- Remove unused `copilot-server-log-level` defcustom (dead code — nothing read it).
- Improve `copilot-log-max` docstring to explain the `*copilot events*` buffer.
- Convert `copilot--satisfy-predicates` from macro to function for simplicity.

### Bug Fixes

- Fix chat `conversation/create` sending `allSkills` as a string array instead of a boolean, which caused a schema validation error on `copilot-language-server` v1.436.0+. ([#452](https://github.com/copilot-emacs/copilot.el/issues/452))
- Fix `copilot-complete` being immediately cancelled when called from wrapper commands (lambdas, user-defined functions) whose name doesn't start with `copilot-`. ([#453](https://github.com/copilot-emacs/copilot.el/issues/453))
- Fix chat not displaying replies from newer server versions. ([#459](https://github.com/copilot-emacs/copilot.el/issues/459))
- Fix `:json-false` treated as truthy for `external` in `window/showDocument` handler.
- Normalize `:json-false` busy flag in `didChangeStatus` handler. ([#457](https://github.com/copilot-emacs/copilot.el/issues/457))
- Guard against `:json-null` in server responses.
- Clear `copilot--completion-initiated-p` flag in idle timer path to prevent stale state. ([#454](https://github.com/copilot-emacs/copilot.el/issues/454))
- Suppress `copilot-max-char` warning when `copilot-max-char-warning-disable` is non-nil.

## 0.4.0 (2026-02-26)

### New Features

- Send workspace folders (`rootUri` and `workspaceFolders`) during LSP initialization and dynamically notify the server when new project roots are encountered. This improves suggestion quality for multi-root workspaces.
- Add `copilot-completion-model` option and `copilot-select-completion-model` command for choosing the AI model used for completions. ([#382](https://github.com/copilot-emacs/copilot.el/issues/382))
- Add `copilot-enable-parentheses-balancer` option to control whether Lisp completions are post-processed for balanced delimiters (enabled by default).
- Add `copilot-accept-completion-by-sentence` command.
- Add `copilot-accept-completion-up-to-char` and `copilot-accept-completion-to-char` commands, similar to `zap-up-to-char` and `zap-to-char`.
- Add `copilot-clear-overlay-on-commands` user option to clear the overlay before specific commands (e.g. `beginning-of-visual-line`).
- Handle `window/showMessageRequest` server requests via `completing-read` and `window/showDocument` to open URIs in the browser or Emacs.
- Handle `didChangeStatus` server notifications to show Copilot status (Normal, Warning, Error, Inactive) in the mode-line.
- Handle `$/progress` server notifications to display progress for long-running operations (e.g. indexing) in the mode-line.
- Send `$/cancelRequest` when a new completion is requested or the overlay is dismissed, so the server stops working on stale in-flight requests.
- Perform a clean LSP shutdown sequence (`shutdown` request + `exit` notification) instead of just killing the server process.
- Add `copilot-on-request` and `copilot-on-notification` for user-defined handlers on incoming server messages.
- Add buttercup test infrastructure with comprehensive tests for core functionality.

### Changes

- `copilot--lsp-settings-changed` now sends `workspace/didChangeConfiguration` instead of restarting the server.
- Remove unnecessary `:dummy` placeholder parameters from JSON-RPC requests.
- Migrate from legacy `getCompletions` API to standard `textDocument/inlineCompletion` for compatibility with newer Copilot language server versions. Adds partial acceptance telemetry via `textDocument/didPartiallyAcceptCompletion`.
- Rename `copilot-version` to `copilot-lsp-server-version` (`copilot-version` is now an obsolete alias).
- Replace the `f` library dependency with `compat`.
- Hardcode `universal-argument`, `digit-argument`, `negative-argument`, and `universal-argument-more` as commands that should not clear the overlay.
- Honor `copilot-clear-overlay-ignore-commands` for remapped commands (e.g. `universal-argument-more`).

### Bug Fixes

- Rewrite `copilot-balancer` to use `parse-partial-sexp` so that parens inside comments and strings are handled correctly. ([#440](https://github.com/copilot-emacs/copilot.el/issues/440))
- Fix balancer dropping closing delimiters inside comments/strings when the server returns a replacement range covering them. ([#449](https://github.com/copilot-emacs/copilot.el/issues/449))
- Fix `copilot-complete` not working without `copilot-mode` until `copilot-diagnose` is run, by sending `textDocument/didOpen` automatically. ([#450](https://github.com/copilot-emacs/copilot.el/issues/450))
- Fix partial accept-by-word losing the replacement range tail, which caused suffix text to be deleted prematurely. ([#448](https://github.com/copilot-emacs/copilot.el/issues/448))
- Send an empty JSON object instead of omitting `params` in JSON-RPC requests, fixing `signInInitiate` and other calls on newer language server versions. ([#445](https://github.com/copilot-emacs/copilot.el/issues/445))
- Fix `copilot--lsp-pos` to use UTF-16 code unit offsets instead of Emacs character counts. Characters above U+FFFF (e.g. emoji) are now correctly reported as 2 UTF-16 code units.
- Fix `textDocument/didFocus` and `textDocument/didOpen` notification names to use plain symbols instead of keyword symbols (which were serialized with a leading colon).
- Fix `PanelSolution` handler to use `(goto-char (point-min))` instead of `(call-interactively #'mark-whole-buffer)`, which was activating the mark and interfering with user state.
- Fix `copilot--get-relative-path` to use `fboundp` instead of `boundp` for checking `vc-root-dir`.
- Use `tab-width` instead of `standard-indent` as the fallback indentation offset for modes not in `copilot-indentation-alist`. ([#312](https://github.com/copilot-emacs/copilot.el/issues/312))
- Set priority on the copilot completion overlay to avoid conflicts with Emacs 30's `completion-preview-mode`. ([#377](https://github.com/copilot-emacs/copilot.el/issues/377))
- Filter out minibuffers and internal buffers in `global-copilot-mode`. ([#337](https://github.com/copilot-emacs/copilot.el/issues/337))
- Don't warn about `copilot-max-char` in temporary and non-file-visiting buffers. ([#313](https://github.com/copilot-emacs/copilot.el/issues/313))
- Handle `events-buffer-scrollback-size` deprecation in newer jsonrpc versions gracefully. ([#329](https://github.com/copilot-emacs/copilot.el/issues/329))
- Remove the `company-mode` dependency that could cause `void-function company--active-p` errors. ([#243](https://github.com/copilot-emacs/copilot.el/issues/243))
- Don't start the Copilot server in `copilot--on-doc-close` when the connection is not alive. ([#265](https://github.com/copilot-emacs/copilot.el/issues/265))
- Add linting CI job and fix checkdoc and indentation lint warnings.
