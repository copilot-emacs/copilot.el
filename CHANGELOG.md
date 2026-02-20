# Changelog

## master (unreleased)

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
