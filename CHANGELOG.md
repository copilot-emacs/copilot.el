# Changelog

## master (unreleased)

### New Features

- Add `copilot-accept-completion-by-sentence` command.
- Add `copilot-accept-completion-up-to-char` and `copilot-accept-completion-to-char` commands, similar to `zap-up-to-char` and `zap-to-char`.
- Add `copilot-clear-overlay-on-commands` user option to clear the overlay before specific commands (e.g. `beginning-of-visual-line`).
- Add buttercup test infrastructure with comprehensive tests for core functionality.

### Changes

- Rename `copilot-version` to `copilot-lsp-server-version` (`copilot-version` is now an obsolete alias).
- Replace the `f` library dependency with `compat`.
- Hardcode `universal-argument`, `digit-argument`, `negative-argument`, and `universal-argument-more` as commands that should not clear the overlay.
- Honor `copilot-clear-overlay-ignore-commands` for remapped commands (e.g. `universal-argument-more`).

### Bug Fixes

- Use `tab-width` instead of `standard-indent` as the fallback indentation offset for modes not in `copilot-indentation-alist`. ([#312](https://github.com/copilot-emacs/copilot.el/issues/312))
- Set priority on the copilot completion overlay to avoid conflicts with Emacs 30's `completion-preview-mode`. ([#377](https://github.com/copilot-emacs/copilot.el/issues/377))
- Filter out minibuffers and internal buffers in `global-copilot-mode`. ([#337](https://github.com/copilot-emacs/copilot.el/issues/337))
- Don't warn about `copilot-max-char` in temporary and non-file-visiting buffers. ([#313](https://github.com/copilot-emacs/copilot.el/issues/313))
- Handle `events-buffer-scrollback-size` deprecation in newer jsonrpc versions gracefully. ([#329](https://github.com/copilot-emacs/copilot.el/issues/329))
- Remove the `company-mode` dependency that could cause `void-function company--active-p` errors. ([#243](https://github.com/copilot-emacs/copilot.el/issues/243))
- Don't start the Copilot server in `copilot--on-doc-close` when the connection is not alive. ([#265](https://github.com/copilot-emacs/copilot.el/issues/265))
- Add linting CI job and fix checkdoc and indentation lint warnings.
