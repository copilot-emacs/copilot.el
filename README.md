[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/copilot.svg)](https://jcs-emacs.github.io/jcs-elpa/#/copilot)
[![MELPA](http://melpa.org/packages/copilot-badge.svg)](http://melpa.org/#/copilot)
[![MELPA Stable](http://stable.melpa.org/packages/copilot-badge.svg)](http://stable.melpa.org/#/copilot)
[![CI](https://github.com/copilot-emacs/copilot.el/actions/workflows/test.yml/badge.svg)](https://github.com/copilot-emacs/copilot.el/actions/workflows/test.yml)

# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

This plugin uses the official [@github/copilot-language-server][] provided by Microsoft.

> [!NOTE]
>
> You need access to [GitHub Copilot][] to use this plugin. The service introduced a free layer in early 2025.

## Requirements

`copilot.el` requires Emacs 27+ and the following packages (installed automatically from MELPA):

- `editorconfig`
- `jsonrpc`
- `compat`
- `track-changes`

[@github/copilot-language-server][] requires Node.js 22+.

## Quick Start

```elisp
(use-package copilot
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)))
```

Then run `M-x copilot-install-server` and `M-x copilot-login`. That's it!

## Installation

### MELPA

The simplest way to install `copilot.el` is from [MELPA](https://melpa.org/#/copilot):

```elisp
(use-package copilot
  :ensure t)
```

Or `M-x package-install RET copilot RET`.

### Emacs 30+ (use-package :vc)

```elisp
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))
```

### Emacs 27-29 (straight / quelpa)

`straight.el`:

```elisp
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)
```

`quelpa` + `quelpa-use-package`:

```elisp
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el")))
```

### Manual

Clone this repository, make sure the dependencies listed in [Requirements](#requirements) are installed, then:

```elisp
(add-to-list 'load-path "/path/to/copilot.el")
(require 'copilot)
```

### Doom Emacs

<details>

Add package definition to `~/.doom.d/packages.el`:

```elisp
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
```

Configure copilot in `~/.doom.d/config.el`:

```elisp
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
```

Strongly recommend to enable `childframe` option in `company` module (`(company +childframe)`) to prevent overlay conflict.

If pressing tab to complete sometimes doesn't work you might want to bind completion to another key or try:

```elisp
(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))
```

If you would love to configure indentation here, this is an example config that may work for you:

```elisp
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))
```

</details>

### Spacemacs

<details>

Edit your `~/.spacemacs` to include the GitHub Copilot layer this will setup everything for you:

```elisp
;; ===================
;; dotspacemacs/layers
;; ===================

;; add or uncomment the auto-completion layer
;; add the GitHub Copilot layer
dotspacemacs-configuration-layers
'(
  ...
  auto-completion
  github-copilot
  ...
 )
```
For details about the default bindings please refer to the Spacemacs documentation for the
github-copilot layer.

</details>

After installing the package, run `M-x copilot-install-server` to install the language server, then `M-x copilot-login` to authenticate. You can verify everything works with `M-x copilot-diagnose` (`NotAuthorized` means you don't have a valid subscription).

## Configuration

### Completion trigger

Use `copilot-mode` to automatically provide completions in a buffer:

```elisp
(add-hook 'prog-mode-hook 'copilot-mode)
```

Or enable it globally with `global-copilot-mode`:

```elisp
(global-copilot-mode)
```

To customize when completions trigger, see `copilot-enable-predicates` and `copilot-disable-predicates`. To customize when completions are displayed, see `copilot-enable-display-predicates` and `copilot-disable-display-predicates`.

Alternatively, you can call `copilot-complete` manually and use `copilot-clear-overlay` in `post-command-hook` to dismiss completions.

### Next Edit Suggestions (NES)

NES predicts the next edit you'll want to make anywhere in the file, based on your recent editing patterns. Unlike inline completions (ghost text at the cursor), NES suggestions can replace or delete existing text at any location.

> [!NOTE]
>
> NES requires `copilot-language-server` version 1.434.0 or newer. Run `M-x copilot-reinstall-server` to upgrade if needed.

Enable `copilot-nes-mode` in a buffer to start receiving suggestions. It can coexist with `copilot-mode`:

```elisp
(add-hook 'prog-mode-hook #'copilot-nes-mode)
```

When a suggestion is pending:
- **TAB** — accept the suggestion (jumps to it first if far away, applies on second press)
- **C-g** — dismiss the suggestion

Customization variables:
- **`copilot-nes-idle-delay`** — seconds of idle time before requesting a suggestion (default `0.5`)
- **`copilot-nes-auto-dismiss-move-count`** — cursor movements before auto-dismissing (default `3`)
- **`copilot-nes-auto-dismiss-distance`** — max lines between point and suggestion before auto-dismissing (default `40`)

### Keybindings

`copilot-mode` does not set any keybindings by default. Use `copilot-completion-map` (active while a completion overlay is visible) to bind keys:

```elisp
(keymap-set copilot-completion-map "<tab>" #'copilot-accept-completion)
(keymap-set copilot-completion-map "TAB" #'copilot-accept-completion)
(keymap-set copilot-completion-map "C-<tab>" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "C-TAB" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "M-n" #'copilot-next-completion)
(keymap-set copilot-completion-map "M-p" #'copilot-previous-completion)
```

#### Fish-style keybindings

If you use `company-mode` or `corfu`, TAB is already taken. An alternative inspired by Fish shell avoids the conflict entirely — right-arrow accepts, and forward-word/end-of-line accept partially:

```elisp
(keymap-set copilot-completion-map "<right>" #'copilot-accept-completion)
(keymap-set copilot-completion-map "C-f" #'copilot-accept-completion)
(keymap-set copilot-completion-map "M-<right>" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "M-f" #'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "C-e" #'copilot-accept-completion-by-line)
(keymap-set copilot-completion-map "<end>" #'copilot-accept-completion-by-line)
(keymap-set copilot-completion-map "M-n" #'copilot-next-completion)
(keymap-set copilot-completion-map "M-p" #'copilot-previous-completion)
```

#### Zap-style partial acceptance

To remap the built-in zap commands automatically whenever the overlay is visible:

```elisp
(keymap-set copilot-completion-map "<remap> <zap-to-char>" #'copilot-accept-completion-to-char)
(keymap-set copilot-completion-map "<remap> <zap-up-to-char>" #'copilot-accept-completion-up-to-char)
```

### LSP settings

You can configure the underlying LSP settings via `copilot-lsp-settings`. The complete list of available options can be found
[here](https://github.com/github/copilot-language-server-release?tab=readme-ov-file#configuration-management).

Here we set the GitHub Enterprise server to `https://example2.ghe.com`, exchange the URL with your own server.

```elisp
(setopt copilot-lsp-settings '(:github-enterprise (:uri "https://example2.ghe.com"))) ;; allows changing the value without restarting the LSP
(setq copilot-lsp-settings '(:github-enterprise (:uri "https://example2.ghe.com"))) ;; alternatively
```

You have to restart the LSP (`M-x copilot-diagnose`) when using `setq` to change the value. When logging in, the URL for the authentication flow should be the same as the one set in `copilot-lsp-settings`.

### Language detection

Copilot.el detects the programming language of a buffer based on the major-mode
name, stripping the `-mode` part. The resulting languageId should match the table
[here](https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers).
You can add unusual major-mode mappings to `copilot-major-mode-alist`. Without
the proper language set suggestions may be of poorer quality.

```elisp
(add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
```

### Network proxy

Format: `'(:host "127.0.0.1" :port 7890 :username "user" :password "password")`, where `:username` and `:password` are optional.

For example:

```elisp
(setq copilot-network-proxy '(:host "127.0.0.1" :port 7890))
```

### Server-side hooks (copilot-on-request / copilot-on-notification)

`copilot-on-request` registers a handler for incoming LSP requests. Return a JSON-serializable value as the result, or call `jsonrpc-error` for errors. [Read more](https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html).

```elisp
;; Display desktop notification if Emacs is built with D-Bus
(copilot-on-request
 'window/showMessageRequest
 (lambda (msg) (notifications-notify :title "Emacs Copilot" :body (plist-get msg :message))))
```

`copilot-on-notification` registers a listener for incoming LSP notifications.

```elisp
(copilot-on-notification
  'window/logMessage
  (lambda (msg) (message (plist-get msg :message))))
```

## Commands

> [!TIP]
>
> You don't need to memorize the list — just type `M-x copilot-` followed by TAB, or use the Copilot menu in the menubar.

| Command | Description |
|---------|-------------|
| **Setup** | |
| `copilot-install-server` | Install the language server |
| `copilot-uninstall-server` | Remove the installed language server |
| `copilot-reinstall-server` | Re-install the language server |
| `copilot-login` | Log in to GitHub Copilot |
| `copilot-logout` | Log out from GitHub Copilot |
| `copilot-diagnose` | Restart the server and show diagnostic info |
| `copilot-select-completion-model` | Choose which model to use for completions |
| **Completion** | |
| `copilot-mode` | Toggle automatic completions in the current buffer |
| `copilot-complete` | Trigger a completion at point |
| `copilot-panel-complete` | Show a panel buffer with multiple completion suggestions |
| `copilot-accept-completion` | Accept the current completion |
| `copilot-accept-completion-by-word` | Accept the next N words (prefix arg) |
| `copilot-accept-completion-by-line` | Accept the next N lines (prefix arg) |
| `copilot-accept-completion-by-sentence` | Accept the next N sentences (prefix arg) |
| `copilot-accept-completion-by-paragraph` | Accept the next N paragraphs (prefix arg) |
| `copilot-accept-completion-to-char` | Accept through a character (inclusive, like `zap-to-char`) |
| `copilot-accept-completion-up-to-char` | Accept up to a character (exclusive, like `zap-up-to-char`) |
| `copilot-clear-overlay` | Dismiss the completion overlay |
| **Navigation** | |
| `copilot-next-completion` | Cycle to the next completion |
| `copilot-previous-completion` | Cycle to the previous completion |
| **Next Edit Suggestions** | |
| `copilot-nes-mode` | Toggle NES in the current buffer |
| `copilot-nes-accept` | Accept (or jump to) the current NES suggestion |
| `copilot-nes-dismiss` | Dismiss the current NES suggestion |

## Customization

> [!TIP]
>
> Use <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `copilot` to browse all available
> configuration options and their documentation.

A few commonly tweaked variables:

- **`copilot-idle-delay`** — Seconds to wait before triggering completion (default `0`). Set to `nil` to disable automatic completion entirely.
- **`copilot-lsp-server-version`** — Pin a specific [@github/copilot-language-server][] version (`nil` means latest).
- **`copilot-enable-predicates`** / **`copilot-disable-predicates`** — Control when `copilot-mode` requests completions.
- **`copilot-enable-display-predicates`** / **`copilot-disable-display-predicates`** — Control when completions are shown.
- **`copilot-clear-overlay-ignore-commands`** — Commands that won't dismiss the overlay.
- **`copilot-indentation-alist`** — Override indentation width per major mode.
- **`copilot-enable-parentheses-balancer`** — Post-process completions to balance parentheses in Lisp modes (default `t`). Set to `nil` to use raw server completions.

## Protocol Support

`copilot.el` communicates with [@github/copilot-language-server][] over JSON-RPC using the LSP protocol plus Copilot-specific extensions. The table below shows the current coverage.

### Client-to-Server Requests

| Method | Status | Notes |
|--------|--------|-------|
| `initialize` | Supported | Sends `rootUri`, `workspaceFolders`, and editor info |
| `shutdown` | Supported | Clean shutdown before exit |
| `textDocument/inlineCompletion` | Supported | Core completion mechanism |
| `workspace/executeCommand` | Supported | Executes server-side commands on accept |
| `signInInitiate` / `signInConfirm` / `checkStatus` / `signOut` | Supported | Authentication flow |
| `copilot/models` | Supported | Lists available completion models |
| `getPanelCompletions` | Supported | Multiple suggestions in a panel buffer |
| `textDocument/copilotInlineEdit` | Supported | Next Edit Suggestions (NES) |

### Client-to-Server Notifications

| Method | Status | Notes |
|--------|--------|-------|
| `initialized` | Supported | |
| `exit` | Supported | Sent after `shutdown` |
| `textDocument/didOpen` | Supported | |
| `textDocument/didClose` | Supported | |
| `textDocument/didChange` | Supported | Incremental sync |
| `textDocument/didFocus` | Supported | |
| `textDocument/didShowCompletion` | Supported | Telemetry when overlay is displayed |
| `textDocument/didPartiallyAcceptCompletion` | Supported | Telemetry for partial acceptance |
| `textDocument/didShowInlineEdit` | Supported | Telemetry when NES overlay is displayed |
| `workspace/didChangeConfiguration` | Supported | Sent on settings change |
| `workspace/didChangeWorkspaceFolders` | Supported | Dynamic workspace roots |
| `$/cancelRequest` | Supported | Cancels stale completion requests |
| `textDocument/didSave` | Not yet | |
| `notebookDocument/*` | Not yet | |

### Server-to-Client Requests

| Method | Status | Notes |
|--------|--------|-------|
| `window/showMessageRequest` | Supported | Prompts via `completing-read` |
| `window/showDocument` | Supported | Opens URIs in browser or Emacs |

### Server-to-Client Notifications

| Method | Status | Notes |
|--------|--------|-------|
| `window/logMessage` | Supported | Logged to `*copilot-language-server-log*` |
| `didChangeStatus` | Supported | Shown in mode-line |
| `$/progress` | Supported | Progress shown in mode-line |
| `PanelSolution` / `PanelSolutionsDone` | Supported | Panel completion results |

Extensible via `copilot-on-request` and `copilot-on-notification` for any messages not handled above.

## Known Issues

### Wrong Position of Other Completion Popups

![](assets/company-overlay.png)

This is an example of using together with default frontend of
`company-mode`. Because both `company-mode` and `copilot.el` use overlay to show
completion, so the conflict is inevitable.  To solve the problem, I recommend
you to use `company-box` (only available on GUI), which is based on child frame
rather than overlay.

After using `company-box`, you have:

![](assets/company-box.png)

In other editors (e.g. `VS Code`, `PyCharm`), completions from copilot and other sources can not show at the same time.
But I decided to allow them to coexist, allowing you to choose a better one at any time.

### Cursor Jumps to End of Line When Typing

If you are using `whitespace-mode`, make sure to remove `newline-mark` from `whitespace-style`.

## FAQ

### Do I need a paid GitHub Copilot subscription?

Not necessarily. GitHub introduced a [free tier](https://github.com/features/copilot#pricing) for Copilot in early 2025 that includes a limited number of completions per month. A paid subscription (Individual or Business) removes these limits.

### TAB doesn't accept the completion

This is usually caused by another package binding TAB in a way that takes
priority. Common culprits include `company-mode`, `corfu`, `yasnippet`, and Evil
mode. A few things to check:

1. **Bind both `<tab>` and `TAB`.** In GUI Emacs these are different events —
   `<tab>` is the function key and `TAB` is the `C-i` character. Some modes only
   intercept one of them. The [Quick Start](#quick-start) example binds both.
2. **Use the fish-style keybindings.** If TAB is hopelessly taken by another
   package, bind acceptance to `<right>` / `C-f` instead. See
   [Fish-style keybindings](#fish-style-keybindings).
3. **Doom Emacs users** — see the [Doom Emacs](#doom-emacs) installation section
   for a workaround using a custom Evil insert-state binding.

### Can I use `copilot-complete` without enabling `copilot-mode`?

Yes. You can call `M-x copilot-complete` manually in any buffer — it will start
the server and open the document automatically. Use `copilot-clear-overlay` (or
simply type) to dismiss the suggestion. This is useful if you prefer on-demand
completions rather than automatic ones.

### Completions are slow or not appearing

A few things to try:

1. **Run `M-x copilot-diagnose`** — it restarts the server and prints diagnostic
   info. Look for `NotAuthorized` (subscription issue) or connection errors.
2. **Check your network** — the language server needs to reach GitHub's API.  If
   you're behind a proxy, configure `copilot-network-proxy`.
3. **Large files** — buffers over `copilot-max-char` characters (default 100 000)
   are skipped. You can raise the limit, but very large files will always be
   slower.
4. **Tune `copilot-idle-delay`** — the default is `0` (immediate). A small delay
   (e.g. `0.2`) reduces server load when typing quickly.

### How do I disable Copilot in certain modes or buffers?

Use `copilot-disable-predicates` to add functions that return `t` when Copilot
should stay quiet:

```elisp
;; Disable in org-mode
(add-to-list 'copilot-disable-predicates
             (lambda () (derived-mode-p 'org-mode)))
```

Or simply don't add `copilot-mode` to the hooks of modes you want to exclude.
If you use `global-copilot-mode`, the predicate approach is the way to go.

### Parentheses are unbalanced in Lisp completions

Copilot.el includes a parentheses balancer that post-processes completions in
Lisp modes (`emacs-lisp-mode`, `clojure-mode`, `scheme-mode`, etc.) to fix
unbalanced delimiters. It is enabled by default. If you still see issues,
make sure `copilot-enable-parentheses-balancer` is `t`, or file a bug report
with the completion text and buffer context.

If the balancer is causing problems for your workflow, you can disable it:

```elisp
(setopt copilot-enable-parentheses-balancer nil)
```

### How do I select a different completion model?

Run `M-x copilot-select-completion-model` to interactively choose from the
models available on your subscription. The selection is saved in
`copilot-completion-model`. Set it to `nil` to revert to the server default.

## Reporting Bugs

- Make sure you have restarted your Emacs (and rebuild the plugin if necessary) after updating the plugin.
- Please enable event logging by customize `copilot-log-max` (to e.g. 1000) and enable debug log `(setq copilot-server-args '("--stdio" "--debug"))`, then paste related logs in the `*copilot events*`, `*copilot stderr*` and `*copilot-language-server-log*` buffer.
- If an exception is thrown, please also paste the stack trace (use `M-x toggle-debug-on-error` to enable stack trace).

## Development

### Running Tests

Unit tests (requires [eask](https://emacs-eask.github.io/)):

```sh
eask test buttercup
```

### Linting

```sh
eask lint checkdoc
eask lint indent
```

### Integration Testing

There's a manual integration test in `dev/integration-smoke.el` that connects to the real Copilot language server and verifies the `textDocument/inlineCompletion` round-trip. It requires the server to be installed and authenticated.

```sh
emacs --batch -L . -l dev/integration-smoke.el
```

## Thanks

These projects helped me a lot:

- <https://github.com/TommyX12/company-tabnine/>
- <https://github.com/cryptobadger/flight-attendant.el>
- <https://github.com/github/copilot.vim>
- [@github/copilot-language-server][]

## Do you want to chat with GitHub Copilot?

Just like the copilot plugin for Intellij or VS Code?

Please take a look at [copilot-chat.el](https://github.com/chep/copilot-chat.el)

> [!NOTE]
>
> It's possible that chat functionality will be added to `copilot.el` as well down the road. PRs welcome!

## Team

Current maintainer(s): [@bbatsov][], [@emil-vdw][], [@jcs090218][], [@rakotomandimby][].

Retired maintainer: [@zerolfx][].

## License

copilot.el is distributed under the MIT license.

Copyright © 2022-2026 copilot-emacs maintainers and
[contributors](https://github.com/copilot-emacs/copilot.el/contributors).

<!-- Links -->

[@bbatsov]: https://github.com/bbatsov
[@emil-vdw]: https://github.com/emil-vdw
[@jcs090218]: https://github.com/jcs090218
[@rakotomandimby]: https://github.com/rakotomandimby
[@zerolfx]: https://github.com/zerolfx

[GitHub Copilot]: https://github.com/features/copilot
[@github/copilot-language-server]: https://www.npmjs.com/package/@github/copilot-language-server
