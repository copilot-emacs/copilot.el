# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Note:** You need access to [GitHub Copilot][] to use this plugin.

Current maintainer: [@emil-vdw][], [@jcs090218][], [@rakotomandimby][].

Retired maintainer: [@zerolfx][].

## Installation

0. Ensure your Emacs version is at least 27, the dependency package `editorconfig` ([melpa](https://melpa.org/#/editorconfig)) and `jsonrpc` ([elpa](https://elpa.gnu.org/packages/jsonrpc.html), >= 1.0.14) are both installed.

1. Install [Node.js][] v18+. (You can specify the path to `node` executable by setting `copilot-node-executable`.)

2. Setup `copilot.el` as described in the next section.

3. Install the copilot server by `M-x copilot-install-server`.

4. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose` (`NotAuthorized` means you don't have a valid subscription).

5. Enjoy!

## Configurations

### Example for Doom Emacs 

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
```
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
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))
```

</details>

### Example for Spacemacs

<details>

Edit your `~/.spacemacs`:

```elisp
;; ===================
;; dotspacemacs/layers
;; ===================

;; add or uncomment the auto-completion layer
dotspacemacs-configuration-layers
'(
  ...
  auto-completion
  ...
 )

;; add copilot.el to additional packages
dotspacemacs-additional-packages
 '((copilot :location (recipe
                       :fetcher github
                       :repo "copilot-emacs/copilot.el"
                       :files ("*.el"))))

;; ========================
;; dotspacemacs/user-config
;; ========================

;; accept completion from copilot and fallback to company

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))

(add-hook 'prog-mode-hook 'copilot-mode)
```

</details>

### General Configurations

<details>

#### 1. Load `copilot.el`

##### Option 1: Load via use-package (recommended)

###### Emacs 27-29:

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

###### On Emacs version 30+:

```elisp
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))
```

Use `:map` `:hook` and `:config` to customize `copilot.el` via `use-package`.

##### Option 3: Load manually

Please make sure you have these dependencies installed (available in ELPA/MELPA):

+ `dash`
+ `s`
+ `editorconfig`
+ `f`

After installing those, clone this repository then insert the below snippet into your config file.

```elisp
(add-to-list 'load-path "/path/to/copilot.el")
(require 'copilot)
```

#### 2. Configure completion

##### Option 1: Use `copilot-mode` to automatically provide completions

```elisp
(add-hook 'prog-mode-hook 'copilot-mode)
```

To customize the behavior of `copilot-mode`, please check `copilot-enable-predicates` and `copilot-disable-predicates`.

##### Option 2: Manually provide completions

You need to bind `copilot-complete` to some key and call `copilot-clear-overlay` inside `post-command-hook`.


#### 3. Configure completion acceptation

Use tab to accept completions (you may also want to bind `copilot-accept-completion-by-word` to some key):

```elisp
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
```

</details>

### Programming language detection

Copilot.el detects the programming language of a buffer based on the major-mode name, stripping the `-mode` part. Resulting languageId should match table [here](https://code.visualstudio.com/docs/languages/identifiers#_known-language-identifiers).
You can add unusual major-mode mappings to `copilot-major-mode-alist`. Without the proper language set suggestions may be of poorer quality.

```elisp
(add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
```

## Commands

#### copilot-diagnose

Check the current status of the plugin. Also you can check logs in the `*copilot events*` buffer and stderr output in the `*copilot stderr*` buffer.

#### copilot-login

Login to GitHub, required for using the plugin.

#### copilot-mode

Enable/disable copilot mode.

#### copilot-complete

Try to complete at the current point.

#### copilot-accept-completion

Accept the current completion.

#### copilot-clear-overlay

Clear copilot overlay in the current buffer.

#### copilot-accept-completion-by-line / copilot-accept-completion-by-word

Similar to `copilot-accept-completion`, but accept the completion by line or word. You can use prefix argument to specify the number of lines or words to accept.

#### copilot-next-completion / copilot-previous-completion

Cycle through the completion list.

#### copilot-logout

Log out from GitHub.

## Customization

#### copilot-node-executable

The executable path of [Node.js][].

#### copilot-idle-delay

Time in seconds to wait before starting completion (default to 0). Note Copilot itself has a ~100ms delay because of network communication.

#### copilot-enable-predicates / copilot-disable-predicates
A list of predicate functions with no argument to enable/disable triggering Copilot in `copilot-mode`.

#### copilot-enable-display-predicates / copilot-disable-display-predicates
A list of predicate functions with no argument to enable/disable showing Copilot's completions in `copilot-mode`.

#### copilot-clear-overlay-ignore-commands
A list of commands that won't cause the overlay to be cleared.

#### copilot-network-proxy

Format: `'(:host "127.0.0.1" :port 7890 :username: "user" :password: "password")`, where `:username` and `:password` are optional.

For example:
```elisp
(setq copilot-network-proxy '(:host "127.0.0.1" :port 7890))
```

## Known Issues

### Wrong Position of Other Completion Popups

![](assets/company-overlay.png)

This is an example of using together with default frontend of `company-mode`. Because both `company-mode` and `copilot.el` use overlay to show completion, so the conflict is inevitable.
To solve the problem, I recommend you to use `company-box` (only available on GUI), which is based on child frame rather than overlay.

After using `company-box`, you have:

![](assets/company-box.png)

In other editors (e.g. `VS Code`, `PyCharm`), completions from copilot and other sources can not show at the same time.
But I decided to allow them to coexist, allowing you to choose a better one at any time.

### Cursor Jump to End of Line When Typing

+ If you are using `whitespace-mode`, make sure to remove `newline-mark` from `whitespace-style`.

## Reporting Bugs

+ Make sure you have restarted your Emacs (and rebuild the plugin if necessary) after updating the plugin.
+ Please enable event logging by customize `copilot-log-max` (to e.g. 1000), then paste related logs in the `*copilot events*` and `*copilot stderr*` buffer.
+ If an exception is thrown, please also paste the stack trace (use `M-x toggle-debug-on-error` to enable stack trace).

## Roadmap

+ [x] Setup Copilot without Neovim
+ [x] Cycle through suggestions
+ [x] Add Copilot minor-mode
+ [ ] ~~Add package to MELPA~~

## Thanks

These projects helped me a lot:

+ https://github.com/TommyX12/company-tabnine/
+ https://github.com/cryptobadger/flight-attendant.el
+ https://github.com/github/copilot.vim


<!-- Links -->

[@emil-vdw]: https://github.com/emil-vdw
[@jcs090218]: https://github.com/jcs090218
[@rakotomandimby]: https://github.com/rakotomandimby
[@zerolfx]: https://github.com/zerolfx

[GitHub Copilot]: https://github.com/features/copilot
[Node.js]: https://nodejs.org/en/download/
