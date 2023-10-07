# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Note:** You need access to [GitHub Copilot](https://github.com/features/copilot) to use this plugin.

Current maintainer: [@emil-vdw](https://github.com/emil-vdw)

## Installation

0. Ensure your Emacs version is at least 27, and the dependency package `editorconfig` ([melpa](https://melpa.org/#/editorconfig)) is also installed.

1. Install [Node.js](https://nodejs.org/en/download/) v16+. (You can specify the path to `node` executable by setting `copilot-node-executable`.)

2. Setup `copilot.el` as described in the next section.

3. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose` (`NotAuthorized` means you don't have a valid subscription).

4. Enjoy!

## Configurations

### Example for Doom Emacs 

<details>

Add package definition to `~/.doom.d/packages.el`:

```elisp
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
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
                       :repo "zerolfx/copilot.el"
                       :files ("*.el" "dist"))))

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

##### Option 1: Load via `straight.el` or `quelpa` (recommended)

###### `straight.el`:
  
```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot
```
  
###### `quelpa` + `quelpa-use-package`:
  
```elisp
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))
;; you can utilize :map :hook and :config to customize copilot
```

##### Option 2: Load manually

Please make sure you have these dependencies installed (available in ELPA/MELPA):

+ `dash`
+ `s`
+ `editorconfig`

After installing those, clone this repository then insert the below snippet into your config file.

```
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

Logout from GitHub.

## Customization

#### copilot-node-executable

The executable path of Node.js.

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
