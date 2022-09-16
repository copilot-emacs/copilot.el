# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Note:** You need access to [GitHub Copilot](https://github.com/features/copilot) to use this plugin.

## Installation

0. Ensure your Emacs version is at least 27.

1. Install [Node.js](https://nodejs.org/en/download/) (only support 12.x to 17.x, limited by upstream). Workaround for node.js v18+ users: install an old version of node.js via [nvm](https://github.com/nvm-sh/nvm#installing-and-updating) and set `copilot-node-executable` to it.

2. Setup `copilot.el` as described in the next section.

3. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose`.

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
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))
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
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)


(add-hook 'prog-mode-hook 'copilot-mode)

(define-key evil-insert-state-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
(define-key evil-insert-state-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
```

</details>

### General Configurations

<details>

#### 1. Load `copilot.el`

##### Option 1: Load via `straight.el` (recommended)


```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot
```


##### Option 2: Load manually

Please make sure you have these dependencies installed, and use `load-file` or `load-path` + `require` to load it.

+ `dash`
+ `s`
+ `editorconfig`


#### 2. Configure completion

##### Option 1: Use `copilot-mode` to automatically provide completions

```elisp
(add-hook 'prog-mode-hook 'copilot-mode)
```

To customize the behavior of `copilot-mode`, please check `copilot-enable-predicates` and `copilot-disable-predicates`.

##### Option 2: Manually provide completions

You need to bind `copilot-complete` to some key and call `copilot-clear-overlay` inside `post-command-hook`.


#### 3. Configure completion acceptation

In general, you need to bind `copilot-accept-completion` to some key in order to accept the completion. Also, you may find `copilot-accept-completion-by-word` is useful.

#### Example of using tab with `company-mode`

```elisp
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
```

#### Example of using tab with `auto-complete`

```elisp
; complete by copilot first, then auto-complete
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (ac-expand nil)))

(with-eval-after-load 'auto-complete
  ; disable inline preview
  (setq ac-disable-inline t)
  ; show menu if have only one candidate
  (setq ac-candidate-menu-min 0))
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
```

#### Example of defining tab in copilot-mode

This is useful if you don't want to depend on a particular completion framework.

```elisp
(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab))
```

Or with evil-mode:
```elisp
(with-eval-after-load 'copilot
  (evil-define-key 'insert copilot-mode-map
    (kbd "<tab>") #'my/copilot-tab))
```

</details>

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

#### copilot-enable-predicates
A list of predicate functions with no argument to enable Copilot in `copilot-mode`. Copilot will be enabled only if all predicates return `t`.

#### copilot-disable-predicates
A list of predicate functions with no argument to disable Copilot in `copilot-mode`. Copilot will be disabled if any predicate returns `t`.

#### copilot-clear-overlay-ignore-commands
A list of commands that will not clear the overlay.

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
+ Please paste related logs in the `*copilot events*` and `*copilot stderr*` buffer.
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
