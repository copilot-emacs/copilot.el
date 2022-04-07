# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Warning:** This plugin is under development. The name of commands and variables may change in the future.

**Note:** You need access to Copilot technical preview to use this plugin.

## Installation

1. Install [Node.js](https://nodejs.org/en/download/) 12 or newer.

2. Install package via `straight.el`, or clone this repository and load `copilot.el` manually. (See examples below.)

3. Modify your emacs configuration to setup `copilot.el`. (See examples below.)

4. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose`.

5. Enjoy!

## Example Configurations

### Load `copilot.el`

#### Load via `straight.el` (recommended)


```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t
  :config
  ; provide completion when programming
  (add-hook 'prog-mode-hook 'copilot-mode))
```

#### Load manually

```elisp
; Load copilot.el, modify this path to your local path.
; Please make sure you have these dependencies installed: dash, s, editorconfig
(load-file "~/.emacs.d/copilot.el")
; provide completion when programming
(add-hook 'prog-mode-hook 'copilot-mode)
```


### Accept completion keybindings

#### with `company-mode`


```elisp
; complete by copilot first, then company-mode
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

; modify company-mode behaviors
(with-eval-after-load 'company
  ; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)
  ; enable tab completion
  (define-key company-mode-map (kbd "<tab>") 'my-tab)
  (define-key company-mode-map (kbd "TAB") 'my-tab)
  (define-key company-active-map (kbd "<tab>") 'my-tab)
  (define-key company-active-map (kbd "TAB") 'my-tab))
```

## Commands

### copilot-diagnose

Check the current status of the plugin.

### copilot-login

Login to GitHub, required for using the plugin.

### copilot-mode

Enable/disable copilot mode.

### copilot-accept-completion

Accept the current completion. You need to bind this to some key.

### copilot-complete

Try to complete at the current point. You need to hook some function to this.

### copilot-clear-overlay

Clear copilot overlay in the current buffer. You may need to hook some function to this.

### copilot-next-completion / copilot-previous-completion

Cycle through the completion list. You may need to bind this to some key.


## Roadmap

+ [x] Setup Copilot without Neovim
+ [x] Cycle through suggestions
+ [x] Add Copilot minor-mode
+ [ ] Add package to MELPA
+ [ ] Test compatibility with vanilla Emacs and other auto completion packages

## Thanks

These projects helped me a lot:

+ https://github.com/TommyX12/company-tabnine/
+ https://github.com/cryptobadger/flight-attendant.el
+ https://github.com/github/copilot.vim
