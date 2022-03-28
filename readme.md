# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Warning:** This plugin is under development.

**Note:** You need access to Copilot technical preview to use this plugin.

## Installation

1. Install [Node.js](https://nodejs.org/en/download/) 12 or newer.

2. Clone this repo.

3. Modify your emacs configuration to load and setup `copilot.el`. (See examples below.)

4. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose`.

5. Enjoy!

## Example Configuration

### Spacemacs + company-mode

Inside your `dotspacemacs/user-config`:

```elisp
; Load copilot.el, modify this path to your local path.
(load-file "~/.emacs.d/copilot.el")

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
  (define-key company-active-map (kbd "<tab>") 'my-tab))

; provide completion when typing
(add-hook 'post-command-hook (lambda ()
                               (copilot-clear-overlay)
                               (when (evil-insert-state-p)
                                 (copilot-complete))))
```

### Straight

```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t
  ;; :hook (kill-emacs . copilot--kill-process)
  :config
  ; provide completion when typing
  (add-hook 'post-command-hook (lambda ()
                                 (copilot-clear-overlay)
                                 (when (evil-insert-state-p)
                                   (copilot-complete)))))
```

## Commands

### copilot-diagnose

Check the current status of the plugin.

### copilot-login

Login to GitHub, required for using the plugin.

### copilot-accept-completion

Accept the current completion. You need to bind this to some key.

### copilot-complete

Try to complete at the current point. You need to hook some function to this.

### copilot-clear-overlay

Clear copilot overlay in the current buffer. You may need to hook some function to this.


## Roadmap

+ [x] Setup Copilot without Neovim
+ [ ] Cycle through suggestions
+ [ ] Add package to MELPA
+ [ ] Test compatibility with vanilla Emacs and other auto completion packages

## Thanks

These projects helped me a lot:

+ https://github.com/TommyX12/company-tabnine/
+ https://github.com/cryptobadger/flight-attendant.el
+ https://github.com/github/copilot.vim
