# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Warning:** This plugin is under development.

**Note:** You need access to Copilot technical preview to use this plugin.

## Installation

1. Install Neovim and [copilot.vim](https://github.com/github/copilot.vim) plugin.
Follow instructions in copilot.vim to setup copilot (run `:Copilot setup` inside Neovim).

2. Clone this repo.

3. Copy `copilot.vim/copilot/dist` directory to `copilot.el` repo root.

4. Modify your emacs configuration to load and setup `copilot.el`

```elisp
;; Configuration example for Spacemacs (inside your user-config):

;; Load copilot.el, modify this path to your local path.
(load-file "~/.emacs.d/copilot.el")

;; Use tab for completion. Assumes that you use company-mode for completion.
(define-key company-mode-map (kbd "<tab>") (lambda ()
                                              (interactive)
                                              (or (copilot-accept-completion) (company-indent-or-complete-common nil))))
;; Enable copilot
(copilot-enable)
```

5. Enjoy!

## Roadmap

+ [ ] Setup Copilot without Neovim
+ [ ] Cycle through suggestions
+ [ ] Add package to MELPA
+ [ ] Test compatibility with vanilla Emacs and other auto completion packages

## Thanks

These projects helped me a lot:

+ https://github.com/TommyX12/company-tabnine/
+ https://github.com/cryptobadger/flight-attendant.el
+ https://github.com/github/copilot.vim
