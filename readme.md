# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Warning:** This plugin is under development.

**Note:** You need access to Copilot technical preview to use this plugin.

## Installation

1. Install [Node.js](https://nodejs.org/en/download/) 12 or newer.

2. Clone this repo.

3. Modify your emacs configuration to load and setup `copilot.el`.

```elisp
; Configuration example for Spacemacs (inside your user-config):

; Load copilot.el, modify this path to your local path.
(load-file "~/.emacs.d/copilot.el")

; Use tab for completion. Assumes that you use company-mode for completion.
(define-key company-mode-map (kbd "<tab>") (lambda ()
                                              (interactive)
                                              (or (copilot-accept-completion) (company-indent-or-complete-common nil))))
; Enable copilot
(copilot-enable)
```

4. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose`.

5. Enjoy!

## Commands

### copilot-diagnose

Check the current status of the plugin.

### copilot-login

Login to GitHub, required for using the plugin.

### copilot-accept-completion

Accept the current completion. You need to bind this to some key.

### copilot-complete

Try to complete at the current point.

### copilot-clear-overlay

Clear copilot overlay in the current buffer.

### copilot-enable / copilot-disable

Enable/disable predefined hooks for copilot-complete and copilot-clear-overlay.

You can use the plugin without copilot-enable by hooking above commands manually.


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
