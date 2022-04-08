# Copilot.el

Copilot.el is an Emacs plugin for GitHub Copilot.

![](assets/demo.gif)

**Warning:** This plugin is unofficial and based on binaries provided by [copilot.vim](https://github.com/github/copilot.vim).

**Warning:** This plugin is under development. The name of commands and variables may change in the future.

**Note:** You need access to Copilot technical preview to use this plugin.

## Installation

1. Install [Node.js](https://nodejs.org/en/download/) 12 or newer.

2. Setup `copilot.el` as described in the next section.

3. Login to Copilot by `M-x copilot-login`. You can also check the status by `M-x copilot-diagnose`.

4. Enjoy!

## Configurations

### 1. Load `copilot.el`

#### Option 1: Load via `straight.el` (recommended)


```elisp
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t)
; you can put your other copilot configurations under :config
```

#### Option 2: Load manually

Please make sure you have these dependencies installed:

+ `dash`
+ `s`
+ `editorconfig`

```elisp
; Load copilot.el, modify this path to your local path.
(load-file "~/path/to/copilot.el")
```


### 2. Configure completion

#### Option 1: Use `copilot-mode` to automatically provide completions

```elisp
; enable copilot in programming modes
(add-hook 'prog-mode-hook 'copilot-mode)
```

For evil users, you will want to add this line to have completions only when in insert state:

```elisp
(customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
```

To customize the behavior of `copilot-mode`, please check `copilot-enable-predicates` and `copilot-disable-predicates`.

#### Option 2: Manually provide completions

You need to bind `copilot-complete` to some key and add a wrapped `copilot-clear-overlay` to `post-command-hook`.


### 3. Configure completion acceptation

In general, you need to bind `copilot-accept-completion` to some key in order to accept the completion.

#### Example of using tab with `company-mode`
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
  (setq ac-candidate-menu-min 0)
  (define-key ac-completing-map (kbd "TAB") 'my-tab)
  (define-key ac-completing-map (kbd "<tab>") 'my-tab))

(define-key global-map [remap indent-for-tab-command] 'my-tab)
```

## Commands

#### copilot-diagnose

Check the current status of the plugin. Also you can check error logs in the `*copilot-log*` buffer.

#### copilot-login

Login to GitHub, required for using the plugin.

#### copilot-mode

Enable/disable copilot mode.

#### copilot-accept-completion

Accept the current completion.

#### copilot-complete

Try to complete at the current point.

#### copilot-clear-overlay

Clear copilot overlay in the current buffer.

#### copilot-next-completion / copilot-previous-completion

Cycle through the completion list.

## Customization

#### copilot-idle-delay

Time in seconds to wait before starting completion (default to 0). Note Copilot itself has a ~100ms delay because of network communication.

#### copilot-enable-predicates
A list of predicate functions with no argument to enable Copilot in `copilot-mode`. Copilot will be enabled only if all predicates return `t`.

#### copilot-disable-predicates
A list of predicate functions with no argument to disable Copilot in `copilot-mode`. Copilot will be disabled if any predicate returns `t`.

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
