;;; copilot-menu.el --- Transient menu for Copilot -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 copilot-emacs maintainers

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/copilot-emacs/copilot.el
;; Keywords: convenience copilot

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A transient (magit-style) menu that collects the most common Copilot
;; commands in one place: completions, chat, agent mode, account and
;; usage information, and server management.  Invoke it with
;; `M-x copilot-menu'.
;;
;; The menu needs the `transient' package, which is bundled with
;; Emacs 28.1 and newer.  To keep copilot.el installable on Emacs 27.2
;; (where current transient releases no longer work), transient is not a
;; hard dependency: without it, `copilot-menu' signals a `user-error'
;; explaining what to install, and everything else in copilot.el works
;; as usual.

;;; Code:

(require 'copilot)

;; The chat commands live in copilot-chat.el, which is part of this
;; package but only loaded once the menu is opened (see `copilot-menu').
(defvar copilot-chat-model)
(defvar copilot-chat-use-agent-mode)
(defvar copilot-chat--mode)

;;
;; Dynamic descriptions
;;

(defun copilot-menu--copilot-mode-description ()
  "Describe `copilot-mode' with its state in the current buffer."
  (format "Automatic completions [%s]" (if copilot-mode "on" "off")))

(defun copilot-menu--agent-mode-description ()
  "Describe the agent mode toggle with its current state."
  (format "Agent mode [%s]" (if copilot-chat-use-agent-mode "on" "off")))

(defun copilot-menu--chat-model-description ()
  "Describe the chat model selection with the current model."
  (format "Select chat model (%s)" (or copilot-chat-model "default")))

(defun copilot-menu--chat-mode-description ()
  "Describe the chat mode selection with the current mode."
  (format "Select chat mode (%s)"
          (cond ((and (boundp 'copilot-chat--mode) copilot-chat--mode)
                 (plist-get copilot-chat--mode :name))
                (copilot-chat-use-agent-mode "Agent")
                (t "Ask"))))

;;
;; Extra suffix commands
;;

(defun copilot-menu-toggle-agent-mode ()
  "Toggle `copilot-chat-use-agent-mode' and report the new state.
Any explicit mode chosen with `copilot-chat-select-mode' is cleared, so
the toggle actually takes effect (a selected mode otherwise wins over
the boolean)."
  (interactive)
  (require 'copilot-chat)
  (setq copilot-chat--mode nil)
  (setq copilot-chat-use-agent-mode (not copilot-chat-use-agent-mode))
  (message "Copilot Chat: agent mode %s"
           (if copilot-chat-use-agent-mode "enabled" "disabled")))

(defun copilot-menu-open-log ()
  "Open the Copilot language server log buffer."
  (interactive)
  (let ((buffer (get-buffer "*copilot-language-server-log*")))
    (if buffer
        (pop-to-buffer buffer)
      (user-error "Copilot: No server log buffer yet"))))

;;
;; The menu itself
;;

(defconst copilot-menu--definition
  '(transient-define-prefix copilot-menu ()
     "Transient menu for the most common Copilot commands."
     [["Completions"
       ("t" copilot-mode
        :description copilot-menu--copilot-mode-description
        :transient t)
       ("c" "Complete at point" copilot-complete)
       ("p" "Panel completions" copilot-panel-complete)]
      ["Chat"
       ("h" "Open chat" copilot-chat)
       ("r" "Send region" copilot-chat-send-region)
       ("/" "Slash command" copilot-chat-slash-command)
       ("f" "Attach file as context" copilot-chat-add-file-reference)
       ("k" "Clear pending context" copilot-chat-clear-references)
       ("C" "Compact conversation" copilot-chat-compact)
       ("m" copilot-chat-select-model
        :description copilot-menu--chat-model-description)]
      ["Agent"
       ("a" copilot-menu-toggle-agent-mode
        :description copilot-menu--agent-mode-description
        :transient t)
       ("M" copilot-chat-select-mode
        :description copilot-menu--chat-mode-description)
       ("T" "List MCP tools" copilot-chat-list-mcp-tools)]]
     [["Account"
       ("i" "Login" copilot-login)
       ("o" "Logout" copilot-logout)
       ("u" "Usage quota" copilot-quota)
       ("R" "Code references" copilot-list-code-references)]
      ["Server"
       ("s" "Install server" copilot-install-server)
       ("S" "Reinstall server" copilot-reinstall-server)
       ("U" "Uninstall server" copilot-uninstall-server)
       ("d" "Restart & diagnose" copilot-diagnose)
       ("l" "Open server log" copilot-menu-open-log)]]
     (interactive)
     (require 'copilot-chat)
     (transient-setup 'copilot-menu))
  "The `copilot-menu' transient definition, kept as data.
`transient-define-prefix' is a macro, so a file using it directly cannot
even be byte-compiled on an Emacs without the `transient' package.
Storing the definition as data and evaluating it at load time (see
below) keeps copilot.el compatible with Emacs 27.2, where current
transient releases are not installable.")

;;;###autoload (autoload 'copilot-menu "copilot-menu" nil t)
(if (require 'transient nil t)
    (eval copilot-menu--definition t)
  (defun copilot-menu ()
    "Placeholder for the Copilot menu when `transient' is missing.
Retries loading transient, so installing it mid-session is picked up
without re-loading copilot-menu.el."
    (interactive)
    (if (require 'transient nil t)
        (progn
          (eval copilot-menu--definition t)
          ;; By now the transient prefix has replaced this placeholder.
          (call-interactively 'copilot-menu))
      (user-error
       "Copilot: `copilot-menu' requires the `transient' package (bundled with Emacs 28.1 and newer)"))))

(provide 'copilot-menu)
;;; copilot-menu.el ends here
