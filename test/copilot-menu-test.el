;;; copilot-menu-test.el --- Tests for copilot-menu.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-menu.el.

;;; Code:

(require 'buttercup)
(require 'copilot-menu)
(require 'copilot)
(require 'copilot-chat)

(defun copilot-menu-test--layout-commands ()
  "Collect every suffix command from `copilot-menu--definition'.
Walk the quoted definition rather than transient's internal layout
representation, which differs across the transient versions bundled
with the supported Emacsen.  A suffix is a list starting with a key
string, whose command is the first symbol after the key (skipping an
optional description string)."
  (let (commands)
    (letrec ((walk
              (lambda (x)
                (cond
                 ((vectorp x) (mapc walk x))
                 ((and (consp x) (stringp (car x)))
                  (let ((cand (if (stringp (nth 1 x)) (nth 2 x) (nth 1 x))))
                    (when (and cand (symbolp cand))
                      (push cand commands))))
                 ((proper-list-p x) (mapc walk x))))))
      (funcall walk copilot-menu--definition))
    (nreverse commands)))

(defun copilot-menu-test--layout-keys ()
  "Collect every suffix key string from `copilot-menu--definition'.
A transient prefix has a single flat keymap, so these must be unique
across the whole menu."
  (let (keys)
    (letrec ((walk
              (lambda (x)
                (cond
                 ((vectorp x) (mapc walk x))
                 ((and (consp x) (stringp (car x)))
                  (let ((cand (if (stringp (nth 1 x)) (nth 2 x) (nth 1 x))))
                    (when (and cand (symbolp cand))
                      (push (car x) keys))))
                 ((proper-list-p x) (mapc walk x))))))
      (funcall walk copilot-menu--definition))
    (nreverse keys)))

(describe "copilot-menu"
  (it "is defined as a transient prefix when transient is available"
    (assume (featurep 'transient) "transient is not available")
    (expect (fboundp 'copilot-menu) :to-be-truthy)
    (expect (get 'copilot-menu 'transient--prefix) :to-be-truthy)
    (expect (get 'copilot-menu 'transient--layout) :to-be-truthy))

  (it "references only commands that exist"
    (let ((commands (copilot-menu-test--layout-commands)))
      (expect commands :not :to-be nil)
      (dolist (command commands)
        (expect (fboundp command) :to-be-truthy)
        (expect (commandp command) :to-be-truthy))))

  (it "includes the expected suffix commands"
    (let ((commands (copilot-menu-test--layout-commands)))
      (dolist (command '(copilot-mode
                         copilot-complete
                         copilot-panel-complete
                         copilot-chat
                         copilot-chat-send-region
                         copilot-chat-slash-command
                         copilot-chat-add-file-reference
                         copilot-chat-clear-references
                         copilot-chat-compact
                         copilot-chat-select-model
                         copilot-menu-toggle-agent-mode
                         copilot-chat-select-mode
                         copilot-chat-list-mcp-tools
                         copilot-login
                         copilot-logout
                         copilot-quota
                         copilot-list-code-references
                         copilot-install-server
                         copilot-reinstall-server
                         copilot-uninstall-server
                         copilot-diagnose
                         copilot-menu-open-log))
        (expect commands :to-contain command))
      ;; Exactly these, so a future suffix shape the walker doesn't
      ;; understand cannot silently drop commands from the check.
      (expect (length commands) :to-equal 22)))

  (it "binds every suffix to a distinct key"
    ;; A transient prefix has a single flat keymap; a duplicate key
    ;; silently shadows an earlier command (see the `c'/`C' split for
    ;; complete vs compact).
    (let* ((keys (copilot-menu-test--layout-keys))
           (dups (seq-uniq
                  (seq-filter (lambda (k) (> (seq-count (lambda (o) (equal o k))
                                                        keys)
                                             1))
                              keys))))
      (expect dups :to-equal nil)))

  (describe "copilot-menu-toggle-agent-mode"
    (it "flips copilot-chat-use-agent-mode"
      (let ((copilot-chat-use-agent-mode nil))
        (copilot-menu-toggle-agent-mode)
        (expect copilot-chat-use-agent-mode :to-be-truthy)
        (copilot-menu-toggle-agent-mode)
        (expect copilot-chat-use-agent-mode :to-be nil)))

    (it "clears an explicit mode selection so the toggle takes effect"
      (let ((copilot-chat--mode '(:name "InlineAgent" :kind "InlineAgent"))
            (copilot-chat-use-agent-mode nil))
        (copilot-menu-toggle-agent-mode)
        (expect copilot-chat--mode :to-be nil)
        (expect copilot-chat-use-agent-mode :to-be-truthy))))

  (describe "copilot-menu-open-log"
    (it "signals a user error when there is no log buffer"
      (when (get-buffer "*copilot-language-server-log*")
        (kill-buffer "*copilot-language-server-log*"))
      (expect (copilot-menu-open-log) :to-throw 'user-error))

    (it "pops to the log buffer when it exists"
      (let ((buffer (get-buffer-create "*copilot-language-server-log*")))
        (unwind-protect
            (progn
              (copilot-menu-open-log)
              (expect (window-buffer) :to-be buffer))
          (kill-buffer buffer))))))

;;; copilot-menu-test.el ends here
