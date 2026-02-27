;;; copilot-nes.el --- Copilot Next Edit Suggestions -*- lexical-binding: t; -*-

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

;; Copilot Next Edit Suggestions (NES) predicts edits the developer will want
;; to make next, based on recent edit history.  Unlike inline completions (ghost
;; text at cursor), NES suggestions can appear anywhere in the file and can
;; replace or delete existing text.
;;
;; Enable `copilot-nes-mode' in a buffer to start receiving suggestions.  It
;; can coexist with `copilot-mode'.
;;
;; Usage:
;;   (add-hook 'prog-mode-hook #'copilot-nes-mode)
;;
;; Key bindings (active when a suggestion is pending):
;;   TAB   — accept the suggestion
;;   C-g   — dismiss the suggestion

;;; Code:

(require 'copilot)

(defconst copilot-nes--min-server-version "1.434.0"
  "Minimum copilot-language-server version required for NES support.")

;;
;; Customization
;;

(defcustom copilot-nes-idle-delay 0.5
  "Seconds of idle time before requesting a NES suggestion."
  :type 'number
  :group 'copilot
  :package-version '(copilot . "0.5"))

(defcustom copilot-nes-auto-dismiss-move-count 3
  "Number of cursor movements before auto-dismissing a suggestion."
  :type 'integer
  :group 'copilot
  :package-version '(copilot . "0.5"))

(defcustom copilot-nes-auto-dismiss-distance 40
  "Max lines between point and suggestion before auto-dismissing."
  :type 'integer
  :group 'copilot
  :package-version '(copilot . "0.5"))

;;
;; Faces
;;

(defface copilot-nes-deletion-face
  '((t :inherit diff-removed :strike-through t))
  "Face for text that a NES suggestion would delete."
  :group 'copilot)

(defface copilot-nes-insertion-face
  '((t :inherit diff-added))
  "Face for text that a NES suggestion would insert."
  :group 'copilot)

;;
;; Buffer-local state
;;

(defvar-local copilot-nes--edit nil
  "The pending NES edit plist, or nil.
Contains keys :text, :range, :command, and :textDocument.")

(defvar-local copilot-nes--overlays nil
  "List of overlays used to display the current NES suggestion.")

(defvar-local copilot-nes--timer nil
  "Idle timer for requesting NES suggestions.")

(defvar-local copilot-nes--move-count 0
  "Number of cursor movements since the suggestion was shown.")

(defvar-local copilot-nes--last-point nil
  "Position of point after the previous command, for detecting actual movement.")

;;
;; Internal helpers
;;

(defun copilot-nes--check-server-version ()
  "Warn if the installed copilot-language-server is too old for NES."
  (let ((installed (copilot-installed-version)))
    (when (and installed
               (version-list-< (version-to-list installed)
                                (version-to-list copilot-nes--min-server-version)))
      (display-warning
       'copilot-nes
       (format "NES requires copilot-language-server >= %s, but %s is installed.
Run `M-x copilot-reinstall-server' to upgrade."
               copilot-nes--min-server-version installed)
       :warning))))

(defun copilot-nes--edit-start-line ()
  "Return the buffer line number where the current edit start, or nil."
  (when copilot-nes--edit
    (let* ((range (plist-get copilot-nes--edit :range))
           (start (plist-get range :start))
           (line (plist-get start :line)))
      (+ line copilot--line-bias))))

(defun copilot-nes--range-to-region (range)
  "Convert an LSP RANGE plist to a (BEG . END) cons in buffer positions."
  (let* ((start (plist-get range :start))
         (end (plist-get range :end))
         (sline (plist-get start :line))
         (schar (plist-get start :character))
         (eline (plist-get end :line))
         (echar (plist-get end :character))
         beg epos)
    (save-excursion
      (goto-char (point-min))
      (forward-line (+ sline (1- copilot--line-bias)))
      (copilot--goto-utf16-offset schar)
      (setq beg (point))
      (goto-char (point-min))
      (forward-line (+ eline (1- copilot--line-bias)))
      (copilot--goto-utf16-offset echar)
      (setq epos (point)))
    (cons beg epos)))

;;
;; Overlay display
;;

(defun copilot-nes--clear ()
  "Clear the current NES suggestion and overlays."
  (mapc #'delete-overlay copilot-nes--overlays)
  (setq copilot-nes--overlays nil)
  (setq copilot-nes--edit nil)
  (setq copilot-nes--move-count 0)
  (setq copilot-nes--last-point nil))

(defun copilot-nes--display (edit)
  "Display EDIT as overlays in the buffer."
  (copilot-nes--clear)
  (setq copilot-nes--edit edit)
  (copilot--dbind (text range) edit
    (let* ((region (copilot-nes--range-to-region range))
           (beg (car region))
           (end (cdr region))
           (has-deletion (> end beg))
           (has-insertion (and text (not (string-empty-p text)))))
      ;; Deletion overlay: highlight replaced text with strikethrough
      (when has-deletion
        (let ((ov (make-overlay beg end nil nil nil)))
          (overlay-put ov 'face 'copilot-nes-deletion-face)
          (overlay-put ov 'copilot-nes t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (push ov copilot-nes--overlays)))
      ;; Insertion overlay: show new text
      (when has-insertion
        (let* ((insertion-text (propertize text 'face 'copilot-nes-insertion-face))
               (ov (make-overlay end end nil nil nil)))
          (overlay-put ov 'after-string insertion-text)
          (overlay-put ov 'copilot-nes t)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority 100)
          (push ov copilot-nes--overlays)))))
  ;; Record point so the post-command hook can detect actual movement
  (setq copilot-nes--last-point (point))
  ;; Notify server that we showed the suggestion
  (copilot--dbind (command) edit
    (when command
      (copilot--notify 'textDocument/didShowInlineEdit
                       (list :item (list :command command))))))

;;
;; Request
;;

(defun copilot-nes--handle-response (response expected-uri expected-version)
  "Process RESPONSE from a NES request.
Only display the suggestion when the document version matches
EXPECTED-VERSION and the URI matches EXPECTED-URI."
  (copilot--dbind (edits) response
    (when (and edits (> (length edits) 0))
      (let* ((edit (if (vectorp edits) (aref edits 0) (car edits))))
        ;; Validate version matches
        (copilot--dbind (textDocument) edit
          (copilot--dbind (version (resp-version version)) textDocument
            (when (and (= resp-version expected-version)
                       (string= (plist-get textDocument :uri) expected-uri))
              (copilot-nes--display edit))))))))

(defun copilot-nes--request ()
  "Request a NES suggestion from the Copilot server."
  (interactive)
  (copilot-nes--clear)
  (when (copilot--connection-alivep)
    (let ((uri (copilot--get-uri))
          (version copilot--doc-version)
          (pos (copilot--lsp-pos)))
      (copilot--async-request
       'textDocument/copilotInlineEdit
       (list :textDocument (list :uri uri :version version)
             :position pos)
       :success-fn
       (lambda (response)
         (copilot-nes--handle-response response uri version))))))

;;
;; Accept
;;

(defun copilot-nes-accept ()
  "Accept the current NES suggestion.
If point is far from the edit, jump there first.  On second
invocation (or when already at the edit), apply the edit."
  (interactive)
  (when copilot-nes--edit
    (let* ((region (copilot-nes--range-to-region
                    (plist-get copilot-nes--edit :range)))
           (beg (car region))
           (end (cdr region))
           (text (plist-get copilot-nes--edit :text))
           (command (plist-get copilot-nes--edit :command))
           (near-edit (<= (abs (- (line-number-at-pos) (line-number-at-pos beg))) 1)))
      (if (not near-edit)
          ;; Jump to the edit location so the user can see what they're accepting
          (goto-char beg)
        ;; Apply the edit
        (delete-region beg end)
        (goto-char beg)
        (insert (or text ""))
        ;; Telemetry: execute the command associated with the edit
        (when command
          (copilot--notify 'workspace/executeCommand
                           (list :command (plist-get command :command)
                                 :arguments (plist-get command :arguments))))
        (copilot-nes--clear)))))

;;
;; Dismiss
;;

(defun copilot-nes-dismiss ()
  "Dismiss the current NES suggestion."
  (interactive)
  (copilot-nes--clear))

;;
;; Auto-triggering
;;

(defvar copilot-nes--text-changing-commands
  '(self-insert-command delete-char delete-backward-char
                        kill-word backward-kill-word kill-line kill-region
                        yank yank-pop newline newline-and-indent open-line
                        delete-forward-char delete-indentation join-line
                        indent-for-tab-command c-electric-brace c-electric-slash
                        c-electric-semicolon c-electric-paren)
  "Commands considered text-modifying for NES triggering.")

(defun copilot-nes--post-command ()
  "Hook run after each command in `copilot-nes-mode'."
  (cond
   ;; After a text-modifying command, schedule a new request
   ((memq this-command copilot-nes--text-changing-commands)
    (copilot-nes--schedule-request))
   ;; When a suggestion is pending and point actually moved, track it
   (copilot-nes--edit
    (when (and copilot-nes--last-point (/= (point) copilot-nes--last-point))
      (cl-incf copilot-nes--move-count)
      (when (or (>= copilot-nes--move-count copilot-nes-auto-dismiss-move-count)
                (copilot-nes--too-far-p))
        (copilot-nes--clear)))))
  (setq copilot-nes--last-point (point)))

(defun copilot-nes--too-far-p ()
  "Return non-nil if point is too far from the current suggestion."
  (when copilot-nes--edit
    (let ((edit-line (copilot-nes--edit-start-line)))
      (and edit-line
           (> (abs (- (line-number-at-pos) edit-line))
              copilot-nes-auto-dismiss-distance)))))

(defun copilot-nes--schedule-request ()
  "Schedule a NES request after idle delay."
  (copilot-nes--cancel-timer)
  (setq copilot-nes--timer
        (run-with-idle-timer copilot-nes-idle-delay nil
                             #'copilot-nes--request-in-buffer
                             (current-buffer))))

(defun copilot-nes--request-in-buffer (buffer)
  "Request a NES suggestion in BUFFER if it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when copilot-nes-mode
        (copilot-nes--request)))))

(defun copilot-nes--cancel-timer ()
  "Cancel the pending NES idle timer."
  (when (timerp copilot-nes--timer)
    (cancel-timer copilot-nes--timer)
    (setq copilot-nes--timer nil)))

;;
;; Minor mode
;;

(defvar copilot-nes-mode-map
  (let ((map (make-sparse-keymap))
        (accept `(menu-item "" copilot-nes-accept
                             :filter ,(lambda (cmd)
                                        (when copilot-nes--edit cmd))))
        (dismiss `(menu-item "" copilot-nes-dismiss
                              :filter ,(lambda (cmd)
                                         (when copilot-nes--edit cmd)))))
    ;; Bind both TAB (C-i) and <tab> (function key) so the binding
    ;; works regardless of whether another mode intercepts <tab>
    ;; before function-key-map translates it to TAB.
    (define-key map (kbd "TAB") accept)
    (define-key map [tab] accept)
    (define-key map (kbd "C-g") dismiss)
    map)
  "Keymap for `copilot-nes-mode'.
Bindings only activate when a NES suggestion is pending.")

;;;###autoload
(define-minor-mode copilot-nes-mode
  "Minor mode for Copilot Next Edit Suggestions.
NES predicts edits you will want to make next, based on recent
edit history.  Suggestions can appear anywhere in the file and
can replace or delete existing text.

\\{copilot-nes-mode-map}"
  :lighter " NES"
  :keymap copilot-nes-mode-map
  (if copilot-nes-mode
      (progn
        (copilot-nes--check-server-version)
        (add-hook 'post-command-hook #'copilot-nes--post-command nil t))
    (copilot-nes--cancel-timer)
    (copilot-nes--clear)
    (remove-hook 'post-command-hook #'copilot-nes--post-command t)))

(provide 'copilot-nes)

;;; copilot-nes.el ends here
