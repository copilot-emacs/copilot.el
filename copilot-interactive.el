;;; copilot-interactive.el --- Interactive Chat support for Copilot -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 copilot-emacs maintainers

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/copilot-emacs/copilot.el
;; Keywords: convenience copilot chat

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

;; Chat support for GitHub Copilot using the conversation/* LSP methods.
;; This reuses the existing copilot.el connection and dispatcher infrastructure.

;;; Code:

(require 'copilot)

;;
;; Customization
;;

(defgroup copilot-interactive nil
  "Copilot Chat."
  :group 'copilot
  :prefix "copilot-interactive-")

(defcustom copilot-interactive-model nil
  "The model to use for Copilot Chat.
When nil, the server picks the default model."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Model ID"))
  :group 'copilot-interactive
  :package-version '(copilot . "0.5"))

(defface copilot-interactive-error-face
  '((t :inherit error))
  "Face for error messages in the chat buffer."
  :group 'copilot-interactive)

(defface copilot-interactive-follow-up-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for follow-up suggestions in the chat buffer."
  :group 'copilot-interactive)

;;
;; Buffer-local state (in *copilot-interactive* buffer)
;;

(defvar-local copilot-interactive--conversation-id nil
  "Current conversation UUID.")

(defvar-local copilot-interactive--current-turn-id nil
  "Turn ID currently being streamed.")

(defvar-local copilot-interactive--work-done-token nil
  "Token for routing `$/progress' notifications.")

(defvar-local copilot-interactive--streaming-p nil
  "Non-nil while a response is being streamed.
This is set when the first progress `begin' notification arrives,
which may be slightly after `copilot-interactive--request-id' is set.")

(defvar-local copilot-interactive--source-buffer nil
  "The code buffer providing context for this chat.")

(defvar-local copilot-interactive--follow-up nil
  "Follow-up suggestion from the last turn.")

(defvar-local copilot-interactive--request-id nil
  "ID of the in-flight async request, used for cancellation.")

;;
;; Global state
;;

(defvar copilot-interactive--active-buffers nil
  "Alist of (TOKEN . CHAT-BUFFER) for routing `$/progress'.")

;;
;; Buffer name
;;

(defconst copilot-interactive--buffer-name "*copilot-interactive*"
  "Name of the Copilot Chat buffer.")

;;
;; Internal helpers
;;

(defun copilot-interactive--remove-active-tokens (buf)
  "Remove all active-buffer entries for BUF."
  (setq copilot-interactive--active-buffers
        (cl-remove-if (lambda (entry) (eq (cdr entry) buf))
                      copilot-interactive--active-buffers)))

(defun copilot-interactive--end-streaming ()
  "Reset streaming state in the current chat buffer."
  (setq copilot-interactive--streaming-p nil)
  (setq copilot-interactive--request-id nil)
  (force-mode-line-update))

(defun copilot-interactive--handle-request-error (err label)
  "Handle error ERR from a chat request identified by LABEL.
Resets streaming state, displays the error in the chat buffer,
and cleans up active tokens."
  (copilot--log 'error "Chat %s failed: %S" label err)
  (when-let* ((buf (get-buffer copilot-interactive--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (copilot-interactive--end-streaming)
        (copilot-interactive--remove-active-tokens buf))))
  (copilot-interactive--insert-error (format "%s" err)))

;;
;; Progress notification handler
;;

(defun copilot-interactive--extract-reply (value)
  "Extract the reply text from a progress report VALUE.
The server may provide the reply directly as `:reply' (older format)
or nested inside `:editAgentRounds' (newer format).
Returns a string or nil."
  (let ((reply (or (plist-get value :reply)
                   (when-let* ((rounds (plist-get value :editAgentRounds))
                               ((vectorp rounds))
                               ((> (length rounds) 0))
                               (last-round (aref rounds (1- (length rounds)))))
                     (plist-get last-round :reply)))))
    (and (stringp reply) reply)))

(defun copilot-interactive--handle-progress (msg)
  "Handle `$/progress' notification MSG for chat streaming."
  (copilot--dbind (token value) msg
    (when-let* ((entry (assoc token copilot-interactive--active-buffers))
                (chat-buf (cdr entry)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (let ((kind (plist-get value :kind)))
            (cond
             ((equal kind "begin")
              (setq copilot-interactive--streaming-p t)
              (force-mode-line-update))
             ((equal kind "report")
              (when-let* ((reply (copilot-interactive--extract-reply value)))
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert reply))
                (copilot-interactive--scroll-to-bottom)))
             ((equal kind "end")
              (copilot-interactive--end-streaming)
              (when-let* ((result (plist-get value :result))
                          ((listp result)))
                (setq copilot-interactive--follow-up (plist-get result :followUp)))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n\n")
                (when (and copilot-interactive--follow-up
                           (stringp copilot-interactive--follow-up)
                           (not (string-empty-p copilot-interactive--follow-up)))
                  (insert (propertize
                           (format "Follow-up: %s\n\n" copilot-interactive--follow-up)
                           'face 'copilot-interactive-follow-up-face))))
              (copilot-interactive--scroll-to-bottom)
              (setq copilot-interactive--active-buffers
                    (assoc-delete-all token copilot-interactive--active-buffers))))))))))

(copilot-on-notification '$/progress #'copilot-interactive--handle-progress)

;;
;; Conversation context request handler
;;

(defun copilot-interactive--handle-context (msg)
  "Handle `conversation/context' request MSG.
Return editor context for the requested skill."
  (let ((skill-id (plist-get msg :skillId)))
    (if (equal skill-id "current-editor")
        (copilot-interactive--generate-context-doc)
      ;; Unknown skill — return empty context
      nil)))

(copilot-on-request 'conversation/context #'copilot-interactive--handle-context)

;;
;; Context doc generation
;;

(defun copilot-interactive--generate-context-doc ()
  "Generate context document from the source buffer."
  (let ((buf copilot-interactive--source-buffer))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((doc (copilot--generate-doc)))
          (plist-put doc :source (copilot--get-source))
          doc)))))

(defun copilot-interactive--insert-error (error-msg)
  "Insert ERROR-MSG into the chat buffer with error styling."
  (when-let* ((buf (get-buffer copilot-interactive--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n[Error: %s]\n\n" error-msg)
                            'face 'copilot-interactive-error-face))
        (copilot-interactive--scroll-to-bottom)))))

;;
;; Protocol methods
;;

(defun copilot-interactive--create (message callback)
  "Create a new conversation with MESSAGE.
CALLBACK is called with the response containing conversationId and turnId."
  (let ((token (format "copilot-interactive-%s" (float-time))))
    (with-current-buffer (get-buffer copilot-interactive--buffer-name)
      (setq copilot-interactive--work-done-token token)
      (push (cons token (current-buffer)) copilot-interactive--active-buffers))
    (let ((req-id
           (copilot--async-request
            'conversation/create
            (append
             (list :workDoneToken token
                   :turns (vector
                           (list :request message
                                 :response ""
                                 :turnId ""))
                   :capabilities (list :skills (vector "current-editor")
                                       :allSkills t)
                   :source "panel")
             (when copilot-interactive-model
               (list :model copilot-interactive-model))
             (list :workspaceFolders
                   (vconcat
                    (when-let* ((root (copilot--workspace-root)))
                      (list (list :uri (concat "file://" root)
                                  :name (file-name-nondirectory
                                         (directory-file-name root))))))))
            :success-fn callback
            :error-fn (lambda (err)
                        (copilot-interactive--handle-request-error err "create")))))
      (with-current-buffer (get-buffer copilot-interactive--buffer-name)
        (setq copilot-interactive--request-id req-id)))))

(defun copilot-interactive--send-turn (message)
  "Send a follow-up MESSAGE in the current conversation."
  (let ((token (format "copilot-interactive-%s" (float-time)))
        (chat-buf (get-buffer copilot-interactive--buffer-name)))
    (with-current-buffer chat-buf
      (setq copilot-interactive--work-done-token token)
      (push (cons token chat-buf) copilot-interactive--active-buffers)
      (let ((conv-id copilot-interactive--conversation-id)
            (doc (copilot-interactive--generate-context-doc)))
        (setq copilot-interactive--request-id
              (copilot--async-request
               'conversation/turn
               (append
                (list :workDoneToken token
                      :conversationId conv-id
                      :message message
                      :source "panel")
                (when doc (list :doc doc)))
               :success-fn (lambda (result)
                             (when (buffer-live-p chat-buf)
                               (with-current-buffer chat-buf
                                 (setq copilot-interactive--current-turn-id
                                       (plist-get result :turnId)))))
               :error-fn (lambda (err)
                           (copilot-interactive--handle-request-error err "turn"))))))))

(defun copilot-interactive--destroy ()
  "Destroy the current conversation."
  (let ((chat-buf (get-buffer copilot-interactive--buffer-name)))
    (when (and chat-buf (buffer-live-p chat-buf))
      (with-current-buffer chat-buf
        (when copilot-interactive--conversation-id
          (when (copilot--connection-alivep)
            (copilot--async-request
             'conversation/destroy
             (list :conversationId copilot-interactive--conversation-id)))
          (setq copilot-interactive--conversation-id nil)
          (setq copilot-interactive--current-turn-id nil)
          (setq copilot-interactive--follow-up nil)
          (copilot-interactive--end-streaming)
          (copilot-interactive--remove-active-tokens chat-buf))))))

;;
;; UI helpers
;;

(defun copilot-interactive--scroll-to-bottom ()
  "Scroll chat window to show the latest output."
  (when-let* ((win (get-buffer-window copilot-interactive--buffer-name)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

(defun copilot-interactive--insert-prompt (message)
  "Insert a user prompt with MESSAGE into the chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "You:" 'face 'bold) "\n"
            message "\n\n"
            (propertize "Copilot:" 'face 'bold) "\n")))

;;
;; Major mode
;;

(defvar copilot-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'copilot-interactive-send)
    (define-key map (kbd "C-c C-c") #'copilot-interactive-send)
    (define-key map (kbd "C-c C-k") #'copilot-interactive-stop)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `copilot-interactive-mode'.")

(defconst copilot-interactive--font-lock-keywords
  `(("^\\(You:\\)" 1 'bold)
    ("^\\(Copilot:\\)" 1 'bold))
  "Extra font-lock keywords added on top of the parent mode's highlighting.")

(defun copilot-interactive--mode-line ()
  "Return the mode-line lighter for `copilot-interactive-mode'."
  (if copilot-interactive--streaming-p
      " Copilot-Interactive[Streaming]"
    " Copilot-Interactive"))

(declare-function gfm-mode "ext:markdown-mode" ())

(defun copilot-interactive--setup-mode ()
  "Set up font-lock, mode-line, and visual-line for `copilot-interactive-mode'.
When `markdown-mode' is available, enable GFM font-lock for full
markdown rendering; otherwise use basic highlighting."
  (when (require 'markdown-mode nil t)
    (setq-local markdown-fontify-code-blocks-natively t)
    (setq-local font-lock-defaults
                (with-temp-buffer
                  (gfm-mode)
                  font-lock-defaults))
    (font-lock-flush))
  (font-lock-add-keywords nil copilot-interactive--font-lock-keywords t)
  (setq mode-name '(:eval (copilot-interactive--mode-line)))
  (visual-line-mode 1)
  (setq word-wrap t))

(define-derived-mode copilot-interactive-mode special-mode "Copilot-Interactive"
  "Major mode for Copilot Chat.
When `markdown-mode' is installed, the buffer gets full GFM
rendering; otherwise basic font-lock is used.

\\{copilot-interactive-mode-map}"
  (copilot-interactive--setup-mode))

;;
;; Interactive commands
;;

;;;###autoload
(defun copilot-interactive (message)
  "Open Copilot Chat and send MESSAGE.
If a conversation already exists, send as a follow-up turn.
Otherwise, create a new conversation."
  (interactive
   (list (read-string
          (if-let* ((buf (get-buffer copilot-interactive--buffer-name))
                    ((buffer-local-value 'copilot-interactive--conversation-id buf)))
              "Copilot Chat (follow-up): "
            "Copilot Chat: "))))
  (when (string-empty-p message)
    (user-error "Empty message"))
  (let ((source-buf (current-buffer))
        (chat-buf (get-buffer-create copilot-interactive--buffer-name)))
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-interactive-mode)
        (copilot-interactive-mode))
      (setq copilot-interactive--source-buffer source-buf))
    (display-buffer chat-buf)
    (with-current-buffer chat-buf
      (copilot-interactive--insert-prompt message)
      (if copilot-interactive--conversation-id
          (copilot-interactive--send-turn message)
        (copilot-interactive--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-interactive--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-interactive--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-interactive-send (message)
  "Send MESSAGE in the current Copilot Chat.
When called interactively, prompt for the message."
  (interactive (list (read-string "Copilot Chat: ")))
  (when (string-empty-p message)
    (user-error "Empty message"))
  (let ((chat-buf (get-buffer copilot-interactive--buffer-name)))
    (unless chat-buf
      (user-error "No active chat buffer"))
    (with-current-buffer chat-buf
      (when copilot-interactive--streaming-p
        (user-error "A response is currently being streamed"))
      (copilot-interactive--insert-prompt message)
      (if copilot-interactive--conversation-id
          (copilot-interactive--send-turn message)
        (copilot-interactive--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-interactive--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-interactive--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-interactive-send-region (start end &optional prompt)
  "Send the region between START and END as context with an optional PROMPT."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Prompt (optional): "))
     (user-error "No active region")))
  (let* ((code (buffer-substring-no-properties start end))
         (lang (copilot--get-language-id))
         (message (if (and prompt (not (string-empty-p prompt)))
                      (format "%s\n\n```%s\n%s\n```" prompt lang code)
                    (format "```%s\n%s\n```" lang code))))
    (copilot-interactive message)))

;;;###autoload
(defun copilot-interactive-stop ()
  "Cancel the in-flight request and stop streaming.
If not currently streaming, reset the conversation instead."
  (interactive)
  (let ((chat-buf (get-buffer copilot-interactive--buffer-name)))
    (if (and chat-buf
             (buffer-local-value 'copilot-interactive--streaming-p chat-buf))
        (with-current-buffer chat-buf
          (when (and copilot-interactive--request-id (copilot--connection-alivep))
            (jsonrpc-notify copilot--connection
                            '$/cancelRequest
                            (list :id copilot-interactive--request-id)))
          (copilot-interactive--end-streaming)
          (copilot-interactive--insert-error "Cancelled")
          (copilot-interactive--remove-active-tokens chat-buf))
      (copilot-interactive-reset))))

;;;###autoload
(defun copilot-interactive-reset ()
  "Destroy the current conversation and clear the chat buffer."
  (interactive)
  (copilot-interactive--destroy)
  (when-let* ((buf (get-buffer copilot-interactive--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(provide 'copilot-interactive)
;;; copilot-interactive.el ends here
