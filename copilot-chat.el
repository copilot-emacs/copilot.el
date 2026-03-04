;;; copilot-chat.el --- Chat support for Copilot -*- lexical-binding: t; -*-

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

(defgroup copilot-chat nil
  "Copilot Chat."
  :group 'copilot
  :prefix "copilot-chat-")

(defcustom copilot-chat-model nil
  "The model to use for Copilot Chat.
When nil, the server picks the default model."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Model ID"))
  :group 'copilot-chat
  :package-version '(copilot . "0.5"))

(defface copilot-chat-error-face
  '((t :inherit error))
  "Face for error messages in the chat buffer."
  :group 'copilot-chat)

(defface copilot-chat-follow-up-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for follow-up suggestions in the chat buffer."
  :group 'copilot-chat)

;;
;; Buffer-local state (in *copilot-chat* buffer)
;;

(defvar-local copilot-chat--conversation-id nil
  "Current conversation UUID.")

(defvar-local copilot-chat--current-turn-id nil
  "Turn ID currently being streamed.")

(defvar-local copilot-chat--work-done-token nil
  "Token for routing `$/progress' notifications.")

(defvar-local copilot-chat--streaming-p nil
  "Non-nil while a response is being streamed.
This is set when the first progress `begin' notification arrives,
which may be slightly after `copilot-chat--request-id' is set.")

(defvar-local copilot-chat--source-buffer nil
  "The code buffer providing context for this chat.")

(defvar-local copilot-chat--follow-up nil
  "Follow-up suggestion from the last turn.")

(defvar-local copilot-chat--request-id nil
  "ID of the in-flight async request, used for cancellation.")

;;
;; Global state
;;

(defvar copilot-chat--active-buffers nil
  "Alist of (TOKEN . CHAT-BUFFER) for routing `$/progress'.")

;;
;; Buffer name
;;

(defconst copilot-chat--buffer-name "*copilot-chat*"
  "Name of the Copilot Chat buffer.")

;;
;; Internal helpers
;;

(defun copilot-chat--remove-active-tokens (buf)
  "Remove all active-buffer entries for BUF."
  (setq copilot-chat--active-buffers
        (cl-remove-if (lambda (entry) (eq (cdr entry) buf))
                      copilot-chat--active-buffers)))

(defun copilot-chat--end-streaming ()
  "Reset streaming state in the current chat buffer."
  (setq copilot-chat--streaming-p nil)
  (setq copilot-chat--request-id nil)
  (force-mode-line-update))

(defun copilot-chat--handle-request-error (err label)
  "Handle error ERR from a chat request identified by LABEL.
Resets streaming state, displays the error in the chat buffer,
and cleans up active tokens."
  (copilot--log 'error "Chat %s failed: %S" label err)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (copilot-chat--end-streaming)
        (copilot-chat--remove-active-tokens buf))))
  (copilot-chat--insert-error (format "%s" err)))

;;
;; Progress notification handler
;;

(defun copilot-chat--handle-progress (msg)
  "Handle `$/progress' notification MSG for chat streaming."
  (copilot--dbind (token value) msg
    (when-let* ((entry (assoc token copilot-chat--active-buffers))
                (chat-buf (cdr entry)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (let ((kind (plist-get value :kind)))
            (cond
             ((equal kind "begin")
              (setq copilot-chat--streaming-p t)
              (force-mode-line-update))
             ((equal kind "report")
              (when-let* ((reply (plist-get value :reply)))
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert reply))
                (copilot-chat--scroll-to-bottom)))
             ((equal kind "end")
              (copilot-chat--end-streaming)
              (when-let* ((result (plist-get value :result)))
                (setq copilot-chat--follow-up (plist-get result :followUp)))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n\n")
                (when (and copilot-chat--follow-up
                           (stringp copilot-chat--follow-up)
                           (not (string-empty-p copilot-chat--follow-up)))
                  (insert (propertize
                           (format "Follow-up: %s\n\n" copilot-chat--follow-up)
                           'face 'copilot-chat-follow-up-face))))
              (copilot-chat--scroll-to-bottom)
              (setq copilot-chat--active-buffers
                    (assoc-delete-all token copilot-chat--active-buffers))))))))))

(copilot-on-notification '$/progress #'copilot-chat--handle-progress)

;;
;; Conversation context request handler
;;

(defun copilot-chat--handle-context (msg)
  "Handle `conversation/context' request MSG.
Return editor context for the requested skill."
  (let ((skill-id (plist-get msg :skillId)))
    (if (equal skill-id "current-editor")
        (copilot-chat--generate-context-doc)
      ;; Unknown skill — return empty context
      nil)))

(copilot-on-request 'conversation/context #'copilot-chat--handle-context)

;;
;; Context doc generation
;;

(defun copilot-chat--generate-context-doc ()
  "Generate context document from the source buffer."
  (let ((buf copilot-chat--source-buffer))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((doc (copilot--generate-doc)))
          (plist-put doc :source (copilot--get-source))
          doc)))))

(defun copilot-chat--insert-error (error-msg)
  "Insert ERROR-MSG into the chat buffer with error styling."
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n[Error: %s]\n\n" error-msg)
                            'face 'copilot-chat-error-face))
        (copilot-chat--scroll-to-bottom)))))

;;
;; Protocol methods
;;

(defun copilot-chat--create (message callback)
  "Create a new conversation with MESSAGE.
CALLBACK is called with the response containing conversationId and turnId."
  (let ((token (format "copilot-chat-%s" (float-time))))
    (with-current-buffer (get-buffer copilot-chat--buffer-name)
      (setq copilot-chat--work-done-token token)
      (push (cons token (current-buffer)) copilot-chat--active-buffers))
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
             (when copilot-chat-model
               (list :model copilot-chat-model)))
            :success-fn callback
            :error-fn (lambda (err)
                        (copilot-chat--handle-request-error err "create")))))
      (with-current-buffer (get-buffer copilot-chat--buffer-name)
        (setq copilot-chat--request-id req-id)))))

(defun copilot-chat--send-turn (message)
  "Send a follow-up MESSAGE in the current conversation."
  (let ((token (format "copilot-chat-%s" (float-time)))
        (chat-buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (setq copilot-chat--work-done-token token)
      (push (cons token chat-buf) copilot-chat--active-buffers)
      (let ((conv-id copilot-chat--conversation-id)
            (doc (copilot-chat--generate-context-doc)))
        (setq copilot-chat--request-id
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
                                 (setq copilot-chat--current-turn-id
                                       (plist-get result :turnId)))))
               :error-fn (lambda (err)
                           (copilot-chat--handle-request-error err "turn"))))))))

(defun copilot-chat--destroy ()
  "Destroy the current conversation."
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (when (and chat-buf (buffer-live-p chat-buf))
      (with-current-buffer chat-buf
        (when copilot-chat--conversation-id
          (when (copilot--connection-alivep)
            (copilot--async-request
             'conversation/destroy
             (list :conversationId copilot-chat--conversation-id)))
          (setq copilot-chat--conversation-id nil)
          (setq copilot-chat--current-turn-id nil)
          (setq copilot-chat--follow-up nil)
          (copilot-chat--end-streaming)
          (copilot-chat--remove-active-tokens chat-buf))))))

;;
;; UI helpers
;;

(defun copilot-chat--scroll-to-bottom ()
  "Scroll chat window to show the latest output."
  (when-let* ((win (get-buffer-window copilot-chat--buffer-name)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

(defun copilot-chat--insert-prompt (message)
  "Insert a user prompt with MESSAGE into the chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "You:" 'face 'bold) "\n"
            message "\n\n"
            (propertize "Copilot:" 'face 'bold) "\n")))

;;
;; Major mode
;;

(defvar copilot-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'copilot-chat-send)
    (define-key map (kbd "C-c C-c") #'copilot-chat-send)
    (define-key map (kbd "C-c C-k") #'copilot-chat-stop)
    map)
  "Keymap for `copilot-chat-mode'.")

(defconst copilot-chat--font-lock-keywords
  `(("^\\(You:\\)" 1 'bold)
    ("^\\(Copilot:\\)" 1 'bold)
    ("^\\(##+ .*\\)" 1 'font-lock-keyword-face)
    ("`\\([^`\n]+\\)`" 1 'font-lock-constant-face)
    ("\\*\\*\\([^*\n]+\\)\\*\\*" 1 'bold)
    ("^```.*$" 0 'font-lock-comment-face))
  "Font-lock keywords for `copilot-chat-mode'.")

(defun copilot-chat--mode-line ()
  "Return the mode-line lighter for `copilot-chat-mode'."
  (if copilot-chat--streaming-p
      " Copilot-Chat[Streaming]"
    " Copilot-Chat"))

(define-derived-mode copilot-chat-mode special-mode "Copilot-Chat"
  "Major mode for Copilot Chat.

\\{copilot-chat-mode-map}"
  (setq-local font-lock-defaults '(copilot-chat--font-lock-keywords t))
  (setq mode-name '(:eval (copilot-chat--mode-line)))
  (visual-line-mode 1)
  (setq word-wrap t))

;;
;; Interactive commands
;;

;;;###autoload
(defun copilot-chat (message)
  "Open Copilot Chat and send MESSAGE.
If a conversation already exists, send as a follow-up turn.
Otherwise, create a new conversation."
  (interactive
   (list (read-string
          (if-let* ((buf (get-buffer copilot-chat--buffer-name))
                    ((buffer-local-value 'copilot-chat--conversation-id buf)))
              "Copilot Chat (follow-up): "
            "Copilot Chat: "))))
  (when (string-empty-p message)
    (user-error "Empty message"))
  (let ((source-buf (current-buffer))
        (chat-buf (get-buffer-create copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-chat-mode)
        (copilot-chat-mode))
      (setq copilot-chat--source-buffer source-buf))
    (display-buffer chat-buf)
    (with-current-buffer chat-buf
      (copilot-chat--insert-prompt message)
      (if copilot-chat--conversation-id
          (copilot-chat--send-turn message)
        (copilot-chat--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-chat--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-chat--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-chat-send (message)
  "Send MESSAGE in the current Copilot Chat.
When called interactively, prompt for the message."
  (interactive (list (read-string "Copilot Chat: ")))
  (when (string-empty-p message)
    (user-error "Empty message"))
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (unless chat-buf
      (user-error "No active chat buffer"))
    (with-current-buffer chat-buf
      (when copilot-chat--streaming-p
        (user-error "A response is currently being streamed"))
      (copilot-chat--insert-prompt message)
      (if copilot-chat--conversation-id
          (copilot-chat--send-turn message)
        (copilot-chat--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-chat--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-chat--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-chat-send-region (start end &optional prompt)
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
    (copilot-chat message)))

;;;###autoload
(defun copilot-chat-stop ()
  "Cancel the in-flight request and stop streaming.
If not currently streaming, reset the conversation instead."
  (interactive)
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (if (and chat-buf
             (buffer-local-value 'copilot-chat--streaming-p chat-buf))
        (with-current-buffer chat-buf
          (when (and copilot-chat--request-id (copilot--connection-alivep))
            (jsonrpc-notify copilot--connection
                            '$/cancelRequest
                            (list :id copilot-chat--request-id)))
          (copilot-chat--end-streaming)
          (copilot-chat--insert-error "Cancelled")
          (copilot-chat--remove-active-tokens chat-buf))
      (copilot-chat-reset))))

;;;###autoload
(defun copilot-chat-reset ()
  "Destroy the current conversation and clear the chat buffer."
  (interactive)
  (copilot-chat--destroy)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(provide 'copilot-chat)
;;; copilot-chat.el ends here
