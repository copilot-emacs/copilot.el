;;; copilot-chat-test.el --- Tests for copilot-chat.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-chat.el.

;;; Code:

(require 'buttercup)
(require 'copilot-chat)

(describe "copilot-chat"
  (describe "loading"
    (it "provides the copilot-chat feature"
      (expect (featurep 'copilot-chat) :to-be-truthy)))

  ;;
  ;; Mode
  ;;

  (describe "copilot-chat-mode"
    (it "derives from special-mode"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect (derived-mode-p 'special-mode) :to-be-truthy)))

    (it "enables visual-line-mode"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect visual-line-mode :to-be-truthy)))

    (it "sets word-wrap"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect word-wrap :to-be-truthy)))

    (it "is read-only"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect buffer-read-only :to-be-truthy)))

    (it "has keybindings for send and reset"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c RET"))
                :to-equal #'copilot-chat-send)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c C-c"))
                :to-equal #'copilot-chat-send)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c C-k"))
                :to-equal #'copilot-chat-reset))))

  ;;
  ;; Progress handler
  ;;

  (describe "copilot-chat--handle-progress"
    (it "ignores tokens not in active-buffers"
      (let ((copilot-chat--active-buffers nil))
        ;; Should not error
        (copilot-chat--handle-progress
         (list :token "unknown" :value (list :kind "begin")))))

    (it "sets streaming-p on begin"
      (let ((buf (get-buffer-create "*copilot-chat-test-progress*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p nil))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "begin")))
                (with-current-buffer buf
                  (expect copilot-chat--streaming-p :to-be-truthy))))
          (kill-buffer buf))))

    (it "appends reply text on report"
      (let ((buf (get-buffer-create "*copilot-chat-test-report*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report" :reply "Hello ")))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report" :reply "world")))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Hello world"))))
          (kill-buffer buf))))

    (it "clears streaming-p on end"
      (let ((buf (get-buffer-create "*copilot-chat-test-end*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end"
                                    :result (list :followUp "Try this next"))))
                (with-current-buffer buf
                  (expect copilot-chat--streaming-p :not :to-be-truthy)
                  (expect copilot-chat--follow-up :to-equal "Try this next"))))
          (kill-buffer buf))))

    (it "removes token from active-buffers on end"
      (let ((buf (get-buffer-create "*copilot-chat-test-cleanup*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end")))
                (expect copilot-chat--active-buffers :not :to-be-truthy)))
          (kill-buffer buf)))))

  ;;
  ;; Context handler
  ;;

  (describe "copilot-chat--handle-context"
    (it "returns nil for unknown skills"
      (expect (copilot-chat--handle-context
               (list :skillId "unknown-skill"))
              :to-equal nil))

    (it "returns context for current-editor skill"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () nil)")
        (let* ((source-buf (current-buffer))
               (chat-buf (get-buffer-create "*copilot-chat-test-ctx*")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (copilot-chat-mode)
                  (setq copilot-chat--source-buffer source-buf))
                (with-current-buffer chat-buf
                  (let ((result (copilot-chat--handle-context
                                 (list :skillId "current-editor"))))
                    (expect result :to-be-truthy)
                    (expect (plist-get result :languageId) :to-equal "emacs-lisp")
                    (expect (plist-get result :source) :to-match "defun foo"))))
            (kill-buffer chat-buf))))))

  ;;
  ;; Context doc generation
  ;;

  (describe "copilot-chat--generate-context-doc"
    (it "returns nil when source buffer is nil"
      (with-temp-buffer
        (setq copilot-chat--source-buffer nil)
        (expect (copilot-chat--generate-context-doc) :not :to-be-truthy)))

    (it "returns nil when source buffer is dead"
      (let ((dead-buf (generate-new-buffer " *dead*")))
        (kill-buffer dead-buf)
        (with-temp-buffer
          (setq copilot-chat--source-buffer dead-buf)
          (expect (copilot-chat--generate-context-doc) :not :to-be-truthy))))

    (it "returns doc with source from live buffer"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(message \"hello\")")
        (let ((source-buf (current-buffer)))
          (with-temp-buffer
            (setq copilot-chat--source-buffer source-buf)
            (let ((doc (copilot-chat--generate-context-doc)))
              (expect doc :to-be-truthy)
              (expect (plist-get doc :source) :to-match "hello")
              (expect (plist-get doc :languageId) :to-equal "emacs-lisp")))))))

  ;;
  ;; Insert prompt
  ;;

  (describe "copilot-chat--insert-prompt"
    (it "inserts formatted prompt into buffer"
      (with-temp-buffer
        (copilot-chat-mode)
        (let ((inhibit-read-only t))
          (copilot-chat--insert-prompt "What is Emacs?")
          (expect (buffer-string) :to-match "You:")
          (expect (buffer-string) :to-match "What is Emacs?")
          (expect (buffer-string) :to-match "Copilot:")))))

  ;;
  ;; Reset
  ;;

  (describe "copilot-chat-reset"
    (it "clears state and buffer"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--conversation-id "test-conv")
                (setq copilot-chat--current-turn-id "test-turn")
                (let ((inhibit-read-only t))
                  (insert "some content")))
              (spy-on 'copilot--connection-alivep :and-return-value nil)
              (copilot-chat-reset)
              (with-current-buffer buf
                (expect copilot-chat--conversation-id :not :to-be-truthy)
                (expect copilot-chat--current-turn-id :not :to-be-truthy)
                (expect (buffer-string) :to-equal "")))
          (kill-buffer buf))))

    (it "does nothing when no chat buffer exists"
      ;; Should not error
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      (copilot-chat-reset)))

  ;;
  ;; Destroy
  ;;

  (describe "copilot-chat--destroy"
    (it "cleans up active-buffers for this chat"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--conversation-id "conv-1"))
              (let ((copilot-chat--active-buffers
                     (list (cons "token-1" buf)
                           (cons "token-2" (get-buffer-create " *other*")))))
                (spy-on 'copilot--connection-alivep :and-return-value nil)
                (copilot-chat--destroy)
                ;; Only the entry for buf should be removed
                (expect (length copilot-chat--active-buffers) :to-equal 1)
                (expect (cdar copilot-chat--active-buffers) :not :to-equal buf)
                (kill-buffer " *other*")))
          (kill-buffer buf)))))

  ;;
  ;; Input validation
  ;;

  (describe "copilot-chat (command)"
    (it "rejects empty messages"
      (expect (copilot-chat "") :to-throw 'user-error)))

  (describe "copilot-chat-send"
    (it "rejects empty messages"
      (expect (copilot-chat-send "") :to-throw 'user-error))

    (it "errors when no chat buffer exists"
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      (expect (copilot-chat-send "hello") :to-throw 'user-error)))

  (describe "copilot-chat-send-region"
    (it "formats code with language id"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun bar () t)")
        ;; Spy on copilot-chat to capture the message
        (spy-on 'copilot-chat :and-return-value nil)
        (copilot-chat-send-region (point-min) (point-max) "Explain this")
        (let ((msg (car (spy-calls-args-for 'copilot-chat 0))))
          (expect msg :to-match "Explain this")
          (expect msg :to-match "```emacs-lisp")
          (expect msg :to-match "defun bar"))))

    (it "sends code without prompt when prompt is empty"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(+ 1 2)")
        (spy-on 'copilot-chat :and-return-value nil)
        (copilot-chat-send-region (point-min) (point-max) "")
        (let ((msg (car (spy-calls-args-for 'copilot-chat 0))))
          (expect msg :to-match "```emacs-lisp")
          (expect msg :to-match "(\\+ 1 2)"))))))

;;; copilot-chat-test.el ends here
