;;; copilot-interactive-test.el --- Tests for copilot-interactive.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-interactive.el.

;;; Code:

(require 'buttercup)
(require 'copilot-interactive)

(describe "copilot-interactive"
  (describe "loading"
    (it "provides the copilot-interactive feature"
      (expect (featurep 'copilot-interactive) :to-be-truthy)))

  ;;
  ;; Mode
  ;;

  (describe "copilot-interactive-mode"
    (it "derives from special-mode"
      (with-temp-buffer
        (copilot-interactive-mode)
        (expect (derived-mode-p 'special-mode) :to-be-truthy)))

    (it "enables visual-line-mode"
      (with-temp-buffer
        (copilot-interactive-mode)
        (expect visual-line-mode :to-be-truthy)))

    (it "sets word-wrap"
      (with-temp-buffer
        (copilot-interactive-mode)
        (expect word-wrap :to-be-truthy)))

    (it "is read-only"
      (with-temp-buffer
        (copilot-interactive-mode)
        (expect buffer-read-only :to-be-truthy)))

    (it "has keybindings for send and stop"
      (with-temp-buffer
        (copilot-interactive-mode)
        (expect (lookup-key copilot-interactive-mode-map (kbd "C-c RET"))
                :to-equal #'copilot-interactive-send)
        (expect (lookup-key copilot-interactive-mode-map (kbd "C-c C-c"))
                :to-equal #'copilot-interactive-send)
        (expect (lookup-key copilot-interactive-mode-map (kbd "C-c C-k"))
                :to-equal #'copilot-interactive-stop))))

  ;;
  ;; Progress handler
  ;;

  (describe "copilot-interactive--extract-reply"
    (it "extracts reply from flat :reply field"
      (expect (copilot-interactive--extract-reply '(:reply "hello"))
              :to-equal "hello"))

    (it "extracts reply from :editAgentRounds"
      (expect (copilot-interactive--extract-reply
               (list :editAgentRounds
                     (vector (list :roundId 1 :reply "hello from agent"))))
              :to-equal "hello from agent"))

    (it "prefers flat :reply over :editAgentRounds"
      (expect (copilot-interactive--extract-reply
               (list :reply "flat"
                     :editAgentRounds
                     (vector (list :roundId 1 :reply "nested"))))
              :to-equal "flat"))

    (it "returns nil when neither field is present"
      (expect (copilot-interactive--extract-reply '(:kind "report"))
              :not :to-be-truthy)))

  (describe "copilot-interactive--handle-progress"
    (it "ignores tokens not in active-buffers"
      (let ((copilot-interactive--active-buffers nil))
        ;; Should not error
        (copilot-interactive--handle-progress
         (list :token "unknown" :value (list :kind "begin")))))

    (it "sets streaming-p on begin"
      (let ((buf (get-buffer-create "*copilot-interactive-test-progress*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p nil))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "begin")))
                (with-current-buffer buf
                  (expect copilot-interactive--streaming-p :to-be-truthy))))
          (kill-buffer buf))))

    (it "appends reply text on report"
      (let ((buf (get-buffer-create "*copilot-interactive-test-report*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report" :reply "Hello ")))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report" :reply "world")))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Hello world"))))
          (kill-buffer buf))))

    (it "appends reply from editAgentRounds on report"
      (let ((buf (get-buffer-create "*copilot-interactive-test-agent*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report"
                                    :editAgentRounds
                                    (vector (list :roundId 1
                                                  :reply "agent reply")))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "agent reply"))))
          (kill-buffer buf))))

    (it "clears streaming-p on end"
      (let ((buf (get-buffer-create "*copilot-interactive-test-end*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end"
                                    :result (list :followUp "Try this next"))))
                (with-current-buffer buf
                  (expect copilot-interactive--streaming-p :not :to-be-truthy)
                  (expect copilot-interactive--follow-up :to-equal "Try this next"))))
          (kill-buffer buf))))

    (it "removes token from active-buffers on end"
      (let ((buf (get-buffer-create "*copilot-interactive-test-cleanup*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end")))
                (expect copilot-interactive--active-buffers :not :to-be-truthy)))
          (kill-buffer buf)))))

  ;;
  ;; Follow-up display
  ;;

  (describe "follow-up display in progress handler"
    (it "inserts follow-up text on end when present"
      (let ((buf (get-buffer-create "*copilot-interactive-test-followup*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end"
                                    :result (list :followUp "Try asking about X"))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Follow-up: Try asking about X"))))
          (kill-buffer buf))))

    (it "does not insert follow-up when nil"
      (let ((buf (get-buffer-create "*copilot-interactive-test-no-followup*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t))
              (let ((copilot-interactive--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-interactive--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end")))
                (with-current-buffer buf
                  (expect (buffer-string) :not :to-match "Follow-up"))))
          (kill-buffer buf)))))

  ;;
  ;; Context handler
  ;;

  (describe "copilot-interactive--handle-context"
    (it "returns nil for unknown skills"
      (expect (copilot-interactive--handle-context
               (list :skillId "unknown-skill"))
              :to-equal nil))

    (it "returns context for current-editor skill"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () nil)")
        (let* ((source-buf (current-buffer))
               (chat-buf (get-buffer-create "*copilot-interactive-test-ctx*")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (copilot-interactive-mode)
                  (setq copilot-interactive--source-buffer source-buf))
                (with-current-buffer chat-buf
                  (let ((result (copilot-interactive--handle-context
                                 (list :skillId "current-editor"))))
                    (expect result :to-be-truthy)
                    (expect (plist-get result :languageId) :to-equal "emacs-lisp")
                    (expect (plist-get result :source) :to-match "defun foo"))))
            (kill-buffer chat-buf))))))

  ;;
  ;; Context doc generation
  ;;

  (describe "copilot-interactive--generate-context-doc"
    (it "returns nil when source buffer is nil"
      (with-temp-buffer
        (setq copilot-interactive--source-buffer nil)
        (expect (copilot-interactive--generate-context-doc) :not :to-be-truthy)))

    (it "returns nil when source buffer is dead"
      (let ((dead-buf (generate-new-buffer " *dead*")))
        (kill-buffer dead-buf)
        (with-temp-buffer
          (setq copilot-interactive--source-buffer dead-buf)
          (expect (copilot-interactive--generate-context-doc) :not :to-be-truthy))))

    (it "returns doc with source from live buffer"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(message \"hello\")")
        (let ((source-buf (current-buffer)))
          (with-temp-buffer
            (setq copilot-interactive--source-buffer source-buf)
            (let ((doc (copilot-interactive--generate-context-doc)))
              (expect doc :to-be-truthy)
              (expect (plist-get doc :source) :to-match "hello")
              (expect (plist-get doc :languageId) :to-equal "emacs-lisp")))))))

  ;;
  ;; Inline error display
  ;;

  (describe "copilot-interactive--insert-error"
    (it "inserts error message in chat buffer"
      (let ((buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode))
              (copilot-interactive--insert-error "Something went wrong")
              (with-current-buffer buf
                (expect (buffer-string) :to-match "\\[Error: Something went wrong\\]")))
          (kill-buffer buf))))

    (it "does nothing when no chat buffer exists"
      (when (get-buffer copilot-interactive--buffer-name)
        (kill-buffer copilot-interactive--buffer-name))
      ;; Should not error
      (copilot-interactive--insert-error "test error")))

  ;;
  ;; Insert prompt
  ;;

  (describe "copilot-interactive--insert-prompt"
    (it "inserts formatted prompt into buffer"
      (with-temp-buffer
        (copilot-interactive-mode)
        (let ((inhibit-read-only t))
          (copilot-interactive--insert-prompt "What is Emacs?")
          (expect (buffer-string) :to-match "You:")
          (expect (buffer-string) :to-match "What is Emacs?")
          (expect (buffer-string) :to-match "Copilot:")))))

  ;;
  ;; Mode-line lighter
  ;;

  (describe "copilot-interactive--mode-line"
    (it "shows streaming status when active"
      (with-temp-buffer
        (copilot-interactive-mode)
        (setq copilot-interactive--streaming-p t)
        (expect (copilot-interactive--mode-line) :to-match "Streaming")))

    (it "shows plain name when idle"
      (with-temp-buffer
        (copilot-interactive-mode)
        (setq copilot-interactive--streaming-p nil)
        (expect (copilot-interactive--mode-line) :to-equal " Copilot-Interactive")
        (expect (copilot-interactive--mode-line) :not :to-match "Streaming"))))

  ;;
  ;; Reset
  ;;

  (describe "copilot-interactive-reset"
    (it "clears state and buffer"
      (let ((buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--conversation-id "test-conv")
                (setq copilot-interactive--current-turn-id "test-turn")
                (let ((inhibit-read-only t))
                  (insert "some content")))
              (spy-on 'copilot--connection-alivep :and-return-value nil)
              (copilot-interactive-reset)
              (with-current-buffer buf
                (expect copilot-interactive--conversation-id :not :to-be-truthy)
                (expect copilot-interactive--current-turn-id :not :to-be-truthy)
                (expect (buffer-string) :to-equal "")))
          (kill-buffer buf))))

    (it "does nothing when no chat buffer exists"
      ;; Should not error
      (when (get-buffer copilot-interactive--buffer-name)
        (kill-buffer copilot-interactive--buffer-name))
      (copilot-interactive-reset)))

  ;;
  ;; Destroy
  ;;

  (describe "copilot-interactive--destroy"
    (it "cleans up active-buffers for this chat"
      (let ((buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--conversation-id "conv-1"))
              (let ((copilot-interactive--active-buffers
                     (list (cons "token-1" buf)
                           (cons "token-2" (get-buffer-create " *other*")))))
                (spy-on 'copilot--connection-alivep :and-return-value nil)
                (copilot-interactive--destroy)
                ;; Only the entry for buf should be removed
                (expect (length copilot-interactive--active-buffers) :to-equal 1)
                (expect (cdar copilot-interactive--active-buffers) :not :to-equal buf)
                (kill-buffer " *other*")))
          (kill-buffer buf)))))

  ;;
  ;; Input validation
  ;;

  (describe "copilot-interactive (command)"
    (it "rejects empty messages"
      (expect (copilot-interactive "") :to-throw 'user-error)))

  (describe "copilot-interactive-send"
    (it "rejects empty messages"
      (expect (copilot-interactive-send "") :to-throw 'user-error))

    (it "errors when no chat buffer exists"
      (when (get-buffer copilot-interactive--buffer-name)
        (kill-buffer copilot-interactive--buffer-name))
      (expect (copilot-interactive-send "hello") :to-throw 'user-error)))

  ;;
  ;; Conversation creation params
  ;;

  (describe "copilot-interactive--create"
    ;; copilot--async-request is a macro that expands to
    ;; jsonrpc--async-request-1, so we spy on the latter to capture params.
    (it "sends allSkills as a boolean"
      (let ((captured-params nil)
            (buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-interactive--create "hello" #'ignore)
              (let ((caps (plist-get captured-params :capabilities)))
                (expect caps :to-be-truthy)
                (expect (plist-get caps :allSkills) :to-equal t)))
          (kill-buffer buf))))

    (it "includes model when copilot-interactive-model is set"
      (let ((captured-params nil)
            (copilot-interactive-model "gpt-4o")
            (buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-interactive--create "hello" #'ignore)
              (expect (plist-get captured-params :model) :to-equal "gpt-4o"))
          (kill-buffer buf))))

    (it "omits model when copilot-interactive-model is nil"
      (let ((captured-params nil)
            (copilot-interactive-model nil)
            (buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-interactive--create "hello" #'ignore)
              (expect (plist-member captured-params :model) :not :to-be-truthy))
          (kill-buffer buf)))))

  ;;
  ;; Streaming cancellation
  ;;

  (describe "copilot-interactive-stop"
    (it "cancels streaming and inserts cancelled marker"
      (let ((buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p t)
                (setq copilot-interactive--request-id 42))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc-notify)
              (copilot-interactive-stop)
              (with-current-buffer buf
                (expect copilot-interactive--streaming-p :not :to-be-truthy)
                (expect copilot-interactive--request-id :not :to-be-truthy)
                (expect (buffer-string) :to-match "Cancelled"))
              (expect 'jsonrpc-notify :to-have-been-called))
          (kill-buffer buf))))

    (it "falls back to reset when not streaming"
      (let ((buf (get-buffer-create copilot-interactive--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-interactive-mode)
                (setq copilot-interactive--streaming-p nil)
                (setq copilot-interactive--conversation-id "conv-1")
                (let ((inhibit-read-only t))
                  (insert "some content")))
              (spy-on 'copilot--connection-alivep :and-return-value nil)
              (copilot-interactive-stop)
              (with-current-buffer buf
                (expect copilot-interactive--conversation-id :not :to-be-truthy)
                (expect (buffer-string) :to-equal "")))
          (kill-buffer buf)))))

  (describe "copilot-interactive-send-region"
    (it "formats code with language id"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun bar () t)")
        ;; Spy on copilot-interactive to capture the message
        (spy-on 'copilot-interactive :and-return-value nil)
        (copilot-interactive-send-region (point-min) (point-max) "Explain this")
        (let ((msg (car (spy-calls-args-for 'copilot-interactive 0))))
          (expect msg :to-match "Explain this")
          (expect msg :to-match "```emacs-lisp")
          (expect msg :to-match "defun bar"))))

    (it "sends code without prompt when prompt is empty"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(+ 1 2)")
        (spy-on 'copilot-interactive :and-return-value nil)
        (copilot-interactive-send-region (point-min) (point-max) "")
        (let ((msg (car (spy-calls-args-for 'copilot-interactive 0))))
          (expect msg :to-match "```emacs-lisp")
          (expect msg :to-match "(\\+ 1 2)"))))))

;;; copilot-interactive-test.el ends here
