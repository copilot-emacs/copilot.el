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

  ;;
  ;; Conversation creation params
  ;;

  (describe "copilot-chat--create"
    ;; copilot--async-request is a macro that expands to
    ;; jsonrpc--async-request-1, so we spy on the latter to capture params.
    (it "sends allSkills as a boolean"
      (let ((captured-params nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (let ((caps (plist-get captured-params :capabilities)))
                (expect caps :to-be-truthy)
                (expect (plist-get caps :allSkills) :to-equal t)))
          (kill-buffer buf))))

    (it "includes model when copilot-chat-model is set"
      (let ((captured-params nil)
            (copilot-chat-model "gpt-4o")
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (expect (plist-get captured-params :model) :to-equal "gpt-4o"))
          (kill-buffer buf))))

    (it "omits model when copilot-chat-model is nil"
      (let ((captured-params nil)
            (copilot-chat-model nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (expect (plist-member captured-params :model) :not :to-be-truthy))
          (kill-buffer buf)))))

  ;;
  ;; Tool definitions
  ;;

  (describe "copilot-chat--tool-definitions"
    (it "returns a vector of tool definitions"
      (let ((tools (copilot-chat--tool-definitions)))
        (expect (vectorp tools) :to-be-truthy)
        (expect (length tools) :to-equal 5)))

    (it "includes run_in_terminal tool"
      (let* ((tools (append (copilot-chat--tool-definitions) nil))
             (tool (cl-find "run_in_terminal" tools
                            :key (lambda (t) (plist-get t :name))
                            :test #'equal)))
        (expect tool :to-be-truthy)
        (expect (plist-get tool :description) :to-be-truthy)
        (expect (plist-get tool :inputSchema) :to-be-truthy))))

  ;;
  ;; Tool result helper
  ;;

  (describe "copilot-chat--tool-result"
    (it "builds a result with status and content"
      (let ((result (copilot-chat--tool-result "success" "output")))
        (expect (plist-get result :status) :to-equal "success")
        (let ((content (plist-get result :content)))
          (expect (vectorp content) :to-be-truthy)
          (expect (plist-get (aref content 0) :value) :to-equal "output")))))

  ;;
  ;; Tool confirmation
  ;;

  (describe "copilot-chat--handle-tool-confirmation"
    (it "auto-approves tools in copilot-chat-auto-approve-tools"
      (let ((copilot-chat-auto-approve-tools '("get_errors" "fetch_web_page")))
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "get_errors"
                       :input (list :filePaths ["foo.el"])))
                :to-equal "Accept")))

    (it "prompts for tools not in auto-approve list"
      (let ((copilot-chat-auto-approve-tools '("get_errors")))
        (spy-on 'yes-or-no-p :and-return-value t)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "ls")))
                :to-equal "Accept")
        (expect 'yes-or-no-p :to-have-been-called)))

    (it "returns Dismiss when user declines"
      (let ((copilot-chat-auto-approve-tools nil))
        (spy-on 'yes-or-no-p :and-return-value nil)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "rm -rf /")))
                :to-equal "Dismiss"))))

  ;;
  ;; Tool invocation dispatch
  ;;

  (describe "copilot-chat--handle-tool-invocation"
    (it "dispatches run_in_terminal"
      (spy-on 'copilot-chat--execute-run-in-terminal
              :and-return-value (copilot-chat--tool-result "success" "ok"))
      (let ((result (copilot-chat--handle-tool-invocation
                     (list :name "run_in_terminal"
                           :input (list :command "echo hi")))))
        (expect 'copilot-chat--execute-run-in-terminal :to-have-been-called)
        (expect (plist-get result :status) :to-equal "success")))

    (it "dispatches insert_edit_into_file"
      (spy-on 'copilot-chat--execute-insert-edit
              :and-return-value (copilot-chat--tool-result "success" "ok"))
      (copilot-chat--handle-tool-invocation
       (list :name "insert_edit_into_file"
             :input (list :filePath "/tmp/test" :code "code")))
      (expect 'copilot-chat--execute-insert-edit :to-have-been-called))

    (it "dispatches create_file"
      (spy-on 'copilot-chat--execute-create-file
              :and-return-value (copilot-chat--tool-result "success" "ok"))
      (copilot-chat--handle-tool-invocation
       (list :name "create_file"
             :input (list :filePath "/tmp/test" :content "content")))
      (expect 'copilot-chat--execute-create-file :to-have-been-called))

    (it "dispatches get_errors"
      (spy-on 'copilot-chat--execute-get-errors
              :and-return-value (copilot-chat--tool-result "success" "ok"))
      (copilot-chat--handle-tool-invocation
       (list :name "get_errors"
             :input (list :filePaths ["foo.el"])))
      (expect 'copilot-chat--execute-get-errors :to-have-been-called))

    (it "dispatches fetch_web_page"
      (spy-on 'copilot-chat--execute-fetch-web-page
              :and-return-value (copilot-chat--tool-result "success" "ok"))
      (copilot-chat--handle-tool-invocation
       (list :name "fetch_web_page"
             :input (list :urls ["https://example.com"])))
      (expect 'copilot-chat--execute-fetch-web-page :to-have-been-called))

    (it "returns error for unknown tools"
      (let ((result (copilot-chat--handle-tool-invocation
                     (list :name "unknown_tool"
                           :input (list :foo "bar")))))
        (expect (plist-get result :status) :to-equal "error"))))

  ;;
  ;; Tool execution
  ;;

  (describe "copilot-chat--execute-run-in-terminal"
    (it "runs shell command and returns output"
      ;; Suppress chat buffer insertion
      (spy-on 'copilot-chat--insert-tool-status)
      (let ((result (copilot-chat--execute-run-in-terminal
                     (list :command "echo hello"))))
        (expect (plist-get result :status) :to-equal "success")
        (expect (plist-get (aref (plist-get result :content) 0) :value)
                :to-match "hello"))))

  (describe "copilot-chat--execute-create-file"
    (it "creates a file with content"
      (spy-on 'copilot-chat--insert-tool-status)
      (let* ((tmp-file (make-temp-name
                        (expand-file-name "copilot-test-" temporary-file-directory)))
             (result (copilot-chat--execute-create-file
                      (list :filePath tmp-file :content "test content"))))
        (unwind-protect
            (progn
              (expect (plist-get result :status) :to-equal "success")
              (expect (file-exists-p tmp-file) :to-be-truthy)
              (expect (with-temp-buffer
                        (insert-file-contents tmp-file)
                        (buffer-string))
                      :to-equal "test content"))
          (when (file-exists-p tmp-file)
            (delete-file tmp-file))))))

  (describe "copilot-chat--execute-get-errors"
    (it "reports files not open in editor"
      (spy-on 'copilot-chat--insert-tool-status)
      (let ((result (copilot-chat--execute-get-errors
                     (list :filePaths ["/nonexistent/file.el"]))))
        (expect (plist-get result :status) :to-equal "success")
        (expect (plist-get (aref (plist-get result :content) 0) :value)
                :to-match "not open in editor"))))

  ;;
  ;; Tool status display
  ;;

  (describe "copilot-chat--insert-tool-status"
    (it "inserts tool status in chat buffer"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (copilot-chat--insert-tool-status "run_in_terminal" "Running: ls")
              (with-current-buffer buf
                (expect (buffer-string) :to-match "\\[Tool: run_in_terminal\\]")
                (expect (buffer-string) :to-match "Running: ls")))
          (kill-buffer buf))))

    (it "does nothing when no chat buffer exists"
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      ;; Should not error
      (copilot-chat--insert-tool-status "test" "noop")))

  ;;
  ;; Agent mode in copilot-chat--create
  ;;

  (describe "copilot-chat--create (agent mode)"
    (it "includes agent mode params when enabled"
      (let ((captured-params nil)
            (copilot-chat-use-agent-mode t)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (expect (plist-get captured-params :chatMode) :to-equal "Agent")
              (expect (plist-get captured-params :needToolCallConfirmation)
                      :to-equal t))
          (kill-buffer buf))))

    (it "omits agent mode params when disabled"
      (let ((captured-params nil)
            (copilot-chat-use-agent-mode nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (expect (plist-member captured-params :chatMode)
                      :not :to-be-truthy))
          (kill-buffer buf))))

    (it "registers tools after successful create"
      (let ((copilot-chat-use-agent-mode t)
            (callback-called nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'copilot-chat--register-tools)
              ;; Capture the success-fn and call it
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method _params &rest args)
                        (let ((success-fn (plist-get args :success-fn)))
                          (when success-fn
                            (funcall success-fn
                                     (list :conversationId "test"
                                           :turnId "turn-1"))))
                        (cons nil nil)))
              (copilot-chat--create "hello"
                                    (lambda (_r) (setq callback-called t)))
              (expect 'copilot-chat--register-tools :to-have-been-called)
              (expect callback-called :to-be-truthy))
          (kill-buffer buf)))))

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
