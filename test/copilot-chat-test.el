;;; copilot-chat-test.el --- Tests for copilot-chat.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-chat.el.

;;; Code:

(require 'buttercup)
(require 'copilot-chat)

(describe "copilot-chat"
  ;; Resolving a default model issues a synchronous request; keep it from
  ;; reaching the (absent) server during specs that don't care about it.
  ;; Specs that exercise resolution override this spy.
  (before-each
    (setq copilot-chat--resolved-model nil
          copilot-chat--model-resolved nil)
    (spy-on 'jsonrpc-request :and-return-value nil))

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

    (it "has keybindings for send and stop"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c RET"))
                :to-equal #'copilot-chat-send)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c C-c"))
                :to-equal #'copilot-chat-send)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c C-k"))
                :to-equal #'copilot-chat-stop))))

  ;;
  ;; Progress handler
  ;;

  (describe "copilot-chat--extract-reply"
    (it "extracts reply from flat :reply field"
      (expect (copilot-chat--extract-reply '(:reply "hello"))
              :to-equal "hello"))

    (it "extracts reply from :editAgentRounds"
      (expect (copilot-chat--extract-reply
               (list :editAgentRounds
                     (vector (list :roundId 1 :reply "hello from agent"))))
              :to-equal "hello from agent"))

    (it "prefers flat :reply over :editAgentRounds"
      (expect (copilot-chat--extract-reply
               (list :reply "flat"
                     :editAgentRounds
                     (vector (list :roundId 1 :reply "nested"))))
              :to-equal "flat"))

    (it "returns nil when neither field is present"
      (expect (copilot-chat--extract-reply '(:kind "report"))
              :not :to-be-truthy)))

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

    (it "appends reply from editAgentRounds on report"
      (let ((buf (get-buffer-create "*copilot-chat-test-agent*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "report"
                                    :editAgentRounds
                                    (vector (list :roundId 1
                                                  :reply "agent reply")))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "agent reply"))))
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
  ;; Follow-up display
  ;;

  (describe "follow-up display in progress handler"
    (it "inserts follow-up text on end when present"
      (let ((buf (get-buffer-create "*copilot-chat-test-followup*")))
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
                                    :result (list :followUp "Try asking about X"))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Follow-up: Try asking about X"))))
          (kill-buffer buf))))

    (it "does not insert follow-up when nil"
      (let ((buf (get-buffer-create "*copilot-chat-test-no-followup*")))
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
                (with-current-buffer buf
                  (expect (buffer-string) :not :to-match "Follow-up"))))
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
  ;; Inline error display
  ;;

  (describe "copilot-chat--insert-error"
    (it "inserts error message in chat buffer"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (copilot-chat--insert-error "Something went wrong")
              (with-current-buffer buf
                (expect (buffer-string) :to-match "\\[Error: Something went wrong\\]")))
          (kill-buffer buf))))

    (it "does nothing when no chat buffer exists"
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      ;; Should not error
      (copilot-chat--insert-error "test error")))

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
  ;; Mode-line lighter
  ;;

  (describe "copilot-chat--mode-line"
    (it "shows streaming status when active"
      (with-temp-buffer
        (copilot-chat-mode)
        (setq copilot-chat--streaming-p t)
        (expect (copilot-chat--mode-line) :to-match "Streaming")))

    (it "shows plain name when idle"
      (with-temp-buffer
        (copilot-chat-mode)
        (setq copilot-chat--streaming-p nil)
        (expect (copilot-chat--mode-line) :to-equal " Copilot-Chat")
        (expect (copilot-chat--mode-line) :not :to-match "Streaming"))))

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
      (expect (copilot-chat "") :to-throw 'user-error))

    (it "attaches a file-visiting buffer as context source"
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      (spy-on 'copilot-chat--create)
      (spy-on 'copilot-chat--send-turn)
      (spy-on 'display-buffer)
      (let ((src (get-buffer-create "*copilot-chat-test-src*")))
        (unwind-protect
            (with-current-buffer src
              (setq buffer-file-name "/tmp/copilot-chat-test.el")
              (copilot-chat "hello")
              (expect (buffer-local-value 'copilot-chat--source-buffer
                                          (get-buffer copilot-chat--buffer-name))
                      :to-be src))
          (with-current-buffer src (setq buffer-file-name nil))
          (kill-buffer src))))

    (it "does not attach a non-file buffer as context source"
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name))
      (spy-on 'copilot-chat--create)
      (spy-on 'copilot-chat--send-turn)
      (spy-on 'display-buffer)
      (let ((src (get-buffer-create "*copilot-chat-test-scratch*")))
        (unwind-protect
            (with-current-buffer src
              (copilot-chat "hello")
              (expect (buffer-local-value 'copilot-chat--source-buffer
                                          (get-buffer copilot-chat--buffer-name))
                      :to-be nil))
          (kill-buffer src)))))

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

    (it "sends a server-resolved default when copilot-chat-model is nil"
      (let ((captured-params nil)
            (copilot-chat-model nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot-chat--default-model :and-return-value "auto")
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--create "hello" #'ignore)
              (expect (plist-get captured-params :model) :to-equal "auto"))
          (kill-buffer buf))))

    (it "omits model when no default can be resolved"
      (let ((captured-params nil)
            (copilot-chat-model nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot-chat--default-model :and-return-value nil)
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
  ;; Default model resolution
  ;;

  (describe "copilot-chat--default-model"
    (before-each
      (setq copilot-chat--resolved-model nil
            copilot-chat--model-resolved nil)
      (spy-on 'copilot--connection-alivep :and-return-value t))

    (it "prefers the server-designated chat default"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "gpt-4o" :isChatDefault :json-false)
                    (list :id "claude" :isChatDefault t)
                    (list :id "auto")))
      (expect (copilot-chat--default-model) :to-equal "claude"))

    (it "falls back to the auto model when no chat default is marked"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "gpt-4o" :isChatDefault :json-false)
                    (list :id "auto" :isChatDefault :json-false)))
      (expect (copilot-chat--default-model) :to-equal "auto"))

    (it "falls back to the first chat model otherwise"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "gpt-4o") (list :id "gemini")))
      (expect (copilot-chat--default-model) :to-equal "gpt-4o"))

    (it "returns nil when the model list is unavailable"
      (spy-on 'copilot-chat--chat-models :and-throw-error 'error)
      (expect (copilot-chat--default-model) :to-be nil))

    (it "does not query the server when the connection is down"
      (spy-on 'copilot--connection-alivep :and-return-value nil)
      (spy-on 'copilot-chat--chat-models)
      (expect (copilot-chat--default-model) :to-be nil)
      (expect 'copilot-chat--chat-models :not :to-have-been-called))

    (it "caches the result, including nil, and queries the server once"
      (spy-on 'copilot-chat--chat-models :and-return-value nil)
      (copilot-chat--default-model)
      (copilot-chat--default-model)
      (expect 'copilot-chat--chat-models :to-have-been-called-times 1)))

  ;;
  ;; Streaming cancellation
  ;;

  (describe "copilot-chat-stop"
    (it "cancels streaming and inserts cancelled marker"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t)
                (setq copilot-chat--request-id 42))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc-notify)
              (copilot-chat-stop)
              (with-current-buffer buf
                (expect copilot-chat--streaming-p :not :to-be-truthy)
                (expect copilot-chat--request-id :not :to-be-truthy)
                (expect (buffer-string) :to-match "Cancelled"))
              (expect 'jsonrpc-notify :to-have-been-called))
          (kill-buffer buf))))

    (it "falls back to reset when not streaming"
      (let ((buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p nil)
                (setq copilot-chat--conversation-id "conv-1")
                (let ((inhibit-read-only t))
                  (insert "some content")))
              (spy-on 'copilot--connection-alivep :and-return-value nil)
              (copilot-chat-stop)
              (with-current-buffer buf
                (expect copilot-chat--conversation-id :not :to-be-truthy)
                (expect (buffer-string) :to-equal "")))
          (kill-buffer buf)))))

  ;;
  ;; Tool definitions
  ;;

  (describe "copilot-chat--tool-definitions"
    (it "returns a vector of tool definitions"
      (let ((tools (copilot-chat--tool-definitions)))
        (expect (vectorp tools) :to-be-truthy)
        (expect (length tools) :to-equal 4)))

    (it "includes run_in_terminal tool"
      (let* ((tools (append (copilot-chat--tool-definitions) nil))
             (tool (cl-find "run_in_terminal" tools
                            :key (lambda (tl) (plist-get tl :name))
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
                :to-equal '(:result "accept"))))

    (it "prompts for tools not in auto-approve list"
      (let ((copilot-chat-auto-approve-tools '("get_errors")))
        (spy-on 'yes-or-no-p :and-return-value t)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "ls")))
                :to-equal '(:result "accept"))
        (expect 'yes-or-no-p :to-have-been-called)))

    (it "dismisses when user declines"
      (let ((copilot-chat-auto-approve-tools nil))
        (spy-on 'yes-or-no-p :and-return-value nil)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "rm -rf /")))
                :to-equal '(:result "dismiss"))))

    (it "summarizes the tool in the confirmation prompt"
      (let ((copilot-chat-auto-approve-tools nil)
            (prompt nil))
        (spy-on 'yes-or-no-p :and-call-fake
                (lambda (msg) (setq prompt msg) nil))
        (copilot-chat--handle-tool-confirmation
         (list :name "run_in_terminal" :input (list :command "make test")))
        ;; The raw command shows, not a plist dump.
        (expect prompt :to-match "run shell command: make test")))

    (it "auto-approves a server tool listed by its full name"
      (let ((copilot-chat-auto-approve-tools '("copilot.read_file")))
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/x.el")))
                :to-equal '(:result "accept"))))

    (it "does not auto-approve a namespaced tool by its base name alone"
      (let ((copilot-chat-auto-approve-tools '("read_file")))
        (spy-on 'yes-or-no-p :and-return-value nil)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/x.el")))
                :to-equal '(:result "dismiss"))
        (expect 'yes-or-no-p :to-have-been-called)))

    (it "falls back to the server message for unknown tools"
      (let ((copilot-chat-auto-approve-tools nil)
            (prompt nil))
        (spy-on 'yes-or-no-p :and-call-fake
                (lambda (msg) (setq prompt msg) nil))
        (copilot-chat--handle-tool-confirmation
         (list :name "copilot.some_new_tool"
               :input (list :foo "bar")
               :message "Allow the new tool to run?"))
        ;; No plist dump: the server-provided message is shown verbatim.
        (expect prompt :to-match "Allow the new tool to run?")
        (expect prompt :not :to-match ":foo")))

    (it "shows the raw input when no summary or server message exists"
      (let ((copilot-chat-auto-approve-tools nil)
            (prompt nil))
        (spy-on 'yes-or-no-p :and-call-fake
                (lambda (msg) (setq prompt msg) nil))
        (copilot-chat--handle-tool-confirmation
         (list :name "copilot.mystery_tool"
               :input (list :command "rm -rf /")))
        ;; The user can still see what is being approved.
        (expect prompt :to-match "mystery_tool")
        (expect prompt :to-match "rm -rf /")))

    (it "prompts sensibly when the request has no tool name"
      (let ((copilot-chat-auto-approve-tools nil)
            (prompt nil))
        (spy-on 'yes-or-no-p :and-call-fake
                (lambda (msg) (setq prompt msg) nil))
        (copilot-chat--handle-tool-confirmation (list :input nil))
        ;; No stray "nil" leaks into the prompt.
        (expect prompt :not :to-match "run nil"))))

  ;;
  ;; Tool summary
  ;;

  (describe "copilot-chat--tool-summary"
    (it "describes a terminal command concisely"
      (expect (copilot-chat--tool-summary
               "run_in_terminal" (list :command "ls -la"))
              :to-equal "run shell command: ls -la"))

    (it "describes a file creation by path"
      (expect (copilot-chat--tool-summary
               "create_file" (list :filePath "/tmp/x.el" :content "huge"))
              :to-equal "create file: /tmp/x.el"))

    (it "joins multiple fetch URLs"
      (expect (copilot-chat--tool-summary
               "fetch_web_page" (list :urls ["https://a.com" "https://b.com"]))
              :to-equal "fetch: https://a.com, https://b.com"))

    (it "describes reading a server file by path"
      (expect (copilot-chat--tool-summary
               "read_file" (list :filePath "/tmp/x.el" :offset 1 :limit 50))
              :to-equal "read file: /tmp/x.el"))

    (it "describes a namespaced server tool by its base name"
      (expect (copilot-chat--tool-summary
               "copilot.read_file" (list :filePath "/tmp/x.el"))
              :to-equal "read file: /tmp/x.el"))

    (it "includes the explanation when editing a file"
      (expect (copilot-chat--tool-summary
               "insert_edit_into_file"
               (list :filePath "/tmp/x.el" :explanation "add error handling"))
              :to-equal "edit file /tmp/x.el: add error handling"))

    (it "describes a string replacement edit by path"
      (expect (copilot-chat--tool-summary
               "replace_string_in_file"
               (list :filePath "/tmp/x.el" :oldString "a" :newString "b"))
              :to-equal "edit file: /tmp/x.el"))

    (it "returns nil for tools without a tailored summary"
      (expect (copilot-chat--tool-summary
               "copilot.some_new_tool" (list :foo "bar"))
              :to-be nil)))

  ;;
  ;; Tool edit preview
  ;;

  (describe "copilot-chat--tool-preview"
    (it "previews a file creation with its content"
      (let ((preview (copilot-chat--tool-preview
                      "create_file"
                      (list :filePath "/tmp/x.el" :content "line1\nline2"))))
        (expect preview :to-match "Create /tmp/x.el")
        (expect preview :to-match "\\+line1")
        (expect preview :to-match "\\+line2")))

    (it "previews an edit with explanation and code"
      (let ((preview (copilot-chat--tool-preview
                      "copilot.insert_edit_into_file"
                      (list :filePath "/tmp/x.el"
                            :explanation "add guard"
                            :code "(when x y)"))))
        (expect preview :to-match "Edit /tmp/x.el")
        (expect preview :to-match "add guard")
        (expect preview :to-match "(when x y)")))

    (it "previews a string replacement as a diff"
      (let ((preview (copilot-chat--tool-preview
                      "replace_string_in_file"
                      (list :filePath "/tmp/x.el"
                            :oldString "old" :newString "new"))))
        (expect preview :to-match "-old")
        (expect preview :to-match "\\+new")))

    (it "omits the removed side for a pure insertion"
      (let ((preview (copilot-chat--tool-preview
                      "replace_string_in_file"
                      (list :filePath "/tmp/x.el"
                            :oldString "" :newString "added"))))
        (expect preview :to-match "\\+added")
        ;; No misleading lone "-" line for an empty old string.
        (expect preview :not :to-match "^-")))

    (it "returns nil for read-only and unknown tools"
      (expect (copilot-chat--tool-preview
               "copilot.read_file" (list :filePath "/tmp/x.el"))
              :to-be nil)
      (expect (copilot-chat--tool-preview
               "run_in_terminal" (list :command "ls"))
              :to-be nil)))

  (describe "tool confirmation preview"
    (it "shows a preview buffer when approving an edit tool"
      (let ((copilot-chat-preview-tool-edits t)
            (shown nil))
        (spy-on 'yes-or-no-p :and-call-fake
                (lambda (&rest _)
                  (setq shown (get-buffer "*copilot-chat-tool-preview*"))
                  t))
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "create_file"
                       :input (list :filePath "/tmp/x.el" :content "hi")))
                :to-equal '(:result "accept"))
        ;; The preview buffer existed during the prompt...
        (expect shown :to-be-truthy)
        ;; ...and is cleaned up afterwards.
        (expect (get-buffer "*copilot-chat-tool-preview*") :to-be nil)))

    (it "skips the preview when copilot-chat-preview-tool-edits is nil"
      (let ((copilot-chat-preview-tool-edits nil))
        (spy-on 'yes-or-no-p :and-return-value t)
        (copilot-chat--handle-tool-confirmation
         (list :name "create_file"
               :input (list :filePath "/tmp/x.el" :content "hi")))
        (expect (get-buffer "*copilot-chat-tool-preview*") :to-be nil)))

    (it "does not preview non-editing tools"
      (let ((copilot-chat-preview-tool-edits t))
        (spy-on 'yes-or-no-p :and-return-value nil)
        (copilot-chat--handle-tool-confirmation
         (list :name "run_in_terminal" :input (list :command "ls")))
        (expect (get-buffer "*copilot-chat-tool-preview*") :to-be nil))))

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

  (describe "copilot-chat--run-process"
    (it "captures output and a zero exit code on success"
      (let ((result (copilot-chat--run-process "echo hello")))
        (expect (plist-get result :status) :to-equal 'success)
        (expect (plist-get result :output) :to-match "hello")
        (expect (plist-get result :exit-code) :to-equal 0)))

    (it "reports a non-zero exit as error with its code"
      (let ((result (copilot-chat--run-process "exit 3")))
        (expect (plist-get result :status) :to-equal 'error)
        (expect (plist-get result :exit-code) :to-equal 3)))

    (it "leaves no stray process buffers behind"
      (copilot-chat--run-process "echo hi")
      (expect (seq-filter (lambda (b)
                            (string-prefix-p " *copilot-chat-terminal*"
                                             (buffer-name b)))
                          (buffer-list))
              :to-equal nil)))

  (describe "copilot-chat--truncate-output"
    (it "keeps short output unchanged"
      (expect (copilot-chat--truncate-output "short") :to-equal "short"))

    (it "truncates long output to the tail with a marker"
      (let* ((copilot-chat--terminal-max-output 10)
             (result (copilot-chat--truncate-output "0123456789ABCDEF")))
        (expect result :to-match "\\[output truncated\\]")
        ;; The tail is preserved.
        (expect result :to-match "ABCDEF")
        (expect result :not :to-match "012345"))))

  (describe "copilot-chat--execute-run-in-terminal"
    (before-each (spy-on 'copilot-chat--insert-tool-status))

    (it "runs shell command and returns output"
      (let ((result (copilot-chat--execute-run-in-terminal
                     (list :command "echo hello"))))
        (expect (plist-get result :status) :to-equal "success")
        (expect (plist-get (aref (plist-get result :content) 0) :value)
                :to-match "hello")))

    (it "passes a non-zero exit to the model as output with the code"
      (spy-on 'copilot-chat--run-process :and-return-value
              '(:output "boom" :status error :exit-code 2))
      (let ((result (copilot-chat--execute-run-in-terminal
                     (list :command "false"))))
        (expect (plist-get result :status) :to-equal "success")
        (expect (plist-get (aref (plist-get result :content) 0) :value)
                :to-match "exited with status 2")))

    (it "reports a timed-out command as an error"
      (spy-on 'copilot-chat--run-process :and-return-value
              '(:output "" :status timeout :exit-code nil))
      (let ((result (copilot-chat--execute-run-in-terminal
                     (list :command "sleep 99"))))
        (expect (plist-get result :status) :to-equal "error")
        (expect (plist-get (aref (plist-get result :content) 0) :value)
                :to-match "timed out")))

    (it "reports a cancelled command as cancelled"
      (spy-on 'copilot-chat--run-process :and-return-value
              '(:output "" :status cancelled :exit-code nil))
      (let ((result (copilot-chat--execute-run-in-terminal
                     (list :command "sleep 99"))))
        (expect (plist-get result :status) :to-equal "cancelled"))))

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
          (expect msg :to-match "(\\+ 1 2)")))))

  (describe "copilot-chat-select-model"
    (it "sets copilot-chat-model from user selection"
      (let ((copilot-chat-model nil))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'jsonrpc-request
                :and-return-value
                (list (list :modelName "GPT-4o" :id "gpt-4o"
                            :scopes (list "chat-panel" "edit-panel"))
                      (list :modelName "Claude 3.5 Sonnet" :id "claude-3.5-sonnet"
                            :scopes (list "chat-panel"))
                      (list :modelName "GPT-4.1 Copilot" :id "gpt-41-copilot"
                            :scopes (list "completion"))))
        (spy-on 'completing-read :and-return-value "Claude 3.5 Sonnet (claude-3.5-sonnet)")
        (copilot-chat-select-model)
        (expect copilot-chat-model :to-equal "claude-3.5-sonnet")))

    (it "excludes completion-only models"
      (let ((copilot-chat-model nil)
            (captured-choices nil))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'jsonrpc-request
                :and-return-value
                (list (list :modelName "GPT-4o" :id "gpt-4o"
                            :scopes (list "chat-panel" "edit-panel"))
                      (list :modelName "GPT-4.1 Copilot" :id "gpt-41-copilot"
                            :scopes (list "completion"))))
        (spy-on 'completing-read
                :and-call-fake
                (lambda (_prompt choices &rest _args)
                  (setq captured-choices choices)
                  "GPT-4o (gpt-4o)"))
        (copilot-chat-select-model)
        (expect (length captured-choices) :to-equal 1)
        (expect (cdar captured-choices) :to-equal "gpt-4o")))))

;;; copilot-chat-test.el ends here
