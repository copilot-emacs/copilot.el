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
          copilot-chat--model-resolved nil
          copilot-chat--session-approved-tools nil)
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
                :to-equal #'copilot-chat-stop)
        (expect (lookup-key copilot-chat-mode-map (kbd "C-c C-i"))
                :to-equal #'copilot-chat-insert-code-block)))

    (it "sets a status header line whose :eval yields the status string"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect header-line-format :to-be-truthy)
        ;; Evaluate the actual `(:eval FORM)' element the way redisplay
        ;; would, so a typo in the referenced function fails the test.
        (expect (car header-line-format) :to-be :eval)
        (expect (substring-no-properties (eval (nth 1 header-line-format) t))
                :to-equal
                (substring-no-properties (copilot-chat--status-header)))))

    (it "sets no header line when copilot-chat-show-status-header is nil"
      (let ((copilot-chat-show-status-header nil))
        (with-temp-buffer
          (copilot-chat-mode)
          (expect header-line-format :to-be nil)))))

  ;;
  ;; Status header
  ;;

  (describe "copilot-chat--status-header"
    (it "shows agent mode with the model and the tool count"
      (let ((copilot-chat-use-agent-mode t)
            (copilot-chat-model "gpt-5-codex")
            (copilot-chat--mcp-servers nil))
        (expect (substring-no-properties (copilot-chat--status-header))
                :to-equal
                (format "Copilot Chat  Agent mode  •  gpt-5-codex  •  %d tools"
                        (length (copilot-chat--client-tool-names))))))

    (it "shows ask mode without a tool count"
      (let ((copilot-chat-use-agent-mode nil)
            (copilot-chat-model "gpt-4o"))
        (expect (substring-no-properties (copilot-chat--status-header))
                :to-equal "Copilot Chat  Ask mode  •  gpt-4o")))

    (it "falls back to the resolved model when no model is customized"
      (let ((copilot-chat-use-agent-mode nil)
            (copilot-chat-model nil)
            (copilot-chat--resolved-model "gpt-4.1"))
        (expect (substring-no-properties (copilot-chat--status-header))
                :to-equal "Copilot Chat  Ask mode  •  gpt-4.1")))

    (it "falls back to the default model when nothing is resolved"
      (let ((copilot-chat-use-agent-mode nil)
            (copilot-chat-model nil)
            (copilot-chat--resolved-model nil))
        (expect (substring-no-properties (copilot-chat--status-header))
                :to-equal "Copilot Chat  Ask mode  •  default model")))

    (it "counts MCP tools reported by the server"
      (let ((copilot-chat-use-agent-mode t)
            (copilot-chat-model "gpt-5-codex")
            (copilot-chat--mcp-servers
             '((:name "fetch" :status "running"
                :tools [(:name "fetch_url") (:name "fetch_html")])
               (:name "memory" :status "running"
                :tools [(:name "store")]))))
        (expect (substring-no-properties (copilot-chat--status-header))
                :to-match
                (format "  %d tools\\'"
                        (+ 3 (length (copilot-chat--client-tool-names))))))))

  ;;
  ;; Code blocks
  ;;

  (describe "copilot-chat--code-blocks"
    (it "parses fenced blocks with language and body"
      (with-temp-buffer
        (insert "intro\n```elisp\n(foo)\n(bar)\n```\nmid\n```\nplain\n```\n")
        (let ((blocks (copilot-chat--code-blocks)))
          (expect (length blocks) :to-equal 2)
          (expect (plist-get (nth 0 blocks) :lang) :to-equal "elisp")
          (expect (plist-get (nth 0 blocks) :code) :to-equal "(foo)\n(bar)\n")
          (expect (plist-get (nth 1 blocks) :lang) :to-equal "")
          (expect (plist-get (nth 1 blocks) :code) :to-equal "plain\n"))))

    (it "returns nil when there are no code blocks"
      (with-temp-buffer
        (insert "just prose, no fences")
        (expect (copilot-chat--code-blocks) :to-be nil))))

  (describe "copilot-chat--read-code-block"
    (it "returns the block surrounding point"
      (with-temp-buffer
        (insert "```js\nconsole.log(1)\n```\n")
        (goto-char (point-min))
        (forward-line 1)
        (let ((blocks (copilot-chat--code-blocks)))
          (expect (plist-get (copilot-chat--read-code-block blocks) :code)
                  :to-equal "console.log(1)\n"))))

    (it "uses the only block when point is outside any block"
      (with-temp-buffer
        (insert "prose\n```js\nx\n```\n")
        (goto-char (point-min))
        (let ((blocks (copilot-chat--code-blocks)))
          (expect (plist-get (copilot-chat--read-code-block blocks) :code)
                  :to-equal "x\n"))))

    (it "errors when there are no blocks"
      (expect (copilot-chat--read-code-block nil) :to-throw 'user-error)))

  (describe "copilot-chat-copy-code-block"
    (it "copies the block at point to the kill ring"
      (with-temp-buffer
        (copilot-chat-mode)
        (let ((inhibit-read-only t))
          (insert "```py\nprint(1)\n```\n"))
        (goto-char (point-min))
        (forward-line 1)
        (spy-on 'message)
        (copilot-chat-copy-code-block)
        (expect (current-kill 0) :to-equal "print(1)\n")))

    (it "errors when there are no code blocks"
      (with-temp-buffer
        (copilot-chat-mode)
        (expect (copilot-chat-copy-code-block) :to-throw 'user-error)))

    (it "errors on an empty code block"
      (with-temp-buffer
        (copilot-chat-mode)
        (let ((inhibit-read-only t))
          (insert "```\n\n```\n"))
        (goto-char (point-min))
        (forward-line 1)
        (expect (copilot-chat-copy-code-block) :to-throw 'user-error)))

    (it "errors outside a chat buffer"
      (with-temp-buffer
        (insert "```py\nx\n```\n")
        (expect (copilot-chat-copy-code-block) :to-throw 'user-error))))

  (describe "copilot-chat-insert-code-block"
    (it "inserts the block into the source buffer"
      (let ((src (get-buffer-create "*copilot-apply-src*")))
        (unwind-protect
            (with-temp-buffer
              (copilot-chat-mode)
              (setq copilot-chat--source-buffer src)
              (let ((inhibit-read-only t))
                (insert "```c\nint x;\n```\n"))
              (goto-char (point-min))
              (forward-line 1)
              (spy-on 'message)
              (copilot-chat-insert-code-block)
              (expect (with-current-buffer src (buffer-string))
                      :to-match "int x;"))
          (kill-buffer src))))

    (it "refuses to insert into a read-only target"
      (let ((src (get-buffer-create "*copilot-apply-ro*")))
        (unwind-protect
            (progn
              (with-current-buffer src (setq buffer-read-only t))
              (with-temp-buffer
                (copilot-chat-mode)
                (setq copilot-chat--source-buffer src)
                (let ((inhibit-read-only t))
                  (insert "```c\ny\n```\n"))
                (goto-char (point-min))
                (forward-line 1)
                (expect (copilot-chat-insert-code-block)
                        :to-throw 'user-error)))
          (kill-buffer src)))))

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
  ;; Turn-end error display
  ;;

  (describe "turn error display in progress handler"
    (it "surfaces an error reported at the end of a turn"
      (let ((buf (get-buffer-create "*copilot-chat-test-turn-error*")))
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
                                    :result (list :error "model overloaded"))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Error: model overloaded"))))
          (kill-buffer buf))))

    (it "shows no error line for a clean turn"
      (let ((buf (get-buffer-create "*copilot-chat-test-clean-turn*")))
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
                                    :result (list :followUp "next"))))
                (with-current-buffer buf
                  (expect (buffer-string) :not :to-match "Error:"))))
          (kill-buffer buf))))

    (it "renders a structured error as its message"
      (let ((buf (get-buffer-create "*copilot-chat-test-struct-error*")))
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
                                    :result (list :error (list :code 500
                                                               :message "overloaded")))))
                (with-current-buffer buf
                  (expect (buffer-string) :to-match "Error: overloaded")
                  ;; Not the raw plist.
                  (expect (buffer-string) :not :to-match ":code"))))
          (kill-buffer buf))))

    (it "clears a stale follow-up when the result is not an object"
      (let ((buf (get-buffer-create "*copilot-chat-test-stale-fu*")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--streaming-p t
                      copilot-chat--follow-up "old suggestion"))
              (let ((copilot-chat--active-buffers
                     (list (cons "test-token" buf))))
                (copilot-chat--handle-progress
                 (list :token "test-token"
                       :value (list :kind "end" :result "not-an-object")))
                (with-current-buffer buf
                  (expect (buffer-string) :not :to-match "old suggestion")
                  (expect copilot-chat--follow-up :to-be nil))))
          (kill-buffer buf)))))

  (describe "copilot-chat--error-text"
    (it "returns a bare string error unchanged"
      (expect (copilot-chat--error-text "boom") :to-equal "boom"))

    (it "extracts the message from a structured error"
      (expect (copilot-chat--error-text '(:code 500 :message "overloaded"))
              :to-equal "overloaded"))

    (it "returns nil when there is no error"
      (expect (copilot-chat--error-text nil) :to-be nil)))

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
              (expect (plist-get doc :languageId) :to-equal "emacs-lisp"))))))

    (it "suppresses the indentation-offset warning while generating"
      (with-temp-buffer
        (let ((source-buf (current-buffer))
              (captured 'unset))
          (setq copilot-chat--source-buffer source-buf)
          (spy-on 'copilot--generate-doc :and-call-fake
                  (lambda () (setq captured copilot-indent-offset-warning-disable)
                    '()))
          (spy-on 'copilot--get-source :and-return-value "")
          (copilot-chat--generate-context-doc)
          (expect captured :to-be t)))))

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
              (expect (plist-get captured-params :model) :to-equal "gpt-4o")
              (expect (plist-get (plist-get captured-params :modelInfo) :id)
                      :to-equal "gpt-4o"))
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
              (expect (plist-get captured-params :model) :to-equal "auto")
              (expect (plist-get (plist-get captured-params :modelInfo) :id)
                      :to-equal "auto"))
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
              (expect (plist-member captured-params :model) :not :to-be-truthy)
              (expect (plist-member captured-params :modelInfo)
                      :not :to-be-truthy))
          (kill-buffer buf))))

    (it "clears session tool approvals for a new conversation"
      (let ((copilot-chat--session-approved-tools '("read_file"))
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode))
              (spy-on 'copilot-chat--default-model :and-return-value nil)
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-return-value (cons nil nil))
              (copilot-chat--create "hello" #'ignore)
              (expect copilot-chat--session-approved-tools :to-be nil))
          (kill-buffer buf)))))

  ;;
  ;; Follow-up turn params
  ;;

  (describe "copilot-chat--send-turn"
    (it "includes model when copilot-chat-model is set"
      (let ((captured-params nil)
            (copilot-chat-model "gpt-4o")
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--conversation-id "conv-1"))
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--send-turn "follow-up")
              (expect (plist-get captured-params :model) :to-equal "gpt-4o")
              (expect (plist-get (plist-get captured-params :modelInfo) :id)
                      :to-equal "gpt-4o"))
          (kill-buffer buf))))

    (it "sends a server-resolved default when copilot-chat-model is nil"
      (let ((captured-params nil)
            (copilot-chat-model nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--conversation-id "conv-1"))
              (spy-on 'copilot-chat--default-model :and-return-value "auto")
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--send-turn "follow-up")
              (expect (plist-get captured-params :model) :to-equal "auto")
              (expect (plist-get (plist-get captured-params :modelInfo) :id)
                      :to-equal "auto"))
          (kill-buffer buf))))

    (it "omits model when no default can be resolved"
      (let ((captured-params nil)
            (copilot-chat-model nil)
            (buf (get-buffer-create copilot-chat--buffer-name)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (copilot-chat-mode)
                (setq copilot-chat--conversation-id "conv-1"))
              (spy-on 'copilot-chat--default-model :and-return-value nil)
              (spy-on 'copilot--connection-alivep :and-return-value t)
              (spy-on 'jsonrpc--async-request-1
                      :and-call-fake
                      (lambda (_conn _method params &rest _args)
                        (setq captured-params params)
                        (cons nil nil)))
              (copilot-chat--send-turn "follow-up")
              (expect (plist-member captured-params :model) :not :to-be-truthy)
              (expect (plist-member captured-params :modelInfo)
                      :not :to-be-truthy))
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

  (describe "copilot-chat--model-supports-tools-p"
    (it "returns non-nil when the model reports tool support"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "gpt-4o"
                          :capabilities (list :supports (list :tool_calls t)))))
      (expect (copilot-chat--model-supports-tools-p "gpt-4o") :to-be-truthy))

    (it "returns nil when the model reports no tool support"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "gpt-4o"
                          :capabilities (list :supports
                                              (list :tool_calls :json-false)))))
      (expect (copilot-chat--model-supports-tools-p "gpt-4o") :to-be nil))

    (it "assumes support when capabilities are absent"
      (spy-on 'copilot-chat--chat-models :and-return-value
              (list (list :id "auto")))
      (expect (copilot-chat--model-supports-tools-p "auto") :to-be-truthy))

    (it "assumes support when the model is unknown"
      (spy-on 'copilot-chat--chat-models :and-return-value nil)
      (expect (copilot-chat--model-supports-tools-p "mystery") :to-be-truthy)))

  (describe "copilot-chat--maybe-warn-model-lacks-tools"
    (before-each
      (spy-on 'message))

    (it "warns in agent mode when the model can't call tools"
      (let ((copilot-chat-use-agent-mode t))
        (spy-on 'copilot-chat--model :and-return-value "gpt-4o")
        (spy-on 'copilot-chat--model-supports-tools-p :and-return-value nil)
        (copilot-chat--maybe-warn-model-lacks-tools)
        (expect 'message :to-have-been-called)))

    (it "stays quiet when the model supports tools"
      (let ((copilot-chat-use-agent-mode t))
        (spy-on 'copilot-chat--model :and-return-value "gpt-4o")
        (spy-on 'copilot-chat--model-supports-tools-p :and-return-value t)
        (copilot-chat--maybe-warn-model-lacks-tools)
        (expect 'message :not :to-have-been-called)))

    (it "stays quiet when agent mode is off"
      (let ((copilot-chat-use-agent-mode nil))
        (spy-on 'copilot-chat--model-supports-tools-p)
        (copilot-chat--maybe-warn-model-lacks-tools)
        (expect 'copilot-chat--model-supports-tools-p :not :to-have-been-called)
        (expect 'message :not :to-have-been-called)))

    (it "never signals when tool support can't be determined"
      (let ((copilot-chat-use-agent-mode t))
        (spy-on 'copilot-chat--model :and-return-value "gpt-4o")
        (spy-on 'copilot-chat--model-supports-tools-p :and-throw-error 'error)
        (expect (copilot-chat--maybe-warn-model-lacks-tools) :not :to-throw))))

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
        (spy-on 'copilot-chat--ask-tool :and-return-value 'allow)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "ls")))
                :to-equal '(:result "accept"))
        (expect 'copilot-chat--ask-tool :to-have-been-called)))

    (it "dismisses when user declines"
      (let ((copilot-chat-auto-approve-tools nil))
        (spy-on 'copilot-chat--ask-tool :and-return-value 'deny)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "run_in_terminal"
                       :input (list :command "rm -rf /")))
                :to-equal '(:result "dismiss"))))

    (it "auto-approves a server tool listed by its full name"
      (let ((copilot-chat-auto-approve-tools '("copilot.read_file")))
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/x.el")))
                :to-equal '(:result "accept"))))

    (it "does not auto-approve a namespaced tool by its base name alone"
      (let ((copilot-chat-auto-approve-tools '("read_file")))
        (spy-on 'copilot-chat--ask-tool :and-return-value 'deny)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/x.el")))
                :to-equal '(:result "dismiss"))
        (expect 'copilot-chat--ask-tool :to-have-been-called)))

    (it "remembers a tool approved with `always' for the session"
      (let ((copilot-chat-auto-approve-tools nil)
            (copilot-chat--session-approved-tools nil))
        (spy-on 'copilot-chat--ask-tool :and-return-value 'always)
        ;; First call prompts and is approved...
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/x.el")))
                :to-equal '(:result "accept"))
        (expect copilot-chat--session-approved-tools :to-contain "read_file")
        ;; ...the next call to the same tool is auto-approved without asking.
        (spy-calls-reset 'copilot-chat--ask-tool)
        (expect (copilot-chat--handle-tool-confirmation
                 (list :name "copilot.read_file"
                       :input (list :filePath "/tmp/y.el")))
                :to-equal '(:result "accept"))
        (expect 'copilot-chat--ask-tool :not :to-have-been-called)))

    (describe "confirmation prompt"
      (it "summarizes the tool"
        (expect (copilot-chat--confirmation-prompt
                 (list :name "run_in_terminal" :input (list :command "make test")))
                :to-match "run shell command: make test"))

      (it "falls back to the server message for unknown tools"
        (let ((prompt (copilot-chat--confirmation-prompt
                       (list :name "copilot.some_new_tool"
                             :input (list :foo "bar")
                             :message "Allow the new tool to run?"))))
          (expect prompt :to-match "Allow the new tool to run?")
          (expect prompt :not :to-match ":foo")))

      (it "shows the raw input when no summary or server message exists"
        (let ((prompt (copilot-chat--confirmation-prompt
                       (list :name "copilot.mystery_tool"
                             :input (list :command "rm -rf /")))))
          (expect prompt :to-match "mystery_tool")
          (expect prompt :to-match "rm -rf /")))

      (it "stays sensible when the request has no tool name"
        (expect (copilot-chat--confirmation-prompt (list :input nil))
                :not :to-match "run nil"))))

  (describe "copilot-chat--ask-tool"
    (it "maps the yes/no/always choices to decisions"
      (spy-on 'read-multiple-choice :and-return-value '(?y "yes" ""))
      (expect (copilot-chat--ask-tool (list :name "x")) :to-equal 'allow)
      (spy-on 'read-multiple-choice :and-return-value '(?n "no" ""))
      (expect (copilot-chat--ask-tool (list :name "x")) :to-equal 'deny)
      (spy-on 'read-multiple-choice :and-return-value '(?a "always" ""))
      (expect (copilot-chat--ask-tool (list :name "x")) :to-equal 'always)))

  (describe "server-side tool activity logging"
    (it "logs a status line for an approved server tool"
      (let ((copilot-chat-auto-approve-tools '("copilot.read_file")))
        (spy-on 'copilot-chat--insert-tool-status)
        (copilot-chat--handle-tool-confirmation
         (list :name "copilot.read_file" :input (list :filePath "/tmp/x.el")))
        (expect 'copilot-chat--insert-tool-status
                :to-have-been-called-with "read_file" "read file: /tmp/x.el")))

    (it "does not log a client tool at confirmation time"
      ;; Client tools (run_in_terminal etc.) log their own progress while
      ;; executing, so they must not also log here.
      (spy-on 'copilot-chat--ask-tool :and-return-value 'allow)
      (spy-on 'copilot-chat--insert-tool-status)
      (copilot-chat--handle-tool-confirmation
       (list :name "run_in_terminal" :input (list :command "ls")))
      (expect 'copilot-chat--insert-tool-status :not :to-have-been-called))

    (it "does not log a dismissed tool"
      (spy-on 'copilot-chat--ask-tool :and-return-value 'deny)
      (spy-on 'copilot-chat--insert-tool-status)
      (copilot-chat--handle-tool-confirmation
       (list :name "copilot.read_file" :input (list :filePath "/tmp/x.el")))
      (expect 'copilot-chat--insert-tool-status :not :to-have-been-called)))

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
        (spy-on 'read-multiple-choice :and-call-fake
                (lambda (&rest _)
                  (setq shown (get-buffer "*copilot-chat-tool-preview*"))
                  '(?y "yes" "")))
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
        (spy-on 'copilot-chat--ask-tool :and-return-value 'allow)
        (copilot-chat--handle-tool-confirmation
         (list :name "create_file"
               :input (list :filePath "/tmp/x.el" :content "hi")))
        (expect (get-buffer "*copilot-chat-tool-preview*") :to-be nil)))

    (it "does not preview non-editing tools"
      (let ((copilot-chat-preview-tool-edits t))
        (spy-on 'copilot-chat--ask-tool :and-return-value 'deny)
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

  ;;
  ;; Workspace search
  ;;

  (describe "copilot-chat--run-argv"
    (it "captures output and a zero exit code"
      (let ((result (copilot-chat--run-argv "t" '("echo" "hi") 30)))
        (expect (plist-get result :status) :to-equal 'success)
        (expect (plist-get result :output) :to-match "hi")
        (expect (plist-get result :exit-code) :to-equal 0))))

  (describe "copilot-chat--run-ripgrep"
    (it "returns matching lines from a real ripgrep run"
      (assume (copilot-chat--ripgrep) "ripgrep not installed")
      (let* ((dir (make-temp-file "copilot-rg" t)))
        (unwind-protect
            (progn
              (with-temp-file (expand-file-name "a.txt" dir) (insert "needle\n"))
              (with-temp-file (expand-file-name "b.txt" dir) (insert "hay\n"))
              ;; exit 0: a match exists.
              (expect (copilot-chat--run-ripgrep
                       '("--no-heading" "-e" "needle" ".") dir)
                      :to-be-truthy)
              ;; exit 1: no match, treated as empty (not an error).
              (expect (copilot-chat--run-ripgrep
                       '("--no-heading" "-e" "zzzmissing" ".") dir)
                      :to-be nil))
          (delete-directory dir t))))

    (it "returns nil when ripgrep is unavailable"
      (spy-on 'copilot-chat--ripgrep :and-return-value nil)
      (expect (copilot-chat--run-ripgrep '("-e" "x" ".") "/tmp") :to-be nil)))

  (describe "copilot-chat--parse-rg-match"
    (it "parses a path:line:text match relative to dir"
      (let ((m (copilot-chat--parse-rg-match "src/foo.el:42:  (defun bar ())"
                                             "/proj/")))
        (expect (plist-get m :lineNumber) :to-equal 42)
        (expect (plist-get m :lineText) :to-equal "  (defun bar ())")
        (expect (plist-get m :uri) :to-match "/proj/src/foo\\.el")))

    (it "returns nil for a non-match line"
      (expect (copilot-chat--parse-rg-match "no colon number here" "/p")
              :to-be nil)))

  (describe "copilot-chat--handle-find-files"
    (it "returns matching files as a vector of URIs"
      (spy-on 'copilot-chat--run-ripgrep :and-return-value
              '("src/a.el" "src/b.el"))
      (let* ((result (copilot-chat--handle-find-files
                      (list :baseUri "file:///proj" :pattern "*.el")))
             (uris (plist-get result :uris)))
        (expect (vectorp uris) :to-be-truthy)
        (expect (length uris) :to-equal 2)
        (expect (aref uris 0) :to-match "/proj/src/a\\.el")))

    (it "caps results at maxResults"
      (spy-on 'copilot-chat--run-ripgrep :and-return-value
              '("a" "b" "c" "d"))
      (let ((result (copilot-chat--handle-find-files
                     (list :baseUri "file:///proj" :maxResults 2))))
        (expect (length (plist-get result :uris)) :to-equal 2)))

    (it "returns an empty vector when ripgrep is unavailable"
      (spy-on 'copilot-chat--run-ripgrep :and-return-value nil)
      (let ((result (copilot-chat--handle-find-files
                     (list :baseUri "file:///proj" :pattern "*.el"))))
        (expect (plist-get result :uris) :to-equal [])))

    (it "returns an empty vector when no directory can be resolved"
      (spy-on 'copilot--workspace-root :and-return-value nil)
      (spy-on 'copilot-chat--run-ripgrep)
      (let ((result (copilot-chat--handle-find-files (list :pattern "*.el"))))
        (expect (plist-get result :uris) :to-equal [])
        (expect 'copilot-chat--run-ripgrep :not :to-have-been-called))))

  (describe "copilot-chat--handle-find-text-in-files"
    (it "returns parsed matches as a vector"
      (spy-on 'copilot-chat--run-ripgrep :and-return-value
              '("src/a.el:3:hello" "src/b.el:7:hello world"))
      (let* ((result (copilot-chat--handle-find-text-in-files
                      (list :baseUri "file:///proj" :query "hello")))
             (matches (plist-get result :matches)))
        (expect (length matches) :to-equal 2)
        (expect (plist-get (aref matches 0) :lineNumber) :to-equal 3)
        (expect (plist-get (aref matches 1) :lineText) :to-equal "hello world")))

    (it "treats the query as a regex by default"
      (let (captured)
        (spy-on 'copilot-chat--run-ripgrep :and-call-fake
                (lambda (args _dir) (setq captured args) nil))
        (copilot-chat--handle-find-text-in-files
         (list :baseUri "file:///proj" :query "f.*o" :isRegexp t))
        (expect (member "--fixed-strings" captured) :to-be nil)))

    (it "passes --fixed-strings for a literal query"
      (let (captured)
        (spy-on 'copilot-chat--run-ripgrep :and-call-fake
                (lambda (args _dir) (setq captured args) nil))
        (copilot-chat--handle-find-text-in-files
         (list :baseUri "file:///proj" :query "literal"))
        (expect (member "--fixed-strings" captured) :to-be-truthy)))

    (it "passes the query after -e so a dash query is not a flag"
      (let (captured)
        (spy-on 'copilot-chat--run-ripgrep :and-call-fake
                (lambda (args _dir) (setq captured args) nil))
        (copilot-chat--handle-find-text-in-files
         (list :baseUri "file:///proj" :query "-foo"))
        ;; "-foo" appears immediately after "-e".
        (expect (cadr (member "-e" captured)) :to-equal "-foo")))

    (it "passes an explicit search path so ripgrep never reads stdin"
      ;; Without a path argument, a piped-stdin ripgrep hangs waiting for
      ;; input instead of searching the directory.
      (let (captured)
        (spy-on 'copilot-chat--run-ripgrep :and-call-fake
                (lambda (args _dir) (setq captured args) nil))
        (copilot-chat--handle-find-text-in-files
         (list :baseUri "file:///proj" :query "x"))
        (expect (member "." captured) :to-be-truthy)))

    (it "returns no matches and runs nothing for a missing query"
      (spy-on 'copilot-chat--run-ripgrep)
      (let ((result (copilot-chat--handle-find-text-in-files
                     (list :baseUri "file:///proj"))))
        (expect (plist-get result :matches) :to-equal [])
        (expect 'copilot-chat--run-ripgrep :not :to-have-been-called))))

  (describe "copilot-chat--handle-watched-files"
    (it "returns workspace files as a vector of URIs"
      (spy-on 'copilot-chat--run-ripgrep :and-return-value
              '("a.el" "src/b.el"))
      (let* ((result (copilot-chat--handle-watched-files
                      (list :uri "file:///proj")))
             (files (plist-get result :files)))
        (expect (vectorp files) :to-be-truthy)
        (expect (length files) :to-equal 2)
        (expect (aref files 0) :to-match "/proj/a\\.el")))

    (it "enumerates files with ripgrep --files"
      (let (captured)
        (spy-on 'copilot-chat--run-ripgrep :and-call-fake
                (lambda (args _dir) (setq captured args) nil))
        (copilot-chat--handle-watched-files (list :uri "file:///proj"))
        (expect (member "--files" captured) :to-be-truthy)))

    (it "returns an empty vector when no directory resolves"
      (spy-on 'copilot--workspace-root :and-return-value nil)
      (spy-on 'copilot-chat--run-ripgrep)
      (let ((result (copilot-chat--handle-watched-files nil)))
        (expect (plist-get result :files) :to-equal [])
        (expect 'copilot-chat--run-ripgrep :not :to-have-been-called)))

    (it "warns once when ripgrep is unavailable"
      (let ((copilot-chat--ripgrep-warned nil))
        (spy-on 'copilot-chat--ripgrep :and-return-value nil)
        (spy-on 'copilot-chat--run-ripgrep)
        (spy-on 'display-warning)
        (copilot-chat--handle-watched-files (list :uri "file:///proj"))
        (copilot-chat--handle-watched-files (list :uri "file:///proj"))
        (expect 'display-warning :to-have-been-called-times 1))))

  (describe "copilot-chat--handle-read-file"
    (it "returns the file contents"
      (let ((tmp (make-temp-file "copilot-read")))
        (unwind-protect
            (progn
              (with-temp-file tmp (insert "hello world\n"))
              (let ((result (copilot-chat--handle-read-file
                             (list :uri (copilot--path-to-uri tmp)))))
                (expect (plist-get result :content) :to-equal "hello world\n")))
          (delete-file tmp))))

    (it "caps content at the byte limit without splitting a character"
      (let ((tmp (make-temp-file "copilot-read"))
            (copilot-chat--read-file-max-bytes 5)
            (coding-system-for-write 'utf-8-unix))
        (unwind-protect
            (progn
              ;; "é" is two UTF-8 bytes; the 5-byte cap lands mid-character.
              (with-temp-file tmp (insert "aaaaéb"))
              (let ((content (plist-get (copilot-chat--handle-read-file
                                         (list :uri (copilot--path-to-uri tmp)))
                                        :content)))
                ;; No raw partial byte at the tail.
                (expect content :to-equal "aaaa")))
          (delete-file tmp))))

    (it "signals an error for an unreadable file"
      (expect (copilot-chat--handle-read-file
               (list :uri "file:///no/such/file.xyz"))
              :to-throw))

    (it "signals an error for a non-file URI"
      (expect (copilot-chat--handle-read-file (list :uri "untitled:/x"))
              :to-throw)))

  (describe "copilot-chat--attr-file-type"
    (it "reports 1 for a regular file (nil attr type)"
      (expect (copilot-chat--attr-file-type "f" "/d" nil) :to-equal 1))

    (it "reports 2 for a directory (t attr type)"
      (expect (copilot-chat--attr-file-type "d" "/d" t) :to-equal 2))

    (it "sets the symlink bit over the target type"
      (let ((target (make-temp-file "copilot-ft"))
            (dir (make-temp-file "copilot-ft" t)))
        (unwind-protect
            (progn
              (make-symbolic-link target (expand-file-name "flink" dir))
              ;; attr type for a symlink is the target path string.
              (expect (copilot-chat--attr-file-type "flink" dir target)
                      :to-equal 65))
          (delete-file target)
          (delete-directory dir t)))))

  (describe "copilot-chat--handle-read-directory"
    (it "lists children with their file types"
      (let ((dir (make-temp-file "copilot-rd" t)))
        (unwind-protect
            (progn
              (with-temp-file (expand-file-name "a.txt" dir) (insert "x"))
              (make-directory (expand-file-name "sub" dir))
              (let* ((result (copilot-chat--handle-read-directory
                              (list :uri (copilot--path-to-uri dir))))
                     ;; Render the entries and match structurally, avoiding
                     ;; a byte-compiled `plist-get' quirk on some Emacsen.
                     (s (prin1-to-string (plist-get result :entries))))
                (expect (length (plist-get result :entries)) :to-equal 2)
                (expect s :to-match ":name \"a.txt\" :type 1")
                (expect s :to-match ":name \"sub\" :type 2")))
          (delete-directory dir t))))

    (it "returns an empty vector for a non-directory file URI"
      (let ((result (copilot-chat--handle-read-directory
                     (list :uri "file:///no/such/dir"))))
        (expect (plist-get result :entries) :to-equal [])))

    (it "lists hidden entries too"
      (let ((dir (make-temp-file "copilot-rd" t)))
        (unwind-protect
            (progn
              (with-temp-file (expand-file-name ".hidden" dir) (insert "x"))
              (let ((result (copilot-chat--handle-read-directory
                             (list :uri (copilot--path-to-uri dir)))))
                (expect (prin1-to-string (plist-get result :entries))
                        :to-match ":name \"\\.hidden\"")))
          (delete-directory dir t)))))

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

  (describe "copilot-chat--buffer-diagnostics"
    (it "uses flymake diagnostics when flymake-mode is on"
      (with-temp-buffer
        (setq-local flymake-mode t)
        (cl-letf (((symbol-function 'flymake-diagnostics) (lambda () (list 'd)))
                  ((symbol-function 'flymake-diagnostic-beg) (lambda (_) (point-min)))
                  ((symbol-function 'flymake-diagnostic-text) (lambda (_) "flym err")))
          (expect (copilot-chat--buffer-diagnostics "/x.el")
                  :to-equal '("/x.el:1: flym err")))))

    (it "falls back to flycheck diagnostics when only flycheck-mode is on"
      (with-temp-buffer
        (setq-local flycheck-mode t)
        (cl-letf (((symbol-function 'flycheck-current-errors) (lambda () (list 'e)))
                  ((symbol-function 'flycheck-error-line) (lambda (_) 7))
                  ((symbol-function 'flycheck-error-message) (lambda (_) "flyc err")))
          (expect (copilot-chat--buffer-diagnostics "/x.el")
                  :to-equal '("/x.el:7: flyc err")))))

    (it "reports no errors when a backend is on but clean"
      (with-temp-buffer
        (setq-local flycheck-mode t)
        (cl-letf (((symbol-function 'flycheck-current-errors) (lambda () nil)))
          (expect (copilot-chat--buffer-diagnostics "/x.el")
                  :to-equal '("/x.el: no errors")))))

    (it "reports when no diagnostics backend is available"
      (with-temp-buffer
        (expect (copilot-chat--buffer-diagnostics "/x.el")
                :to-equal '("/x.el: no diagnostics available"))))

    (it "merges both backends when flymake and flycheck are both on"
      (with-temp-buffer
        (setq-local flymake-mode t flycheck-mode t)
        (cl-letf (((symbol-function 'flymake-diagnostics) (lambda () (list 'd)))
                  ((symbol-function 'flymake-diagnostic-beg) (lambda (_) (point-min)))
                  ((symbol-function 'flymake-diagnostic-text) (lambda (_) "flym err"))
                  ((symbol-function 'flycheck-current-errors) (lambda () (list 'e)))
                  ((symbol-function 'flycheck-error-line) (lambda (_) 7))
                  ((symbol-function 'flycheck-error-message) (lambda (_) "flyc err")))
          (expect (copilot-chat--buffer-diagnostics "/x.el")
                  :to-equal '("/x.el:1: flym err" "/x.el:7: flyc err"))))))

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

  ;;
  ;; One-shot task commands
  ;;

  (describe "copilot-chat--task-bounds"
    (it "uses the active region when there is one"
      (with-temp-buffer
        (insert "(defun foo () 1)\n(defun bar () 2)\n")
        (let ((transient-mark-mode t))
          (goto-char (point-min))
          (set-mark (point))
          (goto-char (line-end-position))
          (expect (copilot-chat--task-bounds)
                  :to-equal (cons (point-min) (line-end-position))))))

    (it "falls back to the defun at point when no region is active"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  42)\n")
        (goto-char (+ (point-min) 3))
        (let ((bounds (copilot-chat--task-bounds)))
          (expect (buffer-substring (car bounds) (cdr bounds))
                  :to-match "defun foo"))))

    (it "errors with neither a region nor a defun at point"
      (with-temp-buffer
        (expect (copilot-chat--task-bounds) :to-throw 'user-error)))

    (it "does not treat prose as a defun outside prog-mode"
      ;; In text-ish modes `bounds-of-thing-at-point' returns sections or
      ;; parenthesized fragments; the fallback must not fire there.
      (with-temp-buffer
        (text-mode)
        (insert "Steps:\n(1) do this\n(2) do that\n")
        (goto-char (+ (point-min) 8))
        (expect (copilot-chat--task-bounds) :to-throw 'user-error)))

    (it "still finds the defun in prog-mode buffers"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  42)\n")
        (goto-char (+ (point-min) 3))
        (expect (copilot-chat--task-bounds) :not :to-be nil))))

  (describe "copilot-chat--task-prompt"
    (it "looks up the task's prompt in copilot-chat-task-prompts"
      (let ((copilot-chat-task-prompts '((review . "Do review:"))))
        (expect (copilot-chat--task-prompt 'review) :to-equal "Do review:")))

    (it "errors for a task without a configured prompt"
      (let ((copilot-chat-task-prompts nil))
        (expect (copilot-chat--task-prompt 'review) :to-throw 'user-error))))

  (describe "copilot-chat-task"
    (it "sends the defun at point with the task prompt through the chat"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  42)\n")
        (goto-char (+ (point-min) 3))
        (spy-on 'copilot-chat)
        (copilot-chat-task 'review)
        (let ((msg (car (spy-calls-args-for 'copilot-chat 0))))
          (expect msg :to-match "Review the following code")
          (expect msg :to-match "```emacs-lisp")
          (expect msg :to-match "defun foo"))))

    (it "sends only the active region, not the surrounding code"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () 1)\n(defun bar () 2)\n")
        (spy-on 'copilot-chat)
        (let ((transient-mark-mode t))
          (goto-char (point-min))
          (set-mark (point))
          (goto-char (line-end-position))
          (copilot-chat-task 'fix))
        (let ((msg (car (spy-calls-args-for 'copilot-chat 0))))
          (expect msg :to-match "defun foo")
          (expect msg :not :to-match "defun bar"))))

    (it "reads the task with completion when called interactively"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () 1)")
        (goto-char (+ (point-min) 3))
        (spy-on 'completing-read :and-return-value "tests")
        (spy-on 'copilot-chat-send-region)
        (call-interactively #'copilot-chat-task)
        (expect (nth 2 (spy-calls-args-for 'copilot-chat-send-region 0))
                :to-equal (alist-get 'tests copilot-chat-task-prompts))))

    (it "rejects empty interactive input instead of interning it"
      (spy-on 'completing-read :and-return-value "")
      (expect (call-interactively #'copilot-chat-task) :to-throw 'user-error))

    (it "offers only symbol keys for completion"
      (let ((copilot-chat-task-prompts '((review . "Do review:")
                                         ("rogue" . "String key:"))))
        (spy-on 'completing-read :and-return-value "review")
        (spy-on 'copilot-chat-send-region)
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "(defun foo () 1)")
          (goto-char (+ (point-min) 3))
          (call-interactively #'copilot-chat-task))
        (expect (cadr (spy-calls-args-for 'completing-read 0))
                :to-equal '(review)))))

  (describe "named task commands"
    (it "dispatches each command to its task"
      (spy-on 'copilot-chat-task)
      (copilot-chat-review)
      (copilot-chat-fix)
      (copilot-chat-doc)
      (copilot-chat-optimize)
      (copilot-chat-write-tests)
      (expect 'copilot-chat-task :to-have-been-called-with 'review)
      (expect 'copilot-chat-task :to-have-been-called-with 'fix)
      (expect 'copilot-chat-task :to-have-been-called-with 'doc)
      (expect 'copilot-chat-task :to-have-been-called-with 'optimize)
      (expect 'copilot-chat-task :to-have-been-called-with 'tests)))

  (describe "context references"
    ;; Start from a clean slate: no stale chat buffer leaked from an
    ;; earlier spec, so the "create if needed" path is exercised honestly.
    (before-each
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name)))
    (after-each
      (when (get-buffer copilot-chat--buffer-name)
        (kill-buffer copilot-chat--buffer-name)))

    (it "builds a file reference with a file URI"
      (let ((ref (copilot-chat--file-reference "/tmp/foo.el")))
        ;; Compare the whole plist so any divergence prints both values.
        (expect (prin1-to-string ref) :to-match
                "\\`(:type \"file\" :uri \"file://")))

    (it "creates the chat buffer when none exists"
      (spy-on 'message)
      (with-temp-buffer
        (expect (get-buffer copilot-chat--buffer-name) :to-be nil)
        (copilot-chat-add-file-reference "/tmp/foo.el")
        (expect (get-buffer copilot-chat--buffer-name) :to-be-truthy)))

    (it "attaches a region reference with the selection range"
      (spy-on 'message)
      (let ((src (get-buffer-create "*copilot-ref-src*")))
        (unwind-protect
            (with-current-buffer src
              (setq buffer-file-name "/tmp/ref-src.el")
              (insert "line1\nline2\nline3\n")
              (copilot-chat-add-region-reference (point-min) (point-max))
              (with-current-buffer copilot-chat--buffer-name
                (let ((ref (car copilot-chat--references)))
                  (expect (plist-get ref :uri) :to-match "ref-src.el")
                  (expect (plist-get (plist-get ref :selection) :start)
                          :to-be-truthy)
                  (expect (plist-get (plist-get ref :selection) :end)
                          :to-be-truthy))))
          (with-current-buffer src (set-buffer-modified-p nil))
          (kill-buffer src))))

    (it "errors on a region reference outside a file"
      (with-temp-buffer
        (insert "x")
        (expect (copilot-chat-add-region-reference (point-min) (point-max))
                :to-throw 'user-error)))

    (it "builds a (:references VECTOR) param without consuming it"
      (with-current-buffer (get-buffer-create copilot-chat--buffer-name)
        (copilot-chat-mode)
        (setq copilot-chat--references
              (list (list :type "file" :uri "file:///a")
                    (list :type "file" :uri "file:///b")))
        (let ((param (copilot-chat--references-param)))
          (expect (vectorp (plist-get param :references)) :to-be-truthy)
          (expect (length (plist-get param :references)) :to-equal 2)
          ;; Reverses to insertion order: /b was pushed last, /a first.
          (expect (plist-get (aref (plist-get param :references) 0) :uri)
                  :to-equal "file:///b"))
        ;; Building the param leaves the pending list intact so a failed
        ;; request does not lose the attachment.
        (expect (length copilot-chat--references) :to-equal 2)
        (copilot-chat--consume-references)
        (expect copilot-chat--references :to-be nil)))

    (it "clears pending references on request"
      (with-current-buffer (get-buffer-create copilot-chat--buffer-name)
        (copilot-chat-mode)
        (setq copilot-chat--references (list (list :type "file" :uri "x"))))
      (spy-on 'message)
      (copilot-chat-clear-references)
      (with-current-buffer copilot-chat--buffer-name
        (expect copilot-chat--references :to-be nil))))

  (describe "copilot-chat--chat-templates"
    ;; copilot--request is a macro over jsonrpc-request, so stub the
    ;; transport rather than the macro.
    (before-each
      (setq copilot-chat--templates nil)
      (spy-on 'copilot--workspace-root :and-return-value nil)
      (spy-on 'copilot--connection-alivep :and-return-value t))
    (after-each (setq copilot-chat--templates nil))

    (it "filters templates to the chat-panel scope"
      (let ((copilot-chat-use-agent-mode nil))
        (spy-on 'jsonrpc-request :and-return-value
                (vector (list :id "explain" :scopes ["chat-panel"])
                        (list :id "agentonly" :scopes ["agent-panel"])))
        (let ((tpls (copilot-chat--chat-templates)))
          (expect (length tpls) :to-equal 1)
          (expect (plist-get (car tpls) :id) :to-equal "explain"))))

    (it "filters to the agent-panel scope in agent mode"
      (let ((copilot-chat-use-agent-mode t))
        (spy-on 'jsonrpc-request :and-return-value
                (vector (list :id "explain" :scopes ["chat-panel"])
                        (list :id "agentcmd" :scopes ["agent-panel"])))
        (expect (plist-get (car (copilot-chat--chat-templates)) :id)
                :to-equal "agentcmd")))

    (it "sends a percent-encoded workspace-folder URI"
      (let ((copilot-chat-use-agent-mode nil)
            (captured nil))
        (spy-on 'copilot--workspace-root :and-return-value "/tmp/My Project")
        (spy-on 'jsonrpc-request :and-call-fake
                (lambda (_conn _method params &rest _)
                  (setq captured params)
                  []))
        (copilot-chat--chat-templates)
        (let ((uri (plist-get (aref (plist-get captured :workspaceFolders) 0)
                              :uri)))
          (expect uri :to-match "My%20Project"))))

    (it "caches the result and queries the server once"
      (let ((copilot-chat-use-agent-mode nil))
        (spy-on 'jsonrpc-request :and-return-value
                (vector (list :id "explain" :scopes ["chat-panel"])))
        (copilot-chat--chat-templates)
        (copilot-chat--chat-templates)
        (expect 'jsonrpc-request :to-have-been-called-times 1)))

    (it "returns no templates when the query fails"
      (let ((copilot-chat-use-agent-mode nil))
        (spy-on 'copilot--log)
        (spy-on 'jsonrpc-request :and-throw-error 'error)
        (expect (copilot-chat--chat-templates) :to-be nil))))

  (describe "copilot-chat-slash-command"
    (it "sends the chosen command with arguments as a message"
      (spy-on 'copilot-chat--chat-templates :and-return-value
              (list (list :id "explain" :description "Explain code"
                          :scopes ["chat-panel"])))
      (spy-on 'completing-read :and-return-value "/explain  Explain code")
      (spy-on 'read-string :and-return-value "this function")
      (spy-on 'copilot-chat)
      (copilot-chat-slash-command)
      (expect 'copilot-chat :to-have-been-called-with "/explain this function"))

    (it "sends a bare command when no arguments are given"
      (spy-on 'copilot-chat--chat-templates :and-return-value
              (list (list :id "tests" :description "Generate tests"
                          :scopes ["chat-panel"])))
      (spy-on 'completing-read :and-return-value "/tests  Generate tests")
      (spy-on 'read-string :and-return-value "  ")
      (spy-on 'copilot-chat)
      (copilot-chat-slash-command)
      (expect 'copilot-chat :to-have-been-called-with "/tests"))

    (it "errors when no slash commands are available"
      (spy-on 'copilot-chat--chat-templates :and-return-value nil)
      (expect (copilot-chat-slash-command) :to-throw 'user-error)))

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
        (expect (cdar captured-choices) :to-equal "gpt-4o"))))

  (describe "copilot-chat--handle-mcp-tools"
    (before-each (setq copilot-chat--mcp-servers nil
                       copilot-chat--mcp-warned-errors nil))
    (after-each (setq copilot-chat--mcp-servers nil
                      copilot-chat--mcp-warned-errors nil))

    (it "caches the reported servers"
      (copilot-chat--handle-mcp-tools
       (list :servers (vector (list :name "fetch" :status "running"
                                    :tools (vector (list :name "get"))))))
      (expect (length copilot-chat--mcp-servers) :to-equal 1)
      (expect (plist-get (car copilot-chat--mcp-servers) :name)
              :to-equal "fetch"))

    (it "warns once about a newly failed server"
      (spy-on 'copilot--log)
      (let ((msg (list :servers
                       (vector (list :name "bad" :status "error"
                                     :error "spawn failed")))))
        (copilot-chat--handle-mcp-tools msg)
        ;; A repeat notification with the same error does not warn again.
        (copilot-chat--handle-mcp-tools msg))
      (expect 'copilot--log :to-have-been-called-times 1))

    (it "does not warn about a healthy server"
      (spy-on 'copilot--log)
      (copilot-chat--handle-mcp-tools
       (list :servers (vector (list :name "ok" :status "running"))))
      (expect 'copilot--log :not :to-have-been-called))

    (it "does not warn about an empty error string"
      (spy-on 'copilot--log)
      (copilot-chat--handle-mcp-tools
       (list :servers (vector (list :name "ok" :status "stopped" :error ""))))
      (expect 'copilot--log :not :to-have-been-called)))

  (describe "copilot-chat-list-mcp-tools"
    (after-each
      (setq copilot-chat--mcp-servers nil)
      (when (get-buffer "*copilot-mcp-tools*")
        (kill-buffer "*copilot-mcp-tools*")))

    (it "lists servers, status, and tools"
      (let ((copilot-chat--mcp-servers
             (list (list :name "fetch" :status "running"
                         :tools (vector (list :name "get_url"
                                              :description "Fetch a URL"))))))
        (spy-on 'display-buffer)
        (copilot-chat-list-mcp-tools)
        (with-current-buffer "*copilot-mcp-tools*"
          (let ((text (buffer-string)))
            (expect text :to-match "fetch")
            (expect text :to-match "running")
            (expect text :to-match "get_url")
            (expect text :to-match "Fetch a URL")))))

    (it "renders tools with a missing name or non-string description"
      (let ((copilot-chat--mcp-servers
             (list (list :name "weird" :status "running"
                         :tools (vector (list :description 42))))))
        (spy-on 'display-buffer)
        ;; Must not raise on the non-string description.
        (copilot-chat-list-mcp-tools)
        (with-current-buffer "*copilot-mcp-tools*"
          (expect (buffer-string) :to-match "(unnamed)"))))

    (it "distinguishes unconfigured from not-yet-reported"
      (spy-on 'display-buffer)
      (let ((copilot-chat--mcp-servers nil)
            (copilot-mcp-servers nil))
        (copilot-chat-list-mcp-tools)
        (with-current-buffer "*copilot-mcp-tools*"
          (expect (buffer-string) :to-match "No MCP servers configured")))
      (let ((copilot-chat--mcp-servers nil)
            (copilot-mcp-servers '(:fetch (:command "uvx"))))
        (copilot-chat-list-mcp-tools)
        (with-current-buffer "*copilot-mcp-tools*"
          (expect (buffer-string) :to-match "reported yet")))))

  ;;
  ;; Commit message generation
  ;;

  (describe "copilot-chat--staged-diff"
    ;; Real git in temp directories: the earlier spy-based specs stubbed
    ;; out exactly the root resolution this function must get right.
    (it "errors when not inside a git repository"
      (let ((default-directory (make-temp-file "copilot-no-repo" t)))
        (expect (copilot-chat--staged-diff) :to-throw 'user-error)))

    (it "errors when there are no staged changes"
      (let ((default-directory (make-temp-file "copilot-repo" t)))
        (process-file "git" nil nil nil "init" "-q")
        (expect (copilot-chat--staged-diff) :to-throw 'user-error)))

    (it "returns the staged diff with git resolving the repository"
      (let ((default-directory (make-temp-file "copilot-repo" t)))
        (process-file "git" nil nil nil "init" "-q")
        (write-region "hello\n" nil (expand-file-name "f.txt") nil 'quiet)
        (process-file "git" nil nil nil "add" "f.txt")
        (expect (copilot-chat--staged-diff) :to-match "\\+hello"))))

  (describe "copilot-chat--commit-message-request"
    (it "prepends the prompt to the staged diff"
      (spy-on 'copilot-chat--staged-diff :and-return-value "THE DIFF")
      (let ((copilot-chat-commit-message-prompt "Write a commit message."))
        (expect (copilot-chat--commit-message-request)
                :to-equal "Write a commit message.\n\nTHE DIFF"))))

  (describe "copilot-chat--strip-code-fences"
    (it "strips a fence wrapping the whole reply"
      (expect (copilot-chat--strip-code-fences "```\nfeat: x\n\nbody\n```")
              :to-equal "feat: x\n\nbody"))

    (it "strips a fence with a language tag and trailing whitespace"
      (expect (copilot-chat--strip-code-fences "```text\nfeat: x\n```\n")
              :to-equal "feat: x"))

    (it "leaves an unfenced reply alone, trimmed"
      (expect (copilot-chat--strip-code-fences "feat: x\n\nbody\n")
              :to-equal "feat: x\n\nbody"))

    (it "keeps a fence that does not wrap the whole reply"
      (expect (copilot-chat--strip-code-fences "feat: x\n\n```\ncode\n```")
              :to-equal "feat: x\n\n```\ncode\n```"))

    (it "strips four-backtick fences"
      (expect (copilot-chat--strip-code-fences "````\nfeat: x\n````")
              :to-equal "feat: x")))

  (describe "copilot-chat--handle-progress with a function sink"
    (it "passes progress values to the sink and unregisters it on end"
      (let* ((seen '())
             (copilot-chat--active-buffers
              (list (cons "tok" (lambda (value) (push value seen))))))
        (copilot-chat--handle-progress
         (list :token "tok" :value '(:kind "report" :reply "x")))
        (copilot-chat--handle-progress
         (list :token "tok" :value '(:kind "end")))
        (expect (length seen) :to-equal 2)
        (expect copilot-chat--active-buffers :not :to-be-truthy)))

    (it "unregisters the sink even when its callback signals"
      (let ((copilot-chat--active-buffers
             (list (cons "tok" (lambda (_value) (error "boom")))))
            (copilot-chat--one-shot-requests '(("tok" . 1))))
        (expect (copilot-chat--handle-progress
                 (list :token "tok" :value '(:kind "end")))
                :to-throw 'error)
        (expect copilot-chat--active-buffers :not :to-be-truthy)
        (expect copilot-chat--one-shot-requests :not :to-be-truthy))))

  (describe "copilot-chat--one-shot"
    (it "collects the streamed reply and destroys the conversation"
      (let ((copilot-chat--active-buffers nil)
            (requests '())
            (results '()))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'copilot-chat--default-model :and-return-value nil)
        (spy-on 'jsonrpc--async-request-1
                :and-call-fake
                (lambda (_conn method params &rest args)
                  (push (cons method params) requests)
                  (when (eq method 'conversation/create)
                    (funcall (plist-get args :success-fn)
                             '(:conversationId "conv-1" :turnId "t-1")))
                  (cons 1 nil)))
        (copilot-chat--one-shot
         "hello"
         (lambda (reply error-msg) (push (list reply error-msg) results)))
        (let ((token (plist-get (cdr (assq 'conversation/create requests))
                                :workDoneToken)))
          (copilot-chat--handle-progress
           (list :token token :value '(:kind "report" :reply "feat: ")))
          (copilot-chat--handle-progress
           (list :token token :value '(:kind "report" :reply "thing")))
          (copilot-chat--handle-progress
           (list :token token :value '(:kind "end"))))
        (expect results :to-equal '(("feat: thing" nil)))
        (expect (plist-get (cdr (assq 'conversation/destroy requests))
                           :conversationId)
                :to-equal "conv-1")
        (expect copilot-chat--active-buffers :not :to-be-truthy)))

    (it "reports a request error and unregisters the token"
      (let ((copilot-chat--active-buffers nil)
            (results '()))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'copilot-chat--default-model :and-return-value nil)
        (spy-on 'jsonrpc--async-request-1
                :and-call-fake
                (lambda (_conn _method _params &rest args)
                  (funcall (plist-get args :error-fn) "boom")
                  (cons 1 nil)))
        (copilot-chat--one-shot
         "hello"
         (lambda (reply error-msg) (push (list reply error-msg) results)))
        (expect results :to-equal '((nil "boom")))
        (expect copilot-chat--active-buffers :not :to-be-truthy)))

    (it "registers nothing when the request cannot be issued"
      (let ((copilot-chat--active-buffers nil)
            (copilot-chat--one-shot-requests nil))
        (spy-on 'copilot--connection-alivep :and-return-value nil)
        (spy-on 'copilot--start-server :and-throw-error 'user-error)
        (expect (copilot-chat--one-shot "hi" #'ignore) :to-throw 'user-error)
        (expect copilot-chat--active-buffers :not :to-be-truthy)
        (expect copilot-chat--one-shot-requests :not :to-be-truthy))))

  (describe "copilot-chat-stop with a pending one-shot"
    (it "cancels the one-shot instead of resetting the conversation"
      (let* ((results '())
             (copilot-chat--one-shot-requests '(("tok" . 42)))
             (copilot-chat--active-buffers
              (list (cons "tok"
                          (copilot-chat--collecting-sink
                           (lambda (reply err)
                             (push (list reply err) results)))))))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'jsonrpc-notify)
        (spy-on 'copilot-chat-reset)
        (copilot-chat-stop)
        (expect 'copilot-chat-reset :not :to-have-been-called)
        (expect 'jsonrpc-notify :to-have-been-called)
        (expect copilot-chat--active-buffers :not :to-be-truthy)
        (expect copilot-chat--one-shot-requests :not :to-be-truthy)
        (expect results :to-equal '(("" "Cancelled"))))))

  (describe "copilot-chat-insert-commit-message"
    (it "errors before sending when the diff cannot be collected"
      (spy-on 'copilot-chat--staged-diff :and-throw-error 'user-error)
      (spy-on 'copilot-chat--one-shot)
      (expect (copilot-chat-insert-commit-message) :to-throw 'user-error)
      (expect 'copilot-chat--one-shot :not :to-have-been-called))

    (it "sends the prompt followed by the staged diff"
      (spy-on 'copilot-chat--staged-diff :and-return-value "THE DIFF")
      (spy-on 'copilot-chat--one-shot)
      (spy-on 'message)
      (let ((copilot-chat-commit-message-prompt "PROMPT"))
        (with-temp-buffer (copilot-chat-insert-commit-message)))
      (expect (car (spy-calls-args-for 'copilot-chat--one-shot 0))
              :to-equal "PROMPT\n\nTHE DIFF"))

    (it "inserts the fence-stripped reply into the originating buffer"
      (let ((reply-fn nil))
        (spy-on 'copilot-chat--staged-diff :and-return-value "diff")
        (spy-on 'copilot-chat--one-shot
                :and-call-fake
                (lambda (_message callback) (setq reply-fn callback)))
        (spy-on 'message)
        (with-temp-buffer
          (copilot-chat-insert-commit-message)
          (let ((origin (current-buffer)))
            ;; The reply arrives later, with another buffer current.
            (with-temp-buffer
              (funcall reply-fn "```\nfeat: add thing\n```" nil))
            (expect (with-current-buffer origin (buffer-string))
                    :to-equal "feat: add thing")))))

    (it "reports a server error instead of inserting"
      (let ((reply-fn nil))
        (spy-on 'copilot-chat--staged-diff :and-return-value "diff")
        (spy-on 'copilot-chat--one-shot
                :and-call-fake
                (lambda (_message callback) (setq reply-fn callback)))
        (spy-on 'message)
        (with-temp-buffer
          (copilot-chat-insert-commit-message)
          (funcall reply-fn nil "boom")
          (expect (buffer-string) :to-equal ""))))

    (it "copies the reply to the kill ring when the buffer is read-only"
      (let ((reply-fn nil))
        (spy-on 'copilot-chat--staged-diff :and-return-value "diff")
        (spy-on 'copilot-chat--one-shot
                :and-call-fake
                (lambda (_message callback) (setq reply-fn callback)))
        (spy-on 'message)
        (with-temp-buffer
          (copilot-chat-insert-commit-message)
          (setq buffer-read-only t)
          (funcall reply-fn "feat: x" nil)
          (expect (buffer-string) :to-equal ""))
        (expect (current-kill 0) :to-equal "feat: x")))))

;;; copilot-chat-test.el ends here
