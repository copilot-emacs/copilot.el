;;; copilot-test.el --- Tests for copilot.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot.el.

;;; Code:

(require 'buttercup)
(require 'copilot)

(describe "copilot"
  (describe "loading"
    (it "provides the copilot feature"
      (expect (featurep 'copilot) :to-be-truthy))

    (it "provides the copilot-balancer feature"
      (expect (featurep 'copilot-balancer) :to-be-truthy)))

  (describe "no company dependency"
    (it "loads copilot without requiring company-mode"
      (expect (featurep 'copilot) :to-be-truthy)
      ;; copilot.el should not require or reference company-mode
      (expect (featurep 'company) :not :to-be-truthy)))

  (describe "copilot--make-connection"
    (it "handles both :events-buffer-config and :events-buffer-scrollback-size"
      ;; copilot--make-connection uses condition-case to try the new
      ;; :events-buffer-config keyword first, falling back to
      ;; :events-buffer-scrollback-size for older jsonrpc versions.
      ;; Verify make-instance is called with the right fallback logic.
      (let ((call-args nil))
        (spy-on 'copilot-server-executable :and-return-value "/bin/true")
        (spy-on 'make-process :and-return-value
                (start-process "dummy" nil "true"))
        (spy-on 'make-instance :and-call-fake
                (lambda (&rest args)
                  (setq call-args args)
                  (signal 'invalid-slot-name '(test))))
        ;; The first call signals invalid-slot-name, triggering fallback.
        ;; The second call also signals (from our spy), but we just want
        ;; to verify both keyword variants are attempted.
        (condition-case nil
            (copilot--make-connection)
          (invalid-slot-name nil))
        (expect (spy-calls-count 'make-instance) :to-be-greater-than 1))))

  (describe "copilot--request"
    (it "sends empty object when params is nil"
      (let ((sent-params nil))
        (spy-on 'copilot--connection-alivep :and-return-value t)
        (spy-on 'jsonrpc-request :and-call-fake
                (lambda (_conn _method params)
                  (setq sent-params params)
                  nil))
        (copilot--request 'signInInitiate nil)
        ;; params should be a hash table (serializes to {}), not nil
        (expect (hash-table-p sent-params) :to-be-truthy))))

  ;;
  ;; Utility functions
  ;;

  (describe "copilot--mode-symbol"
    (it "strips -mode suffix"
      (expect (copilot--mode-symbol "python-mode") :to-equal "python"))

    (it "strips -ts-mode suffix"
      (expect (copilot--mode-symbol "python-ts-mode") :to-equal "python"))

    (it "strips -ts-mode before -mode"
      (expect (copilot--mode-symbol "rust-ts-mode") :to-equal "rust"))

    (it "handles modes without -mode suffix"
      (expect (copilot--mode-symbol "fundamental") :to-equal "fundamental")))

  (describe "copilot--string-common-prefix"
    (it "finds common prefix of two strings"
      (expect (copilot--string-common-prefix "hello" "help") :to-equal "hel"))

    (it "returns empty string when no common prefix"
      (expect (copilot--string-common-prefix "abc" "xyz") :to-equal ""))

    (it "returns the shorter string when it is a prefix"
      (expect (copilot--string-common-prefix "he" "hello") :to-equal "he"))

    (it "handles identical strings"
      (expect (copilot--string-common-prefix "same" "same") :to-equal "same"))

    (it "handles empty strings"
      (expect (copilot--string-common-prefix "" "hello") :to-equal "")
      (expect (copilot--string-common-prefix "hello" "") :to-equal "")))

  (describe "copilot--get-uri"
    (it "returns buffer URI for non-file buffers"
      (with-temp-buffer
        (rename-buffer "test-buffer" t)
        (let ((uri (copilot--get-uri)))
          (expect uri :to-match "^file:///buffer/"))))

    (it "returns file URI for file-visiting buffers"
      (let ((temp-file (make-temp-file "copilot-test")))
        (unwind-protect
            (with-current-buffer (find-file-noselect temp-file)
              (let ((uri (copilot--get-uri)))
                (expect uri :to-match "^file://"))
              (kill-buffer))
          (delete-file temp-file)))))

  (describe "copilot--get-relative-path"
    (it "returns empty string for non-file buffers"
      (with-temp-buffer
        (expect (copilot--get-relative-path) :to-equal ""))))

  (describe "copilot--get-language-id"
    (it "returns language ID for known major modes"
      (with-temp-buffer
        (emacs-lisp-mode)
        (expect (copilot--get-language-id) :to-equal "emacs-lisp")))

    (it "infers language from mode name for unknown modes"
      (with-temp-buffer
        (text-mode)
        ;; text-mode -> copilot--mode-symbol strips -mode -> "text"
        (expect (copilot--get-language-id) :to-equal "text"))))

  (describe "copilot--infer-indentation-offset"
    (it "returns mode-specific offset for emacs-lisp-mode"
      (with-temp-buffer
        (emacs-lisp-mode)
        (let ((lisp-indent-offset 2))
          (expect (copilot--infer-indentation-offset) :to-equal 2))))

    (it "falls back to tab-width for unknown modes"
      (with-temp-buffer
        (fundamental-mode)
        (let ((copilot-indent-offset-warning-disable t)
              (tab-width 4))
          (expect (copilot--infer-indentation-offset) :to-equal 4))))

    (it "respects buffer-local tab-width as fallback"
      (with-temp-buffer
        (fundamental-mode)
        (let ((copilot-indent-offset-warning-disable t)
              (tab-width 8))
          (expect (copilot--infer-indentation-offset) :to-equal 8)))))

  ;;
  ;; copilot--get-source
  ;;

  (describe "copilot--get-source"
    (it "returns buffer contents"
      (with-temp-buffer
        (insert "hello world")
        (expect (copilot--get-source) :to-equal "hello world")))

    (it "returns full contents when under copilot-max-char"
      (with-temp-buffer
        (let ((copilot-max-char 100))
          (insert "short text")
          (expect (copilot--get-source) :to-equal "short text"))))

    (it "returns full contents when copilot-max-char is negative (no limit)"
      (with-temp-buffer
        (let ((copilot-max-char -1))
          (insert (make-string 200 ?x))
          (expect (length (copilot--get-source)) :to-equal 200))))

    (it "truncates buffer when exceeding copilot-max-char"
      (with-temp-buffer
        (let ((copilot-max-char 10)
              (copilot-max-char-warning-disable t))
          (insert (make-string 50 ?x))
          (goto-char (point-min))
          (expect (length (copilot--get-source)) :to-be-less-than 51))))

    (it "does not warn about copilot-max-char in non-file buffers"
      (with-temp-buffer
        (let ((copilot-max-char 10)
              (copilot-max-char-warning-disable nil))
          (insert (make-string 20 ?x))
          (spy-on 'display-warning)
          (copilot--get-source)
          (expect 'display-warning :not :to-have-been-called))))

    (it "warns about copilot-max-char in file-visiting buffers"
      (let ((temp-file (make-temp-file "copilot-test")))
        (unwind-protect
            (with-current-buffer (find-file-noselect temp-file)
              (let ((copilot-max-char 10)
                    (copilot-max-char-warning-disable nil))
                (insert (make-string 20 ?x))
                (spy-on 'display-warning)
                (copilot--get-source)
                (expect 'display-warning :to-have-been-called))
              (set-buffer-modified-p nil)
              (kill-buffer))
          (delete-file temp-file)))))

  ;;
  ;; Overlay management
  ;;

  (describe "copilot--overlay-visible"
    (it "returns nil when no overlay exists"
      (with-temp-buffer
        (setq-local copilot--overlay nil)
        (expect (copilot--overlay-visible) :not :to-be-truthy)))

    (it "returns nil for a deleted overlay"
      (with-temp-buffer
        (insert "test")
        (let ((ov (make-overlay 1 2)))
          (setq-local copilot--overlay ov)
          (delete-overlay ov)
          (expect (copilot--overlay-visible) :not :to-be-truthy)))))

  (describe "copilot-clear-overlay"
    (it "does nothing when overlay is not visible"
      (with-temp-buffer
        (setq-local copilot--overlay nil)
        ;; Should not error
        (copilot-clear-overlay))))

  (describe "copilot--get-overlay"
    (it "creates an overlay if none exists"
      (with-temp-buffer
        (insert "test")
        (setq-local copilot--overlay nil)
        (let ((ov (copilot--get-overlay)))
          (expect (overlayp ov) :to-be-truthy))))

    (it "returns existing overlay"
      (with-temp-buffer
        (insert "test")
        (let ((ov1 (copilot--get-overlay))
              (ov2 (copilot--get-overlay)))
          (expect ov1 :to-equal ov2))))

    (it "sets priority on the overlay"
      (with-temp-buffer
        (insert "test")
        (setq-local copilot--overlay nil)
        (let ((ov (copilot--get-overlay)))
          (expect (overlay-get ov 'priority) :to-equal 100)))))

  ;;
  ;; Global mode buffer filtering
  ;;

  (describe "copilot-turn-on-unless-buffer-read-only"
    (it "enables copilot-mode in writable buffers"
      (let ((buf (get-buffer-create "*copilot-writable-test*")))
        (unwind-protect
            (with-current-buffer buf
              (spy-on 'copilot--on-doc-focus)
              (spy-on 'copilot--on-doc-close)
              (copilot-turn-on-unless-buffer-read-only)
              (expect copilot-mode :to-be-truthy)
              (copilot-mode -1))
          (kill-buffer buf))))

    (it "does not enable copilot-mode in read-only buffers"
      (with-temp-buffer
        (setq buffer-read-only t)
        (copilot-turn-on-unless-buffer-read-only)
        (expect copilot-mode :not :to-be-truthy)))

    (it "does not enable copilot-mode in internal buffers"
      (let ((buf (get-buffer-create " *internal-test*")))
        (unwind-protect
            (with-current-buffer buf
              (spy-on 'copilot--on-doc-focus)
              (spy-on 'copilot--on-doc-close)
              (copilot-turn-on-unless-buffer-read-only)
              (expect copilot-mode :not :to-be-truthy))
          (kill-buffer buf))))

    (it "enables copilot-mode in normal named buffers"
      (let ((buf (get-buffer-create "*scratch-test*")))
        (unwind-protect
            (with-current-buffer buf
              (spy-on 'copilot--on-doc-focus)
              (spy-on 'copilot--on-doc-close)
              (copilot-turn-on-unless-buffer-read-only)
              (expect copilot-mode :to-be-truthy)
              (copilot-mode -1))
          (kill-buffer buf)))))

  ;;
  ;; Minor mode
  ;;

  (describe "copilot-mode"
    (it "can be enabled and disabled"
      (with-temp-buffer
        ;; Prevent actual server connection and notifications
        (spy-on 'copilot--on-doc-focus)
        (spy-on 'copilot--on-doc-close)
        (copilot-mode 1)
        (expect copilot-mode :to-be-truthy)
        (copilot-mode -1)
        (expect copilot-mode :not :to-be-truthy)))

    (it "sets up hooks when enabled"
      (with-temp-buffer
        (spy-on 'copilot--on-doc-focus)
        (spy-on 'copilot--on-doc-close)
        (copilot-mode 1)
        (expect (memq #'copilot--post-command post-command-hook) :to-be-truthy)
        (expect (memq #'copilot--pre-command pre-command-hook) :to-be-truthy)
        (copilot-mode -1)))

    (it "removes hooks when disabled"
      (with-temp-buffer
        (spy-on 'copilot--on-doc-focus)
        (spy-on 'copilot--on-doc-close)
        (copilot-mode 1)
        (copilot-mode -1)
        (expect (memq #'copilot--post-command post-command-hook) :not :to-be-truthy)
        (expect (memq #'copilot--pre-command pre-command-hook) :not :to-be-truthy))))

  ;;
  ;; Document open/close
  ;;

  (describe "copilot--on-doc-close"
    (it "does not start the server when connection is not alive"
      (with-temp-buffer
        (add-to-list 'copilot--opened-buffers (current-buffer))
        (spy-on 'copilot--connection-alivep :and-return-value nil)
        (spy-on 'jsonrpc-notify)
        (copilot--on-doc-close)
        ;; Should not send notification when server is not alive
        (expect 'jsonrpc-notify :not :to-have-been-called)
        ;; But should still clean up opened-buffers
        (expect (seq-contains-p copilot--opened-buffers (current-buffer))
                :not :to-be-truthy)))

    (it "sends notification when connection is alive"
      (with-temp-buffer
        (add-to-list 'copilot--opened-buffers (current-buffer))
        ;; copilot--connection-alivep is a defsubst (inlined when
        ;; byte-compiled), so mock the underlying pieces instead.
        (let ((copilot--connection t))
          (spy-on 'jsonrpc--process :and-return-value t)
          (spy-on 'process-exit-status :and-return-value 0)
          (spy-on 'jsonrpc-notify)
          (copilot--on-doc-close)
          ;; Should send didClose notification
          (expect 'jsonrpc-notify :to-have-been-called)
          ;; And clean up opened-buffers
          (expect (seq-contains-p copilot--opened-buffers (current-buffer))
                  :not :to-be-truthy)))))

  ;;
  ;; Predicates
  ;;

  (describe "copilot--satisfy-trigger-predicates"
    (it "returns t when all enable predicates return t and no disable predicates"
      (let ((copilot-enable-predicates (list (lambda () t)))
            (copilot-disable-predicates nil))
        (expect (copilot--satisfy-trigger-predicates) :to-be-truthy)))

    (it "returns nil when an enable predicate returns nil"
      (let ((copilot-enable-predicates (list (lambda () nil)))
            (copilot-disable-predicates nil))
        (expect (copilot--satisfy-trigger-predicates) :not :to-be-truthy)))

    (it "returns nil when a disable predicate returns t"
      (let ((copilot-enable-predicates (list (lambda () t)))
            (copilot-disable-predicates (list (lambda () t))))
        (expect (copilot--satisfy-trigger-predicates) :not :to-be-truthy))))

  ;;
  ;; Inline completion params
  ;;

  (describe "copilot--inline-completion-params"
    (it "returns correct structure with uri, position, context"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (emacs-lisp-mode)
        (insert "(defun foo () nil)")
        (let ((params (copilot--inline-completion-params 2)))
          (expect (plist-get (plist-get params :textDocument) :uri) :to-be-truthy)
          (expect (plist-get params :position) :to-be-truthy)
          (expect (plist-get (plist-get params :context) :triggerKind) :to-equal 2)
          (expect (plist-get (plist-get params :formattingOptions) :tabSize) :to-be-truthy)
          (expect (plist-member (plist-get params :formattingOptions) :insertSpaces) :to-be-truthy))))

    (it "uses trigger-kind 1 for manual invocation"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (let ((params (copilot--inline-completion-params 1)))
          (expect (plist-get (plist-get params :context) :triggerKind) :to-equal 1)))))

  ;;
  ;; Normalize completion response
  ;;

  (describe "copilot--normalize-completion-response"
    (it "returns nil for nil response"
      (expect (copilot--normalize-completion-response nil) :to-equal nil))

    (it "converts vector response to list"
      (let ((response [(:insertText "foo") (:insertText "bar")]))
        (expect (copilot--normalize-completion-response response) :to-equal
                '((:insertText "foo") (:insertText "bar")))))

    (it "extracts items from plist response"
      (let ((response (list :items [(:insertText "hello")])))
        (expect (copilot--normalize-completion-response response) :to-equal
                '((:insertText "hello")))))

    (it "returns nil for unrecognized response"
      (expect (copilot--normalize-completion-response '(:unknown "data")) :to-equal nil)))

  ;;
  ;; Doc generation
  ;;

  (describe "copilot--generate-doc"
    (it "generates a plist with required keys"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo () nil)")
        (let ((doc (copilot--generate-doc)))
          (expect (plist-get doc :version) :to-be-truthy)
          (expect (plist-get doc :tabSize) :to-be-truthy)
          (expect (plist-get doc :uri) :to-be-truthy)
          (expect (plist-get doc :languageId) :to-equal "emacs-lisp")
          (expect (plist-get doc :position) :to-be-truthy))))

    (it "includes relative path"
      (with-temp-buffer
        (let ((doc (copilot--generate-doc)))
          (expect (plist-member doc :relativePath) :to-be-truthy)))))

  ;;
  ;; Effective LSP settings
  ;;

  (describe "copilot--effective-lsp-settings"
    (it "returns copilot-lsp-settings unchanged when completion model is nil"
      (let ((copilot-completion-model nil)
            (copilot-lsp-settings '(:foo "bar")))
        (expect (copilot--effective-lsp-settings) :to-equal '(:foo "bar"))))

    (it "merges model into empty settings"
      (let ((copilot-completion-model "gpt-4o")
            (copilot-lsp-settings nil))
        (let ((result (copilot--effective-lsp-settings)))
          (expect (plist-get (plist-get (plist-get result :github) :copilot)
                             :selectedCompletionModel)
                  :to-equal "gpt-4o"))))

    (it "merges model when settings has existing :github key"
      (let ((copilot-completion-model "gpt-4o")
            (copilot-lsp-settings '(:github (:copilot (:other "value")))))
        (let ((result (copilot--effective-lsp-settings)))
          (expect (plist-get (plist-get (plist-get result :github) :copilot)
                             :selectedCompletionModel)
                  :to-equal "gpt-4o")
          (expect (plist-get (plist-get (plist-get result :github) :copilot)
                             :other)
                  :to-equal "value")))))

  ;;
  ;; LSP position
  ;;

  (describe "copilot--lsp-pos"
    (it "returns correct position at beginning of buffer"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld")
        (goto-char (point-min))
        (let ((pos (copilot--lsp-pos)))
          (expect (plist-get pos :line) :to-equal 0)
          (expect (plist-get pos :character) :to-equal 0))))

    (it "returns correct position in middle of line"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld")
        (goto-char 4) ; 'l' in "hello"
        (let ((pos (copilot--lsp-pos)))
          (expect (plist-get pos :line) :to-equal 0)
          (expect (plist-get pos :character) :to-equal 3))))

    (it "returns correct position on second line"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld")
        (goto-char 8) ; 'r' in "world"
        (let ((pos (copilot--lsp-pos)))
          (expect (plist-get pos :line) :to-equal 1)
          (expect (plist-get pos :character) :to-equal 1))))

    (it "counts BMP characters as 1 UTF-16 unit"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "café")  ; é is U+00E9, BMP
        (let ((pos (copilot--lsp-pos)))
          (expect (plist-get pos :character) :to-equal 4))))

    (it "counts supplementary plane characters as 2 UTF-16 units"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "a😀b")  ; 😀 is U+1F600, supplementary plane
        (let ((pos (copilot--lsp-pos)))
          ;; a(1) + 😀(2) + b(1) = 4 UTF-16 code units
          (expect (plist-get pos :character) :to-equal 4)))))

  ;;
  ;; UTF-16 helpers
  ;;

  (describe "copilot--utf16-strlen"
    (it "returns length for ASCII string"
      (expect (copilot--utf16-strlen "hello") :to-equal 5))

    (it "returns length for BMP characters"
      (expect (copilot--utf16-strlen "café") :to-equal 4))

    (it "counts supplementary plane characters as 2"
      ;; 😀 is U+1F600
      (expect (copilot--utf16-strlen "a😀b") :to-equal 4)))

  (describe "copilot--goto-utf16-offset"
    (it "moves correctly for ASCII text"
      (with-temp-buffer
        (insert "hello")
        (goto-char (point-min))
        (copilot--goto-utf16-offset 3)
        (expect (char-after) :to-equal ?l)))

    (it "moves correctly past emoji"
      (with-temp-buffer
        (insert "a😀b")
        (goto-char (point-min))
        ;; a=1 UTF-16 unit, 😀=2 UTF-16 units => offset 3 should land on 'b'
        (copilot--goto-utf16-offset 3)
        (expect (char-after) :to-equal ?b))))

  ;;
  ;; Settings change
  ;;

  (describe "copilot--lsp-settings-changed"
    (it "does not restart the server"
      (let ((copilot--connection nil))
        (spy-on 'copilot--start-server)
        (copilot--lsp-settings-changed 'copilot-lsp-settings '(:new "value"))
        (expect 'copilot--start-server :not :to-have-been-called)))

    (it "does not send notification when connection is not alive"
      (let ((copilot--connection nil))
        (spy-on 'jsonrpc-notify)
        (copilot--lsp-settings-changed 'copilot-lsp-settings '(:new "value"))
        (expect 'jsonrpc-notify :not :to-have-been-called))))

  ;;
  ;; Workspace root
  ;;

  (describe "copilot--workspace-root"
    (it "returns nil for non-file buffers"
      (with-temp-buffer
        (expect (copilot--workspace-root) :to-equal nil)))

    (it "returns a directory when project is detected"
      (let ((temp-file (make-temp-file "copilot-test")))
        (unwind-protect
            (with-current-buffer (find-file-noselect temp-file)
              ;; Mock project-current to return a fake project
              (spy-on 'project-current :and-return-value
                      (list 'vc 'Git (file-name-directory temp-file)))
              (spy-on 'project-root :and-return-value
                      (file-name-directory temp-file))
              (let ((root (copilot--workspace-root)))
                (expect root :to-be-truthy)
                (expect (file-directory-p root) :to-be-truthy))
              (kill-buffer))
          (delete-file temp-file)))))

  ;;
  ;; Status lighter
  ;;

  (describe "copilot--status-lighter"
    (it "returns \" Copilot\" when status is nil"
      (let ((copilot--status nil))
        (expect (copilot--status-lighter) :to-equal " Copilot")))

    (it "returns \" Copilot\" for Normal and not busy"
      (let ((copilot--status '(:kind "Normal" :busy nil :message "")))
        (expect (copilot--status-lighter) :to-equal " Copilot")))

    (it "returns \" Copilot*\" for Normal and busy"
      (let ((copilot--status '(:kind "Normal" :busy t :message "")))
        (expect (copilot--status-lighter) :to-equal " Copilot*")))

    (it "returns propertized warning string for Warning kind"
      (let ((copilot--status '(:kind "Warning" :busy nil :message "some warning")))
        (let ((result (copilot--status-lighter)))
          (expect result :to-equal " Copilot:Warning")
          (expect (get-text-property 0 'face result) :to-equal 'warning))))

    (it "returns propertized error string for Error kind"
      (let ((copilot--status '(:kind "Error" :busy nil :message "auth failed")))
        (let ((result (copilot--status-lighter)))
          (expect result :to-equal " Copilot:Error")
          (expect (get-text-property 0 'face result) :to-equal 'error))))

    (it "returns propertized inactive string for Inactive kind"
      (let ((copilot--status '(:kind "Inactive" :busy nil :message "")))
        (let ((result (copilot--status-lighter)))
          (expect result :to-equal " Copilot:Inactive")
          (expect (get-text-property 0 'face result) :to-equal 'shadow)))))

  ;;
  ;; didChangeStatus notification
  ;;

  (describe "didChangeStatus handler"
    (it "sets copilot--status from notification"
      (let ((copilot--status nil))
        (spy-on 'force-mode-line-update)
        ;; Simulate the notification by looking up and calling the handler
        (let ((handlers (gethash 'didChangeStatus copilot--notification-handlers)))
          (expect handlers :to-be-truthy)
          (funcall (car handlers)
                   '(:kind "Warning" :busy nil :message "something"))
          (expect (plist-get copilot--status :kind) :to-equal "Warning")
          (expect (plist-get copilot--status :busy) :to-equal nil)
          (expect (plist-get copilot--status :message) :to-equal "something")
          (expect 'force-mode-line-update :to-have-been-called-with t)))))

  ;;
  ;; window/showMessageRequest handler
  ;;

  (describe "window/showMessageRequest handler"
    (it "returns selected action via completing-read"
      (spy-on 'completing-read :and-return-value "Accept")
      (let* ((handler (gethash 'window/showMessageRequest
                                copilot--request-handlers))
             (result (funcall handler
                              '(:type 3 :message "Choose"
                                :actions [(:title "Accept") (:title "Deny")]))))
        (expect 'completing-read :to-have-been-called)
        (expect (plist-get result :title) :to-equal "Accept")))

    (it "returns json-null when no actions"
      (spy-on 'message)
      (let* ((handler (gethash 'window/showMessageRequest
                                copilot--request-handlers))
             (result (funcall handler '(:type 3 :message "Info msg"))))
        (expect result :to-equal :json-null)
        (expect 'message :to-have-been-called)))

    (it "logs errors at error level"
      (spy-on 'message)
      (let ((handler (gethash 'window/showMessageRequest
                               copilot--request-handlers)))
        (funcall handler '(:type 1 :message "Something failed"))
        (let ((args (spy-calls-args-for 'message 0)))
          (expect (apply #'format args) :to-match "Something failed"))))

    (it "logs warnings at warning level"
      (spy-on 'message)
      (let ((handler (gethash 'window/showMessageRequest
                               copilot--request-handlers)))
        (funcall handler '(:type 2 :message "Watch out"))
        (let ((args (spy-calls-args-for 'message 0)))
          (expect (apply #'format args) :to-match "Watch out")))))

  ;;
  ;; window/showDocument handler
  ;;

  (describe "window/showDocument handler"
    (it "opens HTTP URIs with browse-url"
      (spy-on 'browse-url)
      (let* ((handler (gethash 'window/showDocument
                                copilot--request-handlers))
             (result (funcall handler
                              '(:uri "https://example.com/docs"))))
        (expect 'browse-url :to-have-been-called-with "https://example.com/docs")
        (expect (plist-get result :success) :to-equal t)))

    (it "opens file URIs with find-file when takeFocus is true"
      (let ((temp-file (make-temp-file "copilot-showdoc")))
        (unwind-protect
            (progn
              (spy-on 'find-file)
              (let* ((handler (gethash 'window/showDocument
                                        copilot--request-handlers))
                     (uri (concat "file://" temp-file))
                     (result (funcall handler
                                      (list :uri uri :takeFocus t))))
                (expect 'find-file :to-have-been-called)
                (expect (plist-get result :success) :to-equal t)))
          (delete-file temp-file))))

    (it "opens file URIs with display-buffer when takeFocus is false"
      (let ((temp-file (make-temp-file "copilot-showdoc")))
        (unwind-protect
            (progn
              (spy-on 'find-file-noselect :and-return-value (current-buffer))
              (spy-on 'display-buffer)
              (let* ((handler (gethash 'window/showDocument
                                        copilot--request-handlers))
                     (uri (concat "file://" temp-file))
                     (result (funcall handler
                                      (list :uri uri :takeFocus :json-false))))
                (expect 'display-buffer :to-have-been-called)
                (expect 'find-file-noselect :to-have-been-called)
                (expect (plist-get result :success) :to-equal t)))
          (delete-file temp-file))))

    (it "opens external URIs with browse-url"
      (spy-on 'browse-url)
      (let* ((handler (gethash 'window/showDocument
                                copilot--request-handlers))
             (result (funcall handler
                              '(:uri "vscode://extension" :external t))))
        (expect 'browse-url :to-have-been-called-with "vscode://extension")
        (expect (plist-get result :success) :to-equal t)))

    (it "returns success false on error"
      (spy-on 'browse-url :and-call-fake
              (lambda (&rest _) (error "Cannot open")))
      (let* ((handler (gethash 'window/showDocument
                                copilot--request-handlers))
             (result (funcall handler
                              '(:uri "https://example.com"))))
        (expect (plist-get result :success) :to-equal :json-false))))

  ;;
  ;; $/progress handler
  ;;

  (describe "$/progress handler"
    (it "stores session on begin and reports progress"
      (let ((copilot--progress-sessions (make-hash-table :test 'equal)))
        (spy-on 'force-mode-line-update)
        (let ((handlers (gethash '$/progress copilot--notification-handlers)))
          (expect handlers :to-be-truthy)
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "begin" :title "Indexing" :message "Starting")))
          (expect (hash-table-count copilot--progress-sessions) :to-equal 1)
          (let ((session (gethash "tok1" copilot--progress-sessions)))
            (expect (plist-get session :title) :to-equal "Indexing")
            (expect (plist-get session :message) :to-equal "Starting"))
          (expect (copilot--progress-lighter) :to-equal " [Indexing: Starting]")
          (expect 'force-mode-line-update :to-have-been-called-with t))))

    (it "updates session on report"
      (let ((copilot--progress-sessions (make-hash-table :test 'equal)))
        (spy-on 'force-mode-line-update)
        (let ((handlers (gethash '$/progress copilot--notification-handlers)))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "begin" :title "Indexing" :message "Starting")))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "report" :message "50 files" :percentage 42)))
          (let ((session (gethash "tok1" copilot--progress-sessions)))
            (expect (plist-get session :message) :to-equal "50 files")
            (expect (plist-get session :percentage) :to-equal 42))
          (expect (copilot--progress-lighter) :to-equal " [Indexing: 50 files]"))))

    (it "removes session on end"
      (let ((copilot--progress-sessions (make-hash-table :test 'equal)))
        (spy-on 'force-mode-line-update)
        (let ((handlers (gethash '$/progress copilot--notification-handlers)))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "begin" :title "Indexing")))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "end")))
          (expect (hash-table-count copilot--progress-sessions) :to-equal 0)
          (expect (copilot--progress-lighter) :to-be nil))))

    (it "tracks multiple tokens independently"
      (let ((copilot--progress-sessions (make-hash-table :test 'equal)))
        (spy-on 'force-mode-line-update)
        (let ((handlers (gethash '$/progress copilot--notification-handlers)))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "begin" :title "Indexing")))
          (funcall (car handlers)
                   '(:token "tok2"
                     :value (:kind "begin" :title "Loading")))
          (expect (hash-table-count copilot--progress-sessions) :to-equal 2)
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "end")))
          (expect (hash-table-count copilot--progress-sessions) :to-equal 1)
          (expect (gethash "tok1" copilot--progress-sessions) :to-be nil)
          (expect (gethash "tok2" copilot--progress-sessions) :to-be-truthy))))

    (it "includes progress in mode-line lighter"
      (let ((copilot--progress-sessions (make-hash-table :test 'equal))
            (copilot--status nil))
        (spy-on 'force-mode-line-update)
        (let ((handlers (gethash '$/progress copilot--notification-handlers)))
          (funcall (car handlers)
                   '(:token "tok1"
                     :value (:kind "begin" :title "Indexing" :percentage 42)))
          (expect (copilot--status-lighter) :to-equal " Copilot [Indexing: 42%]")))))

  ;;
  ;; Server shutdown
  ;;

  (describe "copilot--shutdown-server"
    (it "sends shutdown request and exit notification when connection is alive"
      (let* ((conn (make-symbol "fake-conn"))
             (copilot--connection conn)
             (copilot--opened-buffers '(buf1))
             (copilot--workspace-folders '("file:///tmp")))
        (spy-on 'jsonrpc-request)
        (spy-on 'jsonrpc-notify)
        (spy-on 'jsonrpc-shutdown)
        (copilot--shutdown-server)
        (expect 'jsonrpc-request :to-have-been-called-with
                conn 'shutdown nil :timeout 3)
        (expect 'jsonrpc-notify :to-have-been-called-with
                conn 'exit nil)
        (expect 'jsonrpc-shutdown :to-have-been-called)))

    (it "handles unresponsive server gracefully"
      (let ((copilot--connection (make-symbol "fake-conn"))
            (copilot--opened-buffers '(buf1))
            (copilot--workspace-folders '("file:///tmp")))
        (spy-on 'jsonrpc-request :and-call-fake
                (lambda (&rest _) (error "Timeout")))
        (spy-on 'jsonrpc-notify)
        (spy-on 'jsonrpc-shutdown)
        ;; Should not signal an error
        (copilot--shutdown-server)
        ;; Should still attempt exit and cleanup
        (expect 'jsonrpc-notify :to-have-been-called)
        (expect 'jsonrpc-shutdown :to-have-been-called)
        (expect copilot--connection :to-be nil)))

    (it "is a no-op when connection is nil"
      (let ((copilot--connection nil))
        (spy-on 'jsonrpc-request)
        (spy-on 'jsonrpc-notify)
        (spy-on 'jsonrpc-shutdown)
        (copilot--shutdown-server)
        (expect 'jsonrpc-request :not :to-have-been-called)
        (expect 'jsonrpc-notify :not :to-have-been-called)
        (expect 'jsonrpc-shutdown :not :to-have-been-called)))

    (it "resets global state"
      (let ((copilot--connection (make-symbol "fake-conn"))
            (copilot--opened-buffers '(buf1 buf2))
            (copilot--workspace-folders '("file:///a" "file:///b"))
            (copilot--status '(:kind "Error" :busy nil :message "stale")))
        (spy-on 'jsonrpc-request)
        (spy-on 'jsonrpc-notify)
        (spy-on 'jsonrpc-shutdown)
        (copilot--shutdown-server)
        (expect copilot--connection :to-be nil)
        (expect copilot--opened-buffers :to-be nil)
        (expect copilot--workspace-folders :to-be nil)
        (expect copilot--status :to-be nil))))

  ;;
  ;; $/cancelRequest
  ;;

  (describe "copilot--cancel-completion"
    (it "sends $/cancelRequest and clears ID when request is in-flight"
      (with-temp-buffer
        (setq-local copilot--completion-request-id 42)
        (let ((copilot--connection t))
          (spy-on 'jsonrpc--process :and-return-value t)
          (spy-on 'process-exit-status :and-return-value 0)
          (spy-on 'jsonrpc-notify)
          (copilot--cancel-completion)
          (expect 'jsonrpc-notify :to-have-been-called-with
                  t '$/cancelRequest '(:id 42))
          (expect copilot--completion-request-id :to-be nil))))

    (it "is a no-op when no request is in-flight"
      (with-temp-buffer
        (setq-local copilot--completion-request-id nil)
        (spy-on 'jsonrpc-notify)
        (copilot--cancel-completion)
        (expect 'jsonrpc-notify :not :to-have-been-called)))

    (it "clears ID without notifying when connection is dead"
      (with-temp-buffer
        (setq-local copilot--completion-request-id 99)
        (let ((copilot--connection nil))
          (spy-on 'jsonrpc-notify)
          (copilot--cancel-completion)
          (expect 'jsonrpc-notify :not :to-have-been-called)
          (expect copilot--completion-request-id :to-be nil)))))

  (describe "copilot-clear-overlay cancellation"
    (it "cancels in-flight request when clearing overlay"
      (with-temp-buffer
        (spy-on 'copilot--cancel-completion)
        (setq-local copilot--overlay nil)
        (copilot-clear-overlay)
        (expect 'copilot--cancel-completion :to-have-been-called))))

  (describe "copilot--path-to-uri"
    (it "creates a file URI for unix paths"
      (expect (copilot--path-to-uri "/home/user/project")
              :to-match "^file:///home/user/project"))))

;;
;; copilot-balancer
;;

(describe "copilot-balancer"
  (describe "copilot-balancer-trim-closing-pairs-at-end"
    (it "trims trailing closing parens"
      (expect (copilot-balancer-trim-closing-pairs-at-end "foo))") :to-equal "foo"))

    (it "trims mixed trailing closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "bar)}]") :to-equal "bar"))

    (it "preserves string with no trailing closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "hello") :to-equal "hello"))

    (it "does not trim escaped closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "foo\\)") :to-equal "foo\\)"))

    (it "does not trim trailing double quotes"
      (expect (copilot-balancer-trim-closing-pairs-at-end "foo\"") :to-equal "foo\"")))

  (describe "copilot-balancer-fix-completion"
    (it "passes through completion in non-lisp modes"
      (with-temp-buffer
        (text-mode)
        (insert "hello ")
        (let ((result (copilot-balancer-fix-completion (point) (point) "world")))
          (expect (nth 2 result) :to-equal "world"))))

    (it "passes through when balancer is disabled"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  (bar")
        (let ((copilot-enable-parentheses-balancer nil))
          (let ((result (copilot-balancer-fix-completion (point) (point) "")))
            (expect (nth 2 result) :to-equal "")))))

    (it "balances parentheses in emacs-lisp-mode"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  (message \"hello\"")
        (let ((result (copilot-balancer-fix-completion (point) (point) "")))
          ;; The balancer should add closing parens
          (expect (nth 2 result) :to-match ")"))))

    (it "handles already-balanced completion"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  ")
        (let* ((completion "(bar))")
               (result (copilot-balancer-fix-completion (point) (point) completion)))
          ;; Should return something reasonable
          (expect (nth 2 result) :to-be-truthy))))

    (it "ignores parens inside comments"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  ;; a stray (\n  (bar")
        (let ((result (copilot-balancer-fix-completion (point) (point) "")))
          ;; Should close (bar and (defun, not the comment paren
          (expect (nth 2 result) :to-equal "))"))))

    (it "ignores parens inside strings"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  (message \"(\"")
        (let ((result (copilot-balancer-fix-completion (point) (point) "")))
          ;; Should close (message and (defun, not the string paren
          (expect (nth 2 result) :to-equal "))"))))

    (it "handles mixed bracket types"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(let ([a 1]")
        (let ((result (copilot-balancer-fix-completion (point) (point) "")))
          ;; [a 1] is balanced; two ( remain unmatched
          (expect (nth 2 result) :to-equal "))"))))

    (it "accounts for closers in suffix"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun foo ()\n  (bar")
        (save-excursion (insert "))\n"))
        (let ((result (copilot-balancer-fix-completion (point) (point) "")))
          ;; Suffix already has the closing parens, no extras needed
          (expect (nth 2 result) :to-equal ""))))

    (it "strips redundant closers when suffix has them"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defun add (a ")
        (save-excursion (insert "))\n"))
        (let ((result (copilot-balancer-fix-completion (point) (point) "b))")))
          ;; Suffix )) already closes both parens; trimmed completion is just "b"
          (expect (nth 2 result) :to-equal "b"))))

    (it "keeps comment closers when server uses a replacement range"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert ";; (require cl-)")
        (search-backward ")")
        (let* ((start (point))
               (end (1+ start))
               (result (copilot-balancer-fix-completion start end "lib")))
          (expect (nth 2 result) :to-equal "lib)"))))))

  ;;
  ;; copilot-accept-completion
  ;;

  (describe "copilot-accept-completion"
    (it "accepts full completion"
      (with-temp-buffer
        (insert "(defun add (a ")
        (save-excursion (insert "))"))
        (spy-on 'copilot--notify)
        (spy-on 'copilot--async-request)
        (copilot--display-overlay-completion "b)" nil nil (point) (+ (point) 2))
        (copilot-accept-completion)
        (expect (buffer-string) :to-equal "(defun add (a b)")
        (expect (copilot--overlay-visible) :not :to-be-truthy)))

    (it "accepts completion by word"
      (with-temp-buffer
        (insert "(defun add (a ")
        (save-excursion (insert "))"))
        (spy-on 'copilot--notify)
        (spy-on 'copilot--async-request)
        (copilot--display-overlay-completion "b)\n  (+ a b))" nil nil (point) (+ (point) 2))
        (copilot-accept-completion-by-word)
        (expect (buffer-substring-no-properties (point-min) (point))
                :to-equal "(defun add (a b")
        (expect (copilot--overlay-visible) :to-be-truthy)
        (expect (overlay-get copilot--overlay 'completion)
                :to-equal ")\n  (+ a b))")))

    (it "accepts by word with replacement range"
      (with-temp-buffer
        (insert "(defun add (a ")
        (let ((start (point)))
          (save-excursion (insert "))"))
          (let ((end (+ start 2)))
            (spy-on 'copilot--notify)
            (spy-on 'copilot--async-request)
            (copilot--display-overlay-completion "b)\n  (+ a b))" nil nil start end)
            (copilot-accept-completion-by-word)
            ;; First word "b" inserted; replacement range "))" preserved
            (expect (buffer-substring-no-properties (point-min) (line-end-position))
                    :to-equal "(defun add (a b))")
            (expect (copilot--overlay-visible) :to-be-truthy)
            ;; Second accept-by-word inserts ")\n  (+ a", deletes "))"
            (copilot-accept-completion-by-word)
            (expect (buffer-string) :to-equal "(defun add (a b)\n  (+ a))")))))

    (it "accepts by word with replacement range and trailing text"
      (with-temp-buffer
        (insert "(let ((x ")
        (let ((start (point)))
          (save-excursion (insert ")) ; trailing"))
          (let ((end (+ start 2)))
            (spy-on 'copilot--notify)
            (spy-on 'copilot--async-request)
            (copilot--display-overlay-completion "(+ 1 2)" nil nil start end)
            (copilot-accept-completion-by-word)
            ;; forward-word on "(+ 1 2)" skips "(+ " then matches "1" -> "(+ 1"
            ;; Replacement range "))" preserved, trailing text intact
            (expect (buffer-string) :to-equal "(let ((x (+ 1)) ; trailing")
            (expect (copilot--overlay-visible) :to-be-truthy)
            ;; Second accept inserts " 2", remaining ")" shown as overlay
            (copilot-accept-completion-by-word)
            (expect (buffer-string) :to-equal "(let ((x (+ 1 2)) ; trailing"))))))

;;; copilot-test.el ends here
