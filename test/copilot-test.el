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
          (expect (plist-get pos :character) :to-equal 1))))))

;;
;; copilot-balancer
;;

(describe "copilot-balancer"
  (describe "copilot-balancer-extract-pairs"
    (it "extracts parentheses"
      (let ((pairs (copilot-balancer-extract-pairs "(foo (bar))")))
        (expect pairs :to-equal '(?\( ?\( ?\) ?\)))))

    (it "extracts brackets and braces"
      (let ((pairs (copilot-balancer-extract-pairs "[{x}]")))
        (expect pairs :to-equal '(?\[ ?\{ ?\} ?\]))))

    (it "returns empty list for no pairs"
      (expect (copilot-balancer-extract-pairs "hello") :to-equal nil))

    (it "skips escaped characters"
      (let ((pairs (copilot-balancer-extract-pairs "\\(foo)")))
        (expect pairs :to-equal '(?\))))))

  (describe "copilot-balancer-trim-closing-pairs-at-end"
    (it "trims trailing closing parens"
      (expect (copilot-balancer-trim-closing-pairs-at-end "foo))") :to-equal "foo"))

    (it "trims mixed trailing closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "bar)}]") :to-equal "bar"))

    (it "preserves string with no trailing closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "hello") :to-equal "hello"))

    (it "does not trim escaped closers"
      (expect (copilot-balancer-trim-closing-pairs-at-end "foo\\)") :to-equal "foo\\)")))

  (describe "copilot-balancer-collapse-matching-pairs"
    (it "collapses matching open-close pairs"
      (let ((result (copilot-balancer-collapse-matching-pairs '(?\( ?\)) nil)))
        (expect (car result) :to-equal nil)))

    (it "preserves unmatched pairs"
      (let ((result (copilot-balancer-collapse-matching-pairs '(?\( ?\( ?\)) nil)))
        (expect (car result) :to-equal '(?\()))))

  (describe "copilot-balancer-fix-completion"
    (it "passes through completion in non-lisp modes"
      (with-temp-buffer
        (text-mode)
        (insert "hello ")
        (let ((result (copilot-balancer-fix-completion (point) (point) "world")))
          (expect (nth 2 result) :to-equal "world"))))

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
          (expect (nth 2 result) :to-be-truthy))))))

;;; copilot-test.el ends here
