;;; integration-test.el --- Integration test for inlineCompletion API -*- lexical-binding: t; -*-

;; Manual smoke test that connects to the real Copilot language server
;; and exercises the textDocument/inlineCompletion round-trip.
;;
;; Prerequisites:
;;   - Copilot server installed (`M-x copilot-install-server')
;;   - Authenticated (`M-x copilot-login')
;;
;; Usage:
;;   emacs --batch -L . -l dev/integration-test.el

;;; Code:

(require 'copilot)

(defvar test--timeout 30
  "Seconds to wait for server responses.")

(defvar test--result nil)
(defvar test--error nil)
(defvar test--done nil)

(defun test--wait ()
  "Block until `test--done' is set or timeout."
  (let ((deadline (+ (float-time) test--timeout)))
    (while (and (not test--done)
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (unless test--done
      (error "Timed out waiting for server response"))))

(defun test--run ()
  "Run the integration test."
  (message "\n=== Copilot inlineCompletion Integration Test ===\n")

  ;; 1. Start server
  (message "[1/5] Starting server...")
  (copilot--start-server)
  (message "  OK - server started, connection alive: %s" (copilot--connection-alivep))

  ;; 2. Check auth status
  (message "[2/5] Checking auth status...")
  (condition-case err
      (let ((status (copilot--request 'checkStatus '(:dummy "checkStatus"))))
        (message "  OK - status: %s, user: %s"
                 (plist-get status :status)
                 (plist-get status :user)))
    (error
     (message "  WARN - auth check failed: %s (completions may fail)" err)))

  ;; 3. Open a document
  (message "[3/5] Opening test document...")
  (let ((test-buf (generate-new-buffer "*copilot-integration-test*")))
    (with-current-buffer test-buf
      (python-mode)
      (insert "def fibonacci(n):\n    ")
      (setq-local copilot--line-bias 1)
      (setq-local copilot--doc-version 1)

      ;; Send didOpen
      (copilot--notify 'textDocument/didOpen
                       (list :textDocument
                             (list :uri (copilot--get-uri)
                                   :languageId (copilot--get-language-id)
                                   :version copilot--doc-version
                                   :text (buffer-substring-no-properties
                                          (point-min) (point-max)))))
      (message "  OK - sent didOpen for %s (lang: %s)"
               (copilot--get-uri) (copilot--get-language-id))

      ;; 4. Send textDocument/inlineCompletion request
      (message "[4/5] Sending textDocument/inlineCompletion request...")
      (let ((params (copilot--inline-completion-params 1)))
        (message "  Request params: %S" params)

        (setq test--done nil test--result nil test--error nil)
        (copilot--async-request
         'textDocument/inlineCompletion
         params
         :success-fn (lambda (response)
                       (setq test--result response)
                       (setq test--done t))
         :error-fn (lambda (err)
                     (setq test--error err)
                     (setq test--done t))
         :timeout-fn (lambda ()
                       (setq test--error "timeout")
                       (setq test--done t)))

        (test--wait)

        (cond
         (test--error
          (message "  FAIL - error: %S" test--error))
         (t
          (message "  OK - raw response type: %s" (type-of test--result))
          (message "  Raw response: %S"
                   (if (and test--result
                            (> (length (format "%S" test--result)) 500))
                       (substring (format "%S" test--result) 0 500)
                     test--result))

          ;; 5. Normalize and inspect
          (message "[5/5] Normalizing response...")
          (let ((items (copilot--normalize-completion-response test--result)))
            (message "  Normalized items count: %d" (length items))
            (when items
              (let ((first-item (car items)))
                (message "  First item keys: %S"
                         (cl-loop for (k _v) on first-item by #'cddr
                                  collect k))
                (message "  insertText preview: %.100s"
                         (or (plist-get first-item :insertText) "<nil>"))
                (message "  range: %S" (plist-get first-item :range))
                (message "  command: %S"
                         (let ((cmd (plist-get first-item :command)))
                           (if cmd
                               (list :title (plist-get cmd :title)
                                     :command (plist-get cmd :command))
                             "<nil>")))))
            (if items
                (message "\n=== PASS - Got %d completion item(s) ==="
                         (length items))
              (message
               "\n=== WARN - No completions returned (may be auth/quota issue) ==="))))))

      ;; Clean up
      (copilot--notify 'textDocument/didClose
                       (list :textDocument (list :uri (copilot--get-uri))))
      (kill-buffer test-buf)))

  ;; Shutdown
  (when copilot--connection
    (jsonrpc-shutdown copilot--connection)
    (setq copilot--connection nil))
  (message "\nDone."))

(test--run)

;;; integration-test.el ends here
