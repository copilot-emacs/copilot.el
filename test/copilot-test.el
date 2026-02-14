;;; copilot-test.el --- Tests for copilot.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot.el.

;;; Code:

(require 'buttercup)
(require 'copilot)

(describe "copilot"
  (describe "loading"
    (it "provides the copilot feature"
      (expect (featurep 'copilot) :to-be-truthy)))

  (describe "copilot--make-connection"
    (it "handles both :events-buffer-config and :events-buffer-scrollback-size"
      ;; copilot--make-connection uses condition-case to try the new
      ;; :events-buffer-config keyword first, falling back to
      ;; :events-buffer-scrollback-size for older jsonrpc versions.
      ;; Verify make-instance is called with the right fallback logic.
      (let ((call-args nil))
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

  (describe "copilot--get-source"
    (it "returns buffer contents"
      (with-temp-buffer
        (insert "hello world")
        (expect (copilot--get-source) :to-equal "hello world")))))

;;; copilot-test.el ends here
