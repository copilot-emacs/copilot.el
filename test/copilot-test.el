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
        (expect (copilot--get-source) :to-equal "hello world")))

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
          (delete-file temp-file))))))

;;; copilot-test.el ends here
