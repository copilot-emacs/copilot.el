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

  (describe "copilot--get-source"
    (it "returns buffer contents"
      (with-temp-buffer
        (insert "hello world")
        (expect (copilot--get-source) :to-equal "hello world")))))

;;; copilot-test.el ends here
