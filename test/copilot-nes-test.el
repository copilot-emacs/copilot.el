;;; copilot-nes-test.el --- Tests for copilot-nes.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup tests for copilot-nes.el.

;;; Code:

(require 'buttercup)
(require 'copilot-nes)

(describe "copilot-nes"
  (describe "loading"
    (it "provides the copilot-nes feature"
      (expect (featurep 'copilot-nes) :to-be-truthy)))

  ;;
  ;; Range conversion
  ;;

  (describe "copilot-nes--range-to-region"
    (it "converts a single-line range"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (let ((region (copilot-nes--range-to-region
                       (list :start (list :line 0 :character 6)
                             :end (list :line 0 :character 11)))))
          (expect (buffer-substring-no-properties (car region) (cdr region))
                  :to-equal "world"))))

    (it "converts a multi-line range"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "line one\nline two\nline three\n")
        (let ((region (copilot-nes--range-to-region
                       (list :start (list :line 0 :character 5)
                             :end (list :line 1 :character 8)))))
          (expect (buffer-substring-no-properties (car region) (cdr region))
                  :to-equal "one\nline two"))))

    (it "handles a zero-width range (pure insertion point)"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (let ((region (copilot-nes--range-to-region
                       (list :start (list :line 0 :character 5)
                             :end (list :line 0 :character 5)))))
          (expect (car region) :to-equal (cdr region))))))

  ;;
  ;; Display
  ;;

  (describe "copilot-nes--display"
    (it "creates overlays for a replacement edit"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (let ((edit (list :text "planet"
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          ;; Should have 2 overlays: deletion + insertion
          (expect (length copilot-nes--overlays) :to-equal 2)
          (expect copilot-nes--edit :to-equal edit))))

    (it "creates only a deletion overlay when text is empty"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (let ((edit (list :text ""
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (expect (length copilot-nes--overlays) :to-equal 1)
          ;; The overlay should cover the deletion range
          (let ((ov (car copilot-nes--overlays)))
            (expect (overlay-get ov 'face) :to-equal 'copilot-nes-deletion-face)))))

    (it "creates only an insertion overlay for a pure insertion"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (let ((edit (list :text " world"
                          :range (list :start (list :line 0 :character 5)
                                       :end (list :line 0 :character 5))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (expect (length copilot-nes--overlays) :to-equal 1)
          (let ((ov (car copilot-nes--overlays)))
            (expect (overlay-get ov 'after-string) :to-be-truthy)))))

    (it "sends didShowInlineEdit notification when command is present"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (let* ((cmd (list :title "test" :command "test-cmd" :arguments ["id1"]))
               (edit (list :text "new"
                           :range (list :start (list :line 0 :character 0)
                                        :end (list :line 0 :character 5))
                           :command cmd))
               (copilot--connection t))
          (spy-on 'jsonrpc--process :and-return-value t)
          (spy-on 'process-exit-status :and-return-value 0)
          (spy-on 'jsonrpc-notify)
          (copilot-nes--display edit)
          (expect 'jsonrpc-notify :to-have-been-called)))))

  ;;
  ;; Clear
  ;;

  (describe "copilot-nes--clear"
    (it "removes overlays and resets state"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (let ((edit (list :text "planet"
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (expect copilot-nes--overlays :to-be-truthy)
          (copilot-nes--clear)
          (expect copilot-nes--overlays :to-equal nil)
          (expect copilot-nes--edit :to-equal nil)
          (expect copilot-nes--move-count :to-equal 0)))))

  ;;
  ;; Accept
  ;;

  (describe "copilot-nes-accept"
    (it "applies a replacement edit when near the edit"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (goto-char (point-min))
        (let ((edit (list :text "planet"
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (copilot-nes-accept)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "hello planet\n")
          (expect copilot-nes--edit :to-equal nil))))

    (it "applies a pure insertion"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (goto-char (point-min))
        (let ((edit (list :text " world"
                          :range (list :start (list :line 0 :character 5)
                                       :end (list :line 0 :character 5))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (copilot-nes-accept)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "hello world\n"))))

    (it "applies a pure deletion"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (goto-char (point-min))
        (let ((edit (list :text ""
                          :range (list :start (list :line 0 :character 5)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (copilot-nes-accept)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal "hello\n"))))

    (it "jumps to edit location when far away"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert (make-string 5 ?\n))  ; 5 blank lines
        (insert "target line\n")
        (goto-char (point-min))       ; point is far from the edit on line 5
        (let ((edit (list :text "replaced"
                          :range (list :start (list :line 5 :character 0)
                                       :end (list :line 5 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (copilot-nes-accept)
          ;; Should have jumped, not applied yet
          (expect copilot-nes--edit :to-be-truthy)
          (expect (line-number-at-pos) :to-equal 6)
          ;; Second press applies
          (copilot-nes-accept)
          (expect copilot-nes--edit :to-equal nil))))

    (it "is a no-op when no suggestion is pending"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (setq copilot-nes--edit nil)
        (copilot-nes-accept)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal "hello\n"))))

  ;;
  ;; Dismiss
  ;;

  (describe "copilot-nes-dismiss"
    (it "clears the current suggestion"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (let ((edit (list :text "planet"
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (copilot-nes-dismiss)
          (expect copilot-nes--edit :to-equal nil)
          (expect copilot-nes--overlays :to-equal nil))))

    (it "is a no-op when no suggestion is pending"
      (with-temp-buffer
        (setq copilot-nes--edit nil)
        (copilot-nes-dismiss)
        (expect copilot-nes--edit :to-equal nil))))

  ;;
  ;; Auto-dismiss
  ;;

  (describe "copilot-nes--post-command"
    (it "increments move count when point actually moves"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld\n")
        (goto-char (point-min))
        (let ((edit (list :text "x"
                          :range (list :start (list :line 0 :character 0)
                                       :end (list :line 0 :character 1))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          ;; Simulate a command that moves point
          (let ((this-command 'next-line))
            (forward-line 1)
            (copilot-nes--post-command))
          (expect copilot-nes--move-count :to-equal 1))))

    (it "does not increment move count when point stays put"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (goto-char (point-min))
        (let ((edit (list :text "x"
                          :range (list :start (list :line 0 :character 0)
                                       :end (list :line 0 :character 1))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          ;; Simulate a command that does not move point (e.g. scroll)
          (let ((this-command 'scroll-up-command))
            (copilot-nes--post-command))
          (expect copilot-nes--move-count :to-equal 0))))

    (it "auto-dismisses after enough cursor movements"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld\nfoo\n")
        (goto-char (point-min))
        (let ((edit (list :text "x"
                          :range (list :start (list :line 0 :character 0)
                                       :end (list :line 0 :character 1))
                          :command nil))
              (copilot-nes-auto-dismiss-move-count 2))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (let ((this-command 'next-line))
            (forward-line 1)
            (copilot-nes--post-command)
            (forward-line 1)
            (copilot-nes--post-command))
          (expect copilot-nes--edit :to-equal nil))))

    (it "schedules a request after text-modifying commands"
      (with-temp-buffer
        (spy-on 'copilot-nes--schedule-request)
        (let ((this-command 'self-insert-command))
          (copilot-nes--post-command))
        (expect 'copilot-nes--schedule-request :to-have-been-called))))

  ;;
  ;; Timer management
  ;;

  (describe "copilot-nes--cancel-timer"
    (it "cancels an active timer"
      (with-temp-buffer
        (setq copilot-nes--timer (run-with-idle-timer 10 nil #'ignore))
        (copilot-nes--cancel-timer)
        (expect copilot-nes--timer :to-equal nil)))

    (it "handles nil timer gracefully"
      (with-temp-buffer
        (setq copilot-nes--timer nil)
        (copilot-nes--cancel-timer)
        (expect copilot-nes--timer :to-equal nil))))

  ;;
  ;; Minor mode
  ;;

  (describe "copilot-nes-mode"
    (before-each
      (spy-on 'copilot-nes--check-server-version))

    (it "can be enabled and disabled"
      (with-temp-buffer
        (copilot-nes-mode 1)
        (expect copilot-nes-mode :to-be-truthy)
        (copilot-nes-mode -1)
        (expect copilot-nes-mode :not :to-be-truthy)))

    (it "sets up post-command-hook when enabled"
      (with-temp-buffer
        (copilot-nes-mode 1)
        (expect (memq #'copilot-nes--post-command post-command-hook) :to-be-truthy)
        (copilot-nes-mode -1)))

    (it "removes post-command-hook when disabled"
      (with-temp-buffer
        (copilot-nes-mode 1)
        (copilot-nes-mode -1)
        (expect (memq #'copilot-nes--post-command post-command-hook)
                :not :to-be-truthy)))

    (it "clears state when disabled"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (copilot-nes-mode 1)
        (let ((edit (list :text "x"
                          :range (list :start (list :line 0 :character 0)
                                       :end (list :line 0 :character 1))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit))
        (copilot-nes-mode -1)
        (expect copilot-nes--edit :to-equal nil)
        (expect copilot-nes--overlays :to-equal nil)))

    (it "checks server version on activation"
      (with-temp-buffer
        (copilot-nes-mode 1)
        (expect 'copilot-nes--check-server-version :to-have-been-called)
        (copilot-nes-mode -1))))

  ;;
  ;; Distance check
  ;;

  (describe "copilot-nes--too-far-p"
    (it "returns nil when no edit is pending"
      (with-temp-buffer
        (setq copilot-nes--edit nil)
        (expect (copilot-nes--too-far-p) :not :to-be-truthy)))

    (it "returns nil when point is near the edit"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\nworld\n")
        (goto-char (point-min))
        (setq copilot-nes--edit
              (list :range (list :start (list :line 1 :character 0)
                                  :end (list :line 1 :character 5))))
        (let ((copilot-nes-auto-dismiss-distance 40))
          (expect (copilot-nes--too-far-p) :not :to-be-truthy))))

    (it "returns t when point is far from the edit"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (dotimes (_ 50) (insert "line\n"))
        (goto-char (point-max))
        (setq copilot-nes--edit
              (list :range (list :start (list :line 0 :character 0)
                                  :end (list :line 0 :character 4))))
        (let ((copilot-nes-auto-dismiss-distance 40))
          (expect (copilot-nes--too-far-p) :to-be-truthy)))))

  ;;
  ;; Response handler
  ;;

  (describe "copilot-nes--handle-response"
    (it "rejects a response with mismatched version"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (spy-on 'copilot-nes--display)
        (let* ((edit (list :text "planet"
                           :range (list :start (list :line 0 :character 6)
                                        :end (list :line 0 :character 11))
                           :textDocument (list :uri "file:///test.el" :version 5)))
               (response (list :edits (vector edit))))
          (copilot-nes--handle-response response "file:///test.el" 3)
          (expect 'copilot-nes--display :not :to-have-been-called))))

    (it "rejects a response with mismatched URI"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (spy-on 'copilot-nes--display)
        (let* ((edit (list :text "planet"
                           :range (list :start (list :line 0 :character 6)
                                        :end (list :line 0 :character 11))
                           :textDocument (list :uri "file:///other.el" :version 5)))
               (response (list :edits (vector edit))))
          (copilot-nes--handle-response response "file:///test.el" 5)
          (expect 'copilot-nes--display :not :to-have-been-called))))

    (it "accepts a response with matching version and URI"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (spy-on 'copilot-nes--display)
        (let* ((edit (list :text "planet"
                           :range (list :start (list :line 0 :character 6)
                                        :end (list :line 0 :character 11))
                           :textDocument (list :uri "file:///test.el" :version 5)))
               (response (list :edits (vector edit))))
          (copilot-nes--handle-response response "file:///test.el" 5)
          (expect 'copilot-nes--display :to-have-been-called))))

    (it "handles empty edits array"
      (with-temp-buffer
        (spy-on 'copilot-nes--display)
        (copilot-nes--handle-response (list :edits []) "file:///test.el" 1)
        (expect 'copilot-nes--display :not :to-have-been-called)))

    (it "handles nil edits"
      (with-temp-buffer
        (spy-on 'copilot-nes--display)
        (copilot-nes--handle-response (list :edits nil) "file:///test.el" 1)
        (expect 'copilot-nes--display :not :to-have-been-called))))

  ;;
  ;; Server version check
  ;;

  (describe "copilot-nes--check-server-version"
    (it "warns when server version is too old"
      (spy-on 'copilot-installed-version :and-return-value "1.400.0")
      (spy-on 'display-warning)
      (copilot-nes--check-server-version)
      (expect 'display-warning :to-have-been-called))

    (it "is silent when server version is sufficient"
      (spy-on 'copilot-installed-version :and-return-value "1.434.0")
      (spy-on 'display-warning)
      (copilot-nes--check-server-version)
      (expect 'display-warning :not :to-have-been-called))

    (it "is silent when server version is newer"
      (spy-on 'copilot-installed-version :and-return-value "1.500.0")
      (spy-on 'display-warning)
      (copilot-nes--check-server-version)
      (expect 'display-warning :not :to-have-been-called))

    (it "is silent when version is nil (undetermined)"
      (spy-on 'copilot-installed-version :and-return-value nil)
      (spy-on 'display-warning)
      (copilot-nes--check-server-version)
      (expect 'display-warning :not :to-have-been-called)))

  ;;
  ;; Accept telemetry
  ;;

  (describe "copilot-nes-accept telemetry"
    (it "sends workspace/executeCommand when command is present"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (goto-char (point-min))
        (let* ((cmd (list :command "test-cmd" :arguments ["arg1"]))
               (edit (list :text "planet"
                           :range (list :start (list :line 0 :character 6)
                                        :end (list :line 0 :character 11))
                           :command cmd))
               (copilot--connection t))
          (spy-on 'jsonrpc--process :and-return-value t)
          (spy-on 'process-exit-status :and-return-value 0)
          (spy-on 'jsonrpc-notify)
          (copilot-nes--display edit)
          (copilot-nes-accept)
          ;; Should have called jsonrpc-notify for both didShowInlineEdit and executeCommand
          (expect 'jsonrpc-notify :to-have-been-called-times 2))))

    (it "does not send workspace/executeCommand when command is absent"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello world\n")
        (goto-char (point-min))
        (let ((edit (list :text "planet"
                          :range (list :start (list :line 0 :character 6)
                                       :end (list :line 0 :character 11))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit)
          (spy-on 'jsonrpc-notify)
          (copilot-nes-accept)
          ;; No command means no executeCommand notification
          (expect 'jsonrpc-notify :not :to-have-been-called)))))

  ;;
  ;; Mode coexistence
  ;;

  (describe "mode coexistence"
    (before-each
      (spy-on 'copilot-nes--check-server-version)
      (spy-on 'copilot--on-doc-focus))

    (it "copilot-nes-mode and copilot-mode can be enabled independently"
      (with-temp-buffer
        (copilot-nes-mode 1)
        (copilot-mode 1)
        (expect copilot-nes-mode :to-be-truthy)
        (expect copilot-mode :to-be-truthy)
        (copilot-mode -1)
        (expect copilot-nes-mode :to-be-truthy)
        (expect copilot-mode :not :to-be-truthy)
        (copilot-nes-mode -1)))

    (it "disabling copilot-mode does not affect copilot-nes-mode state"
      (with-temp-buffer
        (setq-local copilot--line-bias 1)
        (insert "hello\n")
        (copilot-nes-mode 1)
        (let ((edit (list :text "x"
                          :range (list :start (list :line 0 :character 0)
                                       :end (list :line 0 :character 1))
                          :command nil)))
          (spy-on 'copilot--notify)
          (copilot-nes--display edit))
        (copilot-mode 1)
        (copilot-mode -1)
        ;; NES state should be unaffected
        (expect copilot-nes-mode :to-be-truthy)
        (expect copilot-nes--edit :to-be-truthy)
        (copilot-nes-mode -1)))

    (it "disabling copilot-nes-mode does not affect copilot-mode"
      (with-temp-buffer
        (copilot-mode 1)
        (copilot-nes-mode 1)
        (copilot-nes-mode -1)
        (expect copilot-mode :to-be-truthy)
        (copilot-mode -1)))))

;;; copilot-nes-test.el ends here
