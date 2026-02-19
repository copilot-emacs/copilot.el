;;; copilot-balancer.el --- Parentheses balancer for Lisps  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2025  copilot-emacs maintainers

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; GitHub Copilot's completion engine is trained predominantly on code with
;; C-style syntax and often produces Lisp completions with mismatched
;; parentheses -- typically missing closing delimiters or adding spurious ones.
;; This module post-processes completions for Lisp modes by trimming excess
;; trailing closers from the suggestion, then computing which closers are
;; actually needed to keep the surrounding top-level form balanced.  Parsing
;; is done via Emacs's built-in `parse-partial-sexp', so comments, strings,
;; and escape sequences are handled correctly through syntax tables.
;;
;; If Copilot's completion quality for Lisp languages improves to the point
;; where balanced delimiters are reliably produced, this module can be removed
;; and `copilot-balancer-fix-completion' replaced with a simple pass-through.
;; Users can disable balancing by setting `copilot-enable-parentheses-balancer'
;; to nil.

;;; Code:

(require 'cl-lib)

(defvar copilot-enable-parentheses-balancer)

(defvar copilot-balancer-lisp-modes '( emacs-lisp-mode
                                       lisp-mode
                                       lisp-interaction-mode
                                       scheme-mode
                                       clojure-mode)
  "List of Lisp modes to balance.")

(defun copilot-balancer-trim-closing-pairs-at-end (s)
  "Trim closing brackets from the end of string S.
Only trims `)' `]' `}' -- not double quotes."
  (let ((i (length s)))
    (while (and (> i 0)
                (memq (aref s (1- i)) '(?\) ?\] ?\}))
                (or (< i 2)
                    (/= (aref s (- i 2)) ?\\)))
      (cl-decf i))
    (substring s 0 i)))

(defun copilot-balancer--odd-quotes-p (s)
  "Return non-nil if S contain an odd number of unescaped double quotes."
  (let ((n (length s))
        (i 0)
        (count 0))
    (while (< i n)
      (cond
       ((= (aref s i) ?\\)
        (setq i (+ i 2)))
       ((= (aref s i) ?\")
        (setq count (1+ count))
        (setq i (1+ i)))
       (t
        (setq i (1+ i)))))
    (cl-oddp count)))

(defun copilot-balancer--compute-closers (prefix completion suffix syntax-table)
  "Compute closing delimiters needed to balance PREFIX + COMPLETION + SUFFIX.
Uses SYNTAX-TABLE for parsing.  Returns a string of closing characters."
  (with-temp-buffer
    (set-syntax-table syntax-table)
    (insert prefix completion suffix)
    (let* ((state (parse-partial-sexp (point-min) (point-max)))
           (open-positions (nth 9 state))
           (closers ""))
      (dolist (pos open-positions)
        (let ((closer (matching-paren (char-after pos))))
          (when closer
            (setq closers (concat (string closer) closers)))))
      closers)))

(defun copilot-balancer--fix-lisp (start end completion)
  "Fix Lisp COMPLETION from START to END by balancing delimiters."
  (let* ((trimmed (copilot-balancer-trim-closing-pairs-at-end completion))
         (infix-fixup (and (= start end)
                           (eql (char-after end) ?\")
                           (copilot-balancer--odd-quotes-p completion)))
         (end (if infix-fixup (1+ end) end))
         (prefix (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char start)
                     (condition-case nil
                         (beginning-of-defun)
                       (scan-error (goto-char (point-min))))
                     (buffer-substring-no-properties (point) start))))
         (suffix (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char end)
                     (condition-case nil
                         (end-of-defun)
                       (scan-error (goto-char (point-max))))
                     (buffer-substring-no-properties end (point)))))
         (closers (copilot-balancer--compute-closers
                   prefix trimmed suffix (syntax-table))))
    (list start end (concat trimmed closers))))

(defun copilot-balancer-fix-completion (start end completion)
  "Fix COMPLETION from START to END."
  (cond
   ((and copilot-enable-parentheses-balancer
         (apply #'derived-mode-p copilot-balancer-lisp-modes))
    (copilot-balancer--fix-lisp start end completion))
   (t (list start end completion))))

(provide 'copilot-balancer)
;;; copilot-balancer.el ends here
