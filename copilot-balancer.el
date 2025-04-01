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

;; The `copilot-balancer.el` module helps balance pairs of characters
;; in Lisp-like languages.  This includes parentheses, brackets, braces, and quotation
;; marks.  The module is particularly useful for maintaining the syntactical integrity
;; of code written in Emacs Lisp, Common Lisp, Scheme, and Clojure.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'rx)
(require 'subr-x)

(defvar copilot-balancer-lisp-modes '( emacs-lisp-mode
                                       lisp-mode
                                       lisp-interaction-mode
                                       scheme-mode
                                       clojure-mode)
  "List of Lisp modes to balance.")

(defvar copilot-balancer-lisp-pairs
  (let ((h (make-hash-table :test 'equal :size 7)))
    (puthash ?\( ?\) h)
    (puthash ?\[ ?\] h)
    (puthash ?\{ ?\} h)

    (puthash ?\) ?\( h)
    (puthash ?\} ?\{ h)
    (puthash ?\] ?\[ h)

    (puthash ?\N{QUOTATION MARK} ?\N{QUOTATION MARK} h)

    h)
  "Hash table of Lisp pairs to balance.")

(defvar copilot-balancer-closing-lisp-pairs
  (let ((h (make-hash-table :test 'equal :size 4)))
    (puthash ?\) t h)
    (puthash ?\} t h)
    (puthash ?\] t h)
    (puthash ?\N{QUOTATION MARK} t h)
    h)
  "Hash table of closing Lisp pairs, such as right parenthese, etc.")

(defvar copilot-balancer-debug-buffer (get-buffer-create " *copilot-balancer*")
  "Buffer for debugging copilot-balancer.")

(defmacro copilot-balancer-to-plist (&rest vars)
  "Convert VARS to plist."
  `(list ,@(mapcan
            (lambda (var)
              (list (intern (concat ":" (symbol-name var))) var))
            vars)))

(defun copilot-balancer--debug (args)
  "Debug ARGS."
  (let* ((start (plist-get args :start))
         (end (plist-get args :end))
         (deleted-text (plist-get args :deleted-text))
         (completion (plist-get args :completion))
         (trimmed-completion (plist-get args :trimmed-completion))
         (prefix-pairs (plist-get args :prefix-pairs))
         (completion-pairs (plist-get args :completion-pairs))
         (in-string (plist-get args :in-string))
         (end-is-missing-double-quote (plist-get args :end-is-missing-double-quote))
         (meta-prefix-pairs (plist-get args :meta-prefix-pairs))
         (suffix-pairs (plist-get args :suffix-pairs))
         (flipped-suffix-pairs (plist-get args :flipped-suffix-pairs))
         (completion-suffix-str (plist-get args :completion-suffix-str))
         (new-completion (plist-get args :new-completion))
         (prefix (plist-get args :prefix))
         (suffix (plist-get args :suffix)))
    (with-current-buffer copilot-balancer-debug-buffer
      (erase-buffer)
      (insert "start end " (number-to-string start)
              " "
              (number-to-string end)
              "\n")
      (insert "deleted text:<STX>" deleted-text "<EOT>\n")


      (insert "completion:<STX>" completion "<EOT>\n")
      (insert "trimmed-completion:<STX>" trimmed-completion "<EOT>\n")
      (insert "\n")

      (insert "prefix-pairs:<STX>" (prin1-to-string prefix-pairs) "<EOT>\n")
      (insert "completion-pairs:<STX>" (prin1-to-string completion-pairs) "<EOT>\n")
      (insert "meta-prefix-pairs:<STX>" (prin1-to-string meta-prefix-pairs) "<EOT>\n")
      (insert "suffix-pairs:<STX>" (prin1-to-string suffix-pairs) "<EOT>\n")
      (insert "\n")

      (insert "in-string:" (prin1-to-string in-string) "\n")
      (insert "end-is-missing-double-quote:" (prin1-to-string end-is-missing-double-quote) "\n")

      (insert "flipped-suffix-pairs:<STX>" (prin1-to-string flipped-suffix-pairs) "<EOT>\n")

      (insert "completion-suffix-str:<STX>"
              (prin1-to-string completion-suffix-str) "<EOT>\n")
      (insert "new-completion:<STX>" new-completion "<EOT>\n")
      (insert "\n")

      (insert "prefix:\n<STX>" (substring prefix 0 (min 100 (length prefix))) "\n<EOT>\n")
      (insert "suffix:<STX>" suffix
              "\n<EOT>\n")
      nil)))

(defun copilot-balancer-remove-last (lst)
  "Remove last item from LST."
  (if (not (cdr lst))
      nil
    (cons (car lst) (copilot-balancer-remove-last (cdr lst)))))

(defun copilot-balancer-extract-pairs (s)
  "Extract a list of pair characters from string S.
Like parentheses, braces, brackets, or double quotes.

Note that pairs in the middle of strings are included, so take care."
  (let* ((pairs '())
         (n (length s))
         (i 0))
    (while (< i n)
      (let ((c (elt s i)))
        (cond
         ((= c ?\N{BACKSLASH})
          (setq i (1+ i)))
         (t
          (when (gethash c copilot-balancer-lisp-pairs)
            (push c pairs)))))
      (setq i (1+ i)))
    (nreverse pairs)))

(defun copilot-balancer-trim-closing-pairs-at-end (s)
  "Trim closing pairs from string S starting from the end.
Stops when a non-close-pair character is found."
  (let* ((n (length s))
         (i (1- n))
         (abort nil))
    (while (and (not abort)
                (>= i 0)
                (gethash (elt s i) copilot-balancer-closing-lisp-pairs))
      ;; if the preceding character is a backslash,
      ;; then abort since it's considered a non closing pair character
      (cond
       ((and (>= i 1) (= (elt s (1- i)) ?\N{BACKSLASH}))
        (setq abort t))
       (t (setq i (1- i)))))
    (substring s 0 (1+ i))))

(defun copilot-balancer-collapse-matching-pairs (pairs in-string)
  "Collapse matching PAIRS in list pairs.

Special care has to be taken to ignore pairs in the middle
of strings (IN-STRING)."
  (let ((filtered-pairs '()))
    ;; delete pairs in strings
    (dolist (x pairs)
      (cond
       ((= x ?\N{QUOTATION MARK})
        (setq in-string (not in-string))
        (push x filtered-pairs))
       ((not in-string) (push x filtered-pairs))))
    (setq filtered-pairs (nreverse filtered-pairs))
    ;; collapse complementary pairs
    (let ((collapsed-pairs '()))
      (dolist (x filtered-pairs)
        (let ((y (gethash x copilot-balancer-lisp-pairs)))
          (cond
           ((and collapsed-pairs
                 (eq (car collapsed-pairs) y))
            (setq collapsed-pairs (cdr collapsed-pairs)))
           (t
            (push x collapsed-pairs)))))
      (cons (nreverse collapsed-pairs) in-string))))

(defun copilot-balancer-get-other-pair (c)
  "Get other pair by key C."
  (gethash c copilot-balancer-lisp-pairs))

(defun copilot-balancer-trim-common-prefix (list1 list2)
  "Trim common prefix from LIST1 and LIST2."
  (if (and list1 list2 (equal (car list1) (car list2)))
      (copilot-balancer-trim-common-prefix (cdr list1) (cdr list2))
    (cons list1 list2)))

(defvar copilot-balancer-top-level-form-start-regexp
  (rx line-start (or (literal "(") (literal "[") (literal "{")))
  "Regexp for the start of a top level form.
Assumes cursor is at the start of a line.")

(defvar copilot-balancer-form-end-regexp
  (rx (or (literal "}") (literal "]") (literal ")")) line-end)
  "Regexp for the end of a form.
Assumes cursor is at the last character of the line (not the actual newline
character).")

(defun copilot-balancer-get-top-level-form-beginning-to-point (x)
  "Get top level beginning point by X."
  (save-excursion
    (save-restriction
      (widen)

      (beginning-of-line)
      (while (and (< 1 (point))
                  (not (looking-at-p copilot-balancer-top-level-form-start-regexp)))
        (forward-line -1)
        (beginning-of-line))
      (buffer-substring-no-properties (point) x))))

(defun copilot-balancer-get-point-to-top-level-form-end (x)
  "Get point to top level by X."
  (save-excursion
    (save-restriction
      (widen)

      (let* ((last-line-number (line-number-at-pos (point-max)))
             (on-last-line? (lambda ()
                              (= (line-number-at-pos (point)) last-line-number))))
        ;; first find the start of the next top level form
        (forward-line 1)
        (beginning-of-line)
        (while (and (not (funcall on-last-line?))
                    (not (eobp))
                    (not (looking-at-p copilot-balancer-top-level-form-start-regexp)))
          (forward-line 1)
          (beginning-of-line))

        ;; then find the end of the top level form by going backwards
        (unless (funcall on-last-line?)
          (forward-line -1)
          (end-of-line)
          (unless (bolp) (backward-char))
          (while (and (< 1 (line-number-at-pos (point)))
                      (< 1 (point))
                      (not (eobp))
                      (not (looking-at-p copilot-balancer-form-end-regexp)))
            (forward-line -1)
            (end-of-line)
            (unless (bolp) (backward-char))))
        (end-of-line)

        (buffer-substring-no-properties x (point))))))

(defun copilot-balancer-see-string-end-p (p point-upper-bound)
  "Look for the string at the end between P and POINT-UPPER-BOUND."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char p)
      (let ((flag t)
            (ret nil))
        (while (and flag
                    (not (eobp))
                    (<= (point) point-upper-bound)) ; is it with or without equals?
          (let ((c (char-after)))
            (cond
             ((= c ?\N{QUOTATION MARK})
              (setq flag nil)
              (setq ret t))
             ((= c ?\N{BACKSLASH})
              (forward-char)
              (forward-char))
             (t
              (forward-char)))))
        ret))))

(defun copilot-balancer-odd-dquote-count-p (s)
  "Odd dquote count from S."
  (let ((n (length s))
        (i 0)
        (count 0))
    (while (< i n)
      (let ((c (elt s i)))
        (cond
         ((= c ?\N{BACKSLASH})
          (setq i (1+ i)))
         ((= c ?\N{QUOTATION MARK})
          (setq count (1+ count)))))
      (setq i (1+ i)))
    (cl-oddp count)))

(defun copilot-balancer--fix-lisp (start end completion)
  "Fix Lisp from START to END with COMPLETION."
  (pcase-let*
      ((prefix (copilot-balancer-get-top-level-form-beginning-to-point start))
       (prefix-pairs (copilot-balancer-extract-pairs prefix))

       (trimmed-completion (copilot-balancer-trim-closing-pairs-at-end completion))
       (completion-pairs (copilot-balancer-extract-pairs trimmed-completion))

       (`(,meta-prefix-pairs . ,in-string)
        (thread-first
          (append prefix-pairs completion-pairs)
          (copilot-balancer-collapse-matching-pairs nil)))

       (infix-string-fixup-needed
        (and (= start end)
             (eql (char-after end) ?\N{QUOTATION MARK})
             (copilot-balancer-odd-dquote-count-p completion)))
       (end (if infix-string-fixup-needed
                (1+ end)
              end))

       (deleted-text (buffer-substring-no-properties start end))
       (suffix (copilot-balancer-get-point-to-top-level-form-end end))

       (point-upper-bound (+ (point)
                             (min (length meta-prefix-pairs) (length suffix))))
       (end-is-missing-double-quote
        (and in-string
             (or (string-match-p (string ?\N{QUOTATION MARK}) deleted-text)
                 (not (copilot-balancer-see-string-end-p end point-upper-bound)))))

       (`(,trimmed-completion ,meta-prefix-pairs ,in-string)
        (if end-is-missing-double-quote
            (list (concat trimmed-completion "\"")
                  (copilot-balancer-remove-last meta-prefix-pairs)
                  nil)
          (list trimmed-completion meta-prefix-pairs in-string)))

       (`(,suffix-pairs . _)
        (thread-first
          (copilot-balancer-extract-pairs suffix)
          (copilot-balancer-collapse-matching-pairs in-string)))
       (reversed-suffix-pairs (reverse suffix-pairs))
       (flipped-suffix-pairs (mapcar #'copilot-balancer-get-other-pair
                                     reversed-suffix-pairs))
       (xy (copilot-balancer-trim-common-prefix
            meta-prefix-pairs
            flipped-suffix-pairs))
       (rem-flipped-completion-suffix (car xy))
       (completion-suffix (mapcar #'copilot-balancer-get-other-pair
                                  rem-flipped-completion-suffix))

       (completion-suffix-str (apply #'string (nreverse completion-suffix)))
       (new-completion (concat trimmed-completion
                               completion-suffix-str))

       (debug-vars (copilot-balancer-to-plist
                    start end deleted-text prefix completion trimmed-completion suffix
                    prefix-pairs completion-pairs suffix-pairs
                    in-string end-is-missing-double-quote
                    meta-prefix-pairs flipped-suffix-pairs
                    rem-flipped-completion-suffix new-completion)))
    (copilot-balancer--debug debug-vars)

    (list start end new-completion)))

(defun copilot-balancer-fix-completion (start end completion)
  "Fix COMPLETION from START to END."
  (let* ()
    (cond
     ((apply #'derived-mode-p copilot-balancer-lisp-modes)
      (copilot-balancer--fix-lisp start end completion))
     (t (list start end completion)))))

(provide 'copilot-balancer)
;;; copilot-balancer.el ends here
