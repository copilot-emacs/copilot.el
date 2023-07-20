;; -*- lexical-binding: t -*-

(require 'pcase)
(require 'dash)
(require 'rx)

(defvar copilot-balancer-lisp-modes '(emacs-lisp-mode
                                      lisp-mode
                                      scheme-mode
                                      clojure-mode)
  "List of lisp modes to balance.")

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
  "Hash table of lisp pairs to balance.")

(defvar copilot-balancer-closing-lisp-pairs
  (let ((h (make-hash-table :test 'equal :size 4)))
    (puthash ?\) t h)
    (puthash ?\} t h)
    (puthash ?\] t h)
    (puthash ?\N{QUOTATION MARK} t h)
    h)
  "Hash table of closing lisp pairs, such as right parenthese, etc.")

(defvar copilot-balancer-debug-buffer (get-buffer-create "*copilot-balancer*")
  "Buffer for debugging copilot-balancer.")

(defun copilot-balancer--debug
    (start end prefix completion trimmed-completion suffix
           prefix-pairs completion-pairs suffix-pairs
           meta-prefix-pairs flipped-suffix-pairs
           completion-suffix-str new-completion)
  (let ((region-to-be-deleted (buffer-substring-no-properties start end)))
    (with-current-buffer copilot-balancer-debug-buffer
      (erase-buffer)
      (insert "start end " (number-to-string start)
              " "
              (number-to-string end)
              "\n")
      (insert "region for deletion:<STX>" region-to-be-deleted "<EOT>\n")
      
      
      (insert "completion:<STX>" completion "<EOT>\n")
      (insert "trimmed-completion:<STX>" trimmed-completion "<EOT>\n")
      (insert "\n")
      
      (insert "prefix-pairs:<STX>" (prin1-to-string prefix-pairs) "<EOT>\n")
      (insert "completion-pairs:<STX>" (prin1-to-string completion-pairs) "<EOT>\n")
      (insert "meta-prefix-pairs:<STX>" (prin1-to-string meta-prefix-pairs) "<EOT>\n")
      (insert "suffix-pairs:<STX>" (prin1-to-string suffix-pairs) "<EOT>\n")
      (insert "\n")
      
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
  (if (not (cdr lst))
      nil
    (cons (car lst) (copilot-balancer-remove-last (cdr lst)))))

(defun copilot-balancer-extract-pairs (s)
  "Extract a list of pair characters from string s
like parentheses, braces, brackets, or double quotes.

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
  "Trim closing pairs from string s starting from the end.
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
  "Collapse matching pairs in list pairs.

Special care has to be taken to ignore pairs in the middle of strings."
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
           ((and (not (null collapsed-pairs))
                 (eq (car collapsed-pairs) y))
            (setq collapsed-pairs (cdr collapsed-pairs)))
           (t
            (push x collapsed-pairs)))))
      (cons (nreverse collapsed-pairs) in-string))))

(defun copilot-balancer-get-other-pair (c)
  (gethash c copilot-balancer-lisp-pairs))

(defun copilot-balancer-trim-common-prefix (list1 list2)
  (if (and list1 list2 (equal (car list1) (car list2)))
      (copilot-balancer-trim-common-prefix (cdr list1) (cdr list2))
    (cons list1 list2)))

(defvar copilot-balancer-top-level-form-start-regexp
  (rx line-start (or (literal "(") (literal "[") (literal "{")))
  "Regexp for the start of a top level form. Assumes cursor is at the start of a line.")

(defvar copilot-balancer-form-end-regexp
  (rx (or (literal "}") (literal "]") (literal ")")) line-end)
  "Regexp for the end of a form. Assumes cursor is at the last character of the line
(not the actual newline character).")

(defun copilot-balancer-get-top-level-form-beginning-to-point (x)
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
                    (< (point) (point-max))
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
                      (< (point) (point-max))
                      (not (looking-at-p copilot-balancer-form-end-regexp)))
            (forward-line -1)
            (end-of-line)
            (unless (bolp) (backward-char))))
        (end-of-line)
        
        (buffer-substring-no-properties x (point))))))

(defun copilot-balancer--fix-lisp (start end completion)
  (pcase-let*
      ((prefix (copilot-balancer-get-top-level-form-beginning-to-point start))
       (suffix (copilot-balancer-get-point-to-top-level-form-end end))
       (trimmed-completion (copilot-balancer-trim-closing-pairs-at-end
                            completion))

       (prefix-pairs (copilot-balancer-extract-pairs prefix))
       (completion-pairs (copilot-balancer-extract-pairs trimmed-completion))
               
       (`(,meta-prefix-pairs . ,in-string)
        (-> (append prefix-pairs completion-pairs)
            (copilot-balancer-collapse-matching-pairs nil)))

       (end-is-missing-double-quote
        (and in-string
             (< end (point-max))
             (not (equal "\"" (buffer-substring-no-properties end (1+ end))))))

       (`(,trimmed-completion ,meta-prefix-pairs ,in-string)
        (if end-is-missing-double-quote
            (list (concat trimmed-completion "\"")
                  (copilot-balancer-remove-last meta-prefix-pairs)
                  nil)
          (list trimmed-completion meta-prefix-pairs in-string)))
       
       (`(,suffix-pairs . _)
        (-> (copilot-balancer-extract-pairs suffix)
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
                               completion-suffix-str)))
    (copilot-balancer--debug start end prefix completion trimmed-completion suffix
                             prefix-pairs completion-pairs suffix-pairs
                             meta-prefix-pairs flipped-suffix-pairs
                             rem-flipped-completion-suffix new-completion)
    
    new-completion))

(defun copilot-balancer-fix-completion (start end completion)
  (let* ()
    (cond
     ((apply #'derived-mode-p copilot-balancer-lisp-modes)
      (copilot-balancer--fix-lisp start end completion))
     (t completion))))

(provide 'copilot-balancer)
