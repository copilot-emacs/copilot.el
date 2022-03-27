;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'cl-lib)
(require 'company)
(require 'json)
(require 's)

(defconst copilot--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name))))

;;
;; Customization
;;

(defgroup copilot nil
  "Options for copilot"
  :group 'company
  :prefix "copilot-")

(defvar copilot--process nil
  "Copilot agent process")

(defvar copilot--request-id 0
  "Request Id to distinguish requests. Required by RPC.")

(defvar copilot--callbacks nil
  "(id . callback) alist")

(defvar copilot--output-buffer nil
  "Buffer for process output.")

(defun copilot--start-process ()
  "Start Copilot process"
  (setq copilot--process
        (make-process
         :name "copilot-agent"
         :command (list "node"
                        (concat copilot--base-dir "/dist/agent.js")
                        )
         :coding 'utf-8
         :connection-type 'pipe
         :filter 'copilot--process-filter
         ;; :sentinel 'copilot--process-sentinel
         :noquery t))
  (message "Copilot agent started."))


(defun copilot--kill-process ()
  "Kill Copilot agent process."
  (when copilot--process
    (delete-process copilot--process)
    (setq copilot--process nil)))

(defun copilot--send-request (request)
  "Send REQUEST to Copilot agent."
  (unless copilot--process
    (copilot--start-process))
  (when copilot--process
    (let* ((body (json-serialize request))
           (content (concat "Content-Length: "
                            (int-to-string (length body))
                            "\r\n\r\n"
                            body)))
      ;; (message "-----request-----")
      ;; (message "%s" body)
      ;; (message "-----------------")
      (process-send-string copilot--process content))))


(defun copilot--agent-request (method params callback)
  "Send request and register callback."
  (cl-incf copilot--request-id)
  (let ((request (list :method method
                       :params params
                       :id copilot--request-id)))

    (push (cons copilot--request-id
                callback)
          copilot--callbacks)
    (copilot--send-request request)))


(defun copilot--process-filter (process output)
  "Process filter for Copilot agent. Only care about responses with id."
  ;; (message "-----output-----")
  ;; (message "%S" output)
  ;; (message "----------------")
  (setq copilot--output-buffer (concat copilot--output-buffer output))
  (when (equal (substring copilot--output-buffer -1)
               "}")
    (let* ((body (-> copilot--output-buffer
                     (split-string "\n")
                     last
                     car
                     json-read-from-string))
           (result (->> body
                        (alist-get 'result)))
           (id (alist-get 'id body)))
      (setq copilot--output-buffer nil)
      (when (equal id copilot--request-id)
        (copilot--provide-candidates result (alist-get id copilot--callbacks))
        (assq-delete-all id copilot--callbacks)))))


(defconst copilot--language-id
  '((emacs-lisp-mode . "lisp")))

(defun copilot--generate-doc ()
  (list :source (concat (buffer-substring-no-properties (point-min) (point-max)) "\n")
        :tabSize tab-width
        :indentSize tab-width
        :insertSpaces (if indent-tabs-mode :false t)
        :path (buffer-file-name)
        :relativePath (file-name-nondirectory (buffer-file-name))
        :languageId (or (assoc-default major-mode copilot--language-id) (substring (symbol-name major-mode) 0 -5))
        :position (list :line (1- (line-number-at-pos))
                        :character (length (buffer-substring-no-properties (point-at-bol) (point))))))

(defun copilot--get-candidates (callback)
  (copilot--agent-request "getCompletions"
                          (list :doc (copilot--generate-doc))
                          callback))


(defun copilot--provide-candidates (result callback)
  (let ((completions (alist-get 'completions result)))
    ;; (message "%S" completions)
    (funcall callback (if (seq-empty-p completions) nil (seq-elt completions 0)))))


(defface copilot-overlay-face
  '((t :inherit shadow))
  "Face for Copilot overlay")

(defvar-local copilot-overlay nil)

(defun copilot-display-overlay-completion (completion line col)
  "Show COMPLETION in overlay at LINE and COL."
  (copilot-clear-overlay)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (forward-line line)
    (forward-char col)

    ;; remove common prefix
    (let* ((cur-line (s-chop-suffix "\n" (thing-at-point 'line)))
            (common-prefix-len (length (s-shared-start completion cur-line))))
      (setq completion (substring completion common-prefix-len))
      (forward-char common-prefix-len))

    (unless (s-blank? completion)
      (let* ((ov (make-overlay (point) (point-at-eol) nil t t))
             (p-completion (propertize completion 'face 'copilot-overlay-face))
             (display (substring p-completion 0 1))
             (after-string (substring p-completion 1)))
        (overlay-put ov 'completion completion)
        (if (equal (overlay-start ov) (overlay-end ov))
            (progn
              (put-text-property 0 1 'cursor t p-completion)
              (overlay-put ov 'after-string p-completion))
          (overlay-put ov 'display display)
          (overlay-put ov 'after-string after-string))
        (setq copilot-overlay ov)))))

(defun copilot-clear-overlay ()
  "Clear all overlays from this buffer."
  (when copilot-overlay
    (delete-overlay copilot-overlay)
    (setq copilot-overlay nil)))


(defun copilot-accept-completion ()
  (interactive)
  (when copilot-overlay
    (let ((completion (overlay-get copilot-overlay 'completion)))
      (copilot-clear-overlay)
      (delete-region (point) (line-end-position))
      (insert completion)
      t)))

(defun copilot-complete ()
  (copilot-clear-overlay)
  (when (buffer-file-name)
    (copilot--get-candidates
     (lambda (completion)
       (when completion
         (let* ((text (alist-get 'text completion))
                (range (alist-get 'range completion))
                (start (alist-get 'start range))
                (start-line (alist-get 'line start))
                (start-char (alist-get 'character start)))
           (copilot-display-overlay-completion text start-line start-char)))))))


(defconst copilot--hooks
  '((evil-insert-state-entry-hook copilot-complete)
    (evil-insert-state-exit-hook copilot-clear-overlay)
    (post-self-insert-hook copilot-complete))
  "Hooks for auto completion.")

(defvar copilot--enabled nil
  "Whether copilot is enabled.")

(defun copilot-enable ()
  (interactive)
  (unless copilot--enabled
    (dolist (hook copilot--hooks)
      (add-hook (car hook) (cadr hook)))
    (setq copilot--enabled t)))


(defun copilot-disable ()
  (interactive)
  (when copilot--enabled
    (copilot--kill-process)
    (dolist (hook copilot--hooks)
      (remove-hook (car hook)
                   (cadr hook)))
    (setq copilot--enabled nil)))

(provide 'copilot)
