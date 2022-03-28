;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 's)

(defconst copilot--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name))))

(defconst copilot--request-timeout 5
  "Timeout for blocking requests to Copilot, in seconds.")


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

(defun copilot--agent-http-request (url params callback)
  (copilot--agent-request "httpRequest"
                          (append (list :url url
                                        :timeout 30000)
                                  params)
                          (lambda (result)
                            (->> result (alist-get 'body) json-read-from-string (cons (cons 'status (alist-get 'status result))) (funcall callback)))))

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
           (err (->> body (alist-get 'error)))
           (id (alist-get 'id body)))
      (setq copilot--output-buffer nil)
      (when (equal id copilot--request-id)
        (funcall (alist-get id copilot--callbacks) (cons (cons 'error err) result))
        (assq-delete-all id copilot--callbacks)))))

;;
;; login
;;

(defconst copilot--client-id "Iv1.b507a08c87ecfe98"
  "Copilot client id, copied from copilot.vim")

(defconst copilot--config-root
  (let ((root (concat (or (getenv "XDG_CONFIG_HOME")
                          (if (eq system-type 'windows-nt)
                              (expand-file-name "~/Appdata/Local")
                            (expand-file-name "~/.config")))
                      "/github-copilot")))
    (make-directory root t)
    root)
  "Copilot config root")

(defconst copilot--config-hosts
  (concat copilot--config-root "/hosts.json"))

(defun copilot--blocking (f &rest args)
  (let ((result nil)
        (count 0))
    (apply f (append args (list (lambda (r)
                                  (setq result r)))))
    (while (and (null result) (< (* 0.1 count) copilot--request-timeout))
      (sleep-for 0.1)
      (cl-incf count))
    result))

(defun copilot-login ()
  (interactive)
  (copilot--agent-http-request "https://github.com/login/device/code"
                               `(:method "POST"
                                         :headers (:Accept "application/json")
                                         :json (:client_id ,copilot--client-id :scope "user:read"))
                               #'copilot--login-callback))

(defun copilot--login-callback (result)
  (let* ((device-code (alist-get 'device_code result))
         (verification-uri (alist-get 'verification_uri result))
         (user-code (alist-get 'user_code result))
         (interval (alist-get 'interval result)))
    (when (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
      (if (display-graphic-p)
          (progn
            (read-from-minibuffer "Press ENTER to open GitHub in your browser")
            (browse-url verification-uri)
            (read-from-minibuffer "Press ENTER if you finish authorizing."))
        (read-from-minibuffer "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri))
      (message "Verifying...")
      (copilot--login-verify device-code))))


(defun copilot--login-verify (device-code)
  (copilot--agent-http-request (format "https://github.com/login/oauth/access_token?grant_type=urn:ietf:params:oauth:grant-type:device_code&device_code=%s&client_id=%s"
                                       device-code copilot--client-id)
                               '(:method "GET"
                                         :headers (:Accept "application/json"))
                               #'copilot--login-verify-callback))

(defun copilot--login-verify-callback (result)
  (let ((access-token (alist-get 'access_token result)))
    (if (not access-token)
        (message "Login failed.")
      (let* ((copilot-access-result (copilot--blocking #'copilot--agent-http-request
                                                       "https://api.github.com/copilot_internal/token"
                                                       `(:method "GET"
                                                                 :headers (:Authorization ,(concat "Bearer " access-token)))))
            (status (alist-get 'status copilot-access-result)))
        (if (equal status 403)
            (message "You don't have access to GitHub Copilot. Join the waitlist by visiting https://copilot.github.com")
          (message "Login success!")
          (copilot--oauth-save (copilot--oauth-user access-token) access-token))))))


(defun copilot--oauth-user (access-token)
  (let ((result (copilot--blocking #'copilot--agent-http-request
                                   "https://api.github.com/user"
                                   `(:method "GET"
                                             :headers (:Authorization ,(concat "Bearer " access-token))))))
    (if (not (equal (alist-get 'status result) 200))
        (message "Failed to get user info.")
      (alist-get 'login result))))


(defun copilot--oauth-save (user access-token)
  (with-temp-file copilot--config-hosts
    (insert (json-encode `(:github.com (:user ,user :oauth_token ,access-token))))))

;;
;; diagnose
;;

(defun copilot--diagnose-network ()
  (let ((result (copilot--blocking #'copilot--agent-http-request
                                   "https://copilot-proxy.githubusercontent.com/_ping"
                                   '(:method "GET"))))
    (cond ((not result) "Server connectivity error")
          ((equal (alist-get 'status result) 466) "Server error")
          (t "OK"))))

(defun copilot--diagnose-access ()
  (let* ((result (copilot--blocking #'copilot--agent-request
                                    "getCompletions"
                                    '(:doc (:source "" :path "" :relativePath "" :languageId "" :position (:line 0 :character 0)))))
         (err (alist-get 'error result)))
    (if err (format "error: %S" err) "OK")))

(defun copilot-diagnose ()
  "Diagnose copilot."
  (interactive)
  (if (not copilot--enabled)
      (message "Copilot enabled: No"))
    (message "Copilot enabled: Yes, Network: %s, Access: %s"
            (copilot--diagnose-network)
            (copilot--diagnose-access)))


;;
;; Auto completion
;;

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
     (lambda (result)
       (let* ((completions (alist-get 'completions result))
              (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
        (when completion
          (let* ((text (alist-get 'text completion))
                 (range (alist-get 'range completion))
                 (start (alist-get 'start range))
                 (start-line (alist-get 'line start))
                 (start-char (alist-get 'character start)))
            (copilot-display-overlay-completion text start-line start-char))))))))


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
    (copilot--start-process)
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
