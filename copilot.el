;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 's)
(require 'dash)


(defgroup copilot nil
  "Copilot."
  :group 'completion
  :prefix "copilot-")

(defcustom copilot-idle-delay 0
  "Time in seconds to wait before starting completion. Complete immediately if set to 0."
  :type 'float
  :group 'copilot)

(defconst copilot--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defconst copilot--request-timeout 5
  "Timeout for blocking requests to Copilot, in seconds.")

(defvar copilot--process nil
  "Copilot agent process object.")

(defvar copilot--request-id 0
  "Request Id to distinguish requests.")

(defvar copilot--callbacks nil
  "(request-id . callback) alist")

(defvar copilot--output-buffer nil
  "Buffer for process outputs.")

(defvar copilot--request-timer nil
  "Timer for sending delayed requests.")

;;
;; agent
;;

(defconst copilot--node
  (if (eq system-type 'windows-nt)
      "node.exe"
    "node"))

(defun copilot--start-process ()
  "Start Copilot process"
  (if (not (locate-file copilot--node exec-path))
      (message "Could not find node executable")
    (let ((node-version
           (->> (shell-command-to-string (concat copilot--node " --version")) (s-trim) (s-chop-prefix "v") (string-to-number))))
      (if (< node-version 12)
          (message "Node 12+ required but found %s" node-version)
        (setq copilot--process
              (make-process
              :name "copilot-agent"
              :command (list copilot--node
                             (concat copilot--base-dir "/dist/agent.js"))
              :coding 'utf-8
              :connection-type 'pipe
              :filter 'copilot--process-filter
              ;; :sentinel 'copilot--process-sentinel
              :noquery t))
        (message "Copilot agent started.")))))


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
    (when copilot--request-timer
      (cancel-timer copilot--request-timer)
      (setq copilot--request-timer nil))
    (let* ((body (json-serialize request))
           (content (concat "Content-Length: "
                            (int-to-string (string-bytes body))
                            "\r\n\r\n"
                            body)))
      ;; (message "-----request-----")
      ;; (message "%s" body)
      ;; (message "-----------------")
      (if (> copilot-idle-delay 0)
          (setq copilot--request-timer
                (run-with-timer copilot-idle-delay nil
                                (lambda () (process-send-string copilot--process content))))
        (process-send-string copilot--process content)))))

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
  "Send HTTP request and register callback."
  (copilot--agent-request "httpRequest"
                          (append (list :url url
                                        :timeout 30000)
                                  params)
                          (lambda (result)
                            (->> result (alist-get 'body) json-read-from-string (cons (cons 'status (alist-get 'status result))) (funcall callback)))))

(defun copilot--blocking (f &rest args)
  "Run F with ARGS and wait for response. F can be copilot--agent-request or copilot--agent-http-request."
  (let ((result nil)
        (count 0))
    (apply f (append args (list (lambda (r)
                                  (setq result r)))))
    (while (and (null result) (< (* 0.1 count) copilot--request-timeout))
      (sleep-for 0.1)
      (cl-incf count))
    result))

(defun copilot--process-filter (process output)
  "Process filter for Copilot agent. Only care about responses with id."
  (setq copilot--output-buffer (concat copilot--output-buffer output))
  ;; (message "-----output-----")
  ;; (message "%S" copilot--output-buffer)
  ;; (message "----------------")
  (let ((header-match (s-match "Content-Length: \\([0-9]+\\)\n\n" copilot--output-buffer)))
    (if (and (not header-match) (> (length copilot--output-buffer) 50))
        (progn (setq copilot--output-buffer nil)
               (message "Copilot agent output buffer reset."))
      (when header-match
        (let* ((header (car header-match))
              (content-length (string-to-number (cadr header-match)))
              (full-length (+ (length header) content-length)))
          (when (>= (length copilot--output-buffer) full-length)
            (let ((content (substring copilot--output-buffer (length header) full-length)))
              (setq copilot--output-buffer (substring copilot--output-buffer full-length))
              (copilot--process-response content)
              ; rerun filter to process remaining output
              (copilot--process-filter process nil))))))))

(defun copilot--process-response (content)
  ;; (message "%S" content)
  (let* ((content (json-read-from-string content))
         (result (alist-get 'result content))
         (err (alist-get 'error content))
         (id (alist-get 'id content)))
    (when (equal id copilot--request-id)
      (funcall (alist-get id copilot--callbacks)
               (cons (cons 'error err) result))
      (assq-delete-all id copilot--callbacks))))

;;
;; login
;;

(defconst copilot--client-id "Iv1.b507a08c87ecfe98"
  "Copilot client id, copied from copilot.vim")

(defconst copilot--terms-version "2021-10-14"
  "Copilot terms version, copied from copilot.vim")

(defconst copilot--config-root
  (let ((root (concat (or (getenv "XDG_CONFIG_HOME")
                          (if (eq system-type 'windows-nt)
                              (expand-file-name "~/../Local")
                            (expand-file-name "~/.config")))
                      "/github-copilot")))
    (make-directory root t)
    root)
  "Copilot config root.")

(defconst copilot--config-hosts
  (concat copilot--config-root "/hosts.json"))

(defconst copilot--config-terms
  (concat copilot--config-root "/terms.json"))

(defun copilot-login ()
  "Login to Copilot."
  (interactive)
  (copilot--agent-http-request "https://github.com/login/device/code"
                               `(:method "POST"
                                         :headers (:Accept "application/json")
                                         :json (:client_id ,copilot--client-id :scope "user:read"))
                               #'copilot--login-callback))

(defun copilot--login-callback (result)
  (let* ((device-code (alist-get 'device_code result))
         (verification-uri (alist-get 'verification_uri result))
         (user-code (alist-get 'user_code result)))
    (if (display-graphic-p)
        (progn
          (gui-set-selection 'CLIPBOARD user-code)
          (read-from-minibuffer (format "Your one-time code %s is copied. Press ENTER to open GitHub in your browser." user-code))
          (browse-url verification-uri)
          (read-from-minibuffer "Press ENTER if you finish authorizing."))
      (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
      (read-from-minibuffer (format "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri)))
    (message "Verifying...")
    (copilot--login-verify device-code)))


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
          (when (yes-or-no-p "I agree to these telemetry terms as part of the GitHub Copilot technical preview.\nhttps://github.co/copilot-telemetry-terms")
            (let ((user (copilot--oauth-user access-token)))
              (when user
                (with-temp-file copilot--config-hosts
                  (insert (json-encode `(:github.com (:user ,user :oauth_token ,access-token)))))
                (with-temp-file copilot--config-terms
                  (insert (json-encode `((,user . (("version" . ,copilot--terms-version)))))))
                (message "Copilot: Authenticated as GitHub user %s" user)))))))))


(defun copilot--oauth-user (access-token)
  "Get user name by access token."
  (let ((result (copilot--blocking #'copilot--agent-http-request
                                   "https://api.github.com/user"
                                   `(:method "GET"
                                             :headers (:Authorization ,(concat "Bearer " access-token))))))
    (if (not (equal (alist-get 'status result) 200))
        (message "Failed to get user info.")
      (alist-get 'login result))))

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
  (unless copilot--process
    (copilot--start-process))
  (if (not copilot--process)
      (message "Copilot agent is not running.")
    (message "Copilot agent: Running, Network: %s, Access: %s"
            (copilot--diagnose-network)
            (copilot--diagnose-access))))

;;
;; Auto completion
;;

(defvar-local copilot--completion-cache nil)
(defvar-local copilot--completion-idx 0)

(defun copilot--generate-doc ()
  (list :source (concat (buffer-substring-no-properties (point-min) (point-max)) "\n")
        :tabSize tab-width
        :indentSize tab-width
        :insertSpaces (if indent-tabs-mode :false t)
        :path (buffer-file-name)
        :relativePath (file-name-nondirectory (buffer-file-name))
        :languageId (s-chop-suffix "-mode" (symbol-name major-mode))
        :position (list :line (1- (line-number-at-pos))
                        :character (length (buffer-substring-no-properties (point-at-bol) (point))))))

(defun copilot--get-completion (callback)
  (copilot--agent-request "getCompletions"
                          (list :doc (copilot--generate-doc))
                          callback))

(defun copilot--get-completions-cycling (callback)
  (if copilot--completion-cache
      (funcall callback copilot--completion-cache)
    (copilot--agent-request "getCompletionsCycling"
                            (list :doc (copilot--generate-doc))
                            callback)))

(defun copilot--cycle-completion (direction)
  (lambda (result)
    (unless copilot--completion-cache
      (setq copilot--completion-cache result))
    (let ((completions (cl-remove-duplicates (alist-get 'completions result)
                                             :key (lambda (x) (alist-get 'text x))
                                             :test 'equal)))
      (cond ((seq-empty-p completions)
             (message "No completion is available."))
            ((= (length completions) 1)
             (message "Only one completion is available."))
            (t
             (let ((idx (mod (+ copilot--completion-idx direction) (length completions))))
               (setq copilot--completion-idx idx)
               (let ((completion (elt completions idx)))
                 (copilot--show-completion completion))))))))

(defun copilot-next-completion ()
  (interactive)
  (when copilot--overlay
    (copilot--get-completions-cycling (copilot--cycle-completion 1))))

(defun copilot-previous-completion ()
  (interactive)
  (when copilot--overlay
    (copilot--get-completions-cycling (copilot--cycle-completion -1))))


;;
;; UI
;;


(defface copilot-overlay-face
  '((t :inherit shadow))
  "Face for Copilot overlay")

(defvar-local copilot--overlay nil)

(defun copilot-display-overlay-completion (completion line col)
  "Show COMPLETION in overlay at LINE and COL. For Copilot, COL is always 0."
  (copilot-clear-overlay)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (if (= (line-end-position line) (1- (point-max)))
        ; special case if the last line is empty
        (progn
          (goto-char (point-max))
          (newline)
          (forward-char -1))
      (forward-line line)
      (forward-char col))

    ; remove common prefix
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
        (overlay-put ov 'start (point))
        (if (equal (overlay-start ov) (overlay-end ov))
            (progn
              (put-text-property 0 1 'cursor t p-completion)
              (overlay-put ov 'after-string p-completion))
          (overlay-put ov 'display display)
          (overlay-put ov 'after-string after-string))
        (setq copilot--overlay ov)))))

(defun copilot-clear-overlay ()
  (interactive)
  (when copilot--overlay
    (delete-overlay copilot--overlay)
    (setq copilot--overlay nil)))

(defun copilot-accept-completion ()
  (interactive)
  (when copilot--overlay
    (let ((completion (overlay-get copilot--overlay 'completion))
          (start (overlay-get copilot--overlay 'start)))
      (copilot-clear-overlay)
      (delete-region start (line-end-position))
      (insert completion)
      t)))

(defun copilot--show-completion (completion)
  (when completion
    (let* ((text (alist-get 'text completion))
           (range (alist-get 'range completion))
           (start (alist-get 'start range))
           (start-line (alist-get 'line start))
           (start-char (alist-get 'character start)))
      (copilot-display-overlay-completion text start-line start-char))))

(defun copilot-complete ()
  (interactive)
  (copilot-clear-overlay)

  (setq copilot--completion-cache nil)
  (setq copilot--completion-idx 0)

  (when (buffer-file-name)
    (copilot--get-completion
     (lambda (result)
       (let* ((completions (alist-get 'completions result))
              (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
          (copilot--show-completion completion))))))

(provide 'copilot)
