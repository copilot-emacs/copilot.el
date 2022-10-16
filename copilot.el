;;; copilot.el --- An unofficial Copilot plugin for Emacs  -*- lexical-binding:t -*-

;; Package-Requires: ((emacs "27.2") (s "1.12.0") (dash "2.19.1") (editorconfig "0.8.2") (jsonrpc "1.0.14"))

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'jsonrpc)
(require 's)
(require 'dash)
(require 'editorconfig)

(defgroup copilot nil
  "Copilot."
  :group 'completion
  :prefix "copilot-")

(defcustom copilot-idle-delay 0
  "Time in seconds to wait before starting completion. Complete immediately if set to 0."
  :type 'float
  :group 'copilot)

(defcustom copilot-network-proxy nil
  "Network proxy to use for Copilot. Nil means no proxy.
Format: '(:host \"127.0.0.1\" :port 80 :username \"username\" :password \"password\")
If username and password are not provided, they will be prompted for."
  :group 'copilot)

(defcustom copilot-log-max message-log-max
  "Max size of events buffer. 0 disables, nil means infinite."
  :group 'copilot
  :type 'integer)

(defcustom copilot-node-executable
  (if (eq system-type 'windows-nt)
      "node.exe"
    "node")
  "Node executable path."
  :group 'copilot
  :type 'string)

(defcustom copilot-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'copilot
  :type '(repeat symbol))

(defconst copilot--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defconst copilot-version "0.9.7"
  "Copilot version.")

(defvar-local copilot--overlay nil
  "Overlay for Copilot completion.")

(defvar copilot--connection nil
  "Copilot agent jsonrpc connection instance.")

(defvar copilot--post-command-timer nil)
(defvar-local copilot--buffer-changed nil
  "Non nil if buffer has changed since last time `copilot-complete' has been invoked.")

(defun copilot--buffer-changed ()
  copilot--buffer-changed)
;;
;; agent
;;

(defconst copilot--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(defmacro copilot--request (&rest args)
  "Send a request to the copilot agent with ARGS."
  `(progn
     (unless copilot--connection
       (copilot--start-agent))
     (jsonrpc-request copilot--connection ,@args)))

(cl-defmacro copilot--async-request (method params &rest args &key (success-fn #'copilot--ignore-response) &allow-other-keys)
  "Send an asynchronous request to the copilot agent."
  `(progn
     (unless copilot--connection
       (copilot--start-agent))
     ;; jsonrpc will use temp buffer for callbacks, so we need to save the current buffer and restore it inside callback
     (let ((buf (current-buffer)))
       (jsonrpc-async-request copilot--connection
                              ,method ,params
                              :success-fn (lambda (result)
                                            (with-current-buffer buf
                                              (funcall ,success-fn result)))
                              ,@args))))

(defun copilot--start-agent ()
  "Start the copilot agent process in local."
  (if (not (locate-file copilot-node-executable exec-path))
      (user-error "Could not find node executable")
    (let ((node-version (->> (with-output-to-string
                               (call-process copilot-node-executable nil standard-output nil "--version"))
                             (s-trim)
                             (s-chop-prefix "v")
                             (string-to-number))))
      (cond ((< node-version 12)
             (user-error "Node 12+ is required but found %s" node-version))
            ((>= node-version 18)
             (user-error "Node 18+ is not supported but found %s" node-version))
            (t
             (setq copilot--connection
                   (make-instance 'jsonrpc-process-connection
                                  :name "copilot"
                                  :events-buffer-scrollback-size copilot-log-max
                                  :process (make-process :name "copilot agent"
                                                         :command (list copilot-node-executable
                                                                        (concat copilot--base-dir "/dist/agent.js"))
                                                         :coding 'utf-8-emacs-unix
                                                         :connection-type 'pipe
                                                         :stderr (get-buffer-create "*copilot stderr*")
                                                         :noquery t)))
             (message "Copilot agent started.")
             (copilot--request 'initialize '(:capabilities 'nil))
             (copilot--async-request 'setEditorInfo
                                     `(:editorInfo (:name "Emacs" :version ,emacs-version)
                                       :editorPluginInfo (:name "copilot.el" :version ,copilot-version)
                                       ,@(when copilot-network-proxy
                                           `(:networkProxy ,copilot-network-proxy)))))))))

;;
;; login / logout
;;

(eval-and-compile
  (defun copilot--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (copilot--transform-pattern p)
                      p))
                  pattern))))

(defmacro copilot--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(copilot--transform-pattern pattern) ,source))
     ,@body))

(defun copilot-login ()
  "Login to Copilot."
  (interactive)
  (copilot--dbind
      (:status :user :userCode user-code :verificationUri verification-uri)
      (copilot--request 'signInInitiate ''nil)
    (when (s-equals-p status "AlreadySignedIn")
      (message "Already signed in as %s." user)
      (cl-return-from copilot-login))
    (if (display-graphic-p)
        (progn
          (gui-set-selection 'CLIPBOARD user-code)
          (read-from-minibuffer (format "Your one-time code %s is copied. Press ENTER to open GitHub in your browser." user-code))
          (browse-url verification-uri)
          (read-from-minibuffer "Press ENTER if you finish authorizing."))
      (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
      (read-from-minibuffer (format "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri)))
    (message "Verifying...")
    (condition-case err
        (copilot--request 'signInConfirm (list :userCode user-code))
      (jsonrpc-error
        (message "Authentication failure: %s" (alist-get 'jsonrpc-error-message (cddr err)))))
    (copilot--dbind (:user) (copilot--request 'checkStatus ''nil)
      (message "Authenticated as GitHub user %s." user))))

(defun copilot-logout ()
  "Logout from Copilot."
  (interactive)
  (copilot--request 'signOut ''nil)
  (message "Logged out."))

;;
;; diagnose
;;

(defun copilot-diagnose ()
  "Restart and diagnose copilot."
  (interactive)
  (when copilot--connection
    (jsonrpc-shutdown copilot--connection)
    (setq copilot--connection nil))
  (copilot--async-request 'getCompletions
                          '(:doc (:source "\n"
                                  :path ""
                                  :uri ""
                                  :relativePath ""
                                  :languageId "text"
                                  :position (:line 0 :character 0)))
                          :success-fn (lambda (_)
                                        (message "Copilot OK."))
                          :error-fn (lambda (err)
                                      (message "Copilot error: %S" err))
                          :timeout-fn (lambda ()
                                        (message "Copilot agent timeout."))))


;;
;; Auto completion
;;

(defconst copilot--indentation-alist
  (append '((latex-mode tex-indent-basic)
            (nxml-mode nxml-child-indent)
            (python-mode python-indent py-indent-offset python-indent-offset)
            (web-mode web-mode-markup-indent-offset web-mode-html-offset))
          editorconfig-indentation-alist)
  "Alist of `major-mode' to indentation map with optional fallbacks.")

(defvar-local copilot--completion-cache nil)
(defvar-local copilot--completion-idx 0)

(defun copilot--infer-indentation-offset ()
  "Infer indentation offset."
  (or (let ((mode major-mode))
        (while (and (not (assq mode copilot--indentation-alist))
                    (setq mode (get mode 'derived-mode-parent))))
        (when mode
          (cl-some (lambda (s)
                     (when (boundp s)
                       (symbol-value s)))
                   (alist-get mode copilot--indentation-alist))))
      tab-width))

(defun copilot--get-relative-path ()
  "Get relative path to current buffer."
  (cond
   ((not buffer-file-name)
    "")
   ((fboundp 'projectile-project-root)
    (file-relative-name buffer-file-name (projectile-project-root)))
   ((boundp 'vc-root-dir)
    (file-relative-name buffer-file-name (vc-root-dir)))
   (t
    (file-name-nondirectory buffer-file-name))))

(defun copilot--get-uri ()
  "Get URI of current buffer."
  (cond
   ((not buffer-file-name)
    "")
   ((eq system-type 'windows-nt)
    (concat "file:///" (url-encode-url buffer-file-name)))
   (t
    (concat "file://" (url-encode-url buffer-file-name)))))

(defun copilot--generate-doc ()
  "Generate doc parameters for completion request."
  (list :source (concat (buffer-substring-no-properties (point-min) (point-max)) "\n")
        :tabSize (copilot--infer-indentation-offset)
        :indentSize (copilot--infer-indentation-offset)
        :insertSpaces (if indent-tabs-mode :json-false t)
        :path (buffer-file-name)
        :uri (copilot--get-uri)
        :relativePath (copilot--get-relative-path)
        :languageId (s-chop-suffix "-mode" (symbol-name major-mode))
        :position (list :line (1- (line-number-at-pos))
                        :character (- (point) (point-at-bol)))))

(defun copilot--get-completion (callback)
  "Get completion with CALLBACK."
  (copilot--async-request 'getCompletions
                          (list :doc (copilot--generate-doc))
                          :success-fn callback))

(defun copilot--get-completions-cycling (callback)
  "Get completion cycling options with CALLBACK."
  (if copilot--completion-cache
      (funcall callback copilot--completion-cache)
    (copilot--async-request 'getCompletionsCycling
                            (list :doc (copilot--generate-doc))
                            :success-fn callback)))

(defun copilot--cycle-completion (direction)
  "Cycle completion with DIRECTION."
  (lambda (result)
    (unless copilot--completion-cache
      (setq copilot--completion-cache result))
    (let ((completions (cl-remove-duplicates (plist-get result :completions)
                                             :key (lambda (x) (plist-get x :text))
                                             :test #'s-equals-p)))
      (cond ((seq-empty-p completions)
             (message "No completion is available."))
            ((= (length completions) 1)
             (message "Only one completion is available."))
            (t (let ((idx (mod (+ copilot--completion-idx direction)
                               (length completions))))
                 (setq copilot--completion-idx idx)
                 (let ((completion (elt completions idx)))
                   (copilot--show-completion completion))))))))

(defsubst copilot--overlay-visible ()
  "Return whether the `copilot--overlay' is avaiable."
  (and (overlayp copilot--overlay)
       (overlay-buffer copilot--overlay)))

(defun copilot-next-completion ()
  "Cycle to next completion."
  (interactive)
  (when (copilot--overlay-visible)
    (copilot--get-completions-cycling (copilot--cycle-completion 1))))

(defun copilot-previous-completion ()
  "Cycle to previous completion."
  (interactive)
  (when (copilot--overlay-visible)
    (copilot--get-completions-cycling (copilot--cycle-completion -1))))


;;
;; UI
;;

(defun copilot-current-completion ()
  "Get current completion."
  (and (copilot--overlay-visible)
       (overlay-get copilot--overlay 'completion)))

(defface copilot-overlay-face
  '((t :inherit shadow))
  "Face for Copilot overlay.")

(defvar-local copilot--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defconst copilot-completion-map (make-sparse-keymap)
  "Keymap for Copilot completion overlay.")

(defun copilot-display-overlay-completion (completion uuid line col user-pos)
  "Show COMPLETION with UUID in overlay at LINE and COL.
For Copilot, COL is always 0.
USER-POS is the cursor position (for verification only)."
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
    (let* ((cur-line (s-chop-suffix "\n" (or (thing-at-point 'line) "")))
           (common-prefix-len (length (s-shared-start completion cur-line))))
      (setq completion (substring completion common-prefix-len))
      (forward-char common-prefix-len))

    (when (and (s-present-p completion)
               (or (= (point) user-pos) ; up-to-date completion
                   (and (< (point) user-pos) ; special case for removing indentation
                        (s-blank-p (s-trim (buffer-substring-no-properties (point) user-pos))))))
      (let* ((p-completion (propertize completion 'face 'copilot-overlay-face))
             (ov (if (not (overlayp copilot--overlay))
                     (make-overlay (point) (point-at-eol) nil nil t)
                   (move-overlay copilot--overlay (point) (point-at-eol))
                   copilot--overlay)))
        (if (= (overlay-start ov) (overlay-end ov)) ; end of line
            (progn
              (setq copilot--real-posn (cons (point) (posn-at-point)))
              (put-text-property 0 1 'cursor t p-completion)
              (overlay-put ov 'after-string p-completion))
          (overlay-put ov 'display (substring p-completion 0 1))
          (overlay-put ov 'after-string (substring p-completion 1)))
        (overlay-put ov 'completion completion)
        (overlay-put ov 'start (point))
        (overlay-put ov 'uuid uuid)
        (overlay-put ov 'keymap copilot-completion-map)
        (setq copilot--overlay ov)
        (copilot--async-request 'notifyShown (list :uuid uuid))))))

(defun copilot-clear-overlay ()
  "Clear Copilot overlay."
  (interactive)
  (when (copilot--overlay-visible)
    (copilot--async-request 'notifyRejected
                            (list :uuids `[,(overlay-get copilot--overlay 'uuid)]))
    (delete-overlay copilot--overlay)
    (setq copilot--real-posn nil)))

(defun copilot-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (copilot--overlay-visible)
    (let* ((completion (overlay-get copilot--overlay 'completion))
           (start (overlay-get copilot--overlay 'start))
           (uuid (overlay-get copilot--overlay 'uuid))
           (t-completion (funcall (or transform-fn #'identity) completion)))
      (copilot--async-request 'notifyAccepted (list :uuid uuid))
      (copilot-clear-overlay)
      (delete-region start (line-end-position))
      (insert t-completion)
      ; trigger completion again if not fully accepted
      (unless (equal completion t-completion)
        (copilot-complete))
      t)))

(defun copilot-accept-completion-by-word (n-word)
  "Accept first N-WORD words of completion."
  (interactive "p")
  (setq n-word (or n-word 1))
  (copilot-accept-completion (lambda (completion)
                               (let* ((blank-regexp '(any blank "\r" "\n"))
                                      (separator-regexp (rx-to-string
                                                         `(seq
                                                           (not ,blank-regexp)
                                                           (1+ ,blank-regexp))))
                                      (words (s-split-up-to separator-regexp completion n-word))
                                      (remain (if (<= (length words) n-word)
                                                  ""
                                                (cl-first (last words))))
                                      (length (- (length completion) (length remain)))
                                      (prefix (substring completion 0 length)))
                                 (s-trim-right prefix)))))

(defun copilot-accept-completion-by-line (n-line)
  "Accept first N-LINE lines of completion."
  (interactive "p")
  (setq n-line (or n-line 1))
  (copilot-accept-completion (lambda (completion)
                               (let* ((lines (s-split-up-to (rx anychar (? "\r") "\n") completion n-line))
                                      (remain (if (<= (length lines) n-line)
                                                  ""
                                                (cl-first (last lines))))
                                      (length (- (length completion) (length remain)))
                                      (prefix (substring completion 0 length)))
                                 prefix))))

(defun copilot--show-completion (completion)
  "Show COMPLETION."
  (copilot--dbind (:text :uuid :range (:start (:line :character))) completion
    (copilot-display-overlay-completion text uuid line character (point))))

(defun copilot-complete ()
  "Complete at the current point."
  (interactive)
  (setq copilot--buffer-changed nil)

  (setq copilot--completion-cache nil)
  (setq copilot--completion-idx 0)

  (let ((called-interactively (called-interactively-p 'interactive)))
    (copilot--get-completion
      (jsonrpc-lambda (&key completions)
        (let ((completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
          (if completion
              (copilot--show-completion completion)
            (when called-interactively
              (message "No completion is available."))))))))

;;
;; minor mode
;;

(defcustom copilot-disable-predicates nil
  "A list of predicate functions with no argument to disable Copilot.
Copilot will be disabled if any predicate returns t."
  :type 'list
  :group 'copilot)

(defcustom copilot-enable-predicates '(evil-insert-state-p copilot--buffer-changed)
  "A list of predicate functions with no argument to enable Copilot.
Copilot will be enabled only if all predicates return t."
  :type 'list
  :group 'copilot)

(defvar copilot-mode-map (make-sparse-keymap)
  "Keymap for Copilot minor mode.

Use this for custom bindings in `copilot-mode'.")

;;;###autoload
(define-minor-mode copilot-mode
  "Minor mode for Copilot."
  :init-value nil
  :lighter " Copilot"
  (copilot-clear-overlay)
  (advice-add 'posn-at-point :before-until #'copilot--posn-advice)
  (if copilot-mode
      (progn
        (add-hook 'post-command-hook #'copilot--post-command nil 'local)
        (add-hook 'before-change-functions #'copilot--on-change nil 'local))
    (remove-hook 'post-command-hook #'copilot--post-command 'local)
    (remove-hook 'before-change-functions #'copilot--on-change 'local)))

(defun copilot--posn-advice (&rest args)
  "Remap posn if necessary."
  (when copilot-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and copilot--real-posn
                 (eq pos (car copilot--real-posn)))
        (cdr copilot--real-posn)))))


;;;###autoload
(define-global-minor-mode global-copilot-mode
    copilot-mode copilot-mode)

(defun copilot--on-change (&reset _args)
  (setq copilot--buffer-changed t))

(defun copilot--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "copilot-" (symbol-name this-command))
                        (member this-command copilot-clear-overlay-ignore-commands)))))
    (copilot-clear-overlay)
    (when copilot--post-command-timer
      (cancel-timer copilot--post-command-timer))
    (setq copilot--post-command-timer
          (run-with-idle-timer copilot-idle-delay
                               nil
                               #'copilot--post-command-debounce
                               (current-buffer)))))

(defun copilot--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             copilot-mode
             (cl-every (lambda (pred)
                         (if (functionp pred) (funcall pred) t))
                       copilot-enable-predicates)
             (cl-notany (lambda (pred)
                          (if (functionp pred) (funcall pred) nil))
                        copilot-disable-predicates))
        (copilot-complete)))

(provide 'copilot)
;;; copilot.el ends here
