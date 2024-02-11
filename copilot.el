;;; copilot.el --- An unofficial Copilot plugin for Emacs  -*- lexical-binding:t -*-

;; Package-Requires: ((emacs "27.2") (s "1.12.0") (dash "2.19.1") (editorconfig "0.8.2") (jsonrpc "1.0.23"))
;; Version: 0.0.1
;;; URL: https://github.com/copilot-emacs/copilot.el

;;; Commentary:

;; An unofficial Copilot plugin for Emacs

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'jsonrpc)
(require 's)
(require 'dash)
(require 'editorconfig)
(require 'copilot-balancer)

(defgroup copilot nil
  "Copilot."
  :group 'completion
  :prefix "copilot-")

(defcustom copilot-idle-delay 0
  "Time in seconds to wait before starting completion.

Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'copilot)

(defcustom copilot-network-proxy nil
  "Network proxy to use for Copilot. Nil means no proxy.
Format: \='(:host \"127.0.0.1\" :port 80
:username \"username\" :password \"password\")
Username and password are optional.

If you are using a MITM proxy which intercepts TLS connections, you may need
to disable TLS verification. This can be done by setting a pair
':rejectUnauthorized :json-false' in the proxy plist. For example:

  (:host \"127.0.0.1\" :port 80 :rejectUnauthorized :json-false)
"
  :type '(plist :tag "Uncheck all to disable proxy" :key-type symbol)
  :options '((:host string) (:port integer) (:username string) (:password string))
  :group 'copilot)

(defcustom copilot-log-max 0
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'copilot
  :type 'integer)

(defcustom copilot-node-executable
  (if (eq system-type 'windows-nt)
      "node.exe"
    "node")
  "Node executable path."
  :group 'copilot
  :type 'string)


(defcustom copilot-max-char 100000
  "Maximum number of characters to send to Copilot, -1 means no limit."
  :group 'copilot
  :type 'integer)


(defcustom copilot-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'copilot
  :type '(repeat function))

(defcustom copilot-indent-offset-warning-disable nil
  "Disable warning when copilot--infer-indentation-offset cannot find
indentation offset."
  :group 'copilot
  :type 'boolean)

(defcustom copilot-indentation-alist
  (append '((latex-mode tex-indent-basic)
            (nxml-mode nxml-child-indent)
            (python-mode python-indent py-indent-offset python-indent-offset)
            (python-ts-mode python-indent py-indent-offset python-indent-offset)
            (web-mode web-mode-markup-indent-offset web-mode-html-offset))
          editorconfig-indentation-alist)
  "Alist of `major-mode' to indentation map with optional fallbacks."
  :type '(alist :key-type symbol :value-type (choice integer symbol))
  :group 'copilot)

(defconst copilot--base-dir
  (file-name-directory
   (or load-file-name
       (buffer-file-name)))
  "Directory containing this file.")

(defconst copilot-version "0.10.0"
  "Copilot version.")

(defvar-local copilot--overlay nil
  "Overlay for Copilot completion.")

(defvar-local copilot--keymap-overlay nil
  "Overlay used to surround point and make copilot-completion-keymap activate.")

(defvar copilot--connection nil
  "Copilot agent jsonrpc connection instance.")

(defvar-local copilot--line-bias 1
  "Line bias for Copilot completion.")

(defvar copilot--post-command-timer nil)
(defvar-local copilot--last-doc-version 0
  "The document version of the last completion.")
(defvar-local copilot--doc-version 0
  "The document version of the current buffer. Incremented after each change.")

(defun copilot--buffer-changed ()
  "Return non-nil if the buffer has changed since last completion."
  (not (= copilot--last-doc-version copilot--doc-version)))

(defvar copilot--opened-buffers nil
  "List of buffers that have been opened in Copilot.")

;;
;; agent
;;

(defconst copilot--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(defsubst copilot--connection-alivep ()
  "Non-nil if the `copilot--connection' is alive."
  (and copilot--connection
       (zerop (process-exit-status (jsonrpc--process copilot--connection)))))

(defmacro copilot--request (&rest args)
  "Send a request to the copilot agent with ARGS."
  `(progn
     (unless (copilot--connection-alivep)
       (copilot--start-agent))
     (jsonrpc-request copilot--connection ,@args)))

(defmacro copilot--notify (&rest args)
  "Send a notification to the copilot agent with ARGS."
  `(progn
     (unless (copilot--connection-alivep)
       (copilot--start-agent))
     (jsonrpc-notify copilot--connection ,@args)))

(cl-defmacro copilot--async-request (method params &rest args &key (success-fn #'copilot--ignore-response) &allow-other-keys)
  "Send an asynchronous request to the copilot agent."
  `(progn
     (unless (copilot--connection-alivep)
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
      (cond ((< node-version 18)
             (user-error "Node 18+ is required but found %s" node-version))
            (t
             (setq copilot--connection
                   (make-instance 'jsonrpc-process-connection
                                  :name "copilot"
                                  :events-buffer-config `(:size ,copilot-log-max)
                                  :notification-dispatcher #'copilot--handle-notification
                                  :process (make-process :name "copilot agent"
                                                         :command (list copilot-node-executable
                                                                        (concat copilot--base-dir "/dist/agent.js"))
                                                         :coding 'utf-8-emacs-unix
                                                         :connection-type 'pipe
                                                         :stderr (get-buffer-create "*copilot stderr*")
                                                         :noquery t)))
             (message "Copilot agent started.")
             (copilot--request 'initialize '(:capabilities (:workspace (:workspaceFolders t))))
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
      (copilot--request 'signInInitiate '(:dummy "signInInitiate"))
    (when (s-equals-p status "AlreadySignedIn")
      (user-error "Already signed in as %s" user))
    (if (display-graphic-p)
        (progn
          (gui-set-selection 'CLIPBOARD user-code)
          (read-from-minibuffer (format "Your one-time code %s is copied. Press \
ENTER to open GitHub in your browser. If your browser does not open \
automatically, browse to %s." user-code verification-uri))
          (browse-url verification-uri)
          (read-from-minibuffer "Press ENTER if you finish authorizing."))
      (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
      (read-from-minibuffer (format "Please open %s in your browser. Press ENTER if you finish authorizing." verification-uri)))
    (message "Verifying...")
    (condition-case err
        (copilot--request 'signInConfirm (list :userCode user-code))
      (jsonrpc-error
       (user-error "Authentication failure: %s" (alist-get 'jsonrpc-error-message (cddr err)))))
    (copilot--dbind (:user) (copilot--request 'checkStatus '(:dummy "checkStatus"))
      (message "Authenticated as GitHub user %s." user))))

(defun copilot-logout ()
  "Logout from Copilot."
  (interactive)
  (copilot--request 'signOut '(:dummy "signOut"))
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
  (setq copilot--opened-buffers nil)
  ;; We are going to send a test request for the current buffer so we have to activate the mode
  ;; if it is not already activated.
  ;; If it the mode is already active, we have to make sure the current buffer is loaded in the
  ;; agent.
  (if copilot-mode
      (copilot--on-doc-focus (selected-window))
    (copilot-mode))
  (copilot--async-request 'getCompletions
                          `(:doc (:version 0
                                           :source "\n"
                                           :path ""
                                           :uri ,(copilot--get-uri)
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

;; based on https://code.visualstudio.com/docs/languages/identifiers
;; (more here https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
(defvar copilot-major-mode-alist '(("rustic" . "rust")
                                   ("cperl" . "perl")
                                   ("c++" . "cpp")
                                   ("clojurec" . "clojure")
                                   ("clojurescript" . "clojure")
                                   ("objc" . "objective-c")
                                   ("cuda" . "cuda-cpp")
                                   ("docker-compose" . "dockercompose")
                                   ("coffee" . "coffeescript")
                                   ("js" . "javascript")
                                   ("js2" . "javascript")
                                   ("js2-jsx" . "javascriptreact")
                                   ("typescript-tsx" . "typescriptreact")
                                   ("rjsx" . "typescriptreact")
                                   ("less-css" . "less")
                                   ("text" . "plaintext")
                                   ("ess-r" . "r")
                                   ("enh-ruby" . "ruby")
                                   ("shell-script" . "shellscript")
                                   ("sh" . "shellscript")
                                   ("visual-basic" . "vb")
                                   ("nxml" . "xml"))
  "Alist mapping major mode names (with -mode removed) to copilot language ID's.")

(defvar-local copilot--completion-cache nil)
(defvar-local copilot--completion-idx 0)

(defvar-local copilot--indent-warning-printed-p nil
  "Flag indicating whether indent warning was already printed.")

(defun copilot--infer-indentation-offset ()
  "Infer indentation offset."
  (or (let ((mode major-mode))
        (while (and (not (assq mode copilot-indentation-alist))
                    (setq mode (get mode 'derived-mode-parent))))
        (when mode
          (cl-some (lambda (s)
                     (when (and (boundp s) (numberp (symbol-value s)))
                       (symbol-value s)))
                   (alist-get mode copilot-indentation-alist))))
      (progn
        (when (and
               (not copilot-indent-offset-warning-disable)
               (not copilot--indent-warning-printed-p))
          (display-warning '(copilot copilot-no-mode-indent)
                           "copilot--infer-indentation-offset found no mode-specific indentation offset.")
          (setq-local copilot--indent-warning-printed-p t))
        tab-width)))

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
    (concat "buffer://" (url-encode-url (buffer-name (current-buffer)))))
   ((and (eq system-type 'windows-nt)
         (not (s-starts-with-p "/" buffer-file-name)))
    (concat "file:///" (url-encode-url buffer-file-name)))
   (t
    (concat "file://" (url-encode-url buffer-file-name)))))

(defun copilot--get-source ()
  "Get source code from current buffer."
  (let* ((p (point))
         (pmax (point-max))
         (pmin (point-min))
         (half-window (/ copilot-max-char 2)))
    (when (and (>= copilot-max-char 0) (> pmax copilot-max-char))
      (display-warning '(copilot copilot-exceeds-max-char)
                       (format "%s size exceeds 'copilot-max-char' (%s), copilot completions may not work" (current-buffer) copilot-max-char)))
    (cond
     ;; using whole buffer
     ((or (< copilot-max-char 0) (< pmax copilot-max-char))
      (setq-local copilot--line-bias 1)
      (buffer-substring-no-properties pmin pmax))
     ;; truncate buffer head
     ((< (- pmax p) half-window)
      (setq-local copilot--line-bias (line-number-at-pos (- pmax copilot-max-char)))
      (buffer-substring-no-properties (- pmax copilot-max-char) pmax))
     ;; truncate buffer tail
     ((< (- p pmin) half-window)
      (setq-local copilot--line-bias 1)
      (buffer-substring-no-properties pmin (+ pmin copilot-max-char)))
     ;; truncate head and tail
     (t
      (setq-local copilot--line-bias (line-number-at-pos (- p half-window)))
      (buffer-substring-no-properties (- p half-window)
                                      (+ p half-window))))))

(defun copilot--get-language-id ()
  "Get language ID of current buffer."
  (let ((mode (s-chop-suffixes '("-ts-mode" "-mode") (symbol-name major-mode))))
    (alist-get mode copilot-major-mode-alist mode nil 'equal)))

(defun copilot--generate-doc ()
  "Generate doc parameters for completion request."
  (save-restriction
    (widen)
    (list :version copilot--doc-version
          :tabSize (copilot--infer-indentation-offset)
          :indentSize (copilot--infer-indentation-offset)
          :insertSpaces (if indent-tabs-mode :json-false t)
          :path (buffer-file-name)
          :uri (copilot--get-uri)
          :relativePath (copilot--get-relative-path)
          :languageId (copilot--get-language-id)
          :position (list :line (- (line-number-at-pos) copilot--line-bias)
                          :character (- (point) (line-beginning-position))))))

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

(defvar copilot--panel-lang nil
  "Language of current panel solutions.")

(defun copilot--handle-notification (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'PanelSolution)
    (copilot--dbind (:completionText completion-text :score completion-score) msg
      (with-current-buffer "*copilot-panel*"
        (unless (member (secure-hash 'sha256 completion-text)
                        (org-map-entries (lambda () (org-entry-get nil "SHA"))))
          (save-excursion
            (goto-char (point-max))
            (insert "* Solution\n"
                    "  :PROPERTIES:\n"
                    "  :SCORE: " (number-to-string completion-score) "\n"
                    "  :SHA: " (secure-hash 'sha256 completion-text) "\n"
                    "  :END:\n"
                    "#+BEGIN_SRC " copilot--panel-lang "\n"
                    completion-text "\n#+END_SRC\n\n")
            (mark-whole-buffer)
            (org-sort-entries nil ?R nil nil "SCORE"))))))
  (when (eql method 'PanelSolutionsDone)
    (message "Copilot: Finish synthesizing solutions.")
    (display-buffer "*copilot-panel*")
    (with-current-buffer "*copilot-panel*"
      (save-excursion
        (goto-char (point-max))
        (insert "End of solutions.\n")))))

(defun copilot--get-panel-completions (callback)
  "Get panel completions with CALLBACK."
  (copilot--async-request 'getPanelCompletions
                          (list :doc (copilot--generate-doc)
                                :panelId (generate-new-buffer-name "copilot-panel"))
                          :success-fn callback
                          :error-fn (lambda (err)
                                      (message "Copilot error: %S" err))
                          :timeout-fn (lambda ()
                                        (message "Copilot agent timeout."))))


(defun copilot-panel-complete ()
  "Pop a buffer with a list of suggested completions based on the current file ."
  (interactive)
  (require 'org)
  (setq copilot--last-doc-version copilot--doc-version)
  (setq copilot--panel-lang (copilot--get-language-id))

  (copilot--get-panel-completions
   (jsonrpc-lambda (&key solutionCountTarget)
     (message "Copilot: Synthesizing %d solutions..." solutionCountTarget)))
  (with-current-buffer (get-buffer-create "*copilot-panel*")
    (org-mode)
    (erase-buffer)))

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

(defun copilot--get-or-create-keymap-overlay ()
  "Make or return the local copilot--keymap-overlay."
  (unless (overlayp copilot--keymap-overlay)
    (setq copilot--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put copilot--keymap-overlay 'keymap copilot-completion-map)
    (overlay-put copilot--keymap-overlay 'priority 101))
  copilot--keymap-overlay)

(defun copilot--get-overlay ()
  "Create or get overlay for Copilot."
  (unless (overlayp copilot--overlay)
    (setq copilot--overlay (make-overlay 1 1 nil nil t))
    (overlay-put
     copilot--overlay 'keymap-overlay (copilot--get-or-create-keymap-overlay)))
  copilot--overlay)

(defun copilot--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defun copilot--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))

  ;; set overlay position for the keymap, to activate copilot-completion-map
  ;;
  ;; if the point is at the end of the buffer, we will create a 0-length buffer.
  ;; But this is ok, since the keymap
  ;; will still activate _so long_ as no other overlay contains the point
  ;;
  ;; see https://github.com/copilot-emacs/copilot.el/issues/251 for details.
  (move-overlay (overlay-get ov 'keymap-overlay) (point) (min (point-max) (+ 1 (point))))

  (let* ((tail (buffer-substring (copilot--overlay-end ov) (line-end-position)))
         (p-completion (concat (propertize completion 'face 'copilot-overlay-face)
                               tail)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq copilot--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun copilot--display-overlay-completion (completion uuid start end)
  "Show COMPLETION with UUID between START and END.

(save-excursion) is not necessary since there is only one caller, and they are
already saving an excursion. This is also a private function."
  (copilot-clear-overlay)
  (when (and (s-present-p completion)
             (or (= start (point))      ; up-to-date completion
                 (and (< start (point)) ; special case for removing indentation
                      (s-blank-p (s-trim (buffer-substring-no-properties start (point)))))))
    (goto-char start)                   ; indentation
    (let* ((ov (copilot--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (copilot--set-overlay-text ov completion)
      (overlay-put ov 'uuid uuid)
      (copilot--async-request 'notifyShown (list :uuid uuid)))))

(defun copilot-clear-overlay (&optional is-accepted)
  "Clear Copilot overlay. If IS-ACCEPTED is nil, notify rejected."
  (interactive)
  (when (copilot--overlay-visible)
    (unless is-accepted
      (copilot--async-request 'notifyRejected
                              (list :uuids `[,(overlay-get copilot--overlay 'uuid)])))
    (delete-overlay copilot--overlay)
    (delete-overlay copilot--keymap-overlay)
    (setq copilot--real-posn nil)))

(defun copilot-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (copilot--overlay-visible)
    (let* ((completion (overlay-get copilot--overlay 'completion))
           (start (overlay-get copilot--overlay 'start))
           (end (copilot--overlay-end copilot--overlay))
           (uuid (overlay-get copilot--overlay 'uuid))
           (t-completion (funcall (or transform-fn #'identity) completion)))
      (copilot--async-request 'notifyAccepted (list :uuid uuid))
      (copilot-clear-overlay t)
      (if (eq major-mode 'vterm-mode)
          (progn
            (vterm-delete-region start end)
            (vterm-insert t-completion))
        (delete-region start end)
        (insert t-completion))
      ;; if it is a partial completion
      (when (and (s-prefix-p t-completion completion)
                 (not (s-equals-p t-completion completion)))
        (copilot--set-overlay-text (copilot--get-overlay) (s-chop-prefix t-completion completion)))
      t)))

(defmacro copilot--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (copilot-accept-completion (lambda (completion)
                                  (with-temp-buffer
                                    (insert completion)
                                    (goto-char (point-min))
                                    (funcall ,action n)
                                    (buffer-substring-no-properties (point-min) (point)))))))

(copilot--define-accept-completion-by-action copilot-accept-completion-by-word #'forward-word)
(copilot--define-accept-completion-by-action copilot-accept-completion-by-line #'forward-line)
(copilot--define-accept-completion-by-action copilot-accept-completion-by-paragraph #'forward-paragraph)

(defun copilot--show-completion (completion-data)
  "Show COMPLETION-DATA."
  (when (copilot--satisfy-display-predicates)
    (copilot--dbind
        (:text :uuid :docVersion doc-version
         :range (:start (:line :character start-char)
                 :end (:character end-char)))
        completion-data
      (when (= doc-version copilot--doc-version)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((p (point))
                   (goto-line! (lambda ()
                                 (goto-char (point-min))
                                 (forward-line (1- (+ line copilot--line-bias)))))
                   (start (progn
                            (funcall goto-line!)
                            (forward-char start-char)
                            (let* ((cur-line (buffer-substring-no-properties (point) (line-end-position)))
                                   (common-prefix-len (length (s-shared-start text cur-line))))
                              (setq text (substring text common-prefix-len))
                              (forward-char common-prefix-len)
                              (point))))
                   (end (progn
                          (funcall goto-line!)
                          (forward-char end-char)
                          (point)))
                   (fixed-completion (copilot-balancer-fix-completion start end text)))
              (goto-char p)
              (pcase-let ((`(,start ,end ,balanced-text) fixed-completion))
                (copilot--display-overlay-completion balanced-text uuid start end)))))))))

(defun copilot--on-doc-focus (window)
  "Notify that the document has been focussed or opened."
  ;; When switching windows, this function is called twice, once for the
  ;; window losing focus and once for the window gaining focus. We only want to
  ;; send a notification for the window gaining focus and only if the buffer has
  ;; copilot-mode enabled.
  (when (and copilot-mode (eq window (selected-window)))
    (if (-contains-p copilot--opened-buffers (current-buffer))
        (copilot--notify ':textDocument/didFocus
                         (list :textDocument (list :uri (copilot--get-uri))))
      (add-to-list 'copilot--opened-buffers (current-buffer))
      (copilot--notify ':textDocument/didOpen
                       (list :textDocument (list :uri (copilot--get-uri)
                                                 :languageId (copilot--get-language-id)
                                                 :version copilot--doc-version
                                                 :text (copilot--get-source)))))))

(defun copilot--on-doc-change (&optional beg end chars-replaced)
  "Notify that the document has changed."
  (let* ((is-before-change (eq chars-replaced nil))
         (is-after-change (not is-before-change))
         ;; for a deletion, the post-change beginning and end are at the same place.
         (is-insertion (and is-after-change (not (equal beg end))))
         (is-deletion (and is-before-change (not (equal beg end)))))
    (when (or is-insertion is-deletion)
      (save-restriction
        (widen)
        (let* ((range-start (list :line (- (line-number-at-pos beg) copilot--line-bias)
                                  :character (- beg (save-excursion (goto-char beg) (line-beginning-position)))))
               (range-end (if is-insertion range-start
                            (list :line (- (line-number-at-pos end) copilot--line-bias)
                                  :character (- end (save-excursion (goto-char end) (line-beginning-position))))))
               (text (if is-insertion (buffer-substring-no-properties beg end) ""))
               (content-changes (vector (list :range (list :start range-start :end range-end)
                                              :text text))))
          (cl-incf copilot--doc-version)
          (copilot--notify 'textDocument/didChange
                           (list :textDocument (list :uri (copilot--get-uri) :version copilot--doc-version)
                                 :contentChanges content-changes)))))))

(defun copilot--on-doc-close (&rest _args)
  "Notify that the document has been closed."
  (when (-contains-p copilot--opened-buffers (current-buffer))
    (copilot--notify 'textDocument/didClose
                     (list :textDocument (list :uri (copilot--get-uri))))
    (setq copilot--opened-buffers (delete (current-buffer) copilot--opened-buffers))))


;;;###autoload
(defun copilot-complete ()
  "Complete at the current point."
  (interactive)
  (setq copilot--last-doc-version copilot--doc-version)

  (setq copilot--completion-cache nil)
  (setq copilot--completion-idx 0)

  (let ((called-interactively (called-interactively-p 'interactive)))
    (copilot--get-completion
     (jsonrpc-lambda (&key completions &allow-other-keys)
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
Copilot will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'copilot)

(defcustom copilot-enable-predicates '(evil-insert-state-p copilot--buffer-changed)
  "A list of predicate functions with no argument to enable Copilot.
Copilot will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'copilot)

(defcustom copilot-disable-display-predicates nil
  "A list of predicate functions with no argument to disable Copilot.
Copilot will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'copilot)

(defcustom copilot-enable-display-predicates nil
  "A list of predicate functions with no argument to enable Copilot.
Copilot will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'copilot)

(defmacro copilot--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun copilot--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (copilot--satisfy-predicates copilot-enable-predicates copilot-disable-predicates))

(defun copilot--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (copilot--satisfy-predicates copilot-enable-display-predicates copilot-disable-display-predicates))

(defvar copilot-mode-map (make-sparse-keymap)
  "Keymap for Copilot minor mode.
Use this for custom bindings in `copilot-mode'.")

(defun copilot--mode-enter ()
  "Set up copilot mode when entering."
  (add-hook 'post-command-hook #'copilot--post-command nil 'local)
  (add-hook 'before-change-functions #'copilot--on-doc-change nil 'local)
  (add-hook 'after-change-functions #'copilot--on-doc-change nil 'local)
  ;; Hook onto both window-selection-change-functions and window-buffer-change-functions
  ;; since both are separate ways of 'focussing' a buffer.
  (add-hook 'window-selection-change-functions #'copilot--on-doc-focus nil 'local)
  (add-hook 'window-buffer-change-functions #'copilot--on-doc-focus nil 'local)
  (add-hook 'kill-buffer-hook #'copilot--on-doc-close nil 'local)
  ;; The mode may be activated manually while focus remains on the current window/buffer.
  (copilot--on-doc-focus (selected-window)))

(defun copilot--mode-exit ()
  "Clean up copilot mode when exiting."
  (remove-hook 'post-command-hook #'copilot--post-command 'local)
  (remove-hook 'before-change-functions #'copilot--on-doc-change 'local)
  (remove-hook 'after-change-functions #'copilot--on-doc-change 'local)
  (remove-hook 'window-selection-change-functions #'copilot--on-doc-focus 'local)
  (remove-hook 'window-buffer-change-functions #'copilot--on-doc-focus 'local)
  (remove-hook 'kill-buffer-hook #'copilot--on-doc-close 'local)
  ;; Send the close event for the active buffer since activating the mode will open it again.
  (copilot--on-doc-close))

;;;###autoload
(define-minor-mode copilot-mode
  "Minor mode for Copilot."
  :init-value nil
  :lighter " Copilot"
  (copilot-clear-overlay)
  (advice-add 'posn-at-point :before-until #'copilot--posn-advice)
  (if copilot-mode
      (copilot--mode-enter)
    (copilot--mode-exit)))

(defun copilot--posn-advice (&rest args)
  "Remap posn if in copilot-mode."
  (when copilot-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and copilot--real-posn
                 (eq pos (car copilot--real-posn)))
        (cdr copilot--real-posn)))))


;;;###autoload
(define-global-minor-mode global-copilot-mode
  copilot-mode copilot-turn-on-unless-buffer-read-only)

(defun copilot-turn-on-unless-buffer-read-only ()
  "Turn on `copilot-mode' if the buffer is writable."
  (unless buffer-read-only
    (copilot-mode 1)))

(defun copilot--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "copilot-" (symbol-name this-command))
                        (member this-command copilot-clear-overlay-ignore-commands)
                        (copilot--self-insert this-command)))))
    (copilot-clear-overlay)
    (when copilot--post-command-timer
      (cancel-timer copilot--post-command-timer))
    (when (numberp copilot-idle-delay)
      (setq copilot--post-command-timer
          (run-with-idle-timer copilot-idle-delay
                               nil
                               #'copilot--post-command-debounce
                               (current-buffer))))))

(defun copilot--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (copilot--overlay-visible)
             (copilot--satisfy-display-predicates))
    (let* ((ov copilot--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (copilot-accept-completion)
          (copilot--set-overlay-text ov (substring completion 1)))))))

(defun copilot--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             copilot-mode
             (copilot--satisfy-trigger-predicates))
    (copilot-complete)))

(provide 'copilot)
;;; copilot.el ends here
