;;; copilot.el --- An unofficial Copilot plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 copilot-emacs maintainers

;; Author: zerol <z@zerol.me>
;; Maintainer: Emil van der Westhuizen
;;             Shen, Jen-Chieh <jcs090218@gmail.com>
;;             Rakotomandimby Mihamina <mihamina.rakotomandimby@rktmb.org>
;;             Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/copilot-emacs/copilot.el
;; Package-Requires: ((emacs "27.2") (editorconfig "0.8.2") (jsonrpc "1.0.14") (f "0.20.0"))
;; Version: 0.3.0-snapshot
;; Keywords: convenience copilot

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

;; An unofficial Copilot plugin for Emacs

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'jsonrpc)
(require 'subr-x)

(require 'f)
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
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-network-proxy nil
  "Network proxy to use for Copilot.

Nil means no proxy.
Format: \='(:host \"127.0.0.1\" :port 80 :username \"username\"
            :password \"password\")
Username and password are optional.

If you are using a MITM proxy which intercepts TLS connections, you may need
to disable TLS verification.  This can be done by setting a pair
':rejectUnauthorized :json-false' in the proxy plist.  For example:

  (:host \"127.0.0.1\" :port 80 :rejectUnauthorized :json-false)"
  :type '(plist :tag "Uncheck all to disable proxy" :key-type symbol)
  :options '((:host string) (:port integer) (:username string) (:password string))
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-log-max 0
  "Max size of events buffer.
0 disables, nil means infinite.  Enabling event logging may slightly affect
performance."
  :group 'copilot
  :type 'integer
  :package-version '(copilot . "0.1"))

(defcustom copilot-server-log-level 0
  "Log level of the Copilot server.
0 - no log
1 - error
2 - warning
3 - info
4 - debug"
  :group 'copilot
  :type 'integer
  :package-version '(copilot . "0.1"))

(defcustom copilot-server-args '("--stdio")
  "Additional arguments to pass to the Copilot server."
  :group 'copilot
  :type '(repeat string)
  :package-version '(copilot . "0.1"))

(defcustom copilot-max-char 100000
  "Maximum number of characters to send to Copilot, -1 means no limit."
  :group 'copilot
  :type 'integer
  :package-version '(copilot . "0.1"))


(defcustom copilot-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'copilot
  :type '(repeat function)
  :package-version '(copilot . "0.1"))

(defcustom copilot-indent-offset-warning-disable nil
  "Disable indentation warnings.

Warning occurs when the function `copilot--infer-indentation-offset' cannot
find indentation offset."
  :group 'copilot
  :type 'boolean
  :package-version '(copilot . "0.1"))

(defcustom copilot-max-char-warning-disable nil
  "When non-nil, disable warning about buffer size exceeding `copilot-max-char'."
  :group 'copilot
  :type 'boolean
  :package-version '(copilot . "0.1"))

(defcustom copilot-indentation-alist
  (append '((emacs-lisp-mode lisp-indent-offset)
            (latex-mode tex-indent-basic)
            (lisp-mode lisp-indent-offset)
            (nxml-mode nxml-child-indent)
            (python-mode python-indent py-indent-offset python-indent-offset)
            (python-ts-mode python-indent py-indent-offset python-indent-offset)
            (web-mode web-mode-markup-indent-offset web-mode-html-offset))
          editorconfig-indentation-alist)
  "Alist of `major-mode' to indentation map with optional fallbacks."
  :type '(alist :key-type symbol :value-type (choice integer symbol))
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defconst copilot-server-package-name "@github/copilot-language-server"
  "The name of the package to install copilot server.")

(defcustom copilot-install-dir (expand-file-name
                                (locate-user-emacs-file (f-join ".cache" "copilot")))
  "Directory in which the servers will be installed."
  :risky t
  :type 'directory
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-server-executable "copilot-language-server"
  "The executable of copilot server."
  :type 'string
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-version nil
  "Copilot server version.

The default value is the preferred version and ensures functionality.
You may adjust this variable at your own risk."
  :type '(choice (const :tag "Latest" nil)
                 (string :tag "Specific Version"))
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defun copilot--lsp-settings-changed (symbol value)
  "Restart the Copilot LSP due to SYMBOL changed to VALUE.

This function will be called by the customization framework when the
`copilot-lsp-settings' is changed.  When changed with `setq', then this function
will not be called."
  (let ((was-bound (boundp symbol)))
    (set-default symbol value)
    (when was-bound
      ;; Notifying the agent with the new value does only work if we include the
      ;; last value (as nil) as well. For example, having the value
      ;; '(:github-enterprise (:uri "https://example2.ghe.com")) and setting it
      ;; to nil would require to send the value '(:github-enterprise (:uri nil))
      ;; to the server. Otherwise, the value is ignored, since sending nil is
      ;; not enough.
      (copilot--start-server))))

(defcustom copilot-lsp-settings nil
  "Settings for the Copilot LSP server.

This value will always be sent to the server when the server starts or the value
changes.  See
https://github.com/github/copilot-language-server-release?tab=readme-ov-file#configuration-management
for complete documentation.

To change the value of this variable, the customization framework provided by
Emacs must be used.  Either use `setopt' or `customize' to change the value.  If
the value was set without the customization mechanism, then the LSP has to be
manually restarted with `copilot-diagnose'.  Otherwise, the change will not be
applied.

For example to use GitHub Enterprise use the following configuration:
 '(:github-enterprise (:uri \"https://example.ghe.com\"))

Exchange the URI with the correct URI of your organization."
  :set #'copilot--lsp-settings-changed
  :type 'sexp
  :group 'copilot
  :package-version '(copilot . "0.2"))

(defvar-local copilot--overlay nil
  "Overlay for Copilot completion.")

(defvar-local copilot--keymap-overlay nil
  "Overlay used to surround point and make copilot-completion-keymap activate.")

(defvar copilot--connection nil
  "Copilot server jsonrpc connection instance.")

(defvar-local copilot--line-bias 1
  "Line bias for Copilot completion.")

(defvar copilot--post-command-timer nil)
(defvar-local copilot--last-doc-version 0
  "The document version of the last completion.")
(defvar-local copilot--doc-version 0
  "The document version of the current buffer.
Incremented after each change.")

;;
;; Utility functions
;;

(defun copilot--buffer-changed ()
  "Return non-nil if the buffer has changed since last completion."
  (not (= copilot--last-doc-version copilot--doc-version)))

(defvar copilot--opened-buffers nil
  "List of buffers that have been opened in Copilot.")

(defmacro copilot--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(cl-destructuring-bind (&key ,@pattern &allow-other-keys) ,source
     ,@body))

(defsubst copilot--log (level format &rest args)
  "Log message with LEVEL, FORMAT and ARGS."
  (message "%s: %s" (propertize "Copilot" 'face
                                (pcase level
                                  ('error 'error)
                                  ('warning 'warning)
                                  ('info 'success)
                                  (_ 'warning)))
           (apply #'format format args)))

(defun copilot--mode-symbol (mode-name)
  "Infer the language for MODE-NAME."
  (thread-last
    mode-name
    (string-remove-suffix "-ts-mode")
    (string-remove-suffix "-mode")))

(defun copilot--string-common-prefix (str1 str2)
  "Find the common prefix of STR1 and STR2 directly."
  (let ((min-len (min (length str1) (length str2)))
        (i 0))
    (while (and (< i min-len)
                (= (aref str1 i) (aref str2 i)))
      (setq i (1+ i)))
    (substring str1 0 i)))

;;
;; Externals
;;

(declare-function vterm-delete-region "ext:vterm.el")
(declare-function vterm-insert "ext:vterm.el")
(declare-function org-sort-entries "ext:org.el")
(declare-function org-entry-get "ext:org.el")
(declare-function org-map-entries "ext:org.el")

;;
;;; Copilot Server Installation
;;

(defun copilot-installed-version ()
  "Return the version number of currently installed `copilot-server-package-name'."
  (let ((possible-paths (list
                         (when (eq system-type 'windows-nt)
                           (f-join copilot-install-dir "node_modules" copilot-server-package-name "package.json"))
                         (f-join copilot-install-dir "lib" "node_modules" copilot-server-package-name "package.json")
                         (f-join copilot-install-dir "lib64" "node_modules" copilot-server-package-name "package.json"))))
    (seq-some
     (lambda (path)
       (when (and path (file-exists-p path))
         (with-temp-buffer
           (insert-file-contents path)
           (save-match-data
             (when (re-search-forward "\"version\": \"\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\"" nil t)
               (match-string 1))))))
     possible-paths)))

(defun copilot-server-executable ()
  "Return the location of the `copilot-server-executable' file."
  (cond
   ((and (file-name-absolute-p copilot-server-executable)
         (file-exists-p copilot-server-executable))
    copilot-server-executable)
   ((executable-find copilot-server-executable t))
   (t
    (let ((path (executable-find
                 (f-join copilot-install-dir
                       (cond ((eq system-type 'windows-nt) "")
                             (t "bin"))
                       copilot-server-executable)
                 t)))
      (unless (and path (file-exists-p path))
        (error "The package %s is not installed.  Unable to find %s"
               copilot-server-package-name path))
      path))))

;; XXX: This function is modified from `lsp-mode'; see `lsp-async-start-process'
;; function for more information.
(defun copilot-async-start-process (callback error-callback &rest command)
  "Start async process COMMAND with CALLBACK and ERROR-CALLBACK."
  (with-current-buffer
      (compilation-start
       (mapconcat
        #'shell-quote-argument
        (seq-filter (lambda (cmd) cmd) command)
        " ")
       t
       (lambda (&rest _)
         (generate-new-buffer-name "*copilot-install-server*")))
    (view-mode +1)
    (add-hook
     'compilation-finish-functions
     (lambda (_buf status)
       (if (string= "finished\n" status)
           (when callback
             (condition-case err
                 (funcall callback)
               (error
                (funcall error-callback (error-message-string err)))))
         (when error-callback
           (funcall error-callback (string-trim-right status)))))
     nil t)))

;;;###autoload
(defun copilot-install-server ()
  "Interactively install server."
  (interactive)
  (if-let* ((npm-binary (executable-find "npm")))
      (progn
        (make-directory copilot-install-dir 'parents)
        (copilot-async-start-process
         nil nil
         npm-binary
         "-g" "--prefix" copilot-install-dir
         "install" (concat copilot-server-package-name
                           (when copilot-version (format "@%s" copilot-version)))))
    (copilot--log 'warning "Unable to install %s via `npm' because it is not present" copilot-server-package-name)
    nil))

;;;###autoload
(defun copilot-uninstall-server ()
  "Delete a Copilot server from `copilot-install-dir'."
  (interactive)
  (unless (file-directory-p copilot-install-dir)
    (user-error "Couldn't find %s directory" copilot-install-dir))
  (delete-directory copilot-install-dir 'recursive)
  (copilot--log 'warning "Server `%s' uninstalled." (file-name-nondirectory (directory-file-name copilot-install-dir))))

;;;###autoload
(defun copilot-reinstall-server ()
  "Interactively re-install server."
  (interactive)
  (copilot-uninstall-server)
  (copilot-install-server))

;;
;; Interaction with Copilot Server
;;

(defconst copilot--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(defsubst copilot--connection-alivep ()
  "Non-nil if the `copilot--connection' is alive."
  (and copilot--connection
       (zerop (process-exit-status (jsonrpc--process copilot--connection)))))

(defmacro copilot--request (&rest args)
  "Send a request to the copilot server with ARGS."
  `(progn
     (unless (copilot--connection-alivep)
       (copilot--start-server))
     (jsonrpc-request copilot--connection ,@args)))

(defmacro copilot--notify (&rest args)
  "Send a notification to the copilot server with ARGS."
  `(progn
     (unless (copilot--connection-alivep)
       (copilot--start-server))
     (jsonrpc-notify copilot--connection ,@args)))

(cl-defmacro copilot--async-request (method params &rest args &key (success-fn #'copilot--ignore-response) &allow-other-keys)
  "Send an asynchronous request to the copilot server.

Arguments METHOD, PARAMS and ARGS are used in function `jsonrpc-async-request'.

SUCCESS-FN is the CALLBACK."
  `(progn
     (unless (copilot--connection-alivep)
       (copilot--start-server))
     ;; jsonrpc will use temp buffer for callbacks, so we need to save the current buffer and restore it inside callback
     (let ((buf (current-buffer)))
       (jsonrpc-async-request copilot--connection
                              ,method ,params
                              :success-fn (lambda (result)
                                            (if (buffer-live-p buf)
                                                (with-current-buffer buf
                                                  (funcall ,success-fn result))))
                              ,@args))))

(defun copilot--command ()
  "Return the command-line to start copilot server."
  (append
   (list (copilot-server-executable))
   copilot-server-args))

(defun copilot--make-connection ()
  "Establish copilot jsonrpc connection."
  (let ((make-fn (apply-partially
                  #'make-instance
                  'jsonrpc-process-connection
                  :name "copilot"
                  :request-dispatcher #'copilot--handle-request
                  :notification-dispatcher #'copilot--handle-notification
                  :process (make-process :name "copilot server"
                                         :command (copilot--command)
                                         :coding 'utf-8-emacs-unix
                                         :connection-type 'pipe
                                         :stderr (get-buffer-create "*copilot stderr*")
                                         :noquery t))))
    (condition-case nil
        (funcall make-fn :events-buffer-config `(:size ,copilot-log-max))
      (invalid-slot-name
       ;; handle older jsonrpc versions
       (funcall make-fn :events-buffer-scrollback-size copilot-log-max)))))

(defun copilot--start-server ()
  "Start the copilot server process in local."
  (cond
   ((not (file-exists-p (copilot-server-executable)))
    (user-error "Server is not installed, please install via `M-x copilot-install-server`"))
   (t
    (let ((installed-version (copilot-installed-version)))
      (when (and copilot-version (not (equal installed-version copilot-version)))
        (warn "This package has been tested for Copilot server version %s but version %s has been detected.
You can change the installed version with `M-x copilot-reinstall-server` or remove this warning by changing the value of `copilot-version'."
              copilot-version installed-version)))
    (setq copilot--connection (copilot--make-connection))
    (copilot--log 'info "Copilot server started.")
    (copilot--request
     'initialize
     `(:processId
       ,(emacs-pid)
       :capabilities
       (:workspace
        (:workspaceFolders t))
       :initializationOptions
       (:editorInfo
        (:name "Emacs" :version ,emacs-version)
        :editorPluginInfo
        (:name "copilot.el" :version ,(or (package-get-version) "unknown"))
        ,@(when copilot-network-proxy
            `(:networkProxy ,copilot-network-proxy)))))
    (copilot--notify 'initialized '())
    (copilot--notify 'workspace/didChangeConfiguration `(:settings ,copilot-lsp-settings)))))

;;
;; login / logout
;;

(defun copilot-login ()
  "Login to Copilot."
  (interactive)
  (copilot--dbind
      (status user ((:userCode user-code)) ((:verificationUri verification-uri)))
      (copilot--request 'signInInitiate '(:dummy "signInInitiate"))
    (when (string-equal status "AlreadySignedIn")
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
    (copilot--log 'info "Verifying...")
    (condition-case err
        (copilot--request 'signInConfirm (list :userCode user-code))
      (jsonrpc-error
       (user-error "Authentication failure: %s" (alist-get 'jsonrpc-error-message (cddr err)))))
    (copilot--dbind (user) (copilot--request 'checkStatus '(:dummy "checkStatus"))
      (copilot--log 'info "Authenticated as GitHub user %s." user))))

(defun copilot-logout ()
  "Logout from Copilot."
  (interactive)
  (copilot--request 'signOut '(:dummy "signOut"))
  (copilot--log 'warning "Logged out."))

;;
;; diagnose
;;

(defun copilot-diagnose ()
  "Restart and diagnose copilot."
  (interactive)
  (when copilot--connection
    (jsonrpc-shutdown copilot--connection 'kill)
    (setq copilot--connection nil))
  (setq copilot--opened-buffers nil)
  ;; We are going to send a test request for the current buffer so we have to activate the mode
  ;; if it is not already activated.
  ;; If it the mode is already active, we have to make sure the current buffer is loaded in the
  ;; server.
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
                                        (copilot--log 'info "Copilot OK."))
                          :error-fn (lambda (err)
                                      (copilot--log 'error "%S" err))
                          :timeout-fn (lambda ()
                                        (copilot--log 'warning "Copilot server timeout."))))

;;
;; Auto completion
;;

;; based on https://code.visualstudio.com/docs/languages/identifiers
;; (more here https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
(defvar copilot-major-mode-alist '(("rustic" . "rust")
                                   ("cperl" . "perl")
                                   ("c++" . "cpp")
                                   ("clojure" . "clojure")
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
                                   ("caml" . "ocaml")
                                   ("tuareg" . "ocaml")
                                   ("text" . "plaintext")
                                   ("ess-r" . "r")
                                   ("enh-ruby" . "ruby")
                                   ("shell-script" . "shellscript")
                                   ("sh" . "shellscript")
                                   ("visual-basic" . "vb")
                                   ("nxml" . "xml"))
  "Alist mapping major mode names (with -mode removed) to copilot language ID's.")

(defvar copilot-minor-mode-alist '(("git-commit" . "git-commit"))
  "Alist mapping minor mode names (with -mode removed) to copilot language ID's.")

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
          (let ((indent-spec (alist-get mode copilot-indentation-alist)))
            (cond
             ((listp indent-spec)
              (cl-some (lambda (s)
                         (cond ((numberp s) s)
                               ((and (boundp s) (numberp (symbol-value s)))
                                (symbol-value s))))
                       indent-spec))
             ((functionp indent-spec) ; editorconfig 0.11.0+
              ;; This points to a setter, which do not call
              nil)))))
      (progn
        (when (and
               (not copilot-indent-offset-warning-disable)
               (not copilot--indent-warning-printed-p))
          (display-warning '(copilot copilot-no-mode-indent)
                           "copilot--infer-indentation-offset found no mode-specific indentation offset.")
          (setq-local copilot--indent-warning-printed-p t))
        standard-indent)))

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
    (concat "file:///buffer/" (url-encode-url (buffer-name (current-buffer)))))
   ((and (eq system-type 'windows-nt)
         (not (string-prefix-p "/" buffer-file-name)))
    (concat "file:///" (url-encode-url buffer-file-name)))
   (t
    (concat "file://" (url-encode-url buffer-file-name)))))

(defun copilot--get-source ()
  "Get source code from current buffer."
  (save-restriction
    (widen)
    (let* ((p (point))
           (pmax (point-max))
           (pmin (point-min))
           (half-window (/ copilot-max-char 2)))
      (when (and (>= copilot-max-char 0)
                 (> pmax copilot-max-char))
        (let ((msg (format "%s size exceeds 'copilot-max-char' (%s), copilot completions may not work"
                           (current-buffer) copilot-max-char)))
          (if copilot-max-char-warning-disable
              (message msg)
            (display-warning '(copilot copilot-exceeds-max-char) msg))))
      (cond
       ;; using whole buffer
       ((or (< copilot-max-char 0) (< pmax copilot-max-char))
        (setq-local copilot--line-bias 1)
        (buffer-substring-no-properties pmin pmax))
       ;; truncate buffer head
       ((< (- pmax p) half-window)
        (let ((start (max pmin (- pmax copilot-max-char))))
          (setq-local copilot--line-bias (line-number-at-pos start))
          (buffer-substring-no-properties start pmax)))
       ;; truncate buffer tail
       ((< (- p pmin) half-window)
        (setq-local copilot--line-bias 1)
        (buffer-substring-no-properties pmin (min pmax (+ pmin copilot-max-char))))
       ;; truncate head and tail
       (t
        (let ((start (max pmin (- p half-window)))
              (end (min pmax (+ p half-window))))
          (setq-local copilot--line-bias (line-number-at-pos start))
          (buffer-substring-no-properties start end)))))))

(defun copilot--get-minor-mode-language-id ()
  "Get language ID from minor mode if available."
  (let ((pair
         (seq-find
          (lambda (pair)
            (let ((minor-mode-symbol (intern (concat (car pair) "-mode"))))
              (and (boundp minor-mode-symbol) (symbol-value minor-mode-symbol))))
          copilot-minor-mode-alist)))
    (cdr pair)))

(defun copilot--get-major-mode-language-id ()
  "Get language ID from major mode."
  (let ((major-mode-symbol (copilot--mode-symbol (symbol-name major-mode))))
    (alist-get major-mode copilot-major-mode-alist major-mode-symbol nil 'equal)))

(defun copilot--get-language-id ()
  "Get language ID of current buffer."
  (or (copilot--get-minor-mode-language-id)
      (copilot--get-major-mode-language-id)))

(defun copilot--generate-doc ()
  "Generate doc parameters for completion request."
  (save-restriction
    (widen)
    (list :version copilot--doc-version
          :tabSize (copilot--infer-indentation-offset)
          ;; indentSize doesn't not appear to be used, but has been in this code
          ;; base from the start. For now leave it as is.
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
             (copilot--log 'warning "No completion is available."))
            ((= (length completions) 1)
             (copilot--log 'warning "Only one completion is available."))
            (t (let ((idx (mod (+ copilot--completion-idx direction)
                               (length completions))))
                 (setq copilot--completion-idx idx)
                 (let ((completion (elt completions idx)))
                   (copilot--show-completion completion))))))))

(defsubst copilot--overlay-visible ()
  "Return whether the `copilot--overlay' is available."
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

(defvar copilot--request-handlers (make-hash-table :test 'equal)
  "Hash table storing request handlers.")

(defun copilot-on-request (method handler)
  "Register a request HANDLER for the given METHOD.
Each request METHOD can have only one HANDLER."
  (puthash method handler copilot--request-handlers))

(defun copilot--handle-request (_ method msg)
  "Handle MSG of type METHOD by calling the appropriate registered handler."
  (let ((handler (gethash method copilot--request-handlers)))
    (when handler
      (funcall handler msg))))

(defvar copilot--notification-handlers (make-hash-table :test 'equal)
  "Hash table storing lists of notification handlers.")

(defun copilot-on-notification (method handler)
  "Register a notification HANDLER for the given METHOD."
  (let ((handlers (gethash method copilot--notification-handlers '())))
    (puthash method (cons handler handlers) copilot--notification-handlers)))

(defun copilot--handle-notification (_ method msg)
  "Handle MSG of type METHOD by calling all appropriate registered handlers."
  (let ((handlers (gethash method copilot--notification-handlers '())))
    (dolist (handler handlers)
      (funcall handler msg))))

(copilot-on-notification
 'window/logMessage
 (lambda (msg)
   (copilot--dbind (((:type log-level)) ((:message log-msg))) msg
     (with-current-buffer (get-buffer-create "*copilot-language-server-log*")
       (save-excursion
         (goto-char (point-max))
         (insert (propertize (concat log-msg "\n")
                             'face (pcase log-level
                                     (4 'shadow)
                                     (3 'success)
                                     (2 'warning)
                                     (1 'error)))))))))

(copilot-on-notification
 'PanelSolution
 (lambda (msg)
   (copilot--dbind (((:completionText completion-text)) ((:score completion-score))) msg
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
           (call-interactively #'mark-whole-buffer)
           (org-sort-entries nil ?R nil nil "SCORE")))))))

(copilot-on-notification
 'PanelSolutionsDone
 (lambda (_msg)
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
                                      (copilot--log 'error "%S" err))
                          :timeout-fn (lambda ()
                                        (copilot--log 'warning "Copilot server timeout."))))


(defun copilot-panel-complete ()
  "Pop a buffer with a list of suggested completions based on the current file ."
  (interactive)
  (require 'org)
  (setq copilot--last-doc-version copilot--doc-version)
  (setq copilot--panel-lang (copilot--get-language-id))

  (copilot--get-panel-completions
   (jsonrpc-lambda (&key solutionCountTarget)
     (copilot--log 'info "Synthesizing %d solutions..." solutionCountTarget)))
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

(defun copilot--posn-advice (&rest args)
  "Remap posn if in copilot-mode with ARGS."
  (when copilot-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and copilot--real-posn
                 (eq pos (car copilot--real-posn)))
        (cdr copilot--real-posn)))))

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
  ;; if the point is at the end of the buffer, we will create a
  ;; 0-length buffer. But this is ok, since the keymap will still
  ;; activate _so long_ as no other overlay contains the point.
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

`save-excursion' is not necessary since there is only one caller, and they are
already saving an excursion.  This is also a private function."
  (copilot-clear-overlay)
  (when (and (not (string-blank-p completion))
             (or (<= start (point))))
    (let* ((ov (copilot--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (copilot--set-overlay-text ov completion)
      (overlay-put ov 'uuid uuid)
      (overlay-put ov 'completion-start start)
      (copilot--async-request 'notifyShown (list :uuid uuid)))))

(defun copilot-clear-overlay (&optional is-accepted)
  "Clear Copilot overlay.
If IS-ACCEPTED is nil, notify rejected."
  (interactive)
  (when (copilot--overlay-visible)
    (unless is-accepted
      (copilot--async-request 'notifyRejected
                              (list :uuids `[,(overlay-get copilot--overlay 'uuid)])))
    (delete-overlay copilot--overlay)
    (delete-overlay copilot--keymap-overlay)
    (setq copilot--real-posn nil)))

(defun copilot-accept-completion (&optional transform-fn)
  "Accept completion.
Return t if there is a completion.  Use TRANSFORM-FN to transform completion if
provided."
  (interactive)
  (when (copilot--overlay-visible)
    (let* ((completion (overlay-get copilot--overlay 'completion))
           (start (overlay-get copilot--overlay 'start))
           (end (copilot--overlay-end copilot--overlay))
           (uuid (overlay-get copilot--overlay 'uuid))
           (t-completion (funcall (or transform-fn #'identity) completion))
           (completion-start (overlay-get copilot--overlay 'completion-start)))
      ;; If there is extra indentation before the point, delete it and shift the completion
      (when (and (< completion-start (point))
                 ;; Region we are about to delete contains only blanks …
                 (string-blank-p (buffer-substring-no-properties completion-start (point)))
                 ;; … *and* everything from BOL to completion-start is blank
                 ;; as well — i.e. we are really inside the leading indentation.
                 (string-blank-p (buffer-substring-no-properties (line-beginning-position) completion-start)))
        (setq start completion-start)
        (setq end (- end (- (point) completion-start)))
        (delete-region completion-start (point)))
      (copilot--async-request 'notifyAccepted (list :uuid uuid))
      (copilot-clear-overlay t)
      (if (derived-mode-p 'vterm-mode)
          (progn
            (vterm-delete-region start end)
            (vterm-insert t-completion))
        (delete-region start end)
        (insert t-completion))
      ;; if it is a partial completion
      (when (and (string-prefix-p t-completion completion)
                 (not (string-equal t-completion completion)))
        (copilot--set-overlay-text (copilot--get-overlay) (string-remove-prefix t-completion completion)))
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
        (text uuid ((:docVersion doc-version)) range)
        completion-data
      (when (= doc-version copilot--doc-version)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((p (point))
                   (line (map-nested-elt range '(:start :line)))
                   (start-char (map-nested-elt range '(:start :character)))
                   (end-char (map-nested-elt range '(:end :character)))
                   (goto-line! (lambda ()
                                 (goto-char (point-min))
                                 (forward-line (1- (+ line copilot--line-bias)))))
                   (start (progn
                            (funcall goto-line!)
                            (forward-char start-char)
                            (let* ((cur-line (buffer-substring-no-properties (point) (line-end-position)))
                                   (common-prefix-len (length (copilot--string-common-prefix text cur-line))))
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
  "Notify that the document WINDOW has been focussed or opened."
  ;; When switching windows, this function is called twice, once for the
  ;; window losing focus and once for the window gaining focus. We only want to
  ;; send a notification for the window gaining focus and only if the buffer has
  ;; copilot-mode enabled.
  (when (and copilot-mode (eq window (selected-window)))
    (if (seq-contains-p copilot--opened-buffers (current-buffer))
        (copilot--notify ':textDocument/didFocus
                         (list :textDocument (list :uri (copilot--get-uri))))
      (add-to-list 'copilot--opened-buffers (current-buffer))
      (copilot--notify ':textDocument/didOpen
                       (list :textDocument (list :uri (copilot--get-uri)
                                                 :languageId (copilot--get-language-id)
                                                 :version copilot--doc-version
                                                 :text (copilot--get-source)))))))

(defun copilot--on-doc-change (&optional beg end chars-replaced)
  "Notify that the document has changed.

Arguments BEG, END, and CHARS-REPLACED are metadata for region changed."
  (let* ((is-before-change (null chars-replaced))
         (is-after-change (not is-before-change))
         ;; for a deletion, the post-change beginning and end are at the same place.
         (is-insertion (and is-after-change (not (equal beg end))))
         (is-deletion (and is-before-change (not (equal beg end)))))
    (when (or is-insertion is-deletion)
      (save-restriction
        (save-match-data
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
                                   :contentChanges content-changes))))))))

(defun copilot--on-doc-close (&rest _args)
  "Notify that the document has been closed."
  (when (seq-contains-p copilot--opened-buffers (current-buffer))
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
             (copilot--log 'warning "No completion is available."))))))))

;;
;; minor mode
;;

(defcustom copilot-disable-predicates nil
  "A list of predicate functions with no argument to disable Copilot.
Copilot will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-enable-predicates '(evil-insert-state-p copilot--buffer-changed)
  "A list of predicate functions with no argument to enable Copilot.
Copilot will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-disable-display-predicates nil
  "A list of predicate functions with no argument to disable Copilot.
Copilot will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'copilot
  :package-version '(copilot . "0.1"))

(defcustom copilot-enable-display-predicates nil
  "A list of predicate functions with no argument to enable Copilot.
Copilot will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'copilot
  :package-version '(copilot . "0.1"))

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

(defun copilot--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (string-prefix-p "copilot-" (symbol-name this-command))
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
If so, update the overlays and continue.  COMMAND is the command that triggered
in `post-command-hook'."
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

;;
;; Minor mode definition
;;

(defvar copilot-mode-map (make-sparse-keymap)
  "Keymap for Copilot minor mode.
Use this for custom bindings in `copilot-mode'.")

(easy-menu-define copilot-mode-menu copilot-mode-map "Copilot Menu"
  '("Copilot"
    ["Accept Completion" copilot-accept-completion]
    ["Accept Completion by Word" copilot-accept-completion-by-word]
    ["Accept Completion by Line" copilot-accept-completion-by-line]
    ["Accept Completion by Paragraph" copilot-accept-completion-by-paragraph]
    "--"
    ["Complete" copilot-complete]
    ["Next Completion" copilot-next-completion]
    ["Previous Completion" copilot-previous-completion]
    "--"
    ["Install Server" copilot-install-server]
    ["Uninstall Server" copilot-uninstall-server]
    ["Diagnose" copilot-diagnose]
    "--"
    ["Login" copilot-login]
    ["Logout" copilot-logout]))

(defun copilot--mode-setup ()
  "Set up copilot mode."
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

(defun copilot--mode-teardown ()
  "Tear down copilot mode."
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
      (copilot--mode-setup)
    (copilot--mode-teardown)))

(defun copilot-turn-on-unless-buffer-read-only ()
  "Turn on `copilot-mode' if the buffer is writable."
  (unless buffer-read-only
    (copilot-mode 1)))

;;;###autoload
(define-global-minor-mode global-copilot-mode
  copilot-mode copilot-turn-on-unless-buffer-read-only)

(provide 'copilot)
;;; copilot.el ends here
