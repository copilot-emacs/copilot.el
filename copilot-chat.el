;;; copilot-chat.el --- Chat support for Copilot -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 copilot-emacs maintainers

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/copilot-emacs/copilot.el
;; Keywords: convenience copilot chat

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

;; Chat support for GitHub Copilot using the conversation/* LSP methods.
;; This reuses the existing copilot.el connection and dispatcher infrastructure.

;;; Code:

(require 'copilot)

(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-beg "flymake")
(declare-function flymake-diagnostic-text "flymake")

;;
;; Customization
;;

(defgroup copilot-chat nil
  "Copilot Chat."
  :group 'copilot
  :prefix "copilot-chat-")

(defcustom copilot-chat-model nil
  "The model to use for Copilot Chat.
When nil, a default chat model is resolved from the server (preferring
its designated chat default, then an `auto' model).  Use
`copilot-chat-select-model' to choose one interactively."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Model ID"))
  :group 'copilot-chat
  :package-version '(copilot . "0.5"))

(defcustom copilot-chat-use-agent-mode nil
  "When non-nil, use Agent mode for Copilot Chat conversations.
Agent mode allows Copilot to execute tools such as shell commands
and file edits."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.6"))

(defcustom copilot-chat-auto-approve-tools '("get_errors")
  "Tools that skip confirmation and execute automatically.
Only read-only, local tools should be auto-approved.  Tools that run
shell commands, modify files, or reach the network (e.g.
`fetch_web_page') are intentionally excluded so they always prompt
for confirmation.

Names are matched exactly, and auto-approval bypasses confirmation
completely, including the server's own prompts for sensitive files, so
add an entry only for a tool you fully trust.  The server's built-in
tools are reported with a namespace prefix (e.g. \"copilot.read_file\"
for the read-only file reader); an entry must include that prefix to
match."
  :type '(repeat string)
  :group 'copilot-chat
  :package-version '(copilot . "0.6"))

(defcustom copilot-chat-preview-tool-edits t
  "When non-nil, preview file changes before confirming an edit tool.
For the tools that create or edit files (`create_file',
`insert_edit_into_file', `replace_string_in_file') the proposed change
is shown in a temporary buffer while you are asked to approve it, so
you can see what will be written before answering."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.7"))

(defface copilot-chat-error-face
  '((t :inherit error))
  "Face for error messages in the chat buffer."
  :group 'copilot-chat)

(defface copilot-chat-follow-up-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for follow-up suggestions in the chat buffer."
  :group 'copilot-chat)

(defface copilot-chat-tool-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for tool invocation lines in the chat buffer."
  :group 'copilot-chat)

(defface copilot-chat-diff-added-face
  '((t :inherit diff-added))
  "Face for added lines in a tool edit preview."
  :group 'copilot-chat)

(defface copilot-chat-diff-removed-face
  '((t :inherit diff-removed))
  "Face for removed lines in a tool edit preview."
  :group 'copilot-chat)

;;
;; Buffer-local state (in *copilot-chat* buffer)
;;

(defvar-local copilot-chat--conversation-id nil
  "Current conversation UUID.")

(defvar-local copilot-chat--current-turn-id nil
  "Turn ID currently being streamed.")

(defvar-local copilot-chat--work-done-token nil
  "Token for routing `$/progress' notifications.")

(defvar-local copilot-chat--streaming-p nil
  "Non-nil while a response is being streamed.
This is set when the first progress `begin' notification arrives,
which may be slightly after `copilot-chat--request-id' is set.")

(defvar-local copilot-chat--source-buffer nil
  "The code buffer providing context for this chat.")

(defvar-local copilot-chat--follow-up nil
  "Follow-up suggestion from the last turn.")

(defvar-local copilot-chat--request-id nil
  "ID of the in-flight async request, used for cancellation.")

;;
;; Global state
;;

(defvar copilot-chat--active-buffers nil
  "Alist of (TOKEN . CHAT-BUFFER) for routing `$/progress'.")

;;
;; Buffer name
;;

(defconst copilot-chat--buffer-name "*copilot-chat*"
  "Name of the Copilot Chat buffer.")

;;
;; Model selection
;;

(defvar copilot-chat--resolved-model nil
  "Cached default chat model id resolved from the server, or nil.
Only meaningful once `copilot-chat--model-resolved' is non-nil.")

(defvar copilot-chat--model-resolved nil
  "Non-nil once a default chat model has been resolved this session.
Tracked separately from `copilot-chat--resolved-model' so that a nil
result (no usable default) is cached too and not re-queried on every
message.")

(defun copilot-chat--chat-models ()
  "Return the models the server scopes for the chat panel."
  (seq-filter (lambda (m)
                (seq-contains-p (plist-get m :scopes) "chat-panel"))
              (copilot--request 'copilot/models nil :timeout 5)))

(defun copilot-chat--resolve-default-model ()
  "Query the server for a default chat model id, or nil.
Prefer the model the server marks as the chat default, then an `auto'
model, then the first available chat model."
  (condition-case err
      (let ((models (copilot-chat--chat-models)))
        (or (plist-get (seq-find (lambda (m)
                                   (eq (plist-get m :isChatDefault) t))
                                 models)
                       :id)
            ;; "auto" is the server's catch-all router model; it has no
            ;; dedicated flag, so match it by its stable id.
            (plist-get (seq-find (lambda (m)
                                   (equal (plist-get m :id) "auto"))
                                 models)
                       :id)
            (plist-get (car models) :id)))
    (error
     (copilot--log 'warn "Could not resolve a default chat model: %S" err)
     nil)))

(defun copilot-chat--default-model ()
  "Return a server-resolved default chat model id, or nil.
The server is queried at most once per session, and only when the
connection is already up so chat never blocks on starting it.  The
result, including nil, is cached."
  (when (and (not copilot-chat--model-resolved)
             (copilot--connection-alivep))
    (setq copilot-chat--resolved-model (copilot-chat--resolve-default-model)
          copilot-chat--model-resolved t))
  copilot-chat--resolved-model)

(defun copilot-chat--model ()
  "Return the chat model id to send to the server.
Use `copilot-chat-model' when set, otherwise a server-resolved default."
  (or copilot-chat-model (copilot-chat--default-model)))

;;
;; Internal helpers
;;

(defun copilot-chat--remove-active-tokens (buf)
  "Remove all active-buffer entries for BUF."
  (setq copilot-chat--active-buffers
        (cl-remove-if (lambda (entry) (eq (cdr entry) buf))
                      copilot-chat--active-buffers)))

(defun copilot-chat--end-streaming ()
  "Reset streaming state in the current chat buffer."
  (setq copilot-chat--streaming-p nil)
  (setq copilot-chat--request-id nil)
  (force-mode-line-update))

(defun copilot-chat--handle-request-error (err label)
  "Handle error ERR from a chat request identified by LABEL.
Resets streaming state, displays the error in the chat buffer,
and cleans up active tokens."
  (copilot--log 'error "Chat %s failed: %S" label err)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (copilot-chat--end-streaming)
        (copilot-chat--remove-active-tokens buf))))
  (copilot-chat--insert-error (format "%s" err)))

;;
;; Progress notification handler
;;

(defun copilot-chat--extract-reply (value)
  "Extract the reply text from a progress report VALUE.
The server may provide the reply directly as `:reply' (older format)
or nested inside `:editAgentRounds' (newer format).
Returns a string or nil."
  (let ((reply (or (plist-get value :reply)
                   (when-let* ((rounds (plist-get value :editAgentRounds))
                               ((vectorp rounds))
                               ((> (length rounds) 0))
                               (last-round (aref rounds (1- (length rounds)))))
                     (plist-get last-round :reply)))))
    (and (stringp reply) reply)))

(defun copilot-chat--handle-progress (msg)
  "Handle `$/progress' notification MSG for chat streaming."
  (copilot--dbind (token value) msg
    (when-let* ((entry (assoc token copilot-chat--active-buffers))
                (chat-buf (cdr entry)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (let ((kind (plist-get value :kind)))
            (cond
             ((equal kind "begin")
              (setq copilot-chat--streaming-p t)
              (force-mode-line-update))
             ((equal kind "report")
              (when-let* ((reply (copilot-chat--extract-reply value)))
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert reply))
                (copilot-chat--scroll-to-bottom)))
             ((equal kind "end")
              (copilot-chat--end-streaming)
              (when-let* ((result (plist-get value :result))
                          ((listp result)))
                (setq copilot-chat--follow-up (plist-get result :followUp)))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "\n\n")
                (when (and copilot-chat--follow-up
                           (stringp copilot-chat--follow-up)
                           (not (string-empty-p copilot-chat--follow-up)))
                  (insert (propertize
                           (format "Follow-up: %s\n\n" copilot-chat--follow-up)
                           'face 'copilot-chat-follow-up-face))))
              (copilot-chat--scroll-to-bottom)
              (setq copilot-chat--active-buffers
                    (assoc-delete-all token copilot-chat--active-buffers))))))))))

(copilot-on-notification '$/progress #'copilot-chat--handle-progress)

;;
;; Conversation context request handler
;;

(defun copilot-chat--handle-context (msg)
  "Handle `conversation/context' request MSG.
Return editor context for the requested skill."
  (let ((skill-id (plist-get msg :skillId)))
    (if (equal skill-id "current-editor")
        (copilot-chat--generate-context-doc)
      ;; Unknown skill — return empty context
      nil)))

(copilot-on-request 'conversation/context #'copilot-chat--handle-context)

;;
;; Tool invocation handlers
;;

(defun copilot-chat--insert-tool-status (name detail)
  "Insert a tool status line for tool NAME with DETAIL in the chat buffer."
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "[Tool: %s] %s\n" name detail)
                              'face 'copilot-chat-tool-face))
          (copilot-chat--scroll-to-bottom))))))

(defun copilot-chat--tool-base-name (name)
  "Return tool NAME stripped of any server namespace prefix.
The server's built-in tools may arrive qualified (e.g.
\"copilot.read_file\"), while the client tools we register are
unqualified.  Matching on the base name handles both forms."
  (if (and (stringp name) (string-match "\\.\\([^.]+\\)\\'" name))
      (match-string 1 name)
    name))

(defun copilot-chat--tool-summary (name input)
  "Return a concise, human-readable summary of tool NAME with INPUT.
Return nil for tools we have no tailored summary for, so callers can
fall back to the server-provided confirmation message."
  (pcase (copilot-chat--tool-base-name name)
    ("run_in_terminal"
     (format "run shell command: %s" (plist-get input :command)))
    ("create_file"
     (format "create file: %s" (plist-get input :filePath)))
    ("read_file"
     (format "read file: %s" (plist-get input :filePath)))
    ("insert_edit_into_file"
     (let ((path (plist-get input :filePath))
           (explanation (plist-get input :explanation)))
       (if (and (stringp explanation) (not (string-empty-p explanation)))
           (format "edit file %s: %s" path explanation)
         (format "edit file: %s" path))))
    ("replace_string_in_file"
     (format "edit file: %s" (plist-get input :filePath)))
    ("fetch_web_page"
     (format "fetch: %s"
             (string-join (append (plist-get input :urls) nil) ", ")))
    ("get_errors"
     (format "read diagnostics for: %s"
             (string-join (append (plist-get input :filePaths) nil) ", ")))
    (_ nil)))

(defun copilot-chat--confirmation-prompt (msg)
  "Return the yes-or-no prompt string for confirmation request MSG.
Prefer a tailored summary of the tool, then the server-provided message
or title, and finally the tool name with its raw input so the user can
still tell what they are approving."
  (let* ((name (plist-get msg :name))
         (input (plist-get msg :input))
         (summary (copilot-chat--tool-summary name input))
         (message (plist-get msg :message))
         (title (plist-get msg :title))
         (base (copilot-chat--tool-base-name name)))
    (cond
     (summary
      (format "Copilot wants to %s.  Allow? " summary))
     ((and (stringp message) (not (string-empty-p message)))
      (format "%s  Allow? " message))
     ((and (stringp title) (not (string-empty-p title)))
      (format "%s  Allow? " title))
     ((and (stringp base) (not (string-empty-p base)) input)
      (format "Copilot wants to run %s with input %S.  Allow? " base input))
     ((and (stringp base) (not (string-empty-p base)))
      (format "Copilot wants to run %s.  Allow? " base))
     (input
      (format "Copilot wants to run a tool with input %S.  Allow? " input))
     (t
      "Copilot wants to run a tool.  Allow? "))))

(defun copilot-chat--tool-auto-approved-p (name)
  "Return non-nil when tool NAME is listed in `copilot-chat-auto-approve-tools'.
Matching is exact.  Auto-approval bypasses confirmation entirely
\(including the server's own prompts for sensitive files), so a tool is
auto-approved only when its full reported name is listed, never a
namespace-stripped variant."
  (and (stringp name)
       (member name copilot-chat-auto-approve-tools)
       t))

(defconst copilot-chat--tool-preview-buffer-name "*copilot-chat-tool-preview*"
  "Name of the buffer used to preview a pending tool edit.")

(defun copilot-chat--diff-lines (text prefix face)
  "Prefix each line of TEXT with PREFIX and propertize it with FACE."
  (mapconcat (lambda (line)
               (propertize (concat prefix line) 'face face))
             (split-string (or text "") "\n")
             "\n"))

(defun copilot-chat--tool-preview (name input)
  "Return a textual preview of the change NAME with INPUT will make.
Return nil for tools that do not write files."
  (pcase (copilot-chat--tool-base-name name)
    ("create_file"
     (format "Create %s:\n\n%s"
             (plist-get input :filePath)
             (copilot-chat--diff-lines (plist-get input :content)
                                       "+" 'copilot-chat-diff-added-face)))
    ("insert_edit_into_file"
     ;; The :code here is the model's edit snippet, not the resulting
     ;; file: it uses "...existing code..." markers for the regions it
     ;; leaves untouched.  Frame it as such rather than implying it is
     ;; the literal content that will be written.
     (let ((explanation (plist-get input :explanation)))
       (concat (format "Edit %s" (plist-get input :filePath))
               (when (and (stringp explanation) (not (string-empty-p explanation)))
                 (format "\n%s" explanation))
               "\n\nProposed edit (\"...existing code...\" marks unchanged regions):\n\n"
               (or (plist-get input :code) ""))))
    ("replace_string_in_file"
     (let ((old (plist-get input :oldString))
           (new (plist-get input :newString)))
       (concat (format "Edit %s\n\n" (plist-get input :filePath))
               ;; Omit an empty side so a pure insertion or deletion does
               ;; not render a misleading bare "-"/"+" line.
               (when (and (stringp old) (not (string-empty-p old)))
                 (concat (copilot-chat--diff-lines
                          old "-" 'copilot-chat-diff-removed-face)
                         "\n"))
               (when (and (stringp new) (not (string-empty-p new)))
                 (copilot-chat--diff-lines
                  new "+" 'copilot-chat-diff-added-face)))))
    (_ nil)))

(defun copilot-chat--confirm-with-preview (msg preview)
  "Show PREVIEW in a temporary window, then prompt to confirm MSG.
Return non-nil when the user approves.  The preview buffer is removed
afterwards."
  ;; A fresh buffer per call, so an overlapping confirmation can't erase
  ;; or kill the preview out from under this prompt.
  (let ((buf (generate-new-buffer copilot-chat--tool-preview-buffer-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert preview)
            (goto-char (point-min))
            (setq buffer-read-only t))
          (display-buffer buf '((display-buffer-pop-up-window
                                 display-buffer-use-some-window)
                                (window-height . fit-window-to-buffer)))
          (yes-or-no-p (copilot-chat--confirmation-prompt msg)))
      (when-let* ((win (get-buffer-window buf)))
        (quit-window nil win))
      (kill-buffer buf))))

(defun copilot-chat--confirm-tool (msg)
  "Ask the user whether to allow the tool described by MSG.
Show a preview of the change first when `copilot-chat-preview-tool-edits'
is enabled and the tool writes files.  Return non-nil on approval."
  (let ((preview (and copilot-chat-preview-tool-edits
                      (copilot-chat--tool-preview (plist-get msg :name)
                                                  (plist-get msg :input)))))
    (if preview
        (copilot-chat--confirm-with-preview msg preview)
      (yes-or-no-p (copilot-chat--confirmation-prompt msg)))))

(defun copilot-chat--handle-tool-confirmation (msg)
  "Handle `conversation/invokeClientToolConfirmation' request MSG.
Return a result plist the server accepts: (:result \"accept\") to allow
the tool call or (:result \"dismiss\") to decline it."
  (if (or (copilot-chat--tool-auto-approved-p (plist-get msg :name))
          (copilot-chat--confirm-tool msg))
      (list :result "accept")
    (list :result "dismiss")))

(copilot-on-request 'conversation/invokeClientToolConfirmation
                    #'copilot-chat--handle-tool-confirmation)

(defun copilot-chat--tool-result (status value)
  "Build a LanguageModelToolResult with STATUS and text VALUE."
  (list :status status
        :content (vector (list :value value))))

(defun copilot-chat--execute-run-in-terminal (input)
  "Execute run_in_terminal tool with INPUT."
  (let ((command (plist-get input :command))
        ;; Run in the workspace root rather than whatever buffer was
        ;; current when the request arrived, so relative paths and build
        ;; commands behave the way the user expects.
        (default-directory (or (copilot--workspace-root) default-directory)))
    (copilot-chat--insert-tool-status "run_in_terminal" (format "Running: %s" command))
    (condition-case err
        (let ((output (shell-command-to-string command)))
          (copilot-chat--insert-tool-status "run_in_terminal" "Done.")
          (copilot-chat--tool-result "success" output))
      (error
       (copilot-chat--tool-result "error" (error-message-string err))))))

(defun copilot-chat--execute-create-file (input)
  "Execute create_file tool with INPUT."
  (let ((file-path (plist-get input :filePath))
        (content (plist-get input :content)))
    (copilot-chat--insert-tool-status "create_file"
                                      (format "Creating: %s" file-path))
    (condition-case err
        (progn
          (let ((dir (file-name-directory file-path)))
            (when dir (make-directory dir t)))
          (with-temp-file file-path
            (insert content))
          (copilot-chat--tool-result "success"
                                    (format "File %s created." file-path)))
      (error
       (copilot-chat--tool-result "error" (error-message-string err))))))

(defun copilot-chat--execute-get-errors (input)
  "Execute get_errors tool with INPUT."
  (let ((file-paths (plist-get input :filePaths))
        (results '()))
    (copilot-chat--insert-tool-status "get_errors" "Collecting diagnostics...")
    (dolist (path (append file-paths nil))
      (let ((buf (find-buffer-visiting path)))
        (if (and buf (buffer-live-p buf))
            (with-current-buffer buf
              (if (bound-and-true-p flymake-mode)
                  (let ((diags (flymake-diagnostics)))
                    (if diags
                        (dolist (diag diags)
                          (push (format "%s:%d: %s"
                                        path
                                        (line-number-at-pos
                                         (flymake-diagnostic-beg diag))
                                        (flymake-diagnostic-text diag))
                                results))
                      (push (format "%s: no errors" path) results)))
                (push (format "%s: no diagnostics available" path) results)))
          (push (format "%s: not open in editor" path) results))))
    (copilot-chat--tool-result "success" (string-join (nreverse results) "\n"))))

(defun copilot-chat--execute-fetch-web-page (input)
  "Execute fetch_web_page tool with INPUT."
  (let ((urls (plist-get input :urls))
        (results '()))
    (copilot-chat--insert-tool-status "fetch_web_page" "Fetching...")
    (dolist (url (append urls nil))
      (condition-case err
          (let ((buf (url-retrieve-synchronously url t nil 30)))
            (if buf
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (re-search-forward "\n\n" nil t)
                    (delete-region (point-min) (point)))
                  (push (format "=== %s ===\n%s" url (buffer-string)) results)
                  (kill-buffer))
              (push (format "=== %s ===\nFailed to fetch" url) results)))
        (error
         (push (format "=== %s ===\nError: %s" url (error-message-string err))
               results))))
    (copilot-chat--tool-result "success" (string-join (nreverse results) "\n\n"))))

(defun copilot-chat--handle-tool-invocation (msg)
  "Handle `conversation/invokeClientTool' request MSG.
Dispatch to the appropriate tool and return a LanguageModelToolResult."
  (let ((name (plist-get msg :name))
        (input (plist-get msg :input)))
    (pcase name
      ("run_in_terminal" (copilot-chat--execute-run-in-terminal input))
      ("create_file" (copilot-chat--execute-create-file input))
      ("get_errors" (copilot-chat--execute-get-errors input))
      ("fetch_web_page" (copilot-chat--execute-fetch-web-page input))
      (_ (copilot-chat--tool-result "error"
                                    (format "Unknown tool: %s" name))))))

(copilot-on-request 'conversation/invokeClientTool
                    #'copilot-chat--handle-tool-invocation)

;;
;; Context doc generation
;;

(defun copilot-chat--generate-context-doc ()
  "Generate context document from the source buffer."
  (let ((buf copilot-chat--source-buffer))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((doc (copilot--generate-doc)))
          (plist-put doc :source (copilot--get-source))
          doc)))))

(defun copilot-chat--insert-error (error-msg)
  "Insert ERROR-MSG into the chat buffer with error styling."
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n[Error: %s]\n\n" error-msg)
                            'face 'copilot-chat-error-face))
        (copilot-chat--scroll-to-bottom)))))

;;
;; Tool registration
;;

(defun copilot-chat--tool-definitions ()
  "Return the list of client tool definitions for agent mode."
  (vector
   (list :name "run_in_terminal"
         :description "Run a shell command in the terminal."
         :inputSchema (list :type "object"
                            :properties (list :command (list :type "string")
                                              :explanation (list :type "string"))
                            :required ["command"]))
   (list :name "create_file"
         :description "Create a new file with the given content."
         :inputSchema (list :type "object"
                            :properties (list :filePath (list :type "string")
                                              :content (list :type "string"))
                            :required ["filePath" "content"]))
   (list :name "get_errors"
         :description "Get diagnostics/errors for the given files."
         :inputSchema (list :type "object"
                            :properties (list :filePaths (list :type "array"
                                                               :items (list :type "string")))
                            :required ["filePaths"]))
   (list :name "fetch_web_page"
         :description "Fetch the content of web pages."
         :inputSchema (list :type "object"
                            :properties (list :urls (list :type "array"
                                                          :items (list :type "string")))
                            :required ["urls"]))))

(defun copilot-chat--register-tools ()
  "Register client tools with the server for agent mode."
  (copilot--async-request
   'conversation/registerTools
   (list :tools (copilot-chat--tool-definitions))
   :success-fn (lambda (_result)
                 (copilot--log 'info "Agent tools registered"))
   :error-fn (lambda (err)
               (copilot--log 'error "Tool registration failed: %S" err))))

;;
;; Protocol methods
;;

(defun copilot-chat--create (message callback)
  "Create a new conversation with MESSAGE.
CALLBACK is called with the response containing conversationId and turnId."
  (let ((token (format "copilot-chat-%s" (float-time))))
    (with-current-buffer (get-buffer copilot-chat--buffer-name)
      (setq copilot-chat--work-done-token token)
      (push (cons token (current-buffer)) copilot-chat--active-buffers))
    (let ((req-id
           (copilot--async-request
            'conversation/create
            (append
             (list :workDoneToken token
                   :turns (vector
                           (list :request message
                                 :response ""
                                 :turnId ""))
                   :capabilities (list :skills (vector "current-editor")
                                       :allSkills t)
                   :source "panel")
             (when-let* ((model (copilot-chat--model)))
               (list :model model))
             (list :workspaceFolders
                   (vconcat
                    (when-let* ((root (copilot--workspace-root)))
                      (list (list :uri (concat "file://" root)
                                  :name (file-name-nondirectory
                                         (directory-file-name root)))))))
             (when copilot-chat-use-agent-mode
               (list :chatMode "Agent"
                     :needToolCallConfirmation t)))
            :success-fn (lambda (result)
                          (when copilot-chat-use-agent-mode
                            (copilot-chat--register-tools))
                          (funcall callback result))
            :error-fn (lambda (err)
                        (copilot-chat--handle-request-error err "create")))))
      (with-current-buffer (get-buffer copilot-chat--buffer-name)
        (setq copilot-chat--request-id req-id)))))

(defun copilot-chat--send-turn (message)
  "Send a follow-up MESSAGE in the current conversation."
  (let ((token (format "copilot-chat-%s" (float-time)))
        (chat-buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (setq copilot-chat--work-done-token token)
      (push (cons token chat-buf) copilot-chat--active-buffers)
      (let ((conv-id copilot-chat--conversation-id)
            (doc (copilot-chat--generate-context-doc)))
        (setq copilot-chat--request-id
              (copilot--async-request
               'conversation/turn
               (append
                (list :workDoneToken token
                      :conversationId conv-id
                      :message message
                      :source "panel")
                (when doc (list :doc doc)))
               :success-fn (lambda (result)
                             (when (buffer-live-p chat-buf)
                               (with-current-buffer chat-buf
                                 (setq copilot-chat--current-turn-id
                                       (plist-get result :turnId)))))
               :error-fn (lambda (err)
                           (copilot-chat--handle-request-error err "turn"))))))))

(defun copilot-chat--destroy ()
  "Destroy the current conversation."
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (when (and chat-buf (buffer-live-p chat-buf))
      (with-current-buffer chat-buf
        (when copilot-chat--conversation-id
          (when (copilot--connection-alivep)
            (copilot--async-request
             'conversation/destroy
             (list :conversationId copilot-chat--conversation-id)))
          (setq copilot-chat--conversation-id nil)
          (setq copilot-chat--current-turn-id nil)
          (setq copilot-chat--follow-up nil)
          (copilot-chat--end-streaming)
          (copilot-chat--remove-active-tokens chat-buf))))))

;;
;; UI helpers
;;

(defun copilot-chat--scroll-to-bottom ()
  "Scroll chat window to show the latest output."
  (when-let* ((win (get-buffer-window copilot-chat--buffer-name)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

(defun copilot-chat--insert-prompt (message)
  "Insert a user prompt with MESSAGE into the chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "You:" 'face 'bold) "\n"
            message "\n\n"
            (propertize "Copilot:" 'face 'bold) "\n")))

;;
;; Major mode
;;

(defvar copilot-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'copilot-chat-send)
    (define-key map (kbd "C-c C-c") #'copilot-chat-send)
    (define-key map (kbd "C-c C-k") #'copilot-chat-stop)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `copilot-chat-mode'.")

(defconst copilot-chat--font-lock-keywords
  `(("^\\(You:\\)" 1 'bold)
    ("^\\(Copilot:\\)" 1 'bold))
  "Extra font-lock keywords added on top of the parent mode's highlighting.")

(defun copilot-chat--mode-line ()
  "Return the mode-line lighter for `copilot-chat-mode'."
  (if copilot-chat--streaming-p
      " Copilot-Chat[Streaming]"
    " Copilot-Chat"))

(declare-function gfm-mode "ext:markdown-mode" ())

(defun copilot-chat--setup-mode ()
  "Set up font-lock, mode-line, and visual-line for `copilot-chat-mode'.
When `markdown-mode' is available, enable GFM font-lock for full
markdown rendering; otherwise use basic highlighting."
  (when (require 'markdown-mode nil t)
    (setq-local markdown-fontify-code-blocks-natively t)
    (setq-local font-lock-defaults
                (with-temp-buffer
                  (gfm-mode)
                  font-lock-defaults))
    (font-lock-flush))
  (font-lock-add-keywords nil copilot-chat--font-lock-keywords t)
  (setq mode-name '(:eval (copilot-chat--mode-line)))
  (visual-line-mode 1)
  (setq word-wrap t))

(define-derived-mode copilot-chat-mode special-mode "Copilot-Chat"
  "Major mode for Copilot Chat.
When `markdown-mode' is installed, the buffer gets full GFM
rendering; otherwise basic font-lock is used.

\\{copilot-chat-mode-map}"
  (copilot-chat--setup-mode))

;;
;; Interactive commands
;;

;;;###autoload
(defun copilot-chat (message)
  "Open Copilot Chat and send MESSAGE.
If a conversation already exists, send as a follow-up turn.
Otherwise, create a new conversation."
  (interactive
   (list (read-string
          (if-let* ((buf (get-buffer copilot-chat--buffer-name))
                    ((buffer-local-value 'copilot-chat--conversation-id buf)))
              "Copilot Chat (follow-up): "
            "Copilot Chat: "))))
  (when (string-empty-p message)
    (user-error "Empty message"))
  ;; Only attach the originating buffer as context when it is a real
  ;; file-visiting buffer.  `copilot-chat' can be invoked from anywhere
  ;; (dired, *scratch*, the chat buffer itself, ...), and sending an
  ;; unrelated buffer as context is just noise.  See issue #470.
  (let ((source-buf (when (buffer-file-name) (current-buffer)))
        (chat-buf (get-buffer-create copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-chat-mode)
        (copilot-chat-mode))
      (setq copilot-chat--source-buffer source-buf))
    (display-buffer chat-buf)
    (with-current-buffer chat-buf
      (copilot-chat--insert-prompt message)
      (if copilot-chat--conversation-id
          (copilot-chat--send-turn message)
        (copilot-chat--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-chat--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-chat--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-chat-send (message)
  "Send MESSAGE in the current Copilot Chat.
When called interactively, prompt for the message."
  (interactive (list (read-string "Copilot Chat: ")))
  (when (string-empty-p message)
    (user-error "Empty message"))
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (unless chat-buf
      (user-error "No active chat buffer"))
    (with-current-buffer chat-buf
      (when copilot-chat--streaming-p
        (user-error "A response is currently being streamed"))
      (copilot-chat--insert-prompt message)
      (if copilot-chat--conversation-id
          (copilot-chat--send-turn message)
        (copilot-chat--create
         message
         (lambda (result)
           (when (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (setq copilot-chat--conversation-id
                     (plist-get result :conversationId))
               (setq copilot-chat--current-turn-id
                     (plist-get result :turnId))))))))))

;;;###autoload
(defun copilot-chat-send-region (start end &optional prompt)
  "Send the region between START and END as context with an optional PROMPT."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Prompt (optional): "))
     (user-error "No active region")))
  (let* ((code (buffer-substring-no-properties start end))
         (lang (copilot--get-language-id))
         (message (if (and prompt (not (string-empty-p prompt)))
                      (format "%s\n\n```%s\n%s\n```" prompt lang code)
                    (format "```%s\n%s\n```" lang code))))
    (copilot-chat message)))

;;;###autoload
(defun copilot-chat-stop ()
  "Cancel the in-flight request and stop streaming.
If not currently streaming, reset the conversation instead."
  (interactive)
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (if (and chat-buf
             (buffer-local-value 'copilot-chat--streaming-p chat-buf))
        (with-current-buffer chat-buf
          (when (and copilot-chat--request-id (copilot--connection-alivep))
            (jsonrpc-notify copilot--connection
                            '$/cancelRequest
                            (list :id copilot-chat--request-id)))
          (copilot-chat--end-streaming)
          (copilot-chat--insert-error "Cancelled")
          (copilot-chat--remove-active-tokens chat-buf))
      (copilot-chat-reset))))

;;;###autoload
(defun copilot-chat-reset ()
  "Destroy the current conversation and clear the chat buffer."
  (interactive)
  (copilot-chat--destroy)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun copilot-chat-select-model ()
  "Interactively select a Copilot Chat model."
  (interactive)
  (let* ((choices (mapcar (lambda (m)
                            (cons (format "%s (%s)" (plist-get m :modelName) (plist-get m :id))
                                  (plist-get m :id)))
                          (copilot-chat--chat-models)))
         (choice (completing-read "Chat model: " choices nil t))
         (model-id (cdr (assoc choice choices))))
    (setq copilot-chat-model model-id
          ;; Forget any cached default so clearing the model later
          ;; re-resolves from the server.
          copilot-chat--model-resolved nil)
    (message "Chat model set to %s" model-id)))

(provide 'copilot-chat)
;;; copilot-chat.el ends here
