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
(declare-function flycheck-current-errors "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-message "flycheck")

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

(defcustom copilot-chat-terminal-timeout 30
  "Seconds to allow an agent-mode `run_in_terminal' command to run.
When the limit is reached the command is killed and reported as timed
out.  Set to nil to allow commands to run without a timeout.

The command runs asynchronously: Emacs stays responsive while it works,
the language-server connection keeps flowing, and you can abort a
running command with \\[keyboard-quit]."
  :type '(choice (const :tag "No timeout" nil)
                 (natnum :tag "Seconds"))
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

(defvar-local copilot-chat--references nil
  "References to attach to the next chat turn.
A list of plists like (:type \"file\" :uri URI), sent as the turn's
`references' and cleared once a message is sent.")

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

(defun copilot-chat--error-text (err)
  "Return a human-readable string for a server ERR value, or nil.
ERR may be a string, a structured object with a `:message', or absent."
  (cond ((null err) nil)
        ((stringp err) err)
        ((and (listp err) (stringp (plist-get err :message)))
         (plist-get err :message))
        (t (format "%S" err))))

(defun copilot-chat--format-error (msg)
  "Return MSG rendered as a styled chat error line."
  (propertize (format "[Error: %s]\n\n" msg) 'face 'copilot-chat-error-face))

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
              (let* ((result (plist-get value :result))
                     ;; The server normally sends an object here, but
                     ;; guard against any other shape.
                     (result (and (listp result) result))
                     (error-msg (copilot-chat--error-text
                                 (plist-get result :error))))
                ;; Reset the follow-up every turn so a stale one from a
                ;; previous turn is never re-inserted.
                (setq copilot-chat--follow-up (plist-get result :followUp))
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert "\n\n")
                  ;; Surface a turn-level error the server reports at the
                  ;; end, which would otherwise leave only an empty reply.
                  (when error-msg
                    (insert (copilot-chat--format-error error-msg)))
                  (when (and (stringp copilot-chat--follow-up)
                             (not (string-empty-p copilot-chat--follow-up)))
                    (insert (propertize
                             (format "Follow-up: %s\n\n" copilot-chat--follow-up)
                             'face 'copilot-chat-follow-up-face)))))
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

(defvar copilot-chat--session-approved-tools nil
  "Tool base names approved for the rest of the conversation.
Populated by choosing `always' at a confirmation prompt, and reset when
a new conversation starts.  Unlike `copilot-chat-auto-approve-tools',
this is a live, per-session choice, so it matches on the base name.

This is a global, not a buffer-local, variable: the confirmation request
is handled in the JSON-RPC dispatcher rather than in the chat buffer, so
buffer-local state would not be reachable from there.")

(defun copilot-chat--tool-auto-approved-p (name)
  "Return non-nil when tool NAME should skip the confirmation prompt.
A tool is auto-approved when its full reported name is in
`copilot-chat-auto-approve-tools' (matched exactly, since auto-approval
bypasses confirmation entirely, including the server's own prompts for
sensitive files), or when its base name was approved for this session
via the `always' choice."
  (and (stringp name)
       (or (member name copilot-chat-auto-approve-tools)
           (member (copilot-chat--tool-base-name name)
                   copilot-chat--session-approved-tools))
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

(defun copilot-chat--ask-tool (msg)
  "Prompt to approve the tool described by MSG.
Return `allow' (run it once), `always' (run it and skip future prompts
for this tool this session), or `deny'."
  (pcase (car (read-multiple-choice
               (copilot-chat--confirmation-prompt msg)
               '((?y "yes" "Allow this tool call")
                 (?n "no" "Decline this tool call")
                 (?a "always"
                     "Allow this and future calls to this tool this session"))))
    (?y 'allow)
    (?a 'always)
    (_ 'deny)))

(defun copilot-chat--confirm-with-preview (msg preview)
  "Show PREVIEW in a temporary window, then prompt to confirm MSG.
Return the approval decision (see `copilot-chat--ask-tool').  The preview
buffer is removed afterwards."
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
          (copilot-chat--ask-tool msg))
      (when-let* ((win (get-buffer-window buf)))
        (quit-window nil win))
      (kill-buffer buf))))

(defun copilot-chat--confirm-tool (msg)
  "Ask the user whether to allow the tool described by MSG.
Show a preview of the change first when `copilot-chat-preview-tool-edits'
is enabled and the tool writes files.  Return the approval decision (see
`copilot-chat--ask-tool')."
  (let ((preview (and copilot-chat-preview-tool-edits
                      (copilot-chat--tool-preview (plist-get msg :name)
                                                  (plist-get msg :input)))))
    (if preview
        (copilot-chat--confirm-with-preview msg preview)
      (copilot-chat--ask-tool msg))))

(defun copilot-chat--client-tool-names ()
  "Return the names of copilot.el's own client tools.
These are the tools copilot.el registers and runs itself; they log their
own progress while running, so they need no separate status line at
confirmation time."
  (mapcar (lambda (def) (plist-get def :name))
          (copilot-chat--tool-definitions)))

(defun copilot-chat--log-server-tool (msg)
  "Insert a status line for a server-executed tool approved via MSG.
The server runs its own built-in and MCP tools internally, so for the
ones it asks us to confirm this is the only trace they leave in the chat
buffer.  Tools copilot.el executes itself are skipped, since they log
their own progress."
  (let* ((name (plist-get msg :name))
         (base (copilot-chat--tool-base-name name)))
    (unless (member base (copilot-chat--client-tool-names))
      (copilot-chat--insert-tool-status
       base
       (or (copilot-chat--tool-summary name (plist-get msg :input))
           "running")))))

(defun copilot-chat--handle-tool-confirmation (msg)
  "Handle `conversation/invokeClientToolConfirmation' request MSG.
Return a result plist the server accepts: (:result \"accept\") to allow
the tool call or (:result \"dismiss\") to decline it.  Choosing `always'
at the prompt remembers the tool for the rest of the conversation."
  (let ((name (plist-get msg :name)))
    (if (copilot-chat--tool-auto-approved-p name)
        (copilot-chat--accept-tool msg)
      (pcase (copilot-chat--confirm-tool msg)
        ('deny (list :result "dismiss"))
        (decision
         (when (eq decision 'always)
           (cl-pushnew (copilot-chat--tool-base-name name)
                       copilot-chat--session-approved-tools
                       :test #'equal))
         (copilot-chat--accept-tool msg))))))

(defun copilot-chat--accept-tool (msg)
  "Return an acceptance result for MSG, logging the tool activity."
  ;; Logging must never derail the acceptance reply.
  (ignore-errors (copilot-chat--log-server-tool msg))
  (list :result "accept"))

(copilot-on-request 'conversation/invokeClientToolConfirmation
                    #'copilot-chat--handle-tool-confirmation)

(defun copilot-chat--tool-result (status value)
  "Build a LanguageModelToolResult with STATUS and text VALUE."
  (list :status status
        :content (vector (list :value value))))

(defconst copilot-chat--terminal-max-output 100000
  "Maximum number of characters of `run_in_terminal' output to return.
Longer output is truncated (keeping the tail) before being handed to
the model, to bound memory and payload size.")

(defun copilot-chat--truncate-output (output)
  "Truncate OUTPUT to `copilot-chat--terminal-max-output', keeping the tail."
  (if (> (length output) copilot-chat--terminal-max-output)
      (concat "[output truncated]\n"
              (substring output (- (length output)
                                   copilot-chat--terminal-max-output)))
    output))

(defun copilot-chat--drain-process (proc)
  "Kill PROC and wait for it to die.
`inhibit-quit' keeps a stray \\[keyboard-quit] from escaping this
cleanup and leaving the tool request unanswered."
  (let ((inhibit-quit t))
    (when (process-live-p proc) (kill-process proc))
    (while (process-live-p proc)
      (accept-process-output proc 0.1))))

(defun copilot-chat--run-process (command)
  "Run shell COMMAND asynchronously, keeping Emacs responsive.
Pump process and connection events with `accept-process-output' instead
of blocking like `shell-command-to-string', so redisplay and the
language-server connection keep working.  Honor
`copilot-chat-terminal-timeout'.

\\[keyboard-quit] aborts the running command (reported as `cancelled');
it does not abort the whole agent turn, for which `copilot-chat-stop' is
available.

Return a plist with keys `:output' (combined stdout and stderr,
truncated to `copilot-chat--terminal-max-output'), `:status' (one of
`success', `error', `timeout', `cancelled'), and `:exit-code' (the
process exit code for `success'/`error', nil otherwise)."
  (let ((buffer (generate-new-buffer " *copilot-chat-terminal*")))
    (unwind-protect
        (let* ((proc (make-process
                      :name "copilot-chat-terminal"
                      :buffer buffer
                      :command (list shell-file-name shell-command-switch command)
                      :connection-type 'pipe
                      :noquery t))
               (deadline (and copilot-chat-terminal-timeout
                              (+ (float-time) copilot-chat-terminal-timeout)))
               (status 'success))
          (condition-case nil
              (while (process-live-p proc)
                (if (and deadline (> (float-time) deadline))
                    (progn
                      (setq status 'timeout)
                      (copilot-chat--drain-process proc))
                  (accept-process-output proc 0.1)))
            (quit
             (setq status 'cancelled)
             (copilot-chat--drain-process proc)))
          ;; The process has exited; flush any output still buffered in
          ;; the pipe so a fast command's last lines aren't lost.
          (while (accept-process-output proc 0.05))
          (let ((code (and (eq status 'success) (process-exit-status proc))))
            (when (and code (not (zerop code)))
              (setq status 'error))
            (list :output (copilot-chat--truncate-output
                           (with-current-buffer buffer (buffer-string)))
                  :status status
                  :exit-code code)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun copilot-chat--execute-run-in-terminal (input)
  "Execute run_in_terminal tool with INPUT."
  (let ((command (plist-get input :command))
        ;; Run in the workspace root rather than whatever buffer was
        ;; current when the request arrived, so relative paths and build
        ;; commands behave the way the user expects.
        (default-directory (or (copilot--workspace-root) default-directory)))
    (copilot-chat--insert-tool-status "run_in_terminal" (format "Running: %s" command))
    (condition-case err
        (let* ((result (copilot-chat--run-process command))
               (output (plist-get result :output))
               (code (plist-get result :exit-code)))
          (pcase (plist-get result :status)
            ('success
             (copilot-chat--insert-tool-status "run_in_terminal" "Done.")
             (copilot-chat--tool-result "success" output))
            ('error
             (copilot-chat--insert-tool-status
              "run_in_terminal" (format "Exited with status %s." code))
             ;; Still a successful tool call: hand the model the output and
             ;; the exit code so it can react to the failure.
             (copilot-chat--tool-result
              "success"
              (format "%s\n[command exited with status %s]" output code)))
            ('timeout
             (copilot-chat--insert-tool-status "run_in_terminal" "Timed out.")
             (copilot-chat--tool-result
              "error"
              (format "%s\n[timed out after %s seconds]"
                      output copilot-chat-terminal-timeout)))
            ('cancelled
             (copilot-chat--insert-tool-status "run_in_terminal" "Cancelled.")
             (copilot-chat--tool-result "cancelled" "Command cancelled."))))
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

(defun copilot-chat--buffer-diagnostics (path)
  "Return diagnostic strings for the current buffer, labelled with PATH.
Collect from Flymake and Flycheck whenever they are active (both, if
both are on), and report when no backend is available at all."
  (let ((backends 0)
        (diagnostics '()))
    (when (bound-and-true-p flymake-mode)
      (setq backends (1+ backends))
      (dolist (diag (flymake-diagnostics))
        (push (format "%s:%d: %s" path
                      (line-number-at-pos (flymake-diagnostic-beg diag))
                      (flymake-diagnostic-text diag))
              diagnostics)))
    (when (bound-and-true-p flycheck-mode)
      (setq backends (1+ backends))
      (dolist (err (flycheck-current-errors))
        (push (format "%s:%d: %s" path
                      (or (flycheck-error-line err) 0)
                      (flycheck-error-message err))
              diagnostics)))
    (cond
     ((zerop backends) (list (format "%s: no diagnostics available" path)))
     ((null diagnostics) (list (format "%s: no errors" path)))
     (t (nreverse diagnostics)))))

(defun copilot-chat--execute-get-errors (input)
  "Execute get_errors tool with INPUT."
  (copilot-chat--insert-tool-status "get_errors" "Collecting diagnostics...")
  (let ((results
         (mapcan
          (lambda (path)
            (let ((buf (find-buffer-visiting path)))
              (if (and buf (buffer-live-p buf))
                  (with-current-buffer buf
                    (copilot-chat--buffer-diagnostics path))
                (list (format "%s: not open in editor" path)))))
          (append (plist-get input :filePaths) nil))))
    (copilot-chat--tool-result "success" (string-join results "\n"))))

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
;; MCP server status
;;

(defconst copilot-chat--mcp-buffer-name "*copilot-mcp-tools*"
  "Name of the buffer listing MCP servers and their tools.")

(defvar copilot-chat--mcp-servers nil
  "Latest MCP server snapshots reported by the language server.
A list of plists with keys such as :name, :status, :tools, and :error,
refreshed from `copilot/mcpTools' notifications.")

(defvar copilot-chat--mcp-warned-errors nil
  "Alist of MCP server name to the last error string warned about.
Kept across notifications so a server that flaps in and out of the
reported set is not warned about repeatedly for the same failure.")

(defun copilot-chat--handle-mcp-tools (msg)
  "Handle a `copilot/mcpTools' notification MSG.
Cache the server snapshots and warn once about any newly failed server,
which the language server otherwise reports only silently."
  (setq copilot-chat--mcp-servers (append (plist-get msg :servers) nil))
  (dolist (server copilot-chat--mcp-servers)
    (let ((name (plist-get server :name))
          (err (plist-get server :error)))
      (when (and (stringp err) (not (string-empty-p err))
                 (not (equal err (alist-get name copilot-chat--mcp-warned-errors
                                            nil nil #'equal))))
        (copilot--log 'warning "MCP server %s failed: %s" name err)
        (setf (alist-get name copilot-chat--mcp-warned-errors nil nil #'equal)
              err)))))

(copilot-on-notification 'copilot/mcpTools #'copilot-chat--handle-mcp-tools)

(defun copilot-chat--insert-mcp-server (server)
  "Insert a description of MCP SERVER into the current buffer."
  (insert (propertize (or (plist-get server :name) "(unnamed)") 'face 'bold)
          (format " [%s]\n" (or (plist-get server :status) "unknown")))
  (let ((err (plist-get server :error)))
    (when (and (stringp err) (not (string-empty-p err)))
      (insert (propertize (format "  error: %s\n" err)
                          'face 'copilot-chat-error-face))))
  (let ((tools (append (plist-get server :tools) nil)))
    (if tools
        (dolist (tool tools)
          (insert (format "  - %s" (or (plist-get tool :name) "(unnamed)")))
          (let ((desc (plist-get tool :description)))
            (when (stringp desc)
              (insert (format ": %s" (car (split-string desc "\n"))))))
          (insert "\n"))
      (insert "  (no tools)\n")))
  (insert "\n"))

(defun copilot-chat-list-mcp-tools ()
  "Display the MCP servers and tools currently available to Copilot.
Servers are configured via `copilot-mcp-servers'."
  (interactive)
  (let ((buf (get-buffer-create copilot-chat--mcp-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cond
         (copilot-chat--mcp-servers
          (mapc #'copilot-chat--insert-mcp-server copilot-chat--mcp-servers))
         (copilot-mcp-servers
          (insert "No MCP servers reported yet.\n\n"
                  "They appear once the language server connects them in "
                  "agent mode.\n"))
         (t
          (insert "No MCP servers configured.\n\n"
                  "Set `copilot-mcp-servers' and enable "
                  "`copilot-chat-use-agent-mode'.\n")))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

;;
;; Context doc generation
;;

(defun copilot-chat--generate-context-doc ()
  "Generate context document from the source buffer."
  (let ((buf copilot-chat--source-buffer))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        ;; The indentation-offset warning is meant for completion
        ;; debugging; suppress it here so merely chatting from a buffer
        ;; whose mode has no configured offset doesn't nag the user.
        (let* ((copilot-indent-offset-warning-disable t)
               (doc (copilot--generate-doc)))
          (plist-put doc :source (copilot--get-source))
          doc)))))

(defun copilot-chat--insert-error (error-msg)
  "Insert ERROR-MSG into the chat buffer with error styling."
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (copilot-chat--format-error error-msg))
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
    ;; A fresh conversation starts with a clean slate of session approvals.
    (setq copilot-chat--session-approved-tools nil)
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
                     :needToolCallConfirmation t))
             (copilot-chat--references-param))
            :success-fn (lambda (result)
                          (when-let* ((buf (get-buffer
                                            copilot-chat--buffer-name)))
                            (with-current-buffer buf
                              (copilot-chat--consume-references)))
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
                (when doc (list :doc doc))
                (copilot-chat--references-param))
               :success-fn (lambda (result)
                             (when (buffer-live-p chat-buf)
                               (with-current-buffer chat-buf
                                 (copilot-chat--consume-references)
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
;; Code blocks
;;

(defconst copilot-chat--code-block-re
  "^[ \t]*```[ \t]*\\([^\n]*\\)\n\\(\\(?:.*\n\\)*?\\)[ \t]*```[ \t]*$"
  "Regexp matching a fenced code block.
Group 1 is the info string (language); group 2 is the body.")

(defun copilot-chat--code-blocks ()
  "Return the fenced code blocks in the current buffer.
Each element is a plist with :lang, :code, :start, and :end.

Note: a block whose body itself contains an indented ``` line ends at
that inner fence, since the parser does not track fence nesting."
  (let ((blocks '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward copilot-chat--code-block-re nil t)
        ;; Read every match-data-derived field before any call (such as
        ;; `string-trim') that could clobber the match data.
        (let ((lang (match-string-no-properties 1))
              (code (match-string-no-properties 2))
              (start (match-beginning 0))
              (end (match-end 0)))
          (push (list :lang (string-trim lang) :code code
                      :start start :end end)
                blocks))))
    (nreverse blocks)))

(defun copilot-chat--read-code-block (blocks)
  "Return a code block from BLOCKS: the one at point, or one chosen.
Signal a `user-error' when BLOCKS is empty."
  (or (seq-find (lambda (b)
                  (and (>= (point) (plist-get b :start))
                       (<= (point) (plist-get b :end))))
                blocks)
      (cond
       ((null blocks) (user-error "No code blocks in this chat"))
       ((= (length blocks) 1) (car blocks))
       (t
        (let* ((choices
                (seq-map-indexed
                 (lambda (b i)
                   (cons (format "%d: [%s] %s" (1+ i)
                                 (if (string-empty-p (plist-get b :lang))
                                     "?" (plist-get b :lang))
                                 (car (split-string (plist-get b :code) "\n")))
                         b))
                 blocks))
               (choice (completing-read "Code block: " choices nil t)))
          (cdr (assoc choice choices)))))))

(defun copilot-chat--current-code-block ()
  "Return a chosen code block from the chat buffer.
Signal a `user-error' outside the chat buffer or when the block is empty."
  (unless (derived-mode-p 'copilot-chat-mode)
    (user-error "Not in a Copilot chat buffer"))
  (let* ((block (copilot-chat--read-code-block (copilot-chat--code-blocks)))
         (code (plist-get block :code)))
    (when (or (null code) (string-empty-p (string-trim code)))
      (user-error "That code block is empty"))
    block))

(defun copilot-chat-copy-code-block ()
  "Copy a chat code block to the kill ring.
Use the block at point, or prompt when point is not inside one."
  (interactive)
  (kill-new (plist-get (copilot-chat--current-code-block) :code))
  (message "Copied code block to kill ring"))

(defun copilot-chat-insert-code-block ()
  "Insert a chat code block into the source buffer at its point.
Use the block at point, or prompt when point is not inside one.  When
there is no live source buffer, prompt for a target buffer.  Focus stays
in the chat buffer."
  (interactive)
  (let* ((code (plist-get (copilot-chat--current-code-block) :code))
         (target (if (buffer-live-p copilot-chat--source-buffer)
                     copilot-chat--source-buffer
                   (get-buffer (read-buffer "Insert into buffer: "
                                            nil t)))))
    (unless (buffer-live-p target)
      (user-error "No target buffer to insert into"))
    (with-current-buffer target
      (when buffer-read-only
        (user-error "Target buffer %s is read-only" (buffer-name)))
      ;; Insert at point in the target's visible window when there is
      ;; one, so the snippet lands where the user is looking rather than
      ;; at a stale point; never steal focus from the chat.
      (let ((win (get-buffer-window target t)))
        (if win
            (with-selected-window win (insert code))
          (insert code))))
    (message "Inserted code block into %s" (buffer-name target))))

;;
;; Context references
;;

(defun copilot-chat--add-reference (ref)
  "Add REF to the pending references in the chat buffer.
Create the chat buffer when none exists, so context can be attached
before the first message."
  (with-current-buffer (get-buffer-create copilot-chat--buffer-name)
    (unless (derived-mode-p 'copilot-chat-mode)
      (copilot-chat-mode))
    (let ((added (not (member ref copilot-chat--references))))
      (when added
        ;; `setq-local' rather than relying on the defvar-local default,
        ;; so the binding is unambiguously buffer-local right after the
        ;; major mode has reset local variables.
        (setq-local copilot-chat--references
                    (cons ref copilot-chat--references)))
      (message (if added "Attached %d reference(s) to the next message"
                 "Reference already attached (%d total)")
               (length copilot-chat--references)))))

(defun copilot-chat--references-param ()
  "Return a (:references VECTOR) plist for the pending references, or nil.
The references are left in place; clear them with
`copilot-chat--consume-references' once the message is sent."
  (when copilot-chat--references
    (list :references (vconcat (reverse copilot-chat--references)))))

(defun copilot-chat--consume-references ()
  "Clear the pending references in the current chat buffer."
  (setq copilot-chat--references nil))

(defun copilot-chat--file-reference (file)
  "Return a file-reference plist for FILE."
  (list :type "file" :uri (copilot--path-to-uri (expand-file-name file))))

;;;###autoload
(defun copilot-chat-add-file-reference (file)
  "Attach FILE as context for the next Copilot Chat message."
  (interactive "fAttach file: ")
  (copilot-chat--add-reference (copilot-chat--file-reference file)))

;;;###autoload
(defun copilot-chat-add-region-reference (start end)
  "Attach the region between START and END as context for the next message.
The reference points at the current file with the region's range."
  (interactive "r")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((sel (list :start (copilot--lsp-pos start)
                   :end (copilot--lsp-pos end))))
    (copilot-chat--add-reference
     (list :type "file"
           :uri (copilot--path-to-uri buffer-file-name)
           :selection sel))))

;;;###autoload
(defun copilot-chat-clear-references ()
  "Drop the references pending for the next Copilot Chat message."
  (interactive)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (copilot-chat--consume-references)))
  (message "Cleared pending chat references"))

;;
;; Major mode
;;

(defvar copilot-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'copilot-chat-send)
    (define-key map (kbd "C-c C-c") #'copilot-chat-send)
    (define-key map (kbd "C-c C-k") #'copilot-chat-stop)
    (define-key map (kbd "C-c C-i") #'copilot-chat-insert-code-block)
    (define-key map (kbd "C-c M-w") #'copilot-chat-copy-code-block)
    (define-key map (kbd "C-c /") #'copilot-chat-slash-command)
    (define-key map (kbd "C-c C-f") #'copilot-chat-add-file-reference)
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

(defvar copilot-chat--templates nil
  "Cached slash-command templates from `conversation/templates'.
The set is static per session, so it is fetched once.")

(defun copilot-chat--chat-templates ()
  "Return the server's slash-command templates for the chat.
Filter to the scope matching the current mode (`agent-panel' when
`copilot-chat-use-agent-mode' is on, otherwise `chat-panel').  The
server query is made at most once per session and cached; a failed
query yields no templates rather than an error."
  (unless copilot-chat--templates
    (setq copilot-chat--templates
          (condition-case err
              (let* ((root (copilot--workspace-root))
                     (params (when root
                               (list :workspaceFolders
                                     (vector (list :uri (copilot--path-to-uri root)
                                                   :name (file-name-nondirectory
                                                          (directory-file-name root))))))))
                (or (append (copilot--request 'conversation/templates params)
                            nil)
                    ;; Distinguish "fetched, none" from "not fetched yet".
                    'none))
            (error
             (copilot--log 'warning "Could not fetch chat templates: %S" err)
             'none))))
  (let ((scope (if copilot-chat-use-agent-mode "agent-panel" "chat-panel")))
    (seq-filter (lambda (tpl)
                  (seq-contains-p (plist-get tpl :scopes) scope))
                (if (eq copilot-chat--templates 'none) nil
                  copilot-chat--templates))))

;;;###autoload
(defun copilot-chat-slash-command ()
  "Choose a Copilot Chat slash command and send it.
Slash commands (such as `/explain', `/fix', `/tests', `/doc') are
fetched from the server.  You can supply optional arguments after the
command."
  (interactive)
  (let ((templates (copilot-chat--chat-templates)))
    (unless templates
      (user-error "No slash commands available"))
    (let* ((choices
            (mapcar (lambda (tpl)
                      (cons (format "/%s  %s" (plist-get tpl :id)
                                    (or (plist-get tpl :description) ""))
                            (plist-get tpl :id)))
                    templates))
           (choice (completing-read "Slash command: " choices nil t))
           (id (cdr (assoc choice choices)))
           (args (read-string (format "/%s " id)))
           (message (if (string-empty-p (string-trim args))
                        (format "/%s" id)
                      (format "/%s %s" id args))))
      (copilot-chat message))))

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
