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
(require 'thingatpt)

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

(defcustom copilot-chat-enable-semantic-search nil
  "When non-nil, let the server build a semantic-search index of the project.
This declares the `watchedFiles' capability at server start, after
which the server asks copilot.el for the workspace file list (via the
`copilot/watchedFiles' request) and indexes it for whole-codebase
\(\"@workspace\"-style) questions in chat.

Indexing computes embeddings server-side over the project's files, so it
uses CPU and network; leave it off if you do not need codebase-wide
search.  Changing this takes effect when the server next starts (e.g.
after \\[copilot-diagnose])."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.7"))

(defcustom copilot-chat-show-status-header t
  "When non-nil, show a status header line in the chat buffer.
The header shows at a glance whether Agent or Ask mode is active, which
model answers, and in agent mode how many tools are available.  It is
set up when the chat buffer is created, so changing this takes effect
for new chat buffers."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-task-prompts
  '((review . "Review the following code and point out bugs, risks, and possible improvements:")
    (fix . "Fix the bugs or problems in the following code and explain what was wrong:")
    (doc . "Document the following code: add docstrings and comments in the conventional style for its language:")
    (optimize . "Optimize the following code for performance and readability, and explain each change:")
    (tests . "Write a comprehensive set of unit tests for the following code:"))
  "Prompts used by the one-shot Copilot Chat task commands.
An alist mapping a task symbol to the instruction sent along with the
selected code.  The known tasks are `review' (`copilot-chat-review'),
`fix' (`copilot-chat-fix'), `doc' (`copilot-chat-doc'), `optimize'
\(`copilot-chat-optimize'), and `tests' (`copilot-chat-write-tests');
extra entries become available through `copilot-chat-task'."
  :type '(alist :key-type (symbol :tag "Task")
                :value-type (string :tag "Prompt"))
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-commit-message-prompt
  "Write a commit message for the following staged diff.
Use the conventional commit format: a concise summary line of at most
72 characters, then a blank line, then an optional short body explaining
the why behind the change.  Return ONLY the commit message text, without
markdown code fences and without any commentary."
  "Instruction prepended to the staged diff when generating a commit message.
Used by `copilot-chat-insert-commit-message'."
  :type 'string
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defface copilot-chat-error-face
  '((t :inherit error))
  "Face for error messages in the chat buffer."
  :group 'copilot-chat)

(defface copilot-chat-status-header-face
  '((t :inherit shadow))
  "Face for the status header line in the chat buffer.
Inheriting `header-line' as well would paint active-window colors into
inactive windows on Emacs 31+, which splits the two into separate
faces; the underlying `header-line' face still applies on its own."
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
  "Alist of (TOKEN . SINK) for routing `$/progress'.
SINK is normally the chat buffer the reply streams into, but it can also
be a function called with each progress value (see
`copilot-chat--collecting-sink'), which lets one-shot requests consume a
reply without involving the chat buffer.")

(defvar copilot-chat--one-shot-requests nil
  "Alist of (TOKEN . REQUEST-ID) for in-flight one-shot conversations.
Lets `copilot-chat-stop' cancel a pending one-shot instead of resetting
the chat conversation.")

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

(defun copilot-chat--model-supports-tools-p (model-id)
  "Return non-nil unless MODEL-ID is known to lack tool-call support.
Consult the model's `capabilities.supports.tool_calls' flag from the
server's model list.  Return non-nil when the flag is absent (older
servers omit it, and the `auto' router model carries no capabilities) so
agent mode is never wrongly flagged; return nil only when the server
positively reports that the model cannot call tools."
  (let* ((model (seq-find (lambda (m) (equal (plist-get m :id) model-id))
                          (copilot-chat--chat-models)))
         (supports (plist-get (plist-get model :capabilities) :supports)))
    (not (eq (plist-get supports :tool_calls) :json-false))))

(defun copilot-chat--maybe-warn-model-lacks-tools ()
  "Warn when agent mode is on but the active model cannot call tools.
Without tool support the model just tells the user which commands to run
instead of running them, which reads as agent mode not working.  This is
best-effort: it stays silent when tool support can't be determined and
never blocks starting a conversation."
  (when copilot-chat-use-agent-mode
    (condition-case nil
        (when-let* ((model (copilot-chat--model)))
          (unless (copilot-chat--model-supports-tools-p model)
            (message
             (concat "Copilot Chat: Agent mode is on but model %s does not "
                     "support tool calls, so Copilot can't run tools itself.  "
                     "Pick a tool-capable model with "
                     "M-x copilot-chat-select-model.")
             model)))
      (error nil))))

(defun copilot-chat--model-param ()
  "Return request params identifying the chat model to use.
Send the modern `modelInfo' object alongside the deprecated `model'
field so both current and older language servers resolve a model.
Return nil when no model can be resolved, letting the server pick."
  (when-let* ((model (copilot-chat--model)))
    (list :modelInfo (list :id model)
          :model model)))

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

(defun copilot-chat--collecting-sink (callback)
  "Return a progress sink that accumulates a whole reply for CALLBACK.
The returned function consumes the `$/progress' values of one turn and
concatenates the streamed reply chunks.  When the turn ends, CALLBACK is
called with two arguments: the accumulated reply string and an error
message string reported by the server (or nil)."
  (let ((chunks '()))
    (lambda (value)
      (pcase (plist-get value :kind)
        ("report"
         (when-let* ((reply (copilot-chat--extract-reply value)))
           (push reply chunks)))
        ("end"
         (let* ((result (plist-get value :result))
                ;; The server normally sends an object here, but guard
                ;; against any other shape.
                (result (and (listp result) result)))
           (funcall callback
                    (apply #'concat (nreverse chunks))
                    (copilot-chat--error-text (plist-get result :error)))))))))

(defun copilot-chat--handle-progress (msg)
  "Handle `$/progress' notification MSG for chat streaming.
Dispatch on the sink registered for the token in
`copilot-chat--active-buffers': a function sink consumes the progress
values itself, while a buffer sink streams the reply into the chat
buffer."
  (copilot--dbind (token value) msg
    (when-let* ((entry (assoc token copilot-chat--active-buffers))
                (sink (cdr entry)))
      (if (functionp sink)
          (progn
            ;; Unregister before invoking the sink: if the sink's
            ;; callback signals (it runs arbitrary user-facing code),
            ;; the entry must not leak in the alist forever.
            (when (equal (plist-get value :kind) "end")
              (setq copilot-chat--active-buffers
                    (assoc-delete-all token copilot-chat--active-buffers))
              (setq copilot-chat--one-shot-requests
                    (assoc-delete-all token copilot-chat--one-shot-requests)))
            (funcall sink value))
        (copilot-chat--handle-buffer-progress token sink value)))))

(defun copilot-chat--handle-buffer-progress (token chat-buf value)
  "Stream the progress VALUE of turn TOKEN into CHAT-BUF."
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
                (assoc-delete-all token copilot-chat--active-buffers))))))))

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

(defun copilot-chat--run-argv (name argv timeout)
  "Run ARGV (a program followed by its arguments) asynchronously.
NAME labels the process and its hidden output buffer.  Pump process and
connection events with `accept-process-output' instead of blocking, so
redisplay and the language-server connection keep working, and honor
TIMEOUT seconds (nil for none).  \\[keyboard-quit] aborts the command.

Return a plist with keys `:output' (combined stdout and stderr),
`:status' (one of `success', `error', `timeout', `cancelled'), and
`:exit-code' (the process exit code for `success'/`error', nil
otherwise)."
  (let ((buffer (generate-new-buffer (format " *%s*" name))))
    (unwind-protect
        (let* ((proc (make-process
                      :name name
                      :buffer buffer
                      :command argv
                      :connection-type 'pipe
                      :noquery t
                      ;; Suppress the default "Process ... exited
                      ;; abnormally" status line Emacs would otherwise
                      ;; insert into the output buffer on a non-zero exit.
                      :sentinel #'ignore))
               (deadline (and timeout (+ (float-time) timeout)))
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
            (list :output (with-current-buffer buffer (buffer-string))
                  :status status
                  :exit-code code)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun copilot-chat--run-process (command)
  "Run shell COMMAND asynchronously, keeping Emacs responsive.
Honor `copilot-chat-terminal-timeout'.  \\[keyboard-quit] aborts the
running command (reported as `cancelled'); it does not abort the whole
agent turn, for which `copilot-chat-stop' is available.

Return a plist as `copilot-chat--run-argv' does, with `:output'
truncated to `copilot-chat--terminal-max-output'."
  (let ((result (copilot-chat--run-argv
                 "copilot-chat-terminal"
                 (list shell-file-name shell-command-switch command)
                 copilot-chat-terminal-timeout)))
    (plist-put result :output
               (copilot-chat--truncate-output (plist-get result :output)))))

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
;; Workspace search (server -> client requests)
;;

(defcustom copilot-chat-ripgrep-program "rg"
  "Name or path of the ripgrep executable used for workspace search.
Workspace file and text search (the `workspace/findFiles' and
`workspace/findTextInFiles' requests the server makes during agent
mode) shell out to ripgrep, which honors `.gitignore'.  When ripgrep is
unavailable the search requests return an error result."
  :type 'string
  :group 'copilot-chat
  :package-version '(copilot . "0.7"))

(defconst copilot-chat--workspace-search-timeout 30
  "Seconds to allow a workspace search subprocess to run.")

(defconst copilot-chat--workspace-search-max-results 100
  "Fallback cap on workspace search results when the server sends none.")

(defvar copilot-chat--ripgrep-warned nil
  "Non-nil once the missing-ripgrep warning has been shown this session.")

(defun copilot-chat--ripgrep ()
  "Return the ripgrep executable path, or nil when unavailable."
  (executable-find copilot-chat-ripgrep-program))

(defun copilot-chat--run-ripgrep (args dir)
  "Run ripgrep with ARGS in DIR and return its stdout lines.
Return nil when ripgrep is unavailable or the search fails.  Ripgrep
exits non-zero with no matches, which is treated as an empty result."
  (when-let* ((rg (copilot-chat--ripgrep)))
    (let* ((default-directory (file-name-as-directory dir))
           (result (copilot-chat--run-argv "copilot-chat-search"
                                           (cons rg args)
                                           copilot-chat--workspace-search-timeout))
           (code (plist-get result :exit-code)))
      ;; rg: 0 = matches, 1 = no matches, 2 = error.
      (when (memq code '(0 1))
        (seq-remove #'string-empty-p
                    (split-string (plist-get result :output) "\n"))))))

(defun copilot-chat--search-dir (msg)
  "Return the directory to search for request MSG, or nil.
Prefer the request's base directory URI (`:baseUri' for the search
requests, `:uri' for `copilot/watchedFiles'), else the workspace root."
  (let ((base (or (plist-get msg :baseUri) (plist-get msg :uri))))
    (cond
     ((and (stringp base) (not (string-empty-p base)))
      (copilot--uri-to-path base))
     ;; The root is already a path, so use it directly rather than
     ;; round-tripping through a URI (which would munge a literal `%').
     ((copilot--workspace-root)))))

(defun copilot-chat--search-max-results (msg)
  "Return the result cap for request MSG."
  (or (plist-get msg :maxResults)
      copilot-chat--workspace-search-max-results))

(defun copilot-chat--rels-to-uri-vector (rels dir)
  "Return a vector of file URIs for relative paths RELS under DIR."
  (vconcat (mapcar (lambda (rel)
                     (copilot--path-to-uri (expand-file-name rel dir)))
                   rels)))

(defun copilot-chat--handle-find-files (msg)
  "Handle a `workspace/findFiles' request MSG.
Return (:uris VECTOR) of file URIs under the requested base whose
relative paths match the glob pattern."
  (let* ((dir (copilot-chat--search-dir msg))
         (pattern (plist-get msg :pattern))
         (max (copilot-chat--search-max-results msg))
         (args (append '("--files")
                       (when (and (stringp pattern)
                                  (not (string-empty-p pattern)))
                         (list "-g" pattern))))
         (files (and dir (copilot-chat--run-ripgrep args dir))))
    (list :uris (copilot-chat--rels-to-uri-vector (seq-take files max) dir))))

(defun copilot-chat--parse-rg-match (line dir)
  "Parse a ripgrep LINE of the form PATH:LINENO:TEXT into a match plist.
PATH is resolved relative to DIR.  Return nil when LINE does not have
that shape."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" line)
    ;; Read every group before calling `copilot--path-to-uri', whose
    ;; `url-encode-url' clobbers the match data via its own `string-match'.
    (let ((path (match-string 1 line))
          (lineno (string-to-number (match-string 2 line)))
          (text (match-string 3 line)))
      (list :uri (copilot--path-to-uri (expand-file-name path dir))
            :lineNumber lineno
            :lineText text))))

(defun copilot-chat--handle-find-text-in-files (msg)
  "Handle a `workspace/findTextInFiles' request MSG.
Return (:matches VECTOR) of {uri, lineNumber, lineText} for the query."
  (let* ((dir (copilot-chat--search-dir msg))
         (query (plist-get msg :query))
         (max (copilot-chat--search-max-results msg))
         (include (plist-get msg :includePattern))
         (lines
          (when (and dir (stringp query) (not (string-empty-p query)))
            (copilot-chat--run-ripgrep
             (append (unless (eq (plist-get msg :isRegexp) t)
                       ;; ripgrep treats the pattern as a regex by
                       ;; default; mark it literal otherwise.
                       (list "--fixed-strings"))
                     (list
                      ;; `-e' marks QUERY as the pattern so a dash-leading
                      ;; query isn't read as a flag, and `--max-count'
                      ;; bounds ripgrep's own per-file work.
                      "-e" query
                      "--max-count" (number-to-string max)
                      "--no-heading" "--line-number" "--with-filename"
                      "--color" "never")
                     (when (and (stringp include)
                                (not (string-empty-p include)))
                       (list "-g" include))
                     ;; Explicit search path: with a piped stdin (no TTY)
                     ;; and no path, ripgrep reads stdin and hangs.
                     (list "."))
             dir))))
    (list :matches
          (vconcat (delq nil (mapcar (lambda (line)
                                       (copilot-chat--parse-rg-match line dir))
                                     (seq-take lines max)))))))

(copilot-on-request 'workspace/findFiles #'copilot-chat--handle-find-files)
(copilot-on-request 'workspace/findTextInFiles
                    #'copilot-chat--handle-find-text-in-files)

(defun copilot-chat--handle-watched-files (msg)
  "Handle a `copilot/watchedFiles' request MSG.
Return (:files VECTOR) of file URIs for the workspace, which the server
indexes for semantic search.  Files are enumerated with ripgrep, so
ignored files (per `.gitignore') are excluded.  The full list is
returned deliberately: the server wants the whole tree to index and caps
it on its end."
  ;; Warn (once per session) when the index source can't be produced, so
  ;; enabling semantic search without ripgrep doesn't silently no-op.
  (unless (or (copilot-chat--ripgrep) copilot-chat--ripgrep-warned)
    (setq copilot-chat--ripgrep-warned t)
    (display-warning
     'copilot
     (format "Semantic search needs ripgrep; %s not found on PATH"
             copilot-chat-ripgrep-program)
     :warning))
  (let* ((dir (copilot-chat--search-dir msg))
         (files (and dir (copilot-chat--run-ripgrep '("--files") dir))))
    (list :files (copilot-chat--rels-to-uri-vector files dir))))

(copilot-on-request 'copilot/watchedFiles #'copilot-chat--handle-watched-files)

(defconst copilot-chat--read-file-max-bytes 1000000
  "Maximum number of bytes read for a `workspace/readFile' request.")

(defun copilot-chat--file-uri-path (uri)
  "Return the local path for file URI, or signal an error.
The workspace read requests only make sense for `file://' URIs."
  (unless (and (stringp uri) (string-prefix-p "file://" uri))
    (error "Cannot handle non-file URI: %S" uri))
  (copilot--uri-to-path uri))

(defun copilot-chat--handle-read-file (msg)
  "Handle a `workspace/readFile' request MSG.
Return (:content STRING) with the requested file's contents, capped at
`copilot-chat--read-file-max-bytes'.  Signal an error (reported to the
server) when the file cannot be read, rather than masking it as empty."
  (let ((path (copilot-chat--file-uri-path (plist-get msg :uri)))
        ;; Decode as UTF-8 (what the protocol assumes) so a byte cap that
        ;; splits a character leaves an eight-bit char consistently across
        ;; platforms, rather than a latin-1-decoded byte on some.
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents path nil 0 copilot-chat--read-file-max-bytes)
      ;; Drop any partial multibyte sequence (eight-bit chars) left at the
      ;; tail by the byte cap.
      (goto-char (point-max))
      (while (and (> (point) (point-min))
                  (>= (char-before) #x3fff80))
        (delete-char -1))
      (list :content (buffer-string)))))

(defun copilot-chat--attr-file-type (name dir attr-type)
  "Return the VS Code FileType integer for an entry.
NAME is the entry under DIR and ATTR-TYPE is its `file-attribute-type'
\(t for a directory, nil for a regular file, a string for a symlink).
1 is a regular file, 2 a directory, 64 the symbolic-link bit combined
with the link target's type."
  (cond
   ((eq attr-type t) 2)
   ((null attr-type) 1)
   ;; Symlink: resolve the target's type for the combined bit.  This is
   ;; the only case needing an extra stat, and symlinks are rare.
   ((stringp attr-type)
    (logior 64 (if (file-directory-p (expand-file-name name dir)) 2 1)))
   (t 0)))

(defun copilot-chat--handle-read-directory (msg)
  "Handle a `workspace/readDirectory' request MSG.
Return (:entries VECTOR) of {name, type} for the directory's children,
fetched in a single directory scan."
  (let ((dir (copilot-chat--file-uri-path (plist-get msg :uri))))
    (list :entries
          (vconcat
           (when (file-directory-p dir)
             (delq nil
                   (mapcar
                    (lambda (entry)
                      (let ((name (car entry)))
                        ;; `directory-files-and-attributes' includes "."
                        ;; and ".."; skip them.  Hidden files are kept.
                        (unless (member name '("." ".."))
                          (list :name name
                                :type (copilot-chat--attr-file-type
                                       name dir
                                       (file-attribute-type (cdr entry)))))))
                    (directory-files-and-attributes dir))))))))

(copilot-on-request 'workspace/readFile #'copilot-chat--handle-read-file)
(copilot-on-request 'workspace/readDirectory
                    #'copilot-chat--handle-read-directory)

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
    (copilot-chat--maybe-warn-model-lacks-tools)
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
             (copilot-chat--model-param)
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
                (copilot-chat--model-param)
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

(defun copilot-chat--destroy-one-shot (conversation-id)
  "Destroy the one-shot conversation CONVERSATION-ID, if any.
A nil CONVERSATION-ID and a dead connection are both quietly ignored:
the conversation is throwaway, so there is nothing useful to report."
  (when (and conversation-id (copilot--connection-alivep))
    (copilot--async-request
     'conversation/destroy
     (list :conversationId conversation-id))))

(defun copilot-chat--cancel-one-shots ()
  "Cancel all in-flight one-shot requests.
Their callbacks are invoked with a cancellation error so the initiating
commands can report it.  Return non-nil when anything was cancelled."
  (when copilot-chat--one-shot-requests
    (let ((pending copilot-chat--one-shot-requests))
      (setq copilot-chat--one-shot-requests nil)
      (dolist (entry pending)
        (let* ((token (car entry))
               (request-id (cdr entry))
               (sink (cdr (assoc token copilot-chat--active-buffers))))
          (when (and request-id (copilot--connection-alivep))
            (jsonrpc-notify copilot--connection '$/cancelRequest
                            (list :id request-id)))
          (setq copilot-chat--active-buffers
                (assoc-delete-all token copilot-chat--active-buffers))
          (when (functionp sink)
            (funcall sink '(:kind "end" :result (:error "Cancelled")))))))
    t))

(defun copilot-chat--one-shot (message callback)
  "Send MESSAGE as a stand-alone, throwaway conversation.
The reply is accumulated internally instead of streaming into the chat
buffer, so this neither needs the chat panel nor disturbs an ongoing
conversation there.  CALLBACK is called with the complete reply string
and an error message string (or nil) once the turn finishes, after which
the conversation is destroyed.  Cancellable with `copilot-chat-stop'."
  (let* ((token (format "copilot-chat-one-shot-%s" (float-time)))
         (conversation-id nil)
         (finished nil)
         (called nil)
         (errored nil)
         ;; The server can end the turn and then fail the create
         ;; response, or cancellation can race the reply; make sure the
         ;; caller only ever hears back once.
         (callback-once (lambda (reply error-msg)
                          (unless called
                            (setq called t)
                            (funcall callback reply error-msg))))
         (request-id
          ;; Issue the request BEFORE registering the sink: starting the
          ;; server can signal (e.g. binary not installed), and nothing
          ;; must be left behind in the alist when it does.  Progress
          ;; can't arrive before we get to register: notifications are
          ;; only processed when Emacs reads process output.
          ;;
          ;; The response handlers below don't touch the calling buffer,
          ;; but `copilot--async-request' drops the success handler when
          ;; the buffer current at call time has been killed; issue the
          ;; call from a buffer that is always live so an aborted commit
          ;; buffer can't prevent the conversation id from being
          ;; recorded (and the throwaway conversation from being
          ;; destroyed).
          (with-current-buffer (get-buffer-create " *copilot-chat-one-shot*")
            (copilot--async-request
             'conversation/create
             (append
              (list :workDoneToken token
                    :turns (vector
                            (list :request message
                                  :response ""
                                  :turnId ""))
                    ;; No editor context: the request carries everything
                    ;; it needs.
                    :capabilities (list :skills (vector)
                                        :allSkills :json-false)
                    :source "panel")
              (copilot-chat--model-param)
              (list :workspaceFolders
                    (vconcat
                     (when-let* ((root (copilot--workspace-root)))
                       (list (list :uri (concat "file://" root)
                                   :name (file-name-nondirectory
                                          (directory-file-name root))))))))
             :success-fn (lambda (result)
                           (setq conversation-id
                                 (plist-get result :conversationId))
                           (when finished
                             (copilot-chat--destroy-one-shot conversation-id)))
             :error-fn (lambda (err)
                         ;; The flag covers an error delivered before the
                         ;; registration below has even happened, in
                         ;; which case the alist cleanups are no-ops.
                         (setq errored t)
                         (setq copilot-chat--active-buffers
                               (assoc-delete-all
                                token copilot-chat--active-buffers))
                         (setq copilot-chat--one-shot-requests
                               (assoc-delete-all
                                token copilot-chat--one-shot-requests))
                         (funcall callback-once nil
                                  (or (copilot-chat--error-text err)
                                      (format "%s" err))))))))
    (unless errored
      (push (cons token
                  (copilot-chat--collecting-sink
                   (lambda (reply error-msg)
                     (setq finished t)
                     ;; When the reply somehow completes before the
                     ;; create response has delivered the conversation
                     ;; id, the success handler above destroys the
                     ;; conversation.
                     (copilot-chat--destroy-one-shot conversation-id)
                     (funcall callback-once reply error-msg))))
            copilot-chat--active-buffers)
      (push (cons token request-id) copilot-chat--one-shot-requests))))

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
       ((null blocks) (user-error "Copilot Chat: No code blocks in this chat"))
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
    (user-error "Copilot Chat: Not in a chat buffer"))
  (let* ((block (copilot-chat--read-code-block (copilot-chat--code-blocks)))
         (code (plist-get block :code)))
    (when (or (null code) (string-empty-p (string-trim code)))
      (user-error "Copilot Chat: That code block is empty"))
    block))

(defun copilot-chat-copy-code-block ()
  "Copy a chat code block to the kill ring.
Use the block at point, or prompt when point is not inside one."
  (interactive)
  (kill-new (plist-get (copilot-chat--current-code-block) :code))
  (message "Copilot Chat: Copied code block to kill ring"))

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
      (user-error "Copilot Chat: No target buffer to insert into"))
    (with-current-buffer target
      (when buffer-read-only
        (user-error "Copilot Chat: Target buffer %s is read-only" (buffer-name)))
      ;; Insert at point in the target's visible window when there is
      ;; one, so the snippet lands where the user is looking rather than
      ;; at a stale point; never steal focus from the chat.
      (let ((win (get-buffer-window target t)))
        (if win
            (with-selected-window win (insert code))
          (insert code))))
    (message "Copilot Chat: Inserted code block into %s" (buffer-name target))))

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
      (message (if added "Copilot Chat: Attached context for the next message (%d pending)"
                 "Copilot Chat: That context is already attached (%d pending)")
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
The context points at the current file with the region's range."
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
  "Drop the context pending for the next Copilot Chat message."
  (interactive)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (copilot-chat--consume-references)))
  (message "Copilot Chat: Cleared pending context"))

;;
;; Commit message generation
;;

(defun copilot-chat--staged-diff ()
  "Return the staged diff of the repository around `default-directory'.
Run git in `default-directory' and let it resolve the repository
itself: hunting for the root manually (e.g. `locate-dominating-file')
picks the wrong index for linked worktrees, whose COMMIT_EDITMSG lives
under the main checkout's .git directory.  Use `process-file' so remote
\(TRAMP) buffers diff the remote repository rather than silently running
git locally.  Signal a `user-error' when there is no repository, when
git fails, or when nothing is staged."
  (let* ((exit nil)
         (diff (with-temp-buffer
                 (setq exit (process-file "git" nil t nil
                                          "diff" "--cached" "--no-color"))
                 (buffer-string))))
    (unless (eql exit 0)
      (user-error
       "Copilot Chat: git diff failed (%s); not inside a git repository?"
       exit))
    (when (string-empty-p (string-trim diff))
      (user-error "Copilot Chat: No staged changes"))
    diff))

(defun copilot-chat--commit-message-request ()
  "Return the chat message asking for a commit message for the staged diff."
  (concat copilot-chat-commit-message-prompt "\n\n" (copilot-chat--staged-diff)))

(defun copilot-chat--strip-code-fences (reply)
  "Return REPLY without enclosing markdown code fences, trimmed.
The model is asked not to fence the commit message, but strip a fence
wrapping the whole reply defensively when it adds one anyway."
  (let ((text (string-trim reply)))
    ;; Fences are three or more backticks (models occasionally use four).
    (if (string-match "\\``\\{3,\\}[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?`\\{3,\\}\\'" text)
        (string-trim (match-string 1 text))
      text)))

;;;###autoload
(defun copilot-chat-insert-commit-message ()
  "Generate a commit message from the staged diff and insert it at point.
Meant to be called from a commit message buffer (e.g. Magit's
COMMIT_EDITMSG), but works from any buffer inside a git repository.
The staged diff is sent to Copilot with
`copilot-chat-commit-message-prompt', and the reply is inserted
asynchronously at point once it arrives.  The request runs outside the
chat panel and does not disturb an ongoing chat conversation."
  (interactive)
  (let ((request (copilot-chat--commit-message-request))
        (buf (current-buffer))
        (pos (point-marker)))
    (message "Copilot Chat: Generating commit message...")
    (copilot-chat--one-shot
     request
     (lambda (reply error-msg)
       (let ((reply (and reply (copilot-chat--strip-code-fences reply))))
         (cond
          (error-msg
           (message "Copilot Chat: Commit message generation failed: %s"
                    error-msg))
          ((or (null reply) (string-empty-p reply))
           (message "Copilot Chat: Got an empty commit message"))
          ((not (buffer-live-p buf))
           (message "Copilot Chat: Commit message buffer is gone"))
          ((buffer-local-value 'buffer-read-only buf)
           ;; Don't signal inside the jsonrpc process filter; salvage
           ;; the reply instead.
           (kill-new reply)
           (message
            "Copilot Chat: Buffer is read-only; commit message copied to kill ring"))
          (t
           (with-current-buffer buf
             (save-excursion
               (goto-char pos)
               (insert reply)))
           (set-marker pos nil)
           (message "Copilot Chat: Commit message inserted"))))))))

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

(defvar copilot-chat--client-tool-count nil
  "Cached count of copilot.el's own client tools.
The definitions are static, but building them allocates; the header
line evaluates on every redisplay, so it must not do that each time.")

(defun copilot-chat--tool-count ()
  "Return the number of tools available in agent mode.
Count copilot.el's own client tools plus the tools of the MCP servers
last reported by the language server (`copilot-chat--mcp-servers')."
  (+ (or copilot-chat--client-tool-count
         (setq copilot-chat--client-tool-count
               (length (copilot-chat--client-tool-names))))
     (seq-reduce (lambda (acc server)
                   (+ acc (length (plist-get server :tools))))
                 copilot-chat--mcp-servers
                 0)))

(defun copilot-chat--status-header ()
  "Return the status header line for the chat buffer.
Show the chat mode (Agent or Ask), the active model, and in agent mode
the number of available tools.  Built from variables already in memory
\(`copilot-chat-model', the cached `copilot-chat--resolved-model' and
`copilot-chat--mcp-servers') plus a memoized client-tool count, so it
is cheap enough to evaluate on every redisplay and never contacts the
server."
  (let ((segments
         (list (concat "Copilot Chat  "
                       (if copilot-chat-use-agent-mode "Agent mode" "Ask mode"))
               (or copilot-chat-model
                   copilot-chat--resolved-model
                   "default model")
               (when copilot-chat-use-agent-mode
                 (format "%d tools" (copilot-chat--tool-count))))))
    (propertize (string-join (delq nil segments) "  •  ")
                'face 'copilot-chat-status-header-face)))

(declare-function gfm-mode "ext:markdown-mode" ())

(defun copilot-chat--setup-mode ()
  "Set up font-lock, mode-line, header-line, and visual-line settings.
Used by `copilot-chat-mode'.  When `markdown-mode' is available, enable
GFM font-lock for full markdown rendering; otherwise use basic
highlighting."
  (when (require 'markdown-mode nil t)
    (setq-local markdown-fontify-code-blocks-natively t)
    (setq-local font-lock-defaults
                (with-temp-buffer
                  (gfm-mode)
                  font-lock-defaults))
    (font-lock-flush))
  (font-lock-add-keywords nil copilot-chat--font-lock-keywords t)
  (setq mode-name '(:eval (copilot-chat--mode-line)))
  (when copilot-chat-show-status-header
    (setq header-line-format '(:eval (copilot-chat--status-header))))
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
    (user-error "Copilot Chat: Empty message"))
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
    (user-error "Copilot Chat: Empty message"))
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (unless chat-buf
      (user-error "Copilot Chat: No active chat buffer"))
    (with-current-buffer chat-buf
      (when copilot-chat--streaming-p
        (user-error "Copilot Chat: A response is currently being streamed"))
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
     (user-error "Copilot Chat: No active region")))
  (let* ((code (buffer-substring-no-properties start end))
         (lang (copilot--get-language-id))
         (message (if (and prompt (not (string-empty-p prompt)))
                      (format "%s\n\n```%s\n%s\n```" prompt lang code)
                    (format "```%s\n%s\n```" lang code))))
    (copilot-chat message)))

;;
;; One-shot task commands
;;

(defun copilot-chat--task-bounds ()
  "Return the bounds of the code a task command should act on.
Use the active region when there is one, otherwise the defun at point.
The defun fallback only applies in `prog-mode' buffers: outside of them
\(markdown, text, ...) `bounds-of-thing-at-point' happily returns a
whole prose section or a parenthesized fragment, which is never what a
code task should be sent.  Signal a `user-error' when nothing suitable
is available."
  (or (and (use-region-p) (cons (region-beginning) (region-end)))
      (and (derived-mode-p 'prog-mode)
           (bounds-of-thing-at-point 'defun))
      (user-error "Copilot Chat: No active region or function at point")))

(defun copilot-chat--task-prompt (task)
  "Return the prompt configured for TASK in `copilot-chat-task-prompts'.
Signal a `user-error' when TASK has no prompt."
  (or (alist-get task copilot-chat-task-prompts)
      (user-error "Copilot Chat: No prompt configured for task `%s'" task)))

;;;###autoload
(defun copilot-chat-task (task)
  "Send the region (or the defun at point) to Copilot Chat for TASK.
TASK is a key of `copilot-chat-task-prompts', whose prompt is sent
along with the code.  Interactively, pick the task with completion.
The answer streams into the regular chat buffer."
  (interactive
   (let ((choice (completing-read
                  "Copilot Chat task: "
                  ;; Offer only symbol keys: a string key sneaked into
                  ;; the alist could be offered but never looked up.
                  (seq-filter #'symbolp (mapcar #'car copilot-chat-task-prompts))
                  nil t)))
     ;; With no default, an empty minibuffer input returns "" even
     ;; though REQUIRE-MATCH is t.
     (when (string-empty-p choice)
       (user-error "Copilot Chat: No task selected"))
     (list (intern choice))))
  (let ((bounds (copilot-chat--task-bounds)))
    (copilot-chat-send-region (car bounds) (cdr bounds)
                              (copilot-chat--task-prompt task))))

;;;###autoload
(defun copilot-chat-review ()
  "Ask Copilot Chat to review the region or the defun at point."
  (interactive)
  (copilot-chat-task 'review))

;;;###autoload
(defun copilot-chat-fix ()
  "Ask Copilot Chat to fix the region or the defun at point."
  (interactive)
  (copilot-chat-task 'fix))

;;;###autoload
(defun copilot-chat-doc ()
  "Ask Copilot Chat to document the region or the defun at point."
  (interactive)
  (copilot-chat-task 'doc))

;;;###autoload
(defun copilot-chat-optimize ()
  "Ask Copilot Chat to optimize the region or the defun at point."
  (interactive)
  (copilot-chat-task 'optimize))

;;;###autoload
(defun copilot-chat-write-tests ()
  "Ask Copilot Chat to write a test suite for the region or the defun at point."
  (interactive)
  (copilot-chat-task 'tests))

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
      (user-error "Copilot Chat: No slash commands available"))
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
When nothing is streaming in the chat buffer, cancel any pending
one-shot request (e.g. `copilot-chat-insert-commit-message') instead;
only when there is nothing to cancel at all, reset the conversation."
  (interactive)
  (let ((chat-buf (get-buffer copilot-chat--buffer-name)))
    (cond
     ((and chat-buf
           (buffer-local-value 'copilot-chat--streaming-p chat-buf))
      (with-current-buffer chat-buf
        (when (and copilot-chat--request-id (copilot--connection-alivep))
          (jsonrpc-notify copilot--connection
                          '$/cancelRequest
                          (list :id copilot-chat--request-id)))
        (copilot-chat--end-streaming)
        (copilot-chat--insert-error "Cancelled")
        (copilot-chat--remove-active-tokens chat-buf)))
     ((copilot-chat--cancel-one-shots))
     (t (copilot-chat-reset)))))

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
    (message "Copilot Chat: Model set to %s" model-id)))

(provide 'copilot-chat)
;;; copilot-chat.el ends here
