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

(defcustom copilot-chat-presets nil
  "Named bundles of Copilot Chat settings you can switch between.
Each entry maps a preset name (a string) to a plist of settings that
`copilot-chat-apply-preset' applies in one step.  Recognized keys, all
optional:

  `:model'              a chat model id string (sets `copilot-chat-model')
  `:agent-mode'         a boolean (sets `copilot-chat-use-agent-mode')
  `:auto-approve-tools' a list of tool-name strings
                        (sets `copilot-chat-auto-approve-tools')

A key omitted from a preset leaves the corresponding variable
untouched, so a preset can change just a single setting.

A preset's model takes effect on your next message, including the
current conversation, since the model is sent with every turn.  Agent
mode takes effect on the next new conversation, since it is fixed when
a conversation is created.

Example:

  \\='((\"fast\"  . (:model \"gpt-4o\" :agent-mode nil))
    (\"agent\" . (:model \"gpt-5-codex\" :agent-mode t
               :auto-approve-tools (\"get_errors\" \"copilot.read_file\"))))"
  :type '(alist :key-type (string :tag "Preset name")
                :value-type
                (plist :options
                       ((:model (string :tag "Model ID"))
                        (:agent-mode (boolean :tag "Agent mode"))
                        (:auto-approve-tools
                         (repeat (string :tag "Tool name"))))))
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

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

(defcustom copilot-chat-frontend 'markdown
  "Markup used to render the Copilot Chat buffer.
With the default `markdown' value the buffer is highlighted as
GitHub-flavored Markdown.  With `org' the exchange scaffolding is written
as Org headings (a `* You' heading per turn with a nested `** Copilot'
heading) and the buffer borrows Org highlighting plus outline folding;
this needs the `org' library to be available.

The streamed assistant response is left exactly as the server sends it in
both frontends, so its body stays markdown-ish (fenced code blocks and
all) regardless of this setting.

The frontend is chosen when the chat buffer is created, so changing this
takes effect for new chat buffers."
  :type '(choice (const :tag "Markdown" markdown)
                 (const :tag "Org" org))
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

(defcustom copilot-chat-rewrite-prompt
  "Rewrite the code below according to the instruction that follows it.
Return ONLY the rewritten code, without markdown code fences and
without any commentary or explanations."
  "Instruction prepended to the code sent by `copilot-chat-rewrite'.
The rewrite instruction read from the minibuffer and the region's code
\(in a fenced block tagged with the buffer's language) are appended to
this prompt."
  :type 'string
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-rewrite-indent t
  "When non-nil, re-indent the region rewritten by `copilot-chat-rewrite'.
`indent-region' runs over the inserted text right after an accepted
rewrite replaces the region."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-save-history nil
  "When non-nil, save the chat transcript to disk after each turn.
The whole session is written to a per-workspace file under
`copilot-chat-history-directory' every time a turn completes, and
`copilot-chat-restore' brings a saved conversation back, context
included.  Transcripts land on disk in plain text, so this is off
by default."
  :type 'boolean
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-history-directory
  (locate-user-emacs-file "copilot-chat-history")
  "Directory where saved chat transcripts are stored.
Each workspace gets one file, named after the SHA-1 of its root
directory (or \"global\" outside any workspace), with the `.eld'
extension.  The directory is created on demand when the first
transcript is saved."
  :type 'directory
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-notify-after-seconds 10
  "Notify when a chat turn takes at least this many seconds.
When a streamed response finishes and the chat buffer is not the one
you are looking at, a desktop notification is raised, but only if the
turn ran at least this long.  Set to nil to disable notifications
entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'copilot-chat
  :package-version '(copilot . "0.8"))

(defcustom copilot-chat-notify-function #'copilot-chat--notify
  "Function used to raise a desktop notification for a finished turn.
It is called with two string arguments, a short TITLE and a BODY, and
its return value is ignored.  Replace it to plug in a different
notification backend; the default `copilot-chat--notify' picks a
platform-appropriate one and degrades to `message'."
  :type 'function
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

(defvar-local copilot-chat--turns nil
  "Completed turns of this chat session, oldest first.
A list of (REQUEST . RESPONSE) string pairs, extended when a turn's
final progress notification arrives.  Used for saving the session to
disk and for seeding a restored conversation.")

(defvar-local copilot-chat--current-request nil
  "Request text of the turn currently being answered, or nil.
Paired with the accumulated reply and appended to
`copilot-chat--turns' when the turn ends.")

(defvar-local copilot-chat--reply-chunks nil
  "Reply chunks streamed for the current turn, most recent first.")

(defvar-local copilot-chat--turn-start-time nil
  "`float-time' when the current streamed turn began, or nil.
Set at the progress `begin' notification and read at `end' to decide
whether the turn ran long enough to warrant a desktop notification.")

(defvar-local copilot-chat--restored-turns nil
  "Turns restored by `copilot-chat-restore', pending replay.
When non-nil, the next `conversation/create' replays them (requests
and responses) ahead of the new message so the server reconstructs the
saved context.  Cleared once a conversation is created from them.")

(defvar-local copilot-chat--session-root nil
  "History root this chat session is pinned to, once resolved.
Either a workspace root string or the symbol `global'.  Resolved at
most once per session (at restore time or on the first save) and
reused for every later save, so that saving and restoring can never
silently target two different history files: the save runs in the chat
buffer while a restore may be invoked from a project buffer, and the
two would otherwise each resolve their own root.")

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

;;
;; Chat mode selection
;;

(defvar copilot-chat--mode nil
  "The chat mode selected with `copilot-chat-select-mode', or nil.
Holds the mode plist reported by the server (with `:id', `:name',
`:kind', and `:isBuiltIn').  When nil, the mode falls back to
`copilot-chat-use-agent-mode' (Agent when non-nil, otherwise Ask).")

(defvar copilot-chat--modes nil
  "Cached chat modes from `conversation/modes'.
The set is static per session, so it is fetched once.")

(defun copilot-chat--builtin-modes ()
  "Return the built-in chat modes as a fallback list of plists.
Used when the server reports no modes so a selection is still possible."
  (list (list :id "ask" :name "Ask" :kind "Ask" :isBuiltIn t)
        (list :id "agent" :name "Agent" :kind "Agent" :isBuiltIn t)
        (list :id "inline-agent" :name "InlineAgent" :kind "InlineAgent"
              :isBuiltIn t)))

(defun copilot-chat--available-modes ()
  "Return the chat modes the server reports, or nil when unavailable.
Query `conversation/modes' at most once per session and cache a
successful result.  A successful query that yields no modes falls back
to the built-in modes.  A failed query returns nil and is not cached, so
it can be retried once the connection is up."
  (or copilot-chat--modes
      (setq copilot-chat--modes
            (condition-case err
                (let* ((root (copilot--workspace-root))
                       (params
                        (when root
                          (list :workspaceFolders
                                (vector (list :uri (copilot--path-to-uri root)
                                              :name (file-name-nondirectory
                                                     (directory-file-name
                                                      root)))))))
                       (modes (append (copilot--request 'conversation/modes
                                                        params)
                                      nil)))
                  (or modes (copilot-chat--builtin-modes)))
              (error
               (copilot--log 'warning "Could not fetch chat modes: %S" err)
               nil)))))

(defun copilot-chat--effective-mode ()
  "Return the effective chat mode as a plist.
The plist has `:kind' (one of \"Ask\", \"Agent\", or \"InlineAgent\") and
`:custom-id', a string for a non-built-in custom mode and nil otherwise.
Derived from the mode chosen with `copilot-chat-select-mode', falling
back to `copilot-chat-use-agent-mode' (Agent when non-nil, otherwise
Ask) when no mode is selected."
  (if copilot-chat--mode
      (list :kind (plist-get copilot-chat--mode :kind)
            :custom-id (unless (eq (plist-get copilot-chat--mode :isBuiltIn) t)
                         (plist-get copilot-chat--mode :id)))
    (list :kind (if copilot-chat-use-agent-mode "Agent" "Ask"))))

(defun copilot-chat--agent-mode-p ()
  "Return non-nil when the effective chat mode is an agent-kind mode.
Both \"Agent\" and \"InlineAgent\" are agent-kind: the server maps them
to the same agent machinery, so tool registration, tool-call
confirmation, and the tool-support warning apply to both."
  (and (member (plist-get (copilot-chat--effective-mode) :kind)
               '("Agent" "InlineAgent"))
       t))

(defun copilot-chat--effective-mode-name ()
  "Return a human-readable name for the effective chat mode."
  (let ((name (and copilot-chat--mode (plist-get copilot-chat--mode :name))))
    (if (and (stringp name) (not (string-empty-p name)))
        name
      (plist-get (copilot-chat--effective-mode) :kind))))

(defun copilot-chat--mode-create-params ()
  "Return `conversation/create' params selecting the effective chat mode.
Return nil for the default Ask mode so behavior is unchanged for users
who never pick a mode; otherwise send `chatMode', a `customChatModeId'
for a custom (non-built-in) mode, and `needToolCallConfirmation' for an
agent-kind mode."
  (let* ((mode (copilot-chat--effective-mode))
         (custom-id (plist-get mode :custom-id))
         (agentp (copilot-chat--agent-mode-p)))
    (when (or agentp custom-id)
      (append (list :chatMode (plist-get mode :kind))
              (when custom-id (list :customChatModeId custom-id))
              (when agentp (list :needToolCallConfirmation t))))))

;;;###autoload
(defun copilot-chat-select-mode ()
  "Interactively select the Copilot Chat mode.
Modes (such as `Ask', `Agent', and `InlineAgent', plus any custom
project modes) are fetched from the server.  `Agent' and `InlineAgent'
are agent-kind modes that let Copilot run tools; `InlineAgent' uses a
restricted tool set aimed at inline editing.

The selection takes effect on the next new conversation, since the mode
is fixed when a conversation is created (start one by resetting the chat
with `copilot-chat-reset' or sending a message in a fresh chat)."
  (interactive)
  (let ((modes (copilot-chat--available-modes)))
    (unless modes
      (user-error "Copilot Chat: Could not fetch chat modes"))
    (let* ((choices
            (mapcar (lambda (m)
                      (let ((name (plist-get m :name))
                            (desc (plist-get m :description)))
                        (cons (if (and desc (not (string-empty-p desc)))
                                  (format "%s  %s" name desc)
                                name)
                              m)))
                    modes))
           (choice (completing-read "Chat mode: " choices nil t))
           (mode (cdr (assoc choice choices))))
      (setq copilot-chat--mode mode)
      (message
       "Copilot Chat: Mode set to %s (takes effect on the next new conversation)"
       (plist-get mode :name)))))

(defun copilot-chat--maybe-warn-model-lacks-tools ()
  "Warn when agent mode is on but the active model cannot call tools.
Without tool support the model just tells the user which commands to run
instead of running them, which reads as agent mode not working.  This is
best-effort: it stays silent when tool support can't be determined and
never blocks starting a conversation."
  (when (copilot-chat--agent-mode-p)
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
        (copilot-chat--remove-active-tokens buf)
        ;; A failed turn never reaches the end-progress that would
        ;; clear the capture state; without this the busy guards would
        ;; wedge the chat closed for good.
        (setq copilot-chat--current-request nil
              copilot-chat--reply-chunks nil
              copilot-chat--turn-start-time nil))))
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

(defun copilot-chat--notify-dbus (title body)
  "Raise a D-Bus desktop notification with TITLE and BODY.
Return non-nil on success, nil when D-Bus is unavailable or the call
fails (e.g. a dbus-enabled Emacs with no running session bus), so the
caller can fall through to another backend."
  (and (require 'notifications nil t)
       (fboundp 'notifications-notify)
       (condition-case nil
           (progn (notifications-notify :title title :body body) t)
         (error nil))))

(defun copilot-chat--notify-osascript (title body)
  "Raise a macOS notification with TITLE and BODY via `osascript'.
Return non-nil only when osascript is present and exits successfully,
so the caller can fall through to another backend otherwise."
  (and (eq system-type 'darwin)
       (executable-find "osascript")
       (condition-case nil
           (eql 0 (call-process
                   "osascript" nil nil nil
                   "-e"
                   (format "display notification %s with title %s"
                           (copilot-chat--applescript-quote body)
                           (copilot-chat--applescript-quote title))))
         (error nil))))

(defun copilot-chat--notify (title body)
  "Raise a desktop notification with TITLE and BODY.
Try D-Bus, then `osascript' on macOS, then `message', cascading to the
next backend whenever one is unavailable or fails at runtime (so a
dbus-enabled macOS Emacs with no session bus still reaches osascript).
TITLE and BODY are always passed as data, never as a format string, so
their contents cannot be interpreted as directives."
  (or (copilot-chat--notify-dbus title body)
      (copilot-chat--notify-osascript title body)
      (progn (message "%s: %s" title body) t)))

(defun copilot-chat--applescript-quote (string)
  "Return STRING as a quoted AppleScript string literal.
Backslashes, double quotes, and control characters are escaped so
STRING is treated purely as data by `osascript' and a newline can't
break the literal."
  (let* ((s (replace-regexp-in-string "\\\\" "\\\\\\\\" string t))
         (s (replace-regexp-in-string "\"" "\\\\\"" s t))
         (s (replace-regexp-in-string "\n" "\\\\n" s t))
         (s (replace-regexp-in-string "\r" "\\\\r" s t))
         (s (replace-regexp-in-string "\t" "\\\\t" s t)))
    (concat "\"" s "\"")))

(defun copilot-chat--turn-buffer-focused-p (chat-buf)
  "Return non-nil when CHAT-BUF is the buffer of the selected window.
Only then is the user already watching the chat, so a notification
would be redundant."
  (eq chat-buf (window-buffer (selected-window))))

(defun copilot-chat--maybe-notify-turn-end (chat-buf)
  "Notify that a turn in CHAT-BUF finished, if it is worth surfacing.
The notification fires only when `copilot-chat-notify-after-seconds' is
set, the turn ran at least that long, and CHAT-BUF is not the buffer the
user is currently looking at.  A failing backend is logged and swallowed
so it never disrupts the end of a turn."
  (when (and (numberp copilot-chat-notify-after-seconds)
             copilot-chat--turn-start-time
             (not (copilot-chat--turn-buffer-focused-p chat-buf)))
    (let ((elapsed (- (float-time) copilot-chat--turn-start-time)))
      (when (>= elapsed copilot-chat-notify-after-seconds)
        (condition-case err
            (funcall copilot-chat-notify-function
                     "Copilot Chat"
                     (format "Response ready (%ds)" (round elapsed)))
          (error
           (copilot--log 'warning "Chat notification failed: %S" err)))))))

(defun copilot-chat--handle-buffer-progress (token chat-buf value)
  "Stream the progress VALUE of turn TOKEN into CHAT-BUF."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (let ((kind (plist-get value :kind)))
        (cond
         ((equal kind "begin")
          (setq copilot-chat--streaming-p t)
          (setq copilot-chat--turn-start-time (float-time))
          (force-mode-line-update))
         ((equal kind "report")
          (when-let* ((reply (copilot-chat--extract-reply value)))
            (when copilot-chat--current-request
              (push reply copilot-chat--reply-chunks))
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
                         'face 'copilot-chat-follow-up-face))))
            ;; Record the completed turn in the session log; one-shot
            ;; (function-sink) requests never set a current request, so
            ;; they are never captured here.  An errored or empty turn
            ;; is dropped rather than recorded: replaying a question
            ;; with a blank answer to the server (and re-rendering it on
            ;; restore) helps nobody.
            (when copilot-chat--current-request
              (let ((reply (apply #'concat
                                  (nreverse copilot-chat--reply-chunks))))
                (unless (or error-msg (string-empty-p reply))
                  (setq copilot-chat--turns
                        (append copilot-chat--turns
                                (list (cons copilot-chat--current-request
                                            reply))))
                  (copilot-chat--save-history)))
              (setq copilot-chat--current-request nil
                    copilot-chat--reply-chunks nil)))
          (copilot-chat--scroll-to-bottom)
          (copilot-chat--maybe-notify-turn-end chat-buf)
          (setq copilot-chat--turn-start-time nil)
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
for this tool this session), `edit' (edit its input, then run it once),
or `deny'.  The `edit' choice is offered only for tools copilot.el runs
itself that have a meaningful editable field (see
`copilot-chat--tool-editable-field')."
  (let ((choices
         (append
          '((?y "yes" "Allow this tool call")
            (?n "no" "Decline this tool call")
            (?a "always"
                "Allow this and future calls to this tool this session"))
          (when (copilot-chat--tool-editable-field (plist-get msg :name))
            '((?e "edit"
                  "Edit this tool's input, then allow it once"))))))
    (pcase (car (read-multiple-choice
                 (copilot-chat--confirmation-prompt msg) choices))
      (?y 'allow)
      (?a 'always)
      (?e 'edit)
      (_ 'deny))))

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

(defconst copilot-chat--tool-editable-fields
  '(("run_in_terminal" . :command)
    ("create_file" . :content)
    ("fetch_web_page" . :urls))
  "Map from a client tool's name to the input field the user may edit.
Only the tools copilot.el runs itself appear here, because the server
never reads a modified input back: editing is possible only for input
copilot.el consumes when it invokes the tool.  `get_errors' is omitted,
as its only input is a list of file paths with nothing useful to
hand-edit.")

(defun copilot-chat--tool-editable-field (name)
  "Return the editable input field for tool NAME, or nil.
NAME must be the exact name copilot.el registered (run_in_terminal and
friends): namespaced server tools, even ones whose base name collides
with a client tool, are executed by the server and so cannot be edited."
  (and (member name (copilot-chat--client-tool-names))
       (cdr (assoc name copilot-chat--tool-editable-fields))))

(defvar copilot-chat--tool-edits nil
  "Alist of pending user-edited tool inputs, keyed by tool call.
Populated when the user picks `edit' at a confirmation prompt and
consumed when the matching `conversation/invokeClientTool' request
arrives, so the edited input is used in place of the server's.  Kept as
global state because both the confirmation and the invocation are handled
in the JSON-RPC dispatcher rather than in the chat buffer.")

(defun copilot-chat--tool-edit-key (msg)
  "Return the key that ties a stashed edit to its later invocation for MSG.
Prefer the server's `toolCallId', which is carried on both the
confirmation and the invocation request; fall back to the tool name,
which is safe because the server processes tool calls sequentially,
awaiting each confirmation before invoking."
  (or (plist-get msg :toolCallId)
      (copilot-chat--tool-base-name (plist-get msg :name))))

(defun copilot-chat--stash-tool-edit (msg input)
  "Remember edited INPUT for the tool call described by MSG."
  (setf (alist-get (copilot-chat--tool-edit-key msg)
                   copilot-chat--tool-edits nil nil #'equal)
        input))

(defun copilot-chat--take-tool-edit (msg)
  "Return and forget any stashed edited input for MSG, or nil when none."
  (let ((key (copilot-chat--tool-edit-key msg)))
    (prog1 (alist-get key copilot-chat--tool-edits nil nil #'equal)
      (setf (alist-get key copilot-chat--tool-edits nil t #'equal) nil))))

(defvar copilot-chat--tool-edit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key map (kbd "C-c C-k") #'abort-recursive-edit)
    map)
  "Keymap active while editing a multi-line tool argument in a buffer.
Keys it does not bind fall through to the global map, so ordinary text
editing works as usual.")

(defun copilot-chat--edit-in-buffer (initial description)
  "Let the user edit INITIAL in a temporary buffer and return the new text.
DESCRIPTION names the value being edited and appears in the header line.
Editing runs in a recursive edit, keyed by
`copilot-chat--tool-edit-keymap'; cancelling signals `quit'."
  (let ((buf (generate-new-buffer "*copilot-chat-tool-edit*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert (or initial ""))
            (goto-char (point-min))
            (use-local-map copilot-chat--tool-edit-keymap)
            (setq header-line-format
                  (substitute-command-keys
                   (format "Edit %s, then \\[exit-recursive-edit] to accept \
or \\[abort-recursive-edit] to cancel"
                           description))))
          (pop-to-buffer buf)
          (recursive-edit)
          (with-current-buffer buf (buffer-string)))
      (when-let* ((win (get-buffer-window buf)))
        (quit-window nil win))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun copilot-chat--read-tool-field (field current)
  "Read a replacement value for tool input FIELD, defaulting to CURRENT.
Return the value in the shape the field expects: a string for `:command',
a string for `:content', and a vector of strings for `:urls'."
  (pcase field
    (:command (read-string "Copilot Chat: edit command: " current))
    (:content (copilot-chat--edit-in-buffer current "file content"))
    (:urls (vconcat (split-string
                     (read-string "Copilot Chat: edit URLs (space-separated): "
                                  (mapconcat #'identity (append current nil) " "))
                     nil t)))))

(defun copilot-chat--read-tool-edit (msg)
  "Prompt for a new value for MSG's editable field and stash it.
Read a replacement for the field named by
`copilot-chat--tool-editable-field', then remember a copy of the tool
input with that field replaced, keyed so the later invocation picks it
up.  A cancelled edit signals `quit' and leaves the stash untouched."
  (let* ((name (plist-get msg :name))
         (input (plist-get msg :input))
         (field (copilot-chat--tool-editable-field name))
         (new (copilot-chat--read-tool-field field (plist-get input field))))
    (copilot-chat--stash-tool-edit msg (plist-put (copy-sequence input) field new))))

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
        ('edit
         ;; Stash the edited input now; the server reads only the result
         ;; here and sends the input in a separate invocation request.
         ;; A cancelled edit (C-c C-k or C-g) raises `quit', which the
         ;; jsonrpc dispatcher does not turn into a reply, so the server
         ;; would wait forever; treat it as a plain decline instead.
         (condition-case nil
             (progn
               (copilot-chat--read-tool-edit msg)
               (copilot-chat--accept-tool msg))
           (quit (list :result "dismiss"))))
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
        ;; Prefer an input the user edited at confirmation time, if any.
        (input (or (copilot-chat--take-tool-edit msg)
                   (plist-get msg :input))))
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
;; Cloud coding agent messages
;;

(defconst copilot-chat--coding-agent-buffer-name "*copilot-coding-agent*"
  "Name of the buffer collecting messages from GitHub's cloud coding agent.")

(defun copilot-chat--handle-coding-agent-message (msg)
  "Handle a `copilot/codingAgentMessage' request MSG.
The server sends these when a conversation is delegated to GitHub's
cloud coding agent, typically once the agent has opened a pull request
to continue the work in.  Echo the message and record it (with its
description and PR link) in the coding agent buffer."
  (let ((title (plist-get msg :title))
        (description (plist-get msg :description))
        (pr-link (plist-get msg :prLink)))
    (with-current-buffer
        (get-buffer-create copilot-chat--coding-agent-buffer-name)
      ;; Set the mode only when the buffer is first created, so reading
      ;; it isn't disturbed as later messages are appended.
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (let ((inhibit-read-only t))
        ;; Preserve point: the user may be reading the log while the
        ;; cloud agent is still sending messages.
        (save-excursion
          (goto-char (point-max))
          (insert (propertize (format-time-string "[%Y-%m-%d %H:%M:%S] ")
                              'face 'shadow)
                  (propertize (or title "(untitled)") 'face 'bold)
                  "\n")
          (when (and (stringp description) (not (string-empty-p description)))
            (insert description "\n"))
          (when (and (stringp pr-link) (not (string-empty-p pr-link)))
            (insert pr-link "\n"))
          (insert "\n"))))
    (message "Copilot: %s%s"
             (or title "coding agent update")
             (if (and (stringp pr-link) (not (string-empty-p pr-link)))
                 (format " (%s)" pr-link)
               ""))
    (list :success t)))

(copilot-on-request 'copilot/codingAgentMessage
                    #'copilot-chat--handle-coding-agent-message)

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
CALLBACK is called with the response containing conversationId and
turnId.  Turns restored by `copilot-chat-restore' are replayed ahead
of MESSAGE, so the new conversation picks up the saved context."
  (let ((token (format "copilot-chat-%s" (float-time)))
        (restored nil))
    ;; A fresh conversation starts with a clean slate of session approvals
    ;; and no leftover tool-input edits.
    (setq copilot-chat--session-approved-tools nil)
    (setq copilot-chat--tool-edits nil)
    (copilot-chat--maybe-warn-model-lacks-tools)
    (with-current-buffer (get-buffer copilot-chat--buffer-name)
      (setq copilot-chat--work-done-token token)
      (setq copilot-chat--current-request message
            copilot-chat--reply-chunks nil)
      (setq restored copilot-chat--restored-turns)
      (push (cons token (current-buffer)) copilot-chat--active-buffers))
    (let ((req-id
           (copilot--async-request
            'conversation/create
            (append
             (list :workDoneToken token
                   :turns (copilot-chat--turns-param restored message)
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
             (copilot-chat--mode-create-params)
             (copilot-chat--references-param))
            :success-fn (lambda (result)
                          (when-let* ((buf (get-buffer
                                            copilot-chat--buffer-name)))
                            (with-current-buffer buf
                              (copilot-chat--consume-references)
                              ;; The restored turns are now part of the
                              ;; server-side conversation; replaying them
                              ;; again would duplicate the context.  Only
                              ;; clear the exact set this create sent: a
                              ;; restore run in the meantime must not
                              ;; have its freshly stashed turns dropped
                              ;; by a stale response.
                              (when (eq copilot-chat--restored-turns
                                        restored)
                                (setq copilot-chat--restored-turns nil))))
                          (when (copilot-chat--agent-mode-p)
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
      (setq copilot-chat--current-request message
            copilot-chat--reply-chunks nil)
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
  ;; Pending tool-input edits belong to the conversation being torn down;
  ;; never let one leak into the next.
  (setq copilot-chat--tool-edits nil)
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

(defun copilot-chat--insert-prompt-markdown (message)
  "Insert the markdown prompt scaffolding for MESSAGE at point.
Write the bold `You:' label, the message, and the bold `Copilot:'
label; the streamed reply is appended after it."
  (insert (propertize "You:" 'face 'bold) "\n"
          message "\n\n"
          (propertize "Copilot:" 'face 'bold) "\n"))

(defun copilot-chat--insert-prompt-org (message)
  "Insert the Org prompt scaffolding for MESSAGE at point.
Write a top-level `* You' heading holding the message, then a nested
`** Copilot' heading; the streamed reply is appended under it, and
`outline-minor-mode' can fold either heading."
  (insert "* You\n"
          message "\n\n"
          "** Copilot\n"))

(defun copilot-chat--insert-prompt (message)
  "Insert a user prompt with MESSAGE into the chat buffer.
The markup follows `copilot-chat-frontend': the markdown frontend writes
the `You:'/`Copilot:' scaffolding, the org frontend writes `* You' and
`** Copilot' headings."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (pcase copilot-chat-frontend
      ('org (copilot-chat--insert-prompt-org message))
      (_ (copilot-chat--insert-prompt-markdown message)))))

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
;; Session persistence
;;

(defun copilot-chat--history-root ()
  "Return the workspace root the chat session belongs to, or nil.
The chat buffer visits no file, so `copilot--workspace-root' returns
nil there; fall back to the root of the live source buffer in that
case."
  (or (copilot--workspace-root)
      (when (buffer-live-p copilot-chat--source-buffer)
        (with-current-buffer copilot-chat--source-buffer
          (copilot--workspace-root)))))

(defun copilot-chat--ensure-session-root ()
  "Return the history root this session is pinned to, resolving once.
Return a workspace root string, or nil for the global history.  The
first call resolves via `copilot-chat--history-root' and pins the
result in `copilot-chat--session-root'; later calls (and saves after a
restore, which pins the root itself) reuse it."
  (unless copilot-chat--session-root
    (setq copilot-chat--session-root
          (or (copilot-chat--history-root) 'global)))
  (unless (eq copilot-chat--session-root 'global)
    copilot-chat--session-root))

(defun copilot-chat--history-file (root)
  "Return the chat history file for the workspace ROOT.
One file per workspace root, named after the root's SHA-1 (or
\"global\" when ROOT is nil), under `copilot-chat-history-directory'."
  (expand-file-name (concat (if root (sha1 root) "global") ".eld")
                    copilot-chat-history-directory))

(defun copilot-chat--save-history ()
  "Save this session's turn log to the workspace history file.
A no-op unless `copilot-chat-save-history' is enabled and there is
something to save.  The transcript is private data, so the directory
and file are created unreadable to other users.  Never signals: chat
must keep flowing even when the transcript cannot be written, so any
error is only logged."
  (when (and copilot-chat-save-history copilot-chat--turns)
    (condition-case err
        (let ((print-length nil)
              (print-level nil)
              (root (copilot-chat--ensure-session-root)))
          (with-file-modes #o700
            (make-directory copilot-chat-history-directory t))
          (with-file-modes #o600
            (write-region
             (prin1-to-string
              (list :version 1
                    :timestamp (format-time-string "%FT%T%z")
                    :workspace root
                    :turns copilot-chat--turns))
             nil (copilot-chat--history-file root) nil 'silent)))
      (error
       (copilot--log 'error "Could not save chat history: %S" err)))))

(defun copilot-chat--valid-history-p (data)
  "Return non-nil when DATA is a well-formed saved chat history."
  (and (proper-list-p data)
       (eql (plist-get data :version) 1)
       (let ((turns (plist-get data :turns)))
         (and (proper-list-p turns)
              turns
              (seq-every-p (lambda (turn)
                             (and (consp turn)
                                  (stringp (car turn))
                                  (stringp (cdr turn))))
                           turns)))))

(defun copilot-chat--read-history (root)
  "Read the saved chat history for the workspace ROOT.
Return its plist, with every turn's text stripped of text properties:
`read' happily reconstructs propertized strings, and a tampered (or
merely synced) history file must not be able to inject `keymap',
`display', or `read-only' properties into the chat buffer.  Signal a
`user-error' when there is no history file, or when the file does not
contain a well-formed history."
  (let ((file (copilot-chat--history-file root)))
    (unless (file-exists-p file)
      (user-error "Copilot Chat: No saved chat history for this workspace"))
    (let ((data (condition-case nil
                    (with-temp-buffer
                      (insert-file-contents file)
                      (read (current-buffer)))
                  (error nil))))
      (unless (copilot-chat--valid-history-p data)
        (user-error "Copilot Chat: History file %s is corrupt" file))
      (plist-put data :turns
                 (mapcar (lambda (turn)
                           (cons (substring-no-properties (car turn))
                                 (substring-no-properties (cdr turn))))
                         (plist-get data :turns))))))

(defun copilot-chat--turns-param (restored message)
  "Return the turn vector for a new conversation asking MESSAGE.
RESTORED is a list of saved (REQUEST . RESPONSE) pairs replayed, with
their responses, ahead of MESSAGE so the server reconstructs their
context; the final turn is the one it answers."
  (vconcat
   (mapcar (lambda (turn)
             (list :request (car turn) :response (cdr turn)))
           restored)
   (vector (list :request message :response "" :turnId ""))))

;;;###autoload
(defun copilot-chat-restore ()
  "Restore the saved chat transcript for the current workspace.
Render the saved conversation in the chat buffer without contacting
the server; the next message starts a new conversation that replays
the saved turns first, so Copilot answers with the full context.
Signal a `user-error' when there is no saved history."
  (interactive)
  ;; Resolve the root in the invoking buffer, where the user's notion
  ;; of "this workspace" lives, and pin it on the chat session below so
  ;; later saves target the very same file.
  (let* ((root (copilot-chat--history-root))
         (data (copilot-chat--read-history root))
         (turns (plist-get data :turns))
         (chat-buf (get-buffer-create copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-chat-mode)
        (copilot-chat-mode))
      (when (or copilot-chat--streaming-p copilot-chat--current-request)
        (user-error "Copilot Chat: A response is currently being streamed"))
      ;; Drop any live conversation: the next message must start a new
      ;; one seeded with the restored turns.
      (copilot-chat--destroy)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (turn turns)
          (copilot-chat--insert-prompt (car turn))
          (goto-char (point-max))
          (insert (cdr turn) "\n\n")))
      (setq copilot-chat--turns turns)
      (setq copilot-chat--restored-turns turns)
      (setq copilot-chat--session-root (or root 'global))
      (setq copilot-chat--current-request nil
            copilot-chat--reply-chunks nil))
    (display-buffer chat-buf)
    (message
     "Copilot Chat: Restored %d turn%s; the next message continues the conversation"
     (length turns) (if (= (length turns) 1) "" "s"))))

;;;###autoload
(defun copilot-chat-clear-history ()
  "Delete the saved chat history file for the current workspace.
The conversation in the chat buffer is left alone; this only removes
the file `copilot-chat-restore' would read."
  (interactive)
  (let ((file (copilot-chat--history-file (copilot-chat--history-root))))
    (unless (file-exists-p file)
      (user-error "Copilot Chat: No saved chat history for this workspace"))
    (when (yes-or-no-p
           "Delete the saved Copilot Chat history for this workspace? ")
      (delete-file file)
      (message "Copilot Chat: Saved chat history deleted"))))

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
;; Region rewrite
;;

(defconst copilot-chat--rewrite-preview-buffer-name
  "*copilot-chat-rewrite-preview*"
  "Name of the buffer used to preview a pending region rewrite.")

(defun copilot-chat--rewrite-request (instruction lang code)
  "Return the chat message asking to rewrite CODE per INSTRUCTION.
The message starts with `copilot-chat-rewrite-prompt' and carries CODE
in a fenced block tagged with the language id LANG.  The fence is made
longer than any backtick run inside CODE, so rewriting markdown that
contains its own fenced blocks doesn't terminate the fence early."
  (let ((longest-run 0)
        (pos 0))
    (while (string-match "`+" code pos)
      (setq longest-run (max longest-run
                             (- (match-end 0) (match-beginning 0))))
      (setq pos (match-end 0)))
    (let ((fence (make-string (max 3 (1+ longest-run)) ?`)))
      (format "%s\n\nInstruction: %s\n\n%s%s\n%s\n%s"
              copilot-chat-rewrite-prompt instruction fence lang code
              fence))))

(defun copilot-chat--rewrite-confirm (original proposal)
  "Preview PROPOSAL replacing ORIGINAL and ask whether to apply it.
Show a diff-style preview (removed region, then proposed replacement)
in a temporary window and prompt with `y-or-n-p'.  Return the answer;
the preview buffer is removed afterwards.  The current buffer is
preserved: tearing down the preview window can leave another buffer
current (e.g. when the preview reused the sole window), and the caller
is in the middle of handling an async reply."
  ;; A fresh buffer per call, so an overlapping confirmation can't erase
  ;; or kill the preview out from under this prompt.
  (let ((buf (generate-new-buffer copilot-chat--rewrite-preview-buffer-name))
        (original-buffer (current-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert (copilot-chat--diff-lines
                     original "-" 'copilot-chat-diff-removed-face)
                    "\n"
                    (copilot-chat--diff-lines
                     proposal "+" 'copilot-chat-diff-added-face))
            (goto-char (point-min))
            (setq buffer-read-only t))
          (display-buffer buf '((display-buffer-pop-up-window
                                 display-buffer-use-some-window)
                                (window-height . fit-window-to-buffer)))
          (y-or-n-p "Copilot Chat: Apply this rewrite? "))
      (when-let* ((win (get-buffer-window buf)))
        (quit-window nil win))
      (kill-buffer buf)
      (when (buffer-live-p original-buffer)
        (set-buffer original-buffer)))))

(defun copilot-chat--rewrite-apply (buf start end reply)
  "Replace the region between markers START and END in BUF with REPLY.
Re-indent the inserted text afterwards unless
`copilot-chat-rewrite-indent' is nil.  Widen first: the buffer may be
narrowed to a region excluding the target when the reply arrives."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert reply)
        (when copilot-chat-rewrite-indent
          (indent-region start (point)))))))

(defun copilot-chat--rewrite-region-intact-p (buf start end original)
  "Return non-nil when ORIGINAL is still the text between START and END in BUF.
Widen before comparing: the reply arrives asynchronously and the buffer
may be narrowed to a region that excludes the rewrite target, which
must not signal `args-out-of-range' in the process filter."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (save-restriction
           (widen)
           (string= original
                    (buffer-substring-no-properties start end))))))

(defun copilot-chat--rewrite-reply (buf start end original reply error-msg)
  "Handle the rewrite REPLY for the region ORIGINAL in BUF.
START and END are markers delimiting the region; ERROR-MSG is the error
string from the one-shot request, or nil.  Preview the proposal and
apply it on confirmation; when BUF is gone, the region's text no longer
matches ORIGINAL, or the reply is empty or errored, report the reason
and leave the buffer alone.  The intactness check runs again after the
confirmation: `y-or-n-p' blocks while timers and process filters keep
running (auto-revert, other replies), so the region can change between
the first check and the user's answer.  The markers are cleared either
way, including on a quit at the prompt."
  (unwind-protect
      (let ((reply (and reply (copilot-chat--strip-code-fences reply))))
        (cond
         (error-msg
          (message "Copilot Chat: Rewrite failed: %s" error-msg))
         ((or (null reply) (string-empty-p reply))
          (message "Copilot Chat: Got an empty rewrite"))
         ((not (copilot-chat--rewrite-region-intact-p buf start end original))
          (message
           "Copilot Chat: Region changed since the request; rewrite dropped"))
         ((buffer-local-value 'buffer-read-only buf)
          ;; Don't signal inside the jsonrpc process filter; salvage the
          ;; reply instead.
          (kill-new reply)
          (message
           "Copilot Chat: Buffer is read-only; rewrite copied to kill ring"))
         ((not (copilot-chat--rewrite-confirm original reply))
          (message "Copilot Chat: Rewrite discarded"))
         ((not (copilot-chat--rewrite-region-intact-p buf start end original))
          (message
           "Copilot Chat: Region changed during confirmation; rewrite dropped"))
         (t
          ;; Read-only text properties inside an otherwise writable
          ;; buffer still make `delete-region' signal; degrade to the
          ;; kill-ring salvage instead of erroring in the process
          ;; filter.
          (condition-case nil
              (progn
                (copilot-chat--rewrite-apply buf start end reply)
                (message "Copilot Chat: Region rewritten"))
            (text-read-only
             (kill-new reply)
             (message
              "Copilot Chat: Region is read-only; rewrite copied to kill ring"))))))
    (set-marker start nil)
    (set-marker end nil)))

;;;###autoload
(defun copilot-chat-rewrite (start end instruction)
  "Rewrite the region between START and END according to INSTRUCTION.
The region's code is sent to Copilot with INSTRUCTION (read from the
minibuffer when called interactively) and the buffer's language.  The
reply is previewed as a diff against the region and applied only after
confirmation; the region is tracked with markers, so edits elsewhere in
the buffer while the request is in flight don't shift the target, while
edits to the region itself drop the rewrite.  The request runs outside
the chat panel and does not disturb an ongoing chat conversation; cancel
it with `copilot-chat-stop'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Rewrite instruction: "))
     (user-error "Copilot Chat: No active region")))
  (when (string-empty-p (string-trim instruction))
    (user-error "Copilot Chat: Empty rewrite instruction"))
  (when (= start end)
    (user-error "Copilot Chat: The region is empty"))
  (let* ((buf (current-buffer))
         (code (buffer-substring-no-properties start end))
         (request (copilot-chat--rewrite-request
                   instruction (copilot--get-language-id) code))
         (beg (copy-marker start))
         (fin (copy-marker end)))
    (message "Copilot Chat: Rewriting region...")
    (copilot-chat--one-shot
     request
     (lambda (reply error-msg)
       (copilot-chat--rewrite-reply buf beg fin code reply error-msg)))))

;;
;; Native code review
;;

(defconst copilot-chat--review-max-file-bytes 1000000
  "Largest file, in bytes, included in a native code review request.")

(defun copilot-chat--repo-root ()
  "Return the root directory of the enclosing git repository.
Run git in `default-directory' (via `process-file', so remote buffers
resolve the remote repository) and put any remote prefix back onto the
path git reports.  Signal a `user-error' outside a repository."
  (let* ((exit nil)
         (out (with-temp-buffer
                (setq exit (process-file "git" nil t nil
                                         "rev-parse" "--show-toplevel"))
                (buffer-string))))
    (unless (eql exit 0)
      (user-error "Copilot Chat: Not inside a git repository"))
    (concat (file-remote-p default-directory)
            (file-name-as-directory (string-trim out)))))

(defun copilot-chat--changed-files ()
  "Return the repository-relative paths of the files modified since HEAD.
List the files `git diff HEAD' touches, staged or not.  Deleted files
are excluded (a review needs the file's new content), and renames are
reported as a delete plus an add for the same reason.  Signal a
`user-error' when git fails or when nothing has changed."
  (unless (eql 0 (process-file "git" nil nil nil
                               "rev-parse" "--verify" "--quiet" "HEAD"))
    (user-error
     "Copilot Chat: The repository has no commits yet, so there is no base to review against"))
  (let* ((exit nil)
         (out (with-temp-buffer
                ;; --no-relative: with diff.relative set, paths would
                ;; come back relative to the invoking subdirectory and
                ;; never resolve against the repository root.
                (setq exit (process-file "git" nil t nil
                                         "diff" "--name-only" "--no-renames"
                                         "--no-relative"
                                         "--diff-filter=d" "-z" "HEAD"))
                (buffer-string))))
    (unless (eql exit 0)
      (user-error
       "Copilot Chat: git diff failed (%s); not inside a git repository?"
       exit))
    (let ((files (seq-remove #'string-empty-p (split-string out "\0"))))
      (unless files
        (user-error "Copilot Chat: No uncommitted changes to review"))
      files)))

(defun copilot-chat--review-file-text (path)
  "Return the contents of PATH decoded as UTF-8, or nil.
Return nil when the file is unreadable, larger than
`copilot-chat--review-max-file-bytes', or binary (contains NUL bytes),
none of which belong in a review request."
  (when (and (file-regular-p path)
             (file-readable-p path)
             (when-let* ((size (file-attribute-size (file-attributes path))))
               (<= size copilot-chat--review-max-file-bytes)))
    (let ((coding-system-for-read 'utf-8))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (unless (search-forward "\0" nil t)
          (buffer-string))))))

(defun copilot-chat--review-base-content (path)
  "Return PATH's content at HEAD, or an empty string.
PATH is relative to the repository root.  A file that does not exist at
HEAD (newly added) yields the empty string, which the review request
uses to mean the whole file is new."
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (if (eql 0 (process-file "git" nil t nil "show" (concat "HEAD:" path)))
          (buffer-string)
        ""))))

(defun copilot-chat--uncommitted-changes (root)
  "Return the uncommitted diff under ROOT as review request entries.
Each element is a plist with the :uri, :path, :baseContent, and
:headContent fields the `copilot/codeReview/reviewChanges' method
expects.  Binary and oversized files are skipped."
  (delq nil
        (mapcar
         (lambda (path)
           (let ((file (expand-file-name path root)))
             (when-let* ((head (copilot-chat--review-file-text file)))
               (list :uri (copilot--path-to-uri file)
                     :path path
                     :baseContent (copilot-chat--review-base-content path)
                     :headContent head))))
         (copilot-chat--changed-files))))

(defun copilot-chat--review-workspace-folders (root)
  "Return the workspace-folders vector for a review of ROOT."
  (vector (list :uri (copilot--path-to-uri (directory-file-name root))
                :name (file-name-nondirectory (directory-file-name root)))))

(defun copilot-chat--review-comment-location (comment root)
  "Return a \"path:line\" label for review COMMENT.
The comment's file URI is shortened relative to ROOT when it lies
inside it.  The server reports zero-based lines; the label shows the
one-based line the user knows."
  (let* ((path (copilot--uri-to-path (or (plist-get comment :uri) "")))
         (start (plist-get (plist-get comment :range) :start))
         (line (1+ (or (plist-get start :line) 0))))
    (format "%s:%d"
            (if (and root (string-prefix-p root path))
                (file-relative-name path root)
              path)
            line)))

(defun copilot-chat--format-review-comment (comment root)
  "Return review COMMENT rendered as markdown for the chat buffer.
ROOT is the directory file paths are shortened against."
  (let ((text (or (plist-get comment :message) ""))
        (kind (plist-get comment :kind))
        (suggestion (plist-get comment :suggestion)))
    (concat (format "**%s**" (copilot-chat--review-comment-location comment root))
            (when (and (stringp kind) (not (string-empty-p kind)))
              (format " [%s]" kind))
            "\n" text "\n"
            (when (and (stringp suggestion) (not (string-empty-p suggestion)))
              ;; The fence must outgrow any backtick run inside the
              ;; suggestion (e.g. when reviewing markdown), or the
              ;; block terminates early.
              (let* ((longest 0)
                     (pos 0))
                (while (string-match "`+" suggestion pos)
                  (setq longest (max longest (- (match-end 0)
                                                (match-beginning 0))))
                  (setq pos (match-end 0)))
                (let ((fence (make-string (max 3 (1+ longest)) ?`)))
                  (format "\nSuggested change:\n\n%s\n%s\n%s\n"
                          fence suggestion fence)))))))

(defun copilot-chat--insert-review-results (comments root)
  "Render the review COMMENTS in the chat buffer.
COMMENTS is the (possibly empty) list of structured comments a
`copilot/codeReview' request returned; ROOT is the directory their file
paths are shortened against."
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert
         (if comments
             (mapconcat (lambda (comment)
                          (copilot-chat--format-review-comment comment root))
                        comments "\n")
           "No review comments; the changes look good.\n")
         "\n"))
      (copilot-chat--scroll-to-bottom))))

(defun copilot-chat--request-review (method params request root)
  "Send review METHOD with PARAMS and render its comments when they arrive.
REQUEST is the transcript line describing what is being reviewed, shown
in the chat buffer while the review runs; ROOT is the directory used to
shorten file paths in the rendered comments.  The review runs outside
the chat conversation, so it neither consumes a chat turn nor disturbs
one in flight."
  (let ((chat-buf (get-buffer-create copilot-chat--buffer-name)))
    (with-current-buffer chat-buf
      (unless (derived-mode-p 'copilot-chat-mode)
        (copilot-chat-mode))
      ;; Reviews and chat replies both append at point-max; letting a
      ;; review start while an answer streams would splice their output
      ;; together.
      (when copilot-chat--streaming-p
        (user-error "Copilot Chat: A response is currently being streamed"))
      (copilot-chat--insert-prompt request)
      (copilot-chat--scroll-to-bottom))
    (display-buffer chat-buf)
    (message "Copilot Chat: Requesting code review (this can take a while)...")
    ;; Issue the request from the chat buffer: the response callback is
    ;; dropped when the buffer current at request time has been killed,
    ;; and a review can run long enough for the invoking buffer to be
    ;; gone by then.
    (with-current-buffer chat-buf
      (copilot--async-request
       method params
       ;; jsonrpc's 10s default would silently drop most real reviews:
       ;; the server's own review pipeline allows 120s.
       :timeout 130
       :timeout-fn (lambda ()
                     (copilot-chat--insert-error "Code review timed out")
                     (message "Copilot Chat: Code review timed out"))
       :success-fn (lambda (result)
                     (copilot-chat--insert-review-results
                      (append (plist-get result :comments) nil) root)
                     (message "Copilot Chat: Code review finished"))
       :error-fn (lambda (err)
                   (let ((msg (or (copilot-chat--error-text err)
                                  (format "%s" err))))
                     (copilot-chat--insert-error msg)
                     (message "Copilot Chat: Code review failed: %s" msg)))))))

;;;###autoload
(defun copilot-chat-review-changes ()
  "Run Copilot's native code review over the uncommitted diff.
Collect the working tree's staged and unstaged edits against HEAD and
send them to the language server's `copilot/codeReview/reviewChanges'
method, the reviewer behind GitHub's Copilot Code Review.  The comments
it returns (file, line, message, and a suggested change when there is
one) are rendered in the chat buffer.

Signal a `user-error' when there is nothing to review; when the account
has no access to Copilot Code Review, the server's error is shown in
the chat buffer instead."
  (interactive)
  (let* ((root (copilot-chat--repo-root))
         (changes (copilot-chat--uncommitted-changes root)))
    (unless changes
      (user-error
       "Copilot Chat: Only binary or oversized files changed; nothing to review"))
    (copilot-chat--request-review
     'copilot/codeReview/reviewChanges
     (list :changes (vconcat changes)
           :workspaceFolders (copilot-chat--review-workspace-folders root))
     (format "Review the uncommitted changes (%d file%s)"
             (length changes) (if (= (length changes) 1) "" "s"))
     root)))

;;;###autoload
(defun copilot-chat-review-region (start end)
  "Review the region between START and END with Copilot's native code review.
Unlike `copilot-chat-review', which asks the chat model for prose
feedback, this sends the selection (widened to whole lines) to the
language server's dedicated `copilot/codeReview/reviewSnippets' method
and renders the structured comments it returns (file, line, message,
and suggested change) in the chat buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (user-error "Copilot Chat: No active region")))
  (unless buffer-file-name
    (user-error "Copilot Chat: Buffer is not visiting a file"))
  (let* ((snip-beg (save-excursion
                     (goto-char start) (line-beginning-position)))
         ;; A region ending at the start of a line does not include that
         ;; line, so back the end up onto the last selected line.
         (snip-end (save-excursion
                     (goto-char end)
                     (when (and (> end start) (bolp)) (forward-line -1))
                     (line-end-position)))
         ;; Absolute line numbers: under narrowing the default counts
         ;; from the accessible region's start, not the file's.
         (start-line (line-number-at-pos snip-beg t))
         (end-line (line-number-at-pos snip-end t))
         (path (copilot--get-relative-path))
         (root (or (copilot--workspace-root)
                   (file-name-directory buffer-file-name))))
    (copilot-chat--request-review
     'copilot/codeReview/reviewSnippets
     (list :snippets (vector
                      (list :uri (copilot--path-to-uri buffer-file-name)
                            :path path
                            :content (buffer-substring-no-properties
                                      snip-beg snip-end)
                            :startLine start-line
                            :endLine end-line))
           :workspaceFolders (copilot-chat--review-workspace-folders root))
     (format "Review %s, lines %d-%d" path start-line end-line)
     root)))

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
Show the effective chat mode (Agent, InlineAgent, Ask, or a selected
custom mode), the active model, and for an agent-kind mode the number of
available tools.  Built from variables already in memory
\(`copilot-chat-model', the cached `copilot-chat--resolved-model' and
`copilot-chat--mcp-servers') plus a memoized client-tool count, so it
is cheap enough to evaluate on every redisplay and never contacts the
server."
  (let ((segments
         (list (format "Copilot Chat  %s mode"
                       (copilot-chat--effective-mode-name))
               (or copilot-chat-model
                   copilot-chat--resolved-model
                   "default model")
               (when (copilot-chat--agent-mode-p)
                 (format "%d tools" (copilot-chat--tool-count))))))
    (propertize (string-join (delq nil segments) "  •  ")
                'face 'copilot-chat-status-header-face)))

(declare-function gfm-mode "ext:markdown-mode" ())
(declare-function org-set-font-lock-defaults "org" ())
(declare-function outline-minor-mode "outline" (&optional arg))
(defvar org-font-lock-keywords)
(defvar org-inhibit-startup)

(defun copilot-chat--setup-markdown-font-lock ()
  "Set up GFM font-lock for the markdown frontend.
When `markdown-mode' is available, borrow its GFM `font-lock-defaults'
and enable native code-block fontification; otherwise leave the basic
highlighting in place."
  (when (require 'markdown-mode nil t)
    (setq-local markdown-fontify-code-blocks-natively t)
    (setq-local font-lock-defaults
                (with-temp-buffer
                  (gfm-mode)
                  font-lock-defaults))
    (font-lock-flush)))

(defun copilot-chat--org-outline-level ()
  "Return the outline level of the Org heading at point.
The level is the number of leading asterisks, so a `* You' heading folds
above its nested `** Copilot' reply."
  (- (match-end 0) (match-beginning 0) 1))

(defun copilot-chat--setup-org-font-lock ()
  "Set up Org font-lock and outline folding for the org frontend.
When Org is available, install Org's own font-lock keywords in this
buffer so headings, emphasis, and code render like Org; otherwise leave
the basic highlighting in place.  Either way enable `outline-minor-mode'
so the emitted `* You'/`** Copilot' headings fold."
  ;; Org keeps its keywords in the buffer-local `org-font-lock-keywords',
  ;; which only `org-set-font-lock-defaults' populates; borrowing
  ;; `font-lock-defaults' from a temp Org buffer would just copy the
  ;; globally nil symbol and highlight nothing, so set them up here.
  ;; `org-set-font-lock-defaults' needs `org-element' loaded.
  (when (and (require 'org nil t) (require 'org-element nil t))
    (setq-local org-inhibit-startup t)
    (org-set-font-lock-defaults)
    (setq-local font-lock-defaults '(org-font-lock-keywords t))
    (font-lock-flush))
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-level #'copilot-chat--org-outline-level)
  (outline-minor-mode 1))

(defun copilot-chat--setup-mode ()
  "Set up font-lock, mode-line, header-line, and visual-line settings.
Used by `copilot-chat-mode'.  Font-lock follows `copilot-chat-frontend':
the markdown frontend borrows GFM highlighting, the org frontend borrows
Org highlighting and enables outline folding.  In both cases the buffer
stays `special-mode'-derived and read-only."
  (pcase copilot-chat-frontend
    ('org (copilot-chat--setup-org-font-lock))
    (_ (copilot-chat--setup-markdown-font-lock)))
  (font-lock-add-keywords nil copilot-chat--font-lock-keywords t)
  (setq mode-name '(:eval (copilot-chat--mode-line)))
  (when copilot-chat-show-status-header
    (setq header-line-format '(:eval (copilot-chat--status-header))))
  (visual-line-mode 1)
  (setq word-wrap t))

(define-derived-mode copilot-chat-mode special-mode "Copilot-Chat"
  "Major mode for Copilot Chat.
The buffer is rendered according to `copilot-chat-frontend': the
markdown frontend gets GFM highlighting (via `markdown-mode' when
installed), the org frontend gets Org highlighting and outline folding.
In either case basic font-lock is used when the backing library is
unavailable.

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
      ;; Refuse to start a turn while one is in flight: with a single
      ;; set of per-buffer capture slots, an interleaved turn would
      ;; pair the previous request with the next response in the
      ;; session log (and lose the new turn's answer entirely).
      (when (or copilot-chat--streaming-p copilot-chat--current-request)
        (user-error "Copilot Chat: A response is currently being streamed"))
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
Filter to the scope matching the effective mode (`agent-panel' for an
agent-kind mode, otherwise `chat-panel').  The server query is made at
most once per session and cached; a failed query yields no templates
rather than an error."
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
  (let ((scope (if (copilot-chat--agent-mode-p) "agent-panel" "chat-panel")))
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
        (copilot-chat--remove-active-tokens chat-buf)
        ;; The cancelled turn's end will never arrive; drop its capture
        ;; state so the busy guards don't wedge the chat closed.
        (setq copilot-chat--current-request nil
              copilot-chat--reply-chunks nil
              copilot-chat--turn-start-time nil)))
     ((copilot-chat--cancel-one-shots))
     (t (copilot-chat-reset)))))

;;;###autoload
(defun copilot-chat-reset ()
  "Destroy the current conversation and clear the chat buffer.
The session's turn log and any pending restored turns are cleared as
well; the saved history file on disk, if any, is left untouched (see
`copilot-chat-clear-history')."
  (interactive)
  (copilot-chat--destroy)
  (when-let* ((buf (get-buffer copilot-chat--buffer-name)))
    (with-current-buffer buf
      (setq copilot-chat--turns nil
            copilot-chat--restored-turns nil
            copilot-chat--current-request nil
            copilot-chat--reply-chunks nil
            copilot-chat--turn-start-time nil
            copilot-chat--session-root nil)
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

;;;###autoload
(defun copilot-chat-apply-preset (name)
  "Apply the Copilot Chat preset named NAME from `copilot-chat-presets'.
A preset is a plist of settings; each present key sets its variable
\(`:model' sets `copilot-chat-model', `:agent-mode' sets
`copilot-chat-use-agent-mode', `:auto-approve-tools' sets
`copilot-chat-auto-approve-tools'), and any key the preset omits is
left untouched.

Called interactively, NAME is read with completion from
`copilot-chat-presets'.

The model takes effect on your next message, including the current
conversation (the model is sent with every turn).  Agent mode takes
effect on the next new conversation, since it is fixed when a
conversation is created.  See `copilot-chat-presets' for the recognized
keys and an example."
  (interactive
   (list (progn
           (unless copilot-chat-presets
             (user-error "Copilot Chat: `copilot-chat-presets' is empty"))
           (completing-read "Copilot Chat preset: "
                            (mapcar #'car copilot-chat-presets) nil t))))
  (unless copilot-chat-presets
    (user-error "Copilot Chat: `copilot-chat-presets' is empty"))
  (let ((entry (assoc name copilot-chat-presets))
        (changes nil))
    (unless entry
      (user-error "Copilot Chat: Unknown preset %S" name))
    (let ((preset (cdr entry)))
      (when (and (plist-member preset :auto-approve-tools)
                 (not (listp (plist-get preset :auto-approve-tools))))
        (user-error
         "Copilot Chat: Preset %S has a non-list `:auto-approve-tools'" name))
      (when (plist-member preset :model)
        (setq copilot-chat-model (plist-get preset :model)
              ;; Forget any cached default so clearing the model later
              ;; re-resolves from the server, mirroring
              ;; `copilot-chat-select-model'.
              copilot-chat--model-resolved nil)
        (push (format "model %s" (or copilot-chat-model "default")) changes))
      (when (plist-member preset :agent-mode)
        (setq copilot-chat-use-agent-mode (plist-get preset :agent-mode))
        (push (format "agent mode %s"
                      (if copilot-chat-use-agent-mode "on" "off"))
              changes))
      (when (plist-member preset :auto-approve-tools)
        (setq copilot-chat-auto-approve-tools
              (plist-get preset :auto-approve-tools))
        (push (format "auto-approved tools: %s"
                      (if copilot-chat-auto-approve-tools
                          (string-join copilot-chat-auto-approve-tools ", ")
                        "none"))
              changes)))
    (message "Copilot Chat: Applied preset %S%s"
             name
             (if changes
                 (format " (%s)" (string-join (nreverse changes) ", "))
               ""))))

(provide 'copilot-chat)
;;; copilot-chat.el ends here
