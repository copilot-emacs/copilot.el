(require 'json)
(require 'copilot)

(defun buffer-file-uri ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (and file-name (concat "file://" file-name))))


(defvar buffer-context nil)
(defvar conversation-id nil)
(defvar work-done-token -1)

(defconst chat-buffer-name "*Copilot Chat*")

(defun conversation-turn (conv-id chat-msg)
  (setq work-done-token (1+ work-done-token))
  (copilot--async-request
   'conversation/turn
   (list :workDoneToken work-done-token
         :conversationId conv-id
         :message chat-msg)))



(defun my-chat-send-message (source)
  "Prompt the user to enter a message and add it to the chat buffer."
  (let ((chat-msg (read-string "Enter your message: "))
        (chat-buffer (get-buffer chat-buffer-name)))
    (setq buffer-context (current-buffer))
    (message "[buffer-context] %s" buffer-context)
    (if (and chat-buffer conversation-id)
        (with-current-buffer chat-buffer
          (display-buffer chat-buffer)
          (end-of-buffer)
          (message "conversation/turn %s" chat-buffer)
          (insert (concat "User: " chat-msg "\n\n"))
          (conversation-turn conversation-id chat-msg))
      (let
          ((new-chat-buffer (get-buffer-create chat-buffer-name))
           (document-uri (buffer-file-uri)))
        (display-buffer new-chat-buffer)
        (with-current-buffer new-chat-buffer
          (insert (concat "User: " chat-msg "\n\n"))
          (message "conversation/create")
          (message document-uri)
          (setq work-done-token (1+ work-done-token))
          (copilot--async-request
           'conversation/create
           (list :workDoneToken work-done-token
                 :turns `[(:request ,chat-msg)]
                 :capabilities '(:skills [
                                          "current-editor" ;to inform language server the editor support conversation/context current-editor skill
                                          ])
                 :doc (list :uri document-uri)
                 :source source
                 )
           :success-fn (jsonrpc-lambda (&key conversationId turnId agentSlug)(setq conversation-id conversationId))))))))


;
(defun current-editor-skill (conversation-id turn-id)
  (message "[conversation/context][buffer] %s" buffer-context)
  (with-current-buffer buffer-context
    (let ((res `[(:uri ,(buffer-file-uri)
                       :position (:line ,(line-number-at-pos) :character ,(current-column))
                       :visibleRange (:start (:line ,(line-number-at-pos (window-start)) :character 1)
                                             :end (:line ,(1+ (line-number-at-pos (window-end))) :character 1))
                       ,@(if (region-active-p)
                             `(:selection (:start (:line ,(line-number-at-pos (region-beginning)) :character 1)
                                                  :end (:line ,(1+ (line-number-at-pos (region-end))) :character 1))))

                                        ;:openedAt
                                        ;:activeAt
                   )
                 nil]))
      (message "[conversation/context] %s" res)
      res)))

(copilot-on-request
 'conversation/context
 (lambda (msg)
   (message (format "[conversation/context][buffer] %s" buffer-context))
   (with-current-buffer buffer-context
     (let ((res (copilot--dbind (:conversationId conversation-id :turnId turn-id :skillId skill-id) msg
                  (cond
                   ((string= skill-id "current-editor") (current-editor-skill conversation-id turn-id))
                   (t '[nil (:code -1 :message "handle skill")])))))
       (message (json-encode res))
       res))))

(defun on-progress(msg)
  (message (format "[$/progress] %s" msg))
  (copilot--dbind
      (:token work-done-token
              :value (:kind kind
                            :title title
                            :conversationId conversation-id
                            :turnId turn-id
                            :reply reply ; streaming response by lines when kind="report"
                            :suggestedTitle suggested-title ; receive at the end of turn of "panel" source coversation
                            :followUp follow-up ; receive at the end of turn of "panel" source coversation
                            :updatedDocuments updated-documents ; receive at the end of turn of "inline" source coversation
                            ))

      msg
    (message "[progress][buffer] %s" (get-buffer chat-buffer-name))
    (with-current-buffer (get-buffer chat-buffer-name)
      (end-of-buffer)
      (message "[progress][kind] %s" kind)
      (message "[progress][updateDocuments] %s" updated-documents)
      (cond
       ((string= kind "begin") (insert "Assistant: "))
       ((string= kind "end")
        (when follow-up
          (message "[Suggested followup]")
          (insert "Suggested followup: ")
          (insert follow-up-msg)
          )
        (when updated-documents
          (message "[updated-documents]")
          (message (format "[updated-documents] %s" updated-documents))
          (update-documents updated-documents))
        (insert "\n\n"))
       (reply (message "[reply] %s" reply) (insert reply))))))

(copilot-on-notification '$/progress (lambda (message) (on-progress message)))


(defun update-documents (updated-documents)
  (seq-do (lambda (doc)
            (let ((uri (plist-get doc :uri))
                  (text (plist-get doc :text)))
              (message "[update-documents] uri: %s" uri)
              (message "[update-documents] text: %s" text)
              (with-current-buffer (find-buffer-visiting uri)
                (delete-region (point-min) (point-max))
                (insert text))
              ))
          updated-documents))

(defun chat-inline () (interactive) (my-chat-send-message "inline"))
(defun chat-panel () (interactive) (my-chat-send-message "panel"))

(provide 'copilot-chat)
