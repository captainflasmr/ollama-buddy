;; ollama-buddy preset for role: translator
;; Generated manually

(require 'ollama-buddy)

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)
        
        (swap-model
         :key ?m
         :description "Swap model"
         :action ollama-buddy--swap-model)
        
        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)

        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (help
         :key ?h
         :description "Help assistant"
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (goto-char (point-max))
                   (insert (ollama-buddy--create-intro-message))
                   (ollama-buddy--show-prompt)))

        (switch-role
         :key ?R
         :description "Switch roles"
         :model nil
         :action ollama-buddy-roles-switch-role)

        (create-role
         :key ?N
         :description "Create new role"
         :model nil
         :action ollama-buddy-role-creator-create-new-role)

        (open-roles-directory
         :key ?D
         :description "Open roles directory"
         :model nil
         :action ollama-buddy-roles-open-directory)

        ;; Custom commands for this role
        (translate-to-english
         :key ?e
         :description "Translate to English"
         :model nil
         :prompt "Translate the following text to English:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-english)))

        (translate-to-spanish
         :key ?s
         :description "Translate to Spanish"
         :model nil
         :prompt "Translate the following text to Spanish:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-spanish)))

        (translate-to-french
         :key ?f
         :description "Translate to French"
         :model nil
         :prompt "Translate the following text to French:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-french)))

        (translate-to-german
         :key ?g
         :description "Translate to German"
         :model nil
         :prompt "Translate the following text to German:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-german)))

        (translate-to-japanese
         :key ?j
         :description "Translate to Japanese"
         :model nil
         :prompt "Translate the following text to Japanese:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-japanese)))

        (translate-to-chinese
         :key ?c
         :description "Translate to Chinese"
         :model nil
         :prompt "Translate the following text to Chinese (Simplified):"
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-chinese)))

        (improve-translation
         :key ?i
         :description "Improve/fix translation"
         :model nil
         :prompt "This is a machine translation that needs improvement. Please fix any errors and make it sound more natural:"
         :action (lambda () (ollama-buddy--send-with-command 'improve-translation)))

        (explain-idiom
         :key ?d
         :description "Explain idiom/phrase"
         :model nil
         :prompt "Explain the meaning and cultural context of this idiom or phrase:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-idiom)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                     (unless (use-region-p)
                       (user-error "No region selected.  Select text to use with prompt"))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send
                      (concat prefix "\n\n"
                              (buffer-substring-no-properties
                               (region-beginning) (region-end)))))))
        
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt: " nil nil nil t)))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send prefix))))
        
        (save-chat
         :key ?s
         :description "Save chat"
         :action (lambda ()
                   (with-current-buffer ollama-buddy--chat-buffer
                     (write-region (point-min) (point-max)
                                   (read-file-name "Save conversation to: ")
                                   'append-to-file
                                   nil))))
        (kill-request
         :key ?x
         :description "Kill request"
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))

        (toggle-colors
         :key ?C
         :description "Toggle Colors"
         :action ollama-buddy-toggle-model-colors)

        (token-stats
         :key ?t
         :description "Token Usage Stats"
         :action ollama-buddy-display-token-stats)

        (toggle-history
         :key ?H
         :description "Toggle conversation history"
         :action ollama-buddy-toggle-history)

        (clear-history
         :key ?X
         :description "Clear conversation history"
         :action (lambda () (ollama-buddy-clear-history 1)))

        (show-history
         :key ?V
         :description "View conversation history"
         :action (lambda () (ollama-buddy--display-history 1)))

        (new-session
         :key ?E
         :description "New session"
         :action ollama-buddy-sessions-new)

        (load-session
         :key ?L
         :description "Load session"
         :action ollama-buddy-sessions-load)

        (save-session
         :key ?S
         :description "Save session"
         :action ollama-buddy-sessions-save)

        (list-sessions
         :key ?Y
         :description "List sessions"
         :action ollama-buddy-sessions-list)

        (delete-sessions
         :key ?K
         :description "Delete session"
         :action ollama-buddy-sessions-delete)
        
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))

        )
      )
