;; ollama-buddy preset for role: bard
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
         :action ollama-buddy--menu-help-assistant)

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
        
        ;; Custom text transformation commands
        (bardify-text
         :key ?b
         :description "Turn text into Shakespearean prose"
         :model nil
         :prompt "Rewrite the following text in Shakespearean language, using poetic and dramatic phrasing:"
         :action (lambda () (ollama-buddy--send-with-command 'bardify-text)))

        (write-sonnet
         :key ?t
         :description "Convert text into a sonnet"
         :model nil
         :prompt "Transform the selected text into a 14-line Shakespearean sonnet:"
         :action (lambda () (ollama-buddy--send-with-command 'write-sonnet)))

        (translate-olde-english
         :key ?d
         :description "Ye Olde English"
         :model nil
         :prompt "Convert this modern text into Olde English style:"
         :action (lambda () (ollama-buddy--send-with-command 'translate-olde-english)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)

        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
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
