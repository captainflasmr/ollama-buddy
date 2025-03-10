;; ollama-buddy preset for role: default
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
        
        ;; Code Analysis
        (explain-code
         :key ?a
         :description "Explain code"
         :model nil
         :prompt "explain this code in detail, including its purpose and how it works:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-code)))
        
        (review-code
         :key ?r
         :description "Code review"
         :model nil
         :prompt "review this code for potential issues, bugs, and improvements:"
         :action (lambda () (ollama-buddy--send-with-command 'review-code)))
        
        (optimize-code
         :key ?z
         :description "Optimize code"
         :model nil
         :prompt "suggest optimizations for this code considering performance and readability:"
         :action (lambda () (ollama-buddy--send-with-command 'optimize-code)))
        
        ;; Code Generation
        (generate-tests
         :key ?t
         :description "Generate tests"
         :model nil
         :prompt "generate comprehensive test cases for this code:"
         :action (lambda () (ollama-buddy--send-with-command 'generate-tests)))
        
        (generate-docs
         :key ?d
         :description "Generate documentation"
         :model nil
         :prompt "generate detailed documentation for this code following best practices:"
         :action (lambda () (ollama-buddy--send-with-command 'generate-docs)))
        
        (design-patterns
         :key ?p
         :description "Suggest design patterns"
         :model nil
         :prompt "suggest applicable design patterns for this code and explain why:"
         :action (lambda () (ollama-buddy--send-with-command 'design-patterns)))
        
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
