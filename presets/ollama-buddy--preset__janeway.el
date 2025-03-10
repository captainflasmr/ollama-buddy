;; ollama-buddy preset for role: janeway
;; Generated manually

(require 'ollama-buddy)

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action ollama-buddy--open-chat)

        (help
         :key ?h
         :description "Help assistant"
         :action ollama-buddy--menu-help-assistant)
        
        (show-models
         :key ?v
         :description "View model status"
         :action ollama-buddy-show-model-status)

        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (kill-request
         :key ?k
         :description "Kill request"
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))

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
        
        ;; Custom Starfleet-style transformations
        (captains-log
         :key ?c
         :description "Rewrite as a Captain's Log"
         :model nil
         :prompt "Rework this text into a Starfleet Captain’s Log entry, using precise but reflective language:"
         :action (lambda () (ollama-buddy--send-with-command 'captains-log)))

        (starfleet-briefing
         :key ?b
         :description "Make it a Starfleet mission briefing"
         :model nil
         :prompt "Rewrite the selected text as a professional Starfleet briefing, structured and to the point:"
         :action (lambda () (ollama-buddy--send-with-command 'starfleet-briefing)))

        (borg-negotiation
         :key ?n
         :description "Rewrite as a negotiation with the Borg"
         :model nil
         :prompt "Transform this text into a tense negotiation with the Borg, maintaining diplomacy but firm resistance:"
         :action (lambda () (ollama-buddy--send-with-command 'borg-negotiation)))

        (technobabble-enhance
         :key ?t
         :description "Enhance with Starfleet technobabble"
         :model nil
         :prompt "Rework this text to include appropriate Starfleet technobabble, making it sound scientifically complex but logical:"
         :action (lambda () (ollama-buddy--send-with-command 'technobabble-enhance)))

        (delta-quadrant-danger
         :key ?d
         :description "Add Delta Quadrant-style peril"
         :model nil
         :prompt "Rewrite the selected text to sound like a Starfleet crew facing an unknown and perilous Delta Quadrant anomaly:"
         :action (lambda () (ollama-buddy--send-with-command 'delta-quadrant-danger)))

        (replicate-coffee
         :key ?r
         :description "Make it about coffee (Janeway mode!)"
         :model nil
         :prompt "Modify the selected text to include a reference to coffee in a way that would make Captain Janeway proud:"
         :action (lambda () (ollama-buddy--send-with-command 'replicate-coffee)))

        (prime-directive
         :key ?p
         :description "Make it a Prime Directive dilemma"
         :model nil
         :prompt "Rewrite this as a Starfleet ethical dilemma involving the Prime Directive, balancing logic, morality, and duty:"
         :action (lambda () (ollama-buddy--send-with-command 'prime-directive)))
        
        ;; Custom commands
        (refactor-code
         :key ?r
         :description "Refactor code"
         :prompt "refactor the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
        
        (git-commit
         :key ?g
         :description "Git commit message"
         :prompt "write a concise git commit message for the following:"
         :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
        
        (describe-code
         :key ?c
         :description "Describe code"
         :prompt "describe the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
        
        (dictionary-lookup
         :key ?d
         :description "Dictionary Lookup"
         :prompt "For the following word provide a typical dictionary definition:"
         :action (lambda () (ollama-buddy--send-with-command 'dictionary-lookup)))
        
        (synonym
         :key ?n
         :description "Word synonym"
         :prompt "list synonyms for word:"
         :action (lambda () (ollama-buddy--send-with-command 'synonym)))
        
        (proofread
         :key ?p
         :description "Proofread text"
         :prompt "proofread the following:"
         :action (lambda () (ollama-buddy--send-with-command 'proofread)))
        
        (make-concise
         :key ?z
         :description "Make concise"
         :prompt "reduce wordiness while preserving meaning:"
         :action (lambda () (ollama-buddy--send-with-command 'make-concise)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :action ollama-buddy--menu-custom-prompt)
        
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action ollama-buddy--menu-minibuffer-prompt)
        
        (token-stats
         :key ?U
         :description "Token Usage Stats"
         :action ollama-buddy-display-token-stats)

        (show-history
         :key ?V
         :description "View conversation history"
         :action (lambda () (ollama-buddy--display-history 1)))

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
