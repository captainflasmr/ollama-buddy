;; ollama-buddy preset for role: writer
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
         :key ?E
         :description "Create new role"
         :model nil
         :action ollama-buddy-role-creator-create-new-role)

        (open-roles-directory
         :key ?D
         :description "Open roles directory"
         :model nil
         :action ollama-buddy-roles-open-directory)
        
        ;; Core Writing Assistance
        (brainstorm
         :key ?b
         :description "Brainstorm ideas"
         :model nil
         :prompt "brainstorm creative ideas and possibilities for the following topic:"
         :action (lambda () (ollama-buddy--send-with-command 'brainstorm)))
        
        (outline-generator
         :key ?u
         :description "Generate outline"
         :model nil
         :prompt "create a detailed outline for the following content:"
         :action (lambda () (ollama-buddy--send-with-command 'outline-generator)))
        
        ;; Editorial Functions
        (style-enhance
         :key ?y
         :description "Enhance writing style"
         :model nil
         :prompt "enhance the writing style of this text while maintaining its meaning. Make it more engaging and professional:"
         :action (lambda () (ollama-buddy--send-with-command 'style-enhance)))
        
        (proofread-advanced
         :key ?p
         :description "Detailed proofreading"
         :model nil
         :prompt "perform a comprehensive proofread, checking for grammar, style, clarity, and consistency. Provide specific suggestions:"
         :action (lambda () (ollama-buddy--send-with-command 'proofread-advanced)))
        
        (flow-improve
         :key ?f
         :description "Improve flow"
         :model nil
         :prompt "analyze and improve the flow and transitions in this text:"
         :action (lambda () (ollama-buddy--send-with-command 'flow-improve)))
        
        ;; Creative Writing Assistance
        (dialogue-polish
         :key ?d
         :description "Polish dialogue"
         :model nil
         :prompt "enhance this dialogue to make it more natural and engaging while maintaining character voices:"
         :action (lambda () (ollama-buddy--send-with-command 'dialogue-polish)))
        
        (scene-description
         :key ?n
         :description "Enhance scene description"
         :model nil
         :prompt "enhance this scene description with more vivid and sensory details:"
         :action (lambda () (ollama-buddy--send-with-command 'scene-description)))
        
        ;; Analysis Tools
        (character-analysis
         :key ?c
         :description "Analyze character"
         :model nil
         :prompt "analyze this character's development, motivations, and consistency:"
         :action (lambda () (ollama-buddy--send-with-command 'character-analysis)))
        
        (plot-analysis
         :key ?a
         :description "Analyze plot"
         :model nil
         :prompt "analyze this plot segment for structure, pacing, and coherence:"
         :action (lambda () (ollama-buddy--send-with-command 'plot-analysis)))
        
        ;; Research Assistance
        (research-expand
         :key ?r
         :description "Research expansion"
         :model nil
         :prompt "suggest additional research angles and potential sources for this topic:"
         :action (lambda () (ollama-buddy--send-with-command 'research-expand)))
        
        (fact-check
         :key ?k
         :description "Fact checking suggestions"
         :model nil
         :prompt "identify statements that should be fact-checked and suggest verification approaches:"
         :action (lambda () (ollama-buddy--send-with-command 'fact-check)))
        
        ;; Utility Functions
        (word-choice
         :key ?w
         :description "Word choice suggestions"
         :model nil
         :prompt "suggest alternative word choices to improve precision and impact:"
         :action (lambda () (ollama-buddy--send-with-command 'word-choice)))
        
        (summarize
         :key ?z
         :description "Summarize text"
         :model nil
         :prompt "create a concise summary of this text while preserving key points:"
         :action (lambda () (ollama-buddy--send-with-command 'summarize)))
        
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
