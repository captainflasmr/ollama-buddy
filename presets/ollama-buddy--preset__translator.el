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
         :action ollama-buddy-roles-switch-role)

        (create-role
         :key ?E
         :description "Create new role"
         :action ollama-buddy-role-creator-create-new-role)

        (open-roles-directory
         :key ?D
         :description "Open roles directory"
         :action ollama-buddy-roles-open-directory)

        ;; Custom commands for this role
        (translate-to-english
         :key ?e
         :description "Translate to English"
         :prompt "Translate the following text to English:"
         :system "You are a professional translator who provides accurate, natural-sounding English translations that preserve the original meaning, tone, and cultural nuances while adapting idioms appropriately and maintaining the original formatting and structure."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-english)))

        (translate-to-spanish
         :key ?s
         :description "Translate to Spanish"
         :prompt "Translate the following text to Spanish:"
         :system "You are a professional Spanish translator who provides accurate translations with correct grammar, appropriate formality level, regional variation awareness, proper gender agreement, and natural-sounding expressions while preserving the original meaning and tone."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-spanish)))

        (translate-to-french
         :key ?f
         :description "Translate to French"
         :prompt "Translate the following text to French:"
         :system "You are a professional French translator who provides accurate translations with correct grammar, appropriate formality distinctions (tu/vous), proper gender and number agreement, natural idiomatic expressions, and attention to cultural context while preserving the original meaning."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-french)))

        (translate-to-german
         :key ?g
         :description "Translate to German"
         :prompt "Translate the following text to German:"
         :system "You are a professional German translator who provides accurate translations with correct grammar, case declensions, compound word construction, formal/informal distinctions, and natural-sounding sentence structure while preserving technical precision and the original tone."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-german)))

        (translate-to-japanese
         :key ?j
         :description "Translate to Japanese"
         :prompt "Translate the following text to Japanese:"
         :system "You are a professional Japanese translator who provides accurate translations with appropriate keigo (politeness levels), natural sentence structures, cultural adaptations, proper kanji usage balanced with kana, and contextually appropriate pronouns and relationship terms."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-japanese)))

        (translate-to-chinese
         :key ?c
         :description "Translate to Chinese"
         :prompt "Translate the following text to Chinese (Simplified):"
         :system "You are a professional Chinese translator who provides accurate translations in simplified characters with appropriate measure words, natural word order, proper formal/informal tone, culturally appropriate idioms, and concise expression while preserving the original meaning and intent."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-chinese)))

        (improve-translation
         :key ?i
         :description "Improve/fix translation"
         :prompt "This is a machine translation that needs improvement. Please fix any errors and make it sound more natural:"
         :system "You are a professional translation editor who identifies and corrects grammatical errors, awkward phrasing, mistranslated idioms, inconsistent terminology, and unnatural expressions to produce a polished translation that reads as if originally written in the target language."
         :action (lambda () (ollama-buddy--send-with-command 'improve-translation)))

        (explain-idiom
         :key ?d
         :description "Explain idiom/phrase"
         :prompt "Explain the meaning and cultural context of this idiom or phrase:"
         :system "You are a linguistic and cultural expert who explains idioms by providing their literal translation, figurative meaning, cultural origin and context, usage examples in natural conversation, and equivalent expressions in other languages when possible."
         :action (lambda () (ollama-buddy--send-with-command 'explain-idiom)))

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
