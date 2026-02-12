;; ollama-buddy preset for role: translator
;; Generated manually

(require 'ollama-buddy)

;; Menu display columns for this role
(setq ollama-buddy-menu-columns 3)

(setq ollama-buddy-command-definitions
      '(
        ;; General Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :group "General"
         :action ollama-buddy--open-chat)

        (send-region
         :key ?l
         :description "Send region"
         :group "General"
         :action (lambda ()
                   (let* ((selected-text (when (use-region-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end)))))
                     (when (not selected-text)
                       (user-error "This command requires selected text"))

                     (ollama-buddy--open-chat)
                     (insert selected-text))))

        (switch-role
         :key ?R
         :description "Switch roles"
         :group "General"
         :action ollama-buddy-roles-switch-role)

        ;; Custom commands for this role
        (translate-to-english
         :key ?e
         :description "Translate to English"
         :group "Languages"
         :prompt "Translate the following text to English:"
         :system "You are a professional translator who provides accurate, natural-sounding English translations that preserve the original meaning, tone, and cultural nuances while adapting idioms appropriately and maintaining the original formatting and structure."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-english)))

        (translate-to-spanish
         :key ?s
         :description "Translate to Spanish"
         :group "Languages"
         :prompt "Translate the following text to Spanish:"
         :system "You are a professional Spanish translator who provides accurate translations with correct grammar, appropriate formality level, regional variation awareness, proper gender agreement, and natural-sounding expressions while preserving the original meaning and tone."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-spanish)))

        (translate-to-french
         :key ?f
         :description "Translate to French"
         :group "Languages"
         :prompt "Translate the following text to French:"
         :system "You are a professional French translator who provides accurate translations with correct grammar, appropriate formality distinctions (tu/vous), proper gender and number agreement, natural idiomatic expressions, and attention to cultural context while preserving the original meaning."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-french)))

        (translate-to-german
         :key ?g
         :description "Translate to German"
         :group "Languages"
         :prompt "Translate the following text to German:"
         :system "You are a professional German translator who provides accurate translations with correct grammar, case declensions, compound word construction, formal/informal distinctions, and natural-sounding sentence structure while preserving technical precision and the original tone."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-german)))

        (translate-to-japanese
         :key ?j
         :description "Translate to Japanese"
         :group "Languages"
         :prompt "Translate the following text to Japanese:"
         :system "You are a professional Japanese translator who provides accurate translations with appropriate keigo (politeness levels), natural sentence structures, cultural adaptations, proper kanji usage balanced with kana, and contextually appropriate pronouns and relationship terms."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-japanese)))

        (translate-to-chinese
         :key ?c
         :description "Translate to Chinese"
         :group "Languages"
         :prompt "Translate the following text to Chinese (Simplified):"
         :system "You are a professional Chinese translator who provides accurate translations in simplified characters with appropriate measure words, natural word order, proper formal/informal tone, culturally appropriate idioms, and concise expression while preserving the original meaning and intent."
         :action (lambda () (ollama-buddy--send-with-command 'translate-to-chinese)))

        (improve-translation
         :key ?i
         :description "Improve/fix translation"
         :group "Tools"
         :prompt "This is a machine translation that needs improvement. Please fix any errors and make it sound more natural:"
         :system "You are a professional translation editor who identifies and corrects grammatical errors, awkward phrasing, mistranslated idioms, inconsistent terminology, and unnatural expressions to produce a polished translation that reads as if originally written in the target language."
         :action (lambda () (ollama-buddy--send-with-command 'improve-translation)))

        (explain-idiom
         :key ?d
         :description "Explain idiom/phrase"
         :group "Tools"
         :prompt "Explain the meaning and cultural context of this idiom or phrase:"
         :system "You are a linguistic and cultural expert who explains idioms by providing their literal translation, figurative meaning, cultural origin and context, usage examples in natural conversation, and equivalent expressions in other languages when possible."
         :action (lambda () (ollama-buddy--send-with-command 'explain-idiom)))

        ;; System Commands
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :group "System"
         :action ollama-buddy--menu-custom-prompt)

        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :group "System"
         :action ollama-buddy--menu-minibuffer-prompt)

        (quit
         :key ?q
         :description "Quit"
         :group "System"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
