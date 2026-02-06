;; ollama-buddy preset for role: bard
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
         :action ollama-buddy--open-chat)
        
        (send-region
         :key ?l
         :description "Send region"
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
         :action ollama-buddy-roles-switch-role)
        
        ;; Custom text transformation commands
        (bardify-text
         :key ?b
         :description "Turn text into Shakespearean prose"
         :prompt "Rewrite the following text in Shakespearean language, using poetic and dramatic phrasing:"
         :system "You are William Shakespeare, transforming text with authentic Early Modern English using rich metaphors, thee/thou pronouns, period verb forms, poetic rhythm, and genuine archaic words while maintaining the original meaning and tone."
         :action (lambda () (ollama-buddy--send-with-command 'bardify-text)))
        
        (write-sonnet
         :key ?t
         :description "Convert text into a sonnet"
         :prompt "Transform the selected text into a 14-line Shakespearean sonnet:"
         :system "Transform text into a perfect 14-line Shakespearean sonnet with iambic pentameter, ABABCDCDEFEFGG rhyme scheme, three quatrains with a volta at line 9, and a concluding couplet, preserving the central meaning with rich Elizabethan imagery."
         :action (lambda () (ollama-buddy--send-with-command 'write-sonnet)))
        
        (translate-olde-english
         :key ?d
         :description "Ye Olde English"
         :prompt "Convert this modern text into Olde English style:"
         :system "Transform modern text into authentic Early Modern English using thee/thou pronouns, -eth/-est verb endings, genuine archaic terms (forsooth, prithee), period spelling variations, and characteristic syntax while maintaining readability and the original tone."
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
        
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
