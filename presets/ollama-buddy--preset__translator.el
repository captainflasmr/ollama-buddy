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
        
        (swap-model
         :key ?m
         :description "Swap model"
         :action ollama-buddy--swap-model)
        
        (help
         :key ?h
         :description "Help assistant"
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (goto-char (point-max))
                   (insert (ollama-buddy--create-intro-message))
                   (ollama-buddy--show-prompt)))
        
        (send-region
         :key ?l
         :description "Send region"
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        (switch-role
         :key ?R
         :description "Switch roles"
         :model nil
         :action ollama-buddy-roles-switch-role)
        
        (create-role
         :key ?N
         :description
         "Create new role"
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
        ))
