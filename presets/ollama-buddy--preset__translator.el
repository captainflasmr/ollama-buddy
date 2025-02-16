;; ollama-buddy preset for role: translator
;; Generated manually

(require 'ollama-buddy)

(setq ollama-buddy-command-definitions
      '(
        ;; Standard commands
        (open-chat :key 111 :description "Open chat buffer" :model nil :action (lambda nil (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer)) (when (= (buffer-size) 0) (insert (ollama-buddy--create-intro-message))) (goto-char (point-max))))
        (show-models :key 118 :description "View model status" :model nil :action ollama-buddy-show-model-status)
        (switch-role :key 82 :description "Switch roles" :model nil :action ollama-buddy-roles-switch-role)
        (create-role :key 78 :description "Create new role" :model nil :action ollama-buddy-role-creator-create-new-role)
        (open-roles-directory :key 68 :description "Open roles directory" :model nil :action ollama-buddy-roles-open-directory)
        (swap-model :key 109 :description "Swap model" :model nil :action (lambda nil (if (not (ollama-buddy--ollama-running)) (error "!!WARNING!! ollama server not running") (let ((new-model (completing-read "Model: " (ollama-buddy--get-models) nil t))) (setq ollama-buddy-default-model new-model) (setq ollama-buddy--current-model new-model) (ollama-buddy--update-status "Idle")))))
        (help :key 104 :description "Help assistant" :model nil :action (lambda nil (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer)) (goto-char (point-max)) (insert (ollama-buddy--create-intro-message))))
        (send-region :key 108 :description "Send region" :model nil :action (lambda nil (ollama-buddy--send-with-command 'send-region)))

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
