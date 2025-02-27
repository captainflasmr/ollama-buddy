;; ollama-buddy preset for role: legal-assistant
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
        (summarize-document
         :key ?s
         :description "Summarize legal document"
         :model nil
         :prompt "Summarize this legal document in plain language, highlighting key points and obligations:"
         :action (lambda () (ollama-buddy--send-with-command 'summarize-document)))

        (explain-clause
         :key ?e
         :description "Explain legal clause"
         :model nil
         :prompt "Explain this legal clause in simple terms, noting potential implications:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-clause)))

        (find-issues
         :key ?f
         :description "Find potential issues"
         :model nil
         :prompt "Identify potential legal issues, ambiguities, or risks in this text:"
         :action (lambda () (ollama-buddy--send-with-command 'find-issues)))

        (draft-response
         :key ?d
         :description "Draft response"
         :model nil
         :prompt "Draft a professional response to this legal inquiry or document:"
         :action (lambda () (ollama-buddy--send-with-command 'draft-response)))

        (improve-wording
         :key ?i
         :description "Improve legal wording"
         :model nil
         :prompt "Improve the clarity and precision of this legal text while maintaining its legal meaning:"
         :action (lambda () (ollama-buddy--send-with-command 'improve-wording)))

        (simplify-language
         :key ?p
         :description "Simplify to plain language"
         :model nil
         :prompt "Convert this legal text to plain, accessible language while preserving the key legal points:"
         :action (lambda () (ollama-buddy--send-with-command 'simplify-language)))

        (compare-versions
         :key ?c
         :description "Compare document versions"
         :model nil
         :prompt "Compare these two versions of a legal document and identify all substantive changes:"
         :action (lambda () (ollama-buddy--send-with-command 'compare-versions)))
        ))
