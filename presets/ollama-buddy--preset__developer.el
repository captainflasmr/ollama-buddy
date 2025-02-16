;; ollama-buddy preset for role: developer
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
        
        ;; Code Analysis
        (explain-code
         :key ?e
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
         :key ?o
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
         :key ?c
         :description "Custom prompt"
         :model nil
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter dev prompt: " nil nil nil t)))
                     (unless (string-empty-p prefix)
                       (ollama-buddy--send prefix)))))

        (save-chat
         :key ?s
         :description "Save chat"
         :model nil
         :action (lambda ()
                   (with-current-buffer ollama-buddy--chat-buffer
                     (write-region (point-min) (point-max) 
                                   (read-file-name "Save conversation to: ")
                                   'append-to-file
                                   nil))))
        
        (kill-request
         :key ?x
         :description "Kill request"
         :model nil
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))
        
        (quit
         :key ?q
         :description "Quit"
         :model nil
         :action (lambda () (message "Quit Ollama Shell menu.")))
        ))
