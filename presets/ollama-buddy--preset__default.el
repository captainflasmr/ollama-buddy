;; ollama-buddy preset for role: default
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
        
        (refactor-code
         :key ?r
         :description "Refactor code"
         :model nil
         :prompt "refactor the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'refactor-code)))
        
        (git-commit
         :key ?g
         :description "Git commit message"
         :model nil
         :prompt "write a concise git commit message for the following:"
         :action (lambda () (ollama-buddy--send-with-command 'git-commit)))
        
        (describe-code
         :key ?c
         :description "Describe code"
         :model nil
         :prompt "describe the following code:"
         :action (lambda () (ollama-buddy--send-with-command 'describe-code)))
        
        (dictionary
         :key ?d
         :description "Dictionary Lookup"
         :model nil
         :prompt-fn (lambda ()
                      (concat "For the word {"
                              (buffer-substring-no-properties (region-beginning) (region-end))
                              "} provide a typical dictionary definition:"))
         :action (lambda () (ollama-buddy--send-with-command 'dictionary)))
        
        (synonym
         :key ?n
         :description "Word synonym"
         :model nil
         :prompt "list synonyms for word:"
         :action (lambda () (ollama-buddy--send-with-command 'synonym)))
        
        (proofread
         :key ?p
         :description "Proofread text"
         :model nil
         :prompt "proofread the following:"
         :action (lambda () (ollama-buddy--send-with-command 'proofread)))
        
        (make-concise
         :key ?z
         :description "Make concise"
         :model nil
         :prompt "reduce wordiness while preserving meaning:"
         :action (lambda () (ollama-buddy--send-with-command 'make-concise)))
        
        (custom-prompt
         :key ?e
         :description "Custom prompt"
         :model nil
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
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
