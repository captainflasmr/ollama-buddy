(setq ollama-buddy-command-definitions
      '(
        ;; Geneal Commands
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
        
        ;; Specialized commands
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
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt prefix: " nil nil nil t)))
                     (unless (use-region-p)
                       (user-error "No region selected. Select text to use with prompt"))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send
                      (concat prefix "\n\n"
                              (buffer-substring-no-properties 
                               (region-beginning) (region-end)))))))
        (minibuffer-prompt
         :key ?i
         :description "Minibuffer Prompt"
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter prompt: " nil nil nil t)))
                     (unless (not (string-empty-p prefix))
                       (user-error "Input string is empty"))
                     (ollama-buddy--send prefix))))
        (save-chat
         :key ?s
         :description "Save chat"
         :action (lambda ()
                   (with-current-buffer ollama-buddy--chat-buffer
                     (write-region (point-min) (point-max)
                                   (read-file-name "Save conversation to: ")
                                   'append-to-file
                                   nil))))
        (kill-request
         :key ?x
         :description "Kill request"
         :action (lambda ()
                   (delete-process ollama-buddy--active-process)))
        (quit
         :key ?q
         :description "Quit"
         :action (lambda () (message "Quit Ollama Shell menu.")))
        )
      )
