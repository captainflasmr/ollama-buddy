(setq ollama-buddy-command-definitions
      '((open-chat
         :key ?o
         :description "Open chat buffer"
         :model nil
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (when (= (buffer-size) 0)
                     (insert (ollama-buddy--create-intro-message)))
                   (goto-char (point-max))))

        (show-models
         :key ?v  ; 'v' for view models
         :description "View model status"
         :model nil
         :action ollama-buddy-show-model-status)
        
        (swap-model
         :key ?m
         :description "Swap model"
         :model nil
         :action (lambda ()
                   (if (not (ollama-buddy--ollama-running))
                       (error "!!WARNING!! ollama server not running.")
                     (progn
                       (setq ollama-buddy-current-model 
                             (completing-read "Model: " (ollama-buddy--get-models) nil t))
                       (ollama-buddy--update-status "Idle")))))
        
        (help
         :key ?h
         :description "Help assistant"
         :model nil
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (goto-char (point-max))
                   (insert (ollama-buddy--create-intro-message))))
        
        (send-region
         :key ?l
         :description "Send region"
         :model nil
         :action (lambda () (ollama-buddy--send-with-command 'send-region)))

        ;; Research Assistance
        (literature-review
         :key ?l
         :description "Literature review help"
         :model nil
         :prompt "analyze this text and suggest related papers and research directions:"
         :action (lambda () (ollama-buddy--send-with-command 'literature-review)))
        
        (methodology-review
         :key ?m
         :description "Review methodology"
         :model nil
         :prompt "review this research methodology and suggest improvements:"
         :action (lambda () (ollama-buddy--send-with-command 'methodology-review)))
        
        ;; Writing Assistance
        (academic-style
         :key ?s
         :description "Academic style check"
         :model nil
         :prompt "review this text for academic writing style and suggest improvements:"
         :action (lambda () (ollama-buddy--send-with-command 'academic-style)))
        
        (citation-suggest
         :key ?c
         :description "Citation suggestions"
         :model nil
         :prompt "identify statements that need citations and suggest types of sources:"
         :action (lambda () (ollama-buddy--send-with-command 'citation-suggest)))
        
        ;; Analysis
        (argument-analysis
         :key ?a
         :description "Analyze arguments"
         :model nil
         :prompt "analyze the logical structure and strength of arguments in this text:"
         :action (lambda () (ollama-buddy--send-with-command 'argument-analysis)))
        
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
         :action (lambda () (message "Quit Ollama Shell menu.")))))
