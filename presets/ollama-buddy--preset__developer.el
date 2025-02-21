(setq ollama-buddy-command-definitions
      '(
        ;; Geneal Commands
        (open-chat
         :key ?o
         :description "Open chat buffer"
         :action (lambda ()
                   (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
                   (when (= (buffer-size) 0)
                     (insert (ollama-buddy--create-intro-message))
                     (ollama-buddy--show-prompt))
                   (goto-char (point-max))))
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
