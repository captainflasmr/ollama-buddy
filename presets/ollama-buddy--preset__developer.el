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
    
    (quit
     :key ?q
     :description "Quit"
     :model nil
     :action (lambda () (message "Quit Ollama Shell menu.")))))
