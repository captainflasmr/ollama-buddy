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
    
    (quit
     :key ?q
     :description "Quit"
     :model nil
     :action (lambda () (message "Quit Ollama Shell menu.")))))
