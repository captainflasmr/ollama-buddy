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
         :key ?v  ; 'v' for view models
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
        
        ;; Core Writing Assistance
        (brainstorm
         :key ?b
         :description "Brainstorm ideas"
         :model nil
         :prompt "brainstorm creative ideas and possibilities for the following topic:"
         :action (lambda () (ollama-buddy--send-with-command 'brainstorm)))
        
        (outline-generator
         :key ?u
         :description "Generate outline"
         :model nil
         :prompt "create a detailed outline for the following content:"
         :action (lambda () (ollama-buddy--send-with-command 'outline-generator)))
        
        ;; Editorial Functions
        (style-enhance
         :key ?s
         :description "Enhance writing style"
         :model nil
         :prompt "enhance the writing style of this text while maintaining its meaning. Make it more engaging and professional:"
         :action (lambda () (ollama-buddy--send-with-command 'style-enhance)))
        
        (proofread-advanced
         :key ?p
         :description "Detailed proofreading"
         :model nil
         :prompt "perform a comprehensive proofread, checking for grammar, style, clarity, and consistency. Provide specific suggestions:"
         :action (lambda () (ollama-buddy--send-with-command 'proofread-advanced)))
        
        (flow-improve
         :key ?f
         :description "Improve flow"
         :model nil
         :prompt "analyze and improve the flow and transitions in this text:"
         :action (lambda () (ollama-buddy--send-with-command 'flow-improve)))
        
        ;; Creative Writing Assistance
        (dialogue-polish
         :key ?d
         :description "Polish dialogue"
         :model nil
         :prompt "enhance this dialogue to make it more natural and engaging while maintaining character voices:"
         :action (lambda () (ollama-buddy--send-with-command 'dialogue-polish)))
        
        (scene-description
         :key ?n
         :description "Enhance scene description"
         :model nil
         :prompt "enhance this scene description with more vivid and sensory details:"
         :action (lambda () (ollama-buddy--send-with-command 'scene-description)))
        
        ;; Analysis Tools
        (character-analysis
         :key ?c
         :description "Analyze character"
         :model nil
         :prompt "analyze this character's development, motivations, and consistency:"
         :action (lambda () (ollama-buddy--send-with-command 'character-analysis)))
        
        (plot-analysis
         :key ?l
         :description "Analyze plot"
         :model nil
         :prompt "analyze this plot segment for structure, pacing, and coherence:"
         :action (lambda () (ollama-buddy--send-with-command 'plot-analysis)))
        
        ;; Research Assistance
        (research-expand
         :key ?r
         :description "Research expansion"
         :model nil
         :prompt "suggest additional research angles and potential sources for this topic:"
         :action (lambda () (ollama-buddy--send-with-command 'research-expand)))
        
        (fact-check
         :key ?k
         :description "Fact checking suggestions"
         :model nil
         :prompt "identify statements that should be fact-checked and suggest verification approaches:"
         :action (lambda () (ollama-buddy--send-with-command 'fact-check)))
        
        ;; Format Conversion
        (format-convert
         :key ?v
         :description "Convert format"
         :model nil
         :prompt "convert this text to a different format while preserving content (specify desired format in text):"
         :action (lambda () (ollama-buddy--send-with-command 'format-convert)))
        
        ;; Utility Functions
        (word-choice
         :key ?w
         :description "Word choice suggestions"
         :model nil
         :prompt "suggest alternative word choices to improve precision and impact:"
         :action (lambda () (ollama-buddy--send-with-command 'word-choice)))
        
        (summarize
         :key ?z
         :description "Summarize text"
         :model nil
         :prompt "create a concise summary of this text while preserving key points:"
         :action (lambda () (ollama-buddy--send-with-command 'summarize)))
        
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
