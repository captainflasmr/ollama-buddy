;; ollama-buddy preset for role: writer
;; Generated manually

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
         :description "Custom writing prompt"
         :model nil
         :action (lambda ()
                   (when-let ((prefix (read-string "Enter writing prompt: " nil nil nil t)))
                     (unless (string-empty-p prefix)
                       (ollama-buddy--send prefix)))))
        
        (save-chat
         :key ?a
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
