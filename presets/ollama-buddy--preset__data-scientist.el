;; ollama-buddy preset for role: data-scientist
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
        (explain-data
         :key ?d
         :description "Explain dataset"
         :model nil
         :prompt "Explain this dataset. Include insights about the columns, data types, and potential use cases:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-data)))

        (suggest-analysis
         :key ?a
         :description "Suggest analysis approach"
         :model nil
         :prompt "Suggest appropriate analysis techniques and statistical methods for this data:"
         :action (lambda () (ollama-buddy--send-with-command 'suggest-analysis)))

        (explain-code
         :key ?e
         :description "Explain data code"
         :model nil
         :prompt "Explain this data science/analytics code. Include what each step is doing and potential improvements:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-code)))

        (interpret-results
         :key ?i
         :description "Interpret results"
         :model nil
         :prompt "Interpret these analysis results. Explain key findings, limitations, and potential next steps:"
         :action (lambda () (ollama-buddy--send-with-command 'interpret-results)))

        (fix-data-code
         :key ?f
         :description "Fix data/analytics code"
         :model nil
         :prompt "Debug and fix issues in this data analysis code:"
         :action (lambda () (ollama-buddy--send-with-command 'fix-data-code)))

        (chart-suggestion
         :key ?c
         :description "Suggest visualization"
         :model nil
         :prompt "Suggest appropriate data visualizations for this data and analysis goal:"
         :action (lambda () (ollama-buddy--send-with-command 'chart-suggestion)))

        (explain-stats
         :key ?s
         :description "Explain statistical concept"
         :model nil
         :prompt "Explain this statistical concept in simple terms with examples:"
         :action (lambda () (ollama-buddy--send-with-command 'explain-stats)))
        ))
