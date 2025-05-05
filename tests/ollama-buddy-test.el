;; ollama only

(use-package ollama-buddy
  :load-path "~/source/repos/ollama-buddy"
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-default-model "tinyllama:latest")
  :config
  (ollama-buddy-update-menu-entry
   'git-commit :model "qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'describe-code :model "qwen2.5-coder:7b")
  (ollama-buddy-update-menu-entry
   'dictionary-lookup :model "llama3.2:1b")
  (ollama-buddy-update-menu-entry
   'synonym :model "gemma3:4b")
  (ollama-buddy-update-menu-entry
   'proofread :model "llama3.2:3b"))

;; all online LLMs enabled

(use-package ollama-buddy
  :load-path "~/source/repos/ollama-buddy"
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-default-model "a:gpt-4.1")
  (ollama-buddy-openai-api-key
   (auth-source-pick-first-password :host "ollama-buddy-openai" :user "apikey"))
  (ollama-buddy-claude-api-key
   (auth-source-pick-first-password :host "ollama-buddy-claude" :user "apikey"))
  (ollama-buddy-gemini-api-key
   (auth-source-pick-first-password :host "ollama-buddy-gemini" :user "apikey"))
  (ollama-buddy-grok-api-key
   (auth-source-pick-first-password :host "ollama-buddy-grok" :user "apikey"))
  :config
  (add-to-list 'ollama-buddy-command-definitions
               '(OpenHere
                 :key ?O
                 :description "Open Here"
                 :action (lambda () (switch-to-buffer "*Ollama Buddy Chat*")
                           (ollama-buddy--initialize-chat-buffer)
                           (goto-char (point-max)))))
  (require 'ollama-buddy-openai nil t)
  (require 'ollama-buddy-claude nil t)
  (require 'ollama-buddy-gemini nil t)
  (require 'ollama-buddy-grok nil t)
  (ollama-buddy-update-menu-entry
   'git-commit :model "a:gpt-4o")
  (ollama-buddy-update-menu-entry
   'describe-code :model "o:qwen2.5-coder:3b")
  (ollama-buddy-update-menu-entry
   'dictionary-lookup :model "o:llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'synonym :model "o:llama3.2:3b")
  (ollama-buddy-update-menu-entry
   'proofread :model "a:gpt-4.1"))
