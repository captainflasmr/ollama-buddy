;; --------------------------------------------------------------------------------
;; VARS
;; --------------------------------------------------------------------------------

(defvar my-ollama-host "localhost:11434"
  "Host for the GPT backend.")

;; Define a list of models and token sizes
(defvar my-llm-models
  '(("qwen2.5-coder" . "7b")
    ("deepseek-r1" . "7b"))
  "List of LLM models and their token sizes to configure.")

;; --------------------------------------------------------------------------------
;; chatgpt-shell
;; --------------------------------------------------------------------------------

(use-package shell-maker
  :ensure t)
(use-package chatgpt-shell
  :ensure t
  :after shell-maker
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
  ;; Dynamically configure chatgpt-shell models using the my-llm-models list
  (chatgpt-shell-models
   (let ((default-models
          '(
            ;; OpenAI example model pre-configured
            ((:version . "chatgpt-4o-latest")
             (:short-version)
             (:label . "ChatGPT")
             (:provider . "OpenAI")
             (:path . "/v1/chat/completions")
             (:token-width . 3)
             (:context-window . 12800)
             (:handler . chatgpt-shell-openai--handle-chatgpt-command)
             (:filter . chatgpt-shell-openai--filter-output)
             (:payload . chatgpt-shell-openai--make-payload)
             (:headers . chatgpt-shell-openai--make-headers)
             (:url . chatgpt-shell-openai--make-url)
             (:key . chatgpt-shell-openai-key)
             (:url-base . chatgpt-shell-api-url-base)
             (:validate-command . chatgpt-shell-openai--validate-command))))
         (ollama-models
          (mapcar
           (lambda (model-token-pair)
             (let* ((model-name (car model-token-pair))
                    (token-size (cdr model-token-pair))
                    (model-version (format "%s:%s" model-name token-size))) ;; Full name
               `((:provider . "Ollama")
                 (:label . ,model-name)
                 (:version . ,model-version)
                 (:short-version . ,token-size)
                 (:token-width . 4) ;; Customize as needed
                 (:context-window . 8192) ;; Adjust if needed
                 (:handler . chatgpt-shell-ollama--handle-ollama-command)
                 (:filter . chatgpt-shell-ollama--extract-ollama-response)
                 (:payload . chatgpt-shell-ollama-make-payload)
                 (:url . chatgpt-shell-ollama--make-url))))
           my-llm-models)))
     (append default-models ollama-models))))

;; --------------------------------------------------------------------------------
;; gptel
;; --------------------------------------------------------------------------------

(use-package gptel
  :config
  (dolist (model-token-pair my-llm-models)
    (let* ((model-name (car model-token-pair))
           (token-size (cdr model-token-pair))
           (full-model-name (format "%s:%s" model-name token-size))
           (ollama-backend (gptel-make-ollama model-name
                             :host my-ollama-host
                             :stream t
                             :models `(,(intern full-model-name)))))
      (set (intern (format "gptel-backend-%s-%s" model-name token-size)) ollama-backend)
      (message "Configured Ollama backend for model: %s" full-model-name)))
  (let* ((default-model (car my-llm-models))
         (default-model-name (car default-model))
         (default-token-size (cdr default-model))
         (default-full-model (format "%s:%s" default-model-name default-token-size)))
    (setq gptel-model (intern default-full-model)
          gptel-backend (gptel-make-ollama default-model-name
                          :host my-ollama-host
                          :stream t
                          :models `(,(intern default-full-model))))))

;; --------------------------------------------------------------------------------
;; ellama
;; --------------------------------------------------------------------------------

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	      (make-llm-ollama
	       :chat-model "llama3.2:1b"))
  :config
  (setq ellama-sessions-directory "~/.config/emacs/ellama-sessions/"
        ellama-sessions-auto-save t))

;; --------------------------------------------------------------------------------
;; ollama-buddy
;; --------------------------------------------------------------------------------

(use-package ollama-buddy
  :load-path "~/source/repos/ollama-buddy"
  :bind ("C-c l" . ollama-buddy-menu)
  :config (ollama-buddy-enable-monitor)
  :custom ollama-buddy-current-model "qwen2.5-coder:7b")

;; --------------------------------------------------------------------------------
;; MENU
;; --------------------------------------------------------------------------------

(defun my/llm-shell-menu ()
  "Menu for ChatGPT Shell commands."
  (interactive)
  (let ((key (read-key
              (propertize
               "----- ChatGPT Shell Commands [q] Quit: -----
Model  [o] Start ChatGPT    [m] Swap Model
Check  [p] Proofread Region [r] Refactor Code
Ollama [l] Start Ollama     [n] Menu
       [k] Kill Request"
               'face 'minibuffer-prompt))))
    (pcase key
      (?o (call-interactively 'chatgpt-shell))
      (?m (call-interactively 'chatgpt-shell-swap-model))
      (?p (call-interactively 'chatgpt-shell-proofread-region))
      (?r (call-interactively 'chatgpt-shell-refactor-code))
      (?l (call-interactively 'gptel))
      (?n (call-interactively 'gptel-menu))
      (?k (call-interactively 'gptel-abort))
      (?q (message "Quit ChatGPT Shell menu."))
      (?\C-g (message "Quit ChatGPT Shell menu."))
      (_ (message "Invalid key: %c" key)))))

(global-set-key (kbd "C-c g") #'my/llm-shell-menu)
