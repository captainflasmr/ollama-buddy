;;
;; -> LLM
;;
(defvar llm-base-dir "/mnt/hgfs/SharedVM/source/"
  "Name of the base directory")

(defvar llm-source-dir nil
  "Full name of the source directory.")

(defvar package-source-mode 'melpa
  "Current package source mode. Can be 'local or 'melpa.")

(defvar package-source-packages '()
  "List of packages with tracked source configurations.
Each entry is of form (PACKAGE-NAME LOCAL-DIR MELPA-NAME).")

(defvar my-llm-models
  '(
    ("tinyllama" . "latest")
    ("gemma3" . "4b")
    ("llama3.2" . "3b")
    )
  "List of LLM models and their token sizes to configure.")

(defvar my-ollama-host "localhost:11434"
  "Host for the backend.")

;; Make sure the setup-local-package function still exists
(defun setup-local-package (package-dir)
  "Set up a local package directory similar to how MELPA would.
Compile all .el files and generate autoloads."
  (interactive "DPackage directory: ")
  ;; Ensure the directory exists
  (unless (file-directory-p package-dir)
    (error "Directory %s does not exist" package-dir))
  
  ;; Add to load path
  (add-to-list 'load-path package-dir)
  
  ;; Byte-compile all .el files in the directory that don't have
  ;; an up-to-date .elc file
  (dolist (file (directory-files package-dir t "\\.el$"))
    (let ((elc-file (concat file "c")))
      (when (or (not (file-exists-p elc-file))
                (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))
  
  ;; Generate autoloads file
  (let* ((autoload-file
          (expand-file-name (format "%s-autoloads.el" 
                                    (file-name-nondirectory
                                     (directory-file-name package-dir)))
                            package-dir)))
    (setq generated-autoload-file autoload-file)
    (update-directory-autoloads package-dir)
    
    ;; Load the generated autoloads file
    (when (file-exists-p autoload-file)
      (load-file autoload-file))))

(defun package-source-register (package-name local-dir &optional melpa-name)
  "Register a package for source switching.
PACKAGE-NAME is the symbol name of the package.
LOCAL-DIR is the path to the local development directory.
MELPA-NAME is the package name in MELPA, defaults to PACKAGE-NAME."
  (let ((melpa (or melpa-name (symbol-name package-name))))
    (add-to-list 'package-source-packages 
                 (list package-name local-dir melpa))))

;; Function to switch all packages to a specific source
(defun package-source-switch-all (source)
  "Switch all registered packages to SOURCE.
SOURCE can be 'local or 'melpa."
  (interactive (list (intern (completing-read "Switch to source: " 
                                              '(local melpa) nil t))))
  (unless (memq source '(local melpa))
    (error "Invalid source: %s. Must be 'local or 'melpa" source))
  
  (setq package-source-mode source)
  
  ;; First remove all tracked packages from load-path
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (local-dir (nth 1 pkg))
           (melpa-name (nth 2 pkg)))
      ;; Remove from load path if present
      (setq load-path (delete local-dir load-path))
      
      ;; Unload package features if loaded
      (when (featurep package-name)
        (unload-feature package-name t))))
  
  ;; Now set up packages according to selected mode
  (if (eq source 'local)
      (package-source-setup-local)
    (package-source-setup-melpa))
  
  ;; Reload init file to apply changes
  (message "Reloading configuration to apply changes...")
  (load user-init-file))

(defun package-source-setup-local ()
  "Set up all registered packages from local sources."
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (local-dir (nth 1 pkg)))
      (setup-local-package local-dir))))

(defun package-source-setup-melpa ()
  "Set up all registered packages from MELPA."
  (dolist (pkg package-source-packages)
    (let* ((package-name (car pkg))
           (melpa-name (nth 2 pkg)))
      ;; Make sure package.el is initialized
      (require 'package)
      (unless package--initialized
        (package-initialize))
      
      ;; Ensure package is installed
      (unless (package-installed-p (intern melpa-name))
        (package-refresh-contents)
        (package-install (intern melpa-name)))
      
      ;; Load the package
      (require package-name))))

;; Enhanced setup-my-package function that registers the package
(defun setup-my-package (pkg &optional melpa-name)
  "Set up a local package and register it for source switching.
PKG is the package directory name under llm-base-dir.
MELPA-NAME is the package name in MELPA, defaults to PKG."
  (setq llm-source-dir (concat llm-base-dir pkg))
  (package-source-register (intern (file-name-base pkg)) 
                           llm-source-dir 
                           (or melpa-name (file-name-base pkg)))
  (when (eq package-source-mode 'local)
    (setup-local-package llm-source-dir)))

;; Interactive command to toggle between local and MELPA
(defun package-source-toggle ()
  "Toggle between local and MELPA package sources."
  (interactive)
  (if (eq package-source-mode 'local)
      (package-source-switch-all 'melpa)
    (package-source-switch-all 'local))
  (message "Switched to %s package source" package-source-mode))

;; Create separate macros for MELPA and local configurations
(defmacro use-package-local-or-melpa (name &rest args)
  "Set up a package with different configurations based on package-source-mode.
NAME is the package name. 
ARGS are passed to use-package based on the current mode."
  (declare (indent 1))
  `(progn
     ;; Define the local version
     (when (eq package-source-mode 'local)
       (use-package ,name
         :load-path llm-source-dir
         ,@args))
     
     ;; Define the MELPA version
     (when (eq package-source-mode 'melpa)
       (use-package ,name
         :ensure t
         ,@args))))

;; Add keybinding to toggle between package sources
(global-set-key (kbd "C-c p t") 'package-source-toggle)

;; Register and set up all your packages
;; The third argument is the MELPA package name if different from directory name

(setup-my-package "gptel-master" "gptel")
(use-package-local-or-melpa gptel
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

(setup-my-package "plz.el-master" "plz")
(use-package-local-or-melpa plz)

(setup-my-package "plz-media-type-main" "plz-media-type")
(use-package-local-or-melpa plz-media-type)

(setup-my-package "plz-event-source-main" "plz-event-source")
(use-package-local-or-melpa plz-event-source)

(setup-my-package "llm-main" "llm")
(use-package-local-or-melpa llm)

(setup-my-package "ellama-main" "ellama")
(use-package-local-or-melpa ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
	      (make-llm-ollama
	       :chat-model "tinyllama:latest"))
  :config
  (setq ellama-sessions-directory "~/.config/emacs/ellama-sessions/"
        ellama-sessions-auto-save t))

(setup-my-package "shell-maker-main" "shell-maker")
(use-package-local-or-melpa shell-maker)

(setup-my-package "chatgpt-shell-main" "chatgpt-shell")
(use-package-local-or-melpa chatgpt-shell
  :after shell-maker
  :custom
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pass-get 'secret "openai-key")))
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
                    (model-version (format "%s:%s" model-name token-size)))
               `((:provider . "Ollama")
                 (:label . ,model-name)
                 (:version . ,model-version)
                 (:short-version . ,token-size)
                 (:token-width . 4)
                 (:context-window . 8192)
                 (:handler . chatgpt-shell-ollama--handle-ollama-command)
                 (:filter . chatgpt-shell-ollama--extract-ollama-response)
                 (:payload . chatgpt-shell-ollama-make-payload)
                 (:url . chatgpt-shell-ollama--make-url))))
           my-llm-models)))
     (append default-models ollama-models))))

(require 'transient)

;;; Main LLM Menu
(transient-define-prefix llm-menu ()
  "LLM Client Selection."
  ["Select LLM Client"
   ("g" "GPTel" llm-gptel-menu)
   ("c" "ChatGPT Shell" llm-chatgpt-menu)
   ("e" "Ellama" llm-ellama-menu)
   ("t" "Toggle package source (Local/MELPA)" package-source-toggle)])

  ;;; ollama-buddy
(transient-define-prefix llm-ollama-buddy-menu ()
  "ChatGPT Shell."
  ["ChatGPT Shell"
   ("o" "Open chat" ollama-buddy--open-chat)
   ("m" "Swap model" ollama-buddy--swap-model)
   ("q" "Quit" transient-quit-one)])  

;;; GPTel
(transient-define-prefix llm-gptel-menu ()
  "GPTel."
  ["GPTel"
   ("o" "Open chat" gptel)
   ("m" "Menu" gptel-menu)
   ("q" "Quit" transient-quit-one)])
  
;;; ChatGPT Shell Menu
(transient-define-prefix llm-chatgpt-menu ()
  "ChatGPT Shell."
  ["ChatGPT Shell"
   ("o" "Open chat" chatgpt-shell)
   ("m" "Swap model" chatgpt-shell-swap-model)
   ("q" "Quit" transient-quit-one)])

;;; Ellama Menu
(transient-define-prefix llm-ellama-menu ()
  "Ellama."
  ["Ellama"
   ("o" "Open chat" ellama-chat)
   ("m" "Swap model" ellama-select-ollama-model)
   ("q" "Quit" transient-quit-one)])

;; Bind the main transient menu to a global key
(global-set-key (kbd "C-c g") 'llm-menu)
