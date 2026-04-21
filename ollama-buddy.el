;;; ollama-buddy.el --- Ollama LLM AI Assistant ChatGPT Claude Gemini Grok Codestral DeepSeek OpenRouter Support -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 7.4.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Ollama Buddy is an Emacs package that provides a friendly AI assistant
;; for various tasks such as code refactoring, generating commit messages,
;; dictionary lookups, and more.  It interacts with local LLMs via Ollama
;; and supports remote providers including OpenAI, Claude, Gemini, Grok,
;; GitHub Copilot, Codestral, DeepSeek, and OpenRouter.
;;
;;; Quick Start
;;
;; (use-package ollama-buddy
;;   :ensure t
;;   :bind
;;   ("C-c o" . ollama-buddy-role-transient-menu)
;;   ("C-c O" . ollama-buddy-transient-menu))
;;
;;; Usage
;;
;; C-c o  Role-based transient menu (main entry point)
;; C-c O  Main transient menu (all settings and actions)
;;
;; From the chat buffer:
;;
;;   C-c C-c / C-c RET  Send prompt
;;   C-c C-k            Cancel request
;;   C-c m              Change model
;;
;;; Remote Providers (optional)
;;
;; Register any OpenAI/Claude/Gemini-compatible provider with a single call:
;;
;;   (require 'ollama-buddy-provider)
;;   (ollama-buddy-provider-create
;;    :name "OpenAI" :prefix "a:"
;;    :api-key (lambda () (auth-source-pick-first-password
;;                         :host "ollama-buddy-openai" :user "apikey"))
;;    :endpoint "https://api.openai.com/v1/chat/completions"
;;    :models-endpoint "https://api.openai.com/v1/models"
;;    :models-filter (lambda (id) (string-match-p "gpt" id)))
;;
;; Supported :api-type values: openai (default), claude, gemini.
;; See CHANGELOG.org for migration examples from the legacy require system.
;;
;; Legacy per-provider require files still work but are deprecated:
;;   (require 'ollama-buddy-openai)        ; a: OpenAI
;;   (require 'ollama-buddy-claude)        ; c: Anthropic Claude
;;   (require 'ollama-buddy-gemini)        ; g: Google Gemini
;;   (require 'ollama-buddy-grok)          ; k: xAI Grok
;;   (require 'ollama-buddy-copilot)       ; p: GitHub Copilot
;;   (require 'ollama-buddy-codestral)     ; s: Mistral Codestral
;;   (require 'ollama-buddy-deepseek)      ; d: DeepSeek
;;   (require 'ollama-buddy-openrouter)    ; r: OpenRouter (400+ models)
;;   (require 'ollama-buddy-openai-compat) ; l: any OpenAI-compatible server
;;
;; Each provider needs an API key (see PROVIDERS.org for setup details).
;;
;;; Presets and User Prompts
;;
;; On first launch, install the bundled presets and user prompts:
;;
;;   C-c O → I   (or M-x ollama-buddy-install-extras)
;;
;; This copies role presets and system prompt templates into your Emacs
;; configuration directory.  The chat welcome screen will remind you if
;; they are not yet installed.
;;
;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)
(require 'dired)
(require 'org)
(require 'savehist)
(require 'iso8601)
(require 'ollama-buddy-core)
(require 'ollama-buddy-project) ;; Added by user instruction
(require 'ollama-buddy-transient nil t)
(require 'ollama-buddy-user-prompts)
(require 'ollama-buddy-web-search)
(require 'ollama-buddy-rag)
(require 'ollama-buddy-tools)
(require 'ollama-buddy-rewrite nil t)
(require 'ollama-buddy-plan)

(declare-function ollama-buddy-curl--validate-executable "ollama-buddy-curl")
(declare-function ollama-buddy-curl--test-connection "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request-direct "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request-async "ollama-buddy-curl")

(declare-function ollama-buddy-tools--generate-schema "ollama-buddy-tools")
(declare-function ollama-buddy-tools--process-tool-calls "ollama-buddy-tools")
(declare-function ollama-buddy-tools-toggle "ollama-buddy-tools")
(declare-function ollama-buddy-tools-toggle-auto-execute "ollama-buddy-tools")
(declare-function ollama-buddy-tools-info "ollama-buddy-tools")
(declare-function ollama-buddy-annotate-apply-last-response "ollama-buddy-annotate")
(declare-function ollama-buddy-annotate-directory "ollama-buddy-annotate")
(declare-function ollama-buddy-annotate-directory-cancel "ollama-buddy-annotate")
(declare-function ollama-buddy-curl--process-filter "ollama-buddy-curl")
(declare-function ollama-buddy-curl--process-json-line "ollama-buddy-curl")
(declare-function ollama-buddy-curl--handle-content "ollama-buddy-curl")
(declare-function ollama-buddy-curl--handle-completion "ollama-buddy-curl")
(declare-function ollama-buddy-curl--sentinel "ollama-buddy-curl")
(declare-function ollama-buddy-curl--send "ollama-buddy-curl")
(declare-function ollama-buddy-curl--non-streaming-sentinel "ollama-buddy-curl")
(declare-function ollama-buddy-curl-test "ollama-buddy-curl")
(declare-function ollama-buddy-transient-menu "ollama-buddy-transient")
(declare-function ollama-buddy-transient-auth-menu "ollama-buddy-transient")
(declare-function ollama-buddy-transient-user-prompts-menu "ollama-buddy-transient")
(declare-function ollama-buddy-transient-attachment-menu "ollama-buddy-transient")
(declare-function ollama-buddy-transient-parameter-menu "ollama-buddy-transient")
(declare-function ollama-buddy-transient-profile-menu "ollama-buddy-transient")

;; Web search forward declarations
(declare-function ollama-buddy-web-search "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-attach "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-get-context "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-count "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search--org-escape "ollama-buddy-web-search")

(defvar imenu--index-alist)

(defun ollama-buddy--imenu-create-index ()
  "Create an imenu index for the chat buffer.
Indexes prompt turns as a numbered list."
  (let ((index nil)
        (turn 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\*.+\\*.*>> PROMPT: " nil t)
        (let ((pos (match-beginning 0))
              (text (string-trim
                     (buffer-substring-no-properties
                      (point) (line-end-position)))))
          (setq turn (1+ turn))
          (when (> (length text) 50)
            (setq text (concat (substring text 0 50) "...")))
          (push (cons (format "%02d. %s" turn
                              (if (string-empty-p text) "(empty)" text))
                      pos)
                index))))
    (nreverse index)))

(defun ollama-buddy-jump-to-prompt ()
  "Jump to a prompt in the chat buffer using `completing-read'."
  (interactive)
  (let* ((buf (get-buffer ollama-buddy--chat-buffer))
         (index (when buf
                  (with-current-buffer buf
                    (ollama-buddy--imenu-create-index)))))
    (if (null index)
        (message "No prompts found")
      (let* ((choice (completing-read "Jump to prompt: " index nil t))
             (pos (cdr (assoc choice index))))
        (when pos
          (pop-to-buffer buf)
          (goto-char pos)
          (recenter 0))))))

(defvar ollama-buddy--reasoning-skip-newlines nil
  "Whether to skip leading newlines after reasoning section ends.")

(defvar ollama-buddy--reasoning-marker-found nil
  "Whether we are currently inside a reasoning section.")

(defvar ollama-buddy--reasoning-status-message nil
  "Current reasoning status message.")

(defvar-local ollama-buddy--thinking-block-start nil
  "Marker for the start of the currently-streaming thinking block content.
Set when a thinking start marker is detected; cleared when the block is folded.")


(defvar-local ollama-buddy--thinking-api-active nil
  "Non-nil while thinking tokens are arriving via `message.thinking' API field.
Used by models like deepseek-r1 that use a dedicated thinking field rather than
embedding <think>...</think> tags inside message.content.")

(defvar-local ollama-buddy--thinking-arrow-marker nil
  "Marker at the start of the `*** Think' heading for the current block.
Set when the heading is inserted; passed to
`ollama-buddy--finalize-thinking-block' and then cleared.")

(defvar-local ollama-buddy--last-think-heading-marker nil
  "Marker at the `*** Think' heading from the most recent finalized block.
Used to re-fold the heading after all post-streaming buffer modifications
\(property drawer, md-to-org conversion, prompt area) are complete, since
those modifications can trigger org-fold's fragility check and reveal the
fold.  Cleared after re-folding in the normal completion path.")

(defvar-local ollama-buddy--current-original-model nil
  "The original model requested for the current turn.")

(defvar-local ollama-buddy--current-has-images nil
  "Flag indicating if images were included in the current turn.")

(defvar-local ollama-buddy--thinking-content-accumulator nil
  "String accumulating thinking tokens during streaming.
Tokens are also inserted into the buffer under a folded heading so the
user can peek with TAB.  On completion, `ollama-buddy--finalize-thinking-block'
replaces the raw text with md-to-org converted content and re-folds.")

(defvar-local ollama-buddy--header-inserted-p nil
  "Flag to track if the response header has been inserted for the current turn.")

(defvar-local ollama-buddy--turn-start-position nil
  "Marker at the true start of the current turn, before any streaming artifacts.
Unlike `ollama-buddy--response-start-position' (which is advanced by
`insert-response-header' and `finalize-thinking-block'), this marker
is set once per turn in `ollama-buddy--send' and never modified during
streaming.  Used by `ollama-buddy--rebuild-tool-batch' to delete all
artifacts for the turn.")


(defun ollama-buddy--insert-response-header (model original-model &optional has-images)
  "Insert the response header for MODEL in the chat buffer.
ORIGINAL-MODEL is the requested model if different.
HAS-IMAGES is non-nil if the request included images.
Returns the position where the response content should start."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let ((inhibit-read-only t)
          (start-pos (if (markerp ollama-buddy--response-start-position)
                         (marker-position ollama-buddy--response-start-position)
                       ollama-buddy--response-start-position)))
      (save-excursion
        (goto-char (or start-pos (point-max)))
        ;; Ensure we are after any previous turn's final newline but before current turn content
        (unless (bolp) (insert "\n"))
        ;; After a folded subtree the preceding newlines are invisible;
        ;; insert an extra newline so the header has visual separation.
        (when (and (> (point) (point-min))
                   (invisible-p (1- (point))))
          (insert "\n"))
        (insert "\n")
        (let ((avg-wait (ollama-buddy--model-average-wait-time model))
              (heading-start (point)))
          (if has-images
              (insert (format "** [%s: RESPONSE with %d image(s)]"
                              model (length (or (bound-and-true-p ollama-buddy--current-attachments) nil))))
            (insert (format "** [%s: RESPONSE]" model)))
          ;; Save marker for property drawer insertion after response completes
          (setq ollama-buddy--response-heading-marker (copy-marker heading-start))
          ;; Insert countdown estimate before the closing ]
          (when (and avg-wait (>= avg-wait 1))
            (backward-char 1)  ; before ]
            (setq ollama-buddy--response-countdown-marker (copy-marker (point)))
            (insert (format " ~%ds" (round avg-wait)))
            (forward-char 1)))  ; past ] only, not end-of-line
        (insert "\n\n")
        (when (and original-model model (not (string= original-model model)))
          (insert (format "\n*[Using %s instead of %s]*\n" model original-model)))
        (setq ollama-buddy--header-inserted-p t)
        (point-marker)))))


(defvar ollama-buddy-mode-line-segment nil
  "Mode line segment for Ollama Buddy.")

(defvar-local ollama-buddy--history-view-mode 'display
  "Current mode of the history buffer.")

(defvar ollama-buddy-history-model-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'ollama-buddy-history-cancel)
    map)
  "Keymap for model-specific history viewing mode.")

(defvar ollama-buddy--start-point nil
  "General store of a starting point.")

(defvar ollama-buddy--cloud-usage-cache nil
  "Cached cloud usage data: ((session . \"N%\") (weekly . \"N%\")).")

(defvar ollama-buddy--cloud-usage-cache-time nil
  "Time when cloud usage was last fetched.")

(defcustom ollama-buddy-cloud-usage-cache-seconds 300
  "Seconds to cache cloud usage data before re-fetching."
  :type 'integer
  :group 'ollama-buddy)

;; creating vision payload
(defun ollama-buddy--create-vision-message (prompt image-files)
  "Create a message with PROMPT and IMAGE-FILES for vision models."
  (if image-files
      ;; Create a message with content and images array according to Ollama API
      `((role . "user")
        (content . ,prompt)
        (images . ,(vconcat
                    (mapcar
                     (lambda (file)
                       (ollama-buddy--encode-image-to-base64 file))
                     image-files))))
    ;; No images, just use text
    `((role . "user")
      (content . ,prompt))))

;; Function to detect file paths in a prompt and check if they are image files
(defun ollama-buddy--detect-image-files (prompt)
  "Detect potential image file paths in PROMPT."
  (when (and ollama-buddy-vision-enabled prompt)
    (let ((image-files nil))
      
      ;; Method 1: Look for quoted file paths (single or double quotes)
      (let ((quoted-pattern "\\(?:\"\\([^\"]+\\)\"\\|'\\([^']+\\)'\\)"))
        (let ((start 0))
          (while (string-match quoted-pattern prompt start)
            (let ((quoted-path (or (match-string 1 prompt) (match-string 2 prompt))))
              (when (and (file-exists-p quoted-path)
                         (cl-some (lambda (format-regex)
                                    (string-match-p format-regex quoted-path))
                                  ollama-buddy-image-formats))
                (push quoted-path image-files)))
            (setq start (match-end 0)))))
      
      ;; Method 2: Smart path detection - look for potential file paths and test them
      ;; This method tries to build complete paths by looking ahead for image extensions
      (let ((start 0))
        (while (string-match "\\(?:^\\|\\s-\\)\\([/.~]\\|[A-Za-z]:[\\\\]\\)" prompt start)
          (let ((path-start (match-beginning 1)))
            ;; Look for image file extensions from this position
            (dolist (format-regex ollama-buddy-image-formats)
              (let ((ext-pattern (replace-regexp-in-string "\\\\\\." "\\\\." 
                                                           (replace-regexp-in-string "\\$" "" format-regex))))
                (when (string-match (concat "\\(" (regexp-quote (substring prompt path-start)) 
                                            "[^\\n]*?" ext-pattern "\\)") prompt)
                  (let ((potential-path (match-string 1 prompt)))
                    (when (and (file-exists-p potential-path)
                               (not (member potential-path image-files)))
                      (push potential-path image-files))))))
            (setq start (1+ path-start)))))
      
      ;; Method 3: Aggressive search - split on likely path separators and reconstruct
      ;; This handles cases where paths might be embedded in longer text
      (let ((tokens (split-string prompt "[ \t\n]+"))
            (current-path ""))
        (dolist (token tokens)
          (if (string-match "^[/.~]\\|^[A-Za-z]:[\\\\]" token)
              ;; Start of a new potential path
              (progn
                (setq current-path token)
                ;; Check if this token alone is a valid image file
                (when (and (file-exists-p current-path)
                           (cl-some (lambda (format-regex)
                                      (string-match-p format-regex current-path))
                                    ollama-buddy-image-formats)
                           (not (member current-path image-files)))
                  (push current-path image-files)))
            ;; Continue building the current path if it makes sense
            (when (and (not (string-empty-p current-path))
                       (or (string-match "\\.[a-zA-Z0-9]+$" token) ; ends with extension
                           (not (string-match "^[a-zA-Z]+:" token)))) ; not a new scheme
              (let ((extended-path (concat current-path " " token)))
                (when (and (file-exists-p extended-path)
                           (cl-some (lambda (format-regex)
                                      (string-match-p format-regex extended-path))
                                    ollama-buddy-image-formats)
                           (not (member extended-path image-files)))
                  (push extended-path image-files)
                  (setq current-path extended-path))
                ;; If the extended path doesn't exist, reset
                (unless (file-exists-p extended-path)
                  (setq current-path "")))))))
      
      ;; Method 4: Fallback to original word-based approach for simple paths without spaces
      (let ((words (split-string prompt)))
        (dolist (word words)
          (when (and (file-exists-p word)
                     (cl-some (lambda (format-regex)
                                (string-match-p format-regex word))
                              ollama-buddy-image-formats)
                     ;; Only add if not already found by other methods
                     (not (member word image-files)))
            (push word image-files))))
      
      ;; Remove duplicates and return
      (delete-dups (nreverse image-files)))))

;; Function to encode image file to base64
(defun ollama-buddy--encode-image-to-base64 (file-path)
  "Encode the image at FILE-PATH to base64 string."
  (with-temp-buffer
    (insert-file-contents-literally file-path)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

;; Function to check if the current model supports vision
(defun ollama-buddy--model-supports-vision (model)
  "Check if MODEL supports vision capabilities.
When /api/show capabilities have been fetched, trusts that data.
Falls back to the static `ollama-buddy-vision-models' list when
capabilities are not yet available."
  (when model
    (let* ((real-model (ollama-buddy--get-real-model-name model))
           ;; Strip cloud suffixes for matching
           (base-model (replace-regexp-in-string "[-:]cloud$" "" real-model))
           ;; Also get name without tag (e.g. "gemma3:4b" -> "gemma3")
           (name-only (car (split-string base-model ":")))
           (meta (gethash model ollama-buddy--models-metadata-cache)))
      (if (and meta (alist-get 'capabilities-fetched meta))
          ;; Capabilities fetched from /api/show — trust that data
          (alist-get 'vision meta)
        ;; Not yet fetched — fall back to static list
        (or (member base-model ollama-buddy-vision-models)
            (member name-only ollama-buddy-vision-models))))))

(defun ollama-buddy--model-supports-tools (model)
  "Check if MODEL supports tool calling capabilities.
When /api/show capabilities have been fetched, trusts that data.
Falls back to the static `ollama-buddy-tools-models' list when
capabilities are not yet available.
Models registered via `ollama-buddy-provider-create' are always
considered tool-capable."
  (when model
    (let* ((real-model (ollama-buddy--get-real-model-name model))
           ;; Strip cloud suffixes for matching
           (base-model (replace-regexp-in-string "[-:]cloud$" "" real-model))
           ;; Also get name without tag (e.g. "qwen3:32b" -> "qwen3")
           (name-only (car (split-string base-model ":")))
           (meta (gethash model ollama-buddy--models-metadata-cache)))
      (cond
       ;; Generic provider models — always tool-capable
       ((and (featurep 'ollama-buddy-provider)
             (fboundp 'ollama-buddy-provider--model-is-provider-p)
             (ollama-buddy-provider--model-is-provider-p model))
        t)
       ;; Capabilities fetched from /api/show — trust that data
       ((and meta (alist-get 'capabilities-fetched meta))
        (alist-get 'tools meta))
       ;; Not yet fetched — fall back to static list
       (t
        (or (member base-model ollama-buddy-tools-models)
            (member name-only ollama-buddy-tools-models)))))))

(defun ollama-buddy--model-supports-thinking (model)
  "Check if MODEL supports thinking/reasoning capabilities.
When /api/show capabilities have been fetched, trusts that data.
Falls back to the static lists and patterns when capabilities
are not yet available."
  (when model
    (let* ((real-model (ollama-buddy--get-real-model-name model))
           ;; Strip cloud suffixes for matching
           (base-model (replace-regexp-in-string "[-:]cloud$" "" real-model))
           ;; Also get name without tag (e.g. "deepseek-r1:7b" -> "deepseek-r1")
           (name-only (car (split-string base-model ":")))
           (base-lower (downcase base-model))
           (meta (gethash model ollama-buddy--models-metadata-cache)))
      (if (and meta (alist-get 'capabilities-fetched meta))
          ;; Capabilities fetched from /api/show — trust that data
          (alist-get 'thinking meta)
        ;; Not yet fetched — fall back to static lists and patterns
        (or (member base-model ollama-buddy-thinking-models)
            (member name-only ollama-buddy-thinking-models)
            (cl-some (lambda (pattern)
                       (string-match-p (regexp-quote (downcase pattern)) base-lower))
                     ollama-buddy-thinking-model-patterns))))))

(defun ollama-buddy--fetch-model-capabilities (models)
  "Proactively fetch /api/show capabilities for MODELS.
Populates the metadata cache with thinking, vision and tool
capabilities so that indicators are accurate before any model
is selected."
  (when (ollama-buddy--ollama-running)
    (dolist (model models)
      ;; Skip models that already have capabilities cached.
      (let ((meta (gethash model ollama-buddy--models-metadata-cache)))
        (unless (alist-get 'capabilities-fetched meta)
          (condition-case err
              (let* ((real-model (ollama-buddy--get-real-model-name model))
                     (payload (json-encode `((model . ,real-model))))
                     (response (ollama-buddy--make-request "/api/show" "POST" payload)))
                (when response
                  (ollama-buddy--store-model-capabilities model response)))
            (error
             (message "ollama-buddy: failed to fetch capabilities for %s: %s"
                      model (error-message-string err)))))))))

(defun ollama-buddy--store-model-capabilities (model response)
  "Store capabilities from /api/show RESPONSE into cache for MODEL.
Return non-nil if the stored capabilities differ from what the
static-list fallbacks would have shown (i.e. indicators changed)."
  (let ((old-tools (ollama-buddy--model-supports-tools model))
        (old-vision (ollama-buddy--model-supports-vision model))
        (old-thinking (ollama-buddy--model-supports-thinking model)))
    (let* ((capabilities (append (alist-get 'capabilities response) nil))
           (cached-meta (or (gethash model ollama-buddy--models-metadata-cache) '())))
      (push '(capabilities-fetched . t) cached-meta)
      (when (member "thinking" capabilities)
        (push '(thinking . t) cached-meta))
      (when (member "vision" capabilities)
        (push '(vision . t) cached-meta))
      (when (member "tools" capabilities)
        (push '(tools . t) cached-meta))
      (puthash model cached-meta
               ollama-buddy--models-metadata-cache))
    ;; Return non-nil when any indicator changed
    (not (and (eq (not old-tools) (not (ollama-buddy--model-supports-tools model)))
              (eq (not old-vision) (not (ollama-buddy--model-supports-vision model)))
              (eq (not old-thinking) (not (ollama-buddy--model-supports-thinking model)))))))

(defun ollama-buddy--fetch-model-capabilities-async (models &optional callback)
  "Asynchronously fetch /api/show capabilities for MODELS.
Like `ollama-buddy--fetch-model-capabilities' but non-blocking.
When all fetches complete, call CALLBACK only if any model's
displayed indicators actually changed compared to static-list fallbacks."
  (let* ((models-to-fetch
          (cl-remove-if
           (lambda (model)
             (alist-get 'capabilities-fetched
                        (gethash model ollama-buddy--models-metadata-cache)))
           models))
         (remaining (length models-to-fetch))
         (changed nil))
    (unless (zerop remaining)
      (dolist (model models-to-fetch)
        (let ((model model)) ;; lexical capture
          (condition-case nil
              (let* ((real-model (ollama-buddy--get-real-model-name model))
                     (payload (json-encode `((model . ,real-model)))))
                (ollama-buddy--make-request-async-backend
                 "/api/show" "POST" payload
                 (lambda (_status result)
                   (when result
                     (when (ollama-buddy--store-model-capabilities model result)
                       (setq changed t)))
                   (cl-decf remaining)
                   (when (and (zerop remaining) changed callback)
                     (funcall callback)))))
            (error (cl-decf remaining)
                   (when (and (zerop remaining) changed callback)
                     (funcall callback)))))))))

;; Function to unload a single model
(defun ollama-buddy--unload-single-model (model)
  "Unload MODEL from Ollama to free up resources.
According to Ollama API, unloading is done by sending a chat request
with an empty messages array and keep_alive set to 0."
  (let* ((real-model-name (ollama-buddy--get-real-model-name model))
         (payload (json-encode `((model . ,real-model-name)
                                 (messages . ,(vconcat []))
                                 (keep_alive . 0))))
         (operation-id (gensym "unload-")))

    (ollama-buddy--register-background-operation
     operation-id
     (format "Unloading %s" model))
    
    (ollama-buddy--make-request-async
     "/api/chat"
     "POST"
     payload
     (lambda (status _result)
       (if (plist-get status :error)
           (progn
             (message "Error unloading %s: %s" model (cdr (plist-get status :error)))
             (ollama-buddy--complete-background-operation
              operation-id
              (format "Error unloading %s" model)))
         (progn
           (message "Successfully unloaded model %s" model)
           (ollama-buddy--complete-background-operation
            operation-id
            (format "Successfully unloaded model %s" model))))))))

;; Function to unload all running models
(defun ollama-buddy-unload-all-models ()
  "Unload all currently running Ollama models to free up resources."
  (interactive)
  (let ((running-models (ollama-buddy--get-running-models)))
    (if (null running-models)
        (message "No models are currently running")
      (when (yes-or-no-p (format "Unload all %d running models? " (length running-models)))
        (dolist (model running-models)
          (ollama-buddy--unload-single-model model))))))

;; --- Thinking block org-heading helpers ---

(defun ollama-buddy--fold-all-thinking-blocks ()
  "Fold all `*** Think' headings in the current buffer.
Called after loading a session to restore collapsed thinking blocks."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*\\* Think$" nil t)
      (beginning-of-line)
      (org-fold-hide-subtree)
      (forward-line 1))))

(defun ollama-buddy--insert-thinking-header ()
  "Insert `*** Thinking' heading.
Thinking tokens are NOT inserted into the buffer during streaming;
they accumulate in `ollama-buddy--thinking-content-accumulator'.
When thinking ends, `ollama-buddy--finalize-thinking-block' inserts
the accumulated content, the `*** Response' heading, and folds.
Must be called inside an `inhibit-read-only' block.
Returns a marker at the start of the heading line.
The marker uses t insertion-type so that if a response header is
later inserted at the same position, the marker advances past it
and continues to point at the heading.
If no response header has been inserted yet, inserts one first so
the *** heading nests properly under the ** header."
  ;; Ensure the ** response header exists so *** nests under it
  (when (and (not ollama-buddy--header-inserted-p)
             (boundp 'ollama-buddy--current-model)
             ollama-buddy--current-model)
    (ollama-buddy--insert-response-header
     ollama-buddy--current-model
     ollama-buddy--current-original-model
     ollama-buddy--current-has-images)
    ;; Move point to end of buffer (past the newly inserted header)
    (goto-char (point-max)))
  ;; Ensure we start on a fresh line
  (unless (bolp) (insert "\n"))
  (let ((heading-start (point)))
    (insert "*** Thinking\n\n")
    (setq ollama-buddy--thinking-content-accumulator "")
    (let ((m (copy-marker heading-start)))
      (set-marker-insertion-type m t)
      ;; Fold heading immediately so streamed tokens are hidden by default;
      ;; user can TAB on the heading to peek at accumulated content.
      (save-excursion
        (goto-char heading-start)
        (org-fold-hide-subtree))
      m)))

(defun ollama-buddy--finalize-thinking-block (heading-marker)
  "Finalise the thinking block: insert content, fold, rename heading.
Inserts the accumulated thinking content after the heading, appends
the `*** Response' heading, folds the Think subtree via
`org-fold-hide-subtree', renames `Thinking' to `Think', and
registers the heading for toggle-all."
  (when (and heading-marker (marker-buffer heading-marker))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (marker-position heading-marker))
        (end-of-line)
        (forward-char 1)              ;; past the heading's \n, onto blank line
        ;; If thinking was streamed visibly, delete the raw text first
        (when (and ollama-buddy--thinking-block-start
                   (marker-position ollama-buddy--thinking-block-start))
          (delete-region (point) (marker-position ollama-buddy--thinking-block-start)))
        ;; Insert accumulated thinking content (convert md→org if enabled)
        (when (and ollama-buddy--thinking-content-accumulator
                   (not (string-empty-p ollama-buddy--thinking-content-accumulator)))
          (let ((content ollama-buddy--thinking-content-accumulator))
            (when ollama-buddy-convert-markdown-to-org
              (setq content
                    (with-temp-buffer
                      (insert content)
                      (ollama-buddy--md-to-org-convert-region (point-min) (point-max) 3)
                      (buffer-string))))
            (insert "\n\n" content)
            (unless (string-suffix-p "\n" content)
              (insert "\n"))))
        ;; Insert *** Response heading after thinking content
        (insert "\n*** Response\n\n"))
      ;; Rename "Thinking" -> "Think" BEFORE folding so the heading text is
      ;; stable when org-fold's fragility check runs after the fold is applied.
      (save-excursion
        (goto-char (marker-position heading-marker))
        (when (looking-at "\\*\\*\\* Thinking")
          (replace-match "*** Think")))
      ;; Fold the Think subtree
      (save-excursion
        (goto-char (marker-position heading-marker))
        (org-fold-hide-subtree))
      ;; Record heading position so the completion path can re-fold after all
      ;; post-streaming buffer modifications (property drawer, md-to-org, prompt
      ;; area) are done.  Those modifications can trigger org-fold's fragility
      ;; check and inadvertently reveal the fold.
      (when (marker-buffer heading-marker)
        (setq ollama-buddy--last-think-heading-marker
              (copy-marker (marker-position heading-marker))))
      ;; Advance response-start-position past the *** Response heading
      ;; so md-to-org conversion doesn't touch the thinking block
      (when ollama-buddy--response-start-position
        (save-excursion
          (goto-char (marker-position heading-marker))
          (when (re-search-forward "^\\*\\*\\* Response\n+" nil t)
            (setq ollama-buddy--response-start-position (copy-marker (point))))))))
  ;; Clean up
  (setq ollama-buddy--thinking-content-accumulator nil))

(defun ollama-buddy--extract-thinking-from-response (response)
  "Separate tag-style thinking content from RESPONSE text.
Returns (THINKING-CONTENT . CLEAN-RESPONSE).
THINKING-CONTENT is nil when no tags are found."
  (let ((thinking nil)
        (clean response))
    (catch 'found
      (dolist (pair ollama-buddy-reasoning-markers)
        (let* ((open (car pair))
               (close (cdr pair))
               (re (concat (regexp-quote open)
                           "\\(\\(?:.\\|\n\\)*?\\)"
                           (regexp-quote close))))
          (when (string-match re response)
            (setq thinking (string-trim (match-string 1 response))
                  clean (string-trim
                         (replace-regexp-in-string re "" response nil nil 0)))
            (throw 'found nil)))))
    (cons thinking clean)))

(defun ollama-buddy--rebuild-tool-batch (batch-start model tool-calls tool-results
                                                      &optional thinking-content response-text)
  "Delete streaming artifacts from BATCH-START and rebuild a clean tool batch.
MODEL is the model name for the header.
TOOL-CALLS and TOOL-RESULTS are parallel lists.
THINKING-CONTENT (if non-nil/empty) produces a folded *** Think section.
RESPONSE-TEXT (if non-nil/empty) is inserted before the Tools section."
  (let ((inhibit-read-only t)
        (think-marker nil)
        (tool-heading-positions nil))
    ;; 1. Nuke all streaming artifacts and overlays
    (delete-region batch-start (point-max))
    (goto-char (point-max))
    ;; 2. Insert structured output
    (insert (format "\n\n** [%s: TOOLS]\n" model))
    ;; Optional response text before thinking
    (when (and response-text (not (string-empty-p response-text)))
      (insert response-text)
      (unless (string-suffix-p "\n" response-text)
        (insert "\n")))
    ;; Optional thinking section
    (when (and thinking-content (not (string-empty-p thinking-content)))
      (setq think-marker (copy-marker (point)))
      (insert "*** Think\n\n"
              thinking-content)
      (unless (string-suffix-p "\n" thinking-content)
        (insert "\n"))
      (insert "\n"))
    ;; Tools section
    (insert "*** Tools\n")
    ;; Each tool call + result
    (cl-mapc
     (lambda (call result)
       (let* ((func (alist-get 'function call))
              (name (alist-get 'name func))
              (args (alist-get 'arguments func))
              (content (alist-get 'content result)))
         (push (point-marker) tool-heading-positions)
         (let* ((args-json (json-encode args))
                (args-summary (if (and (featurep 'ollama-buddy-tools)
                                       (fboundp 'ollama-buddy-tools--format-args-for-display))
                                  (ollama-buddy-tools--format-args-for-display args)
                                args-json))
                (args-heading (if (> (length args-summary) 60)
                                  (concat (substring args-summary 0 57) "…")
                                args-summary)))
           (insert (format "**** %s(%s)\n***** call\n\n#+begin_src json\n%s\n#+end_src\n***** results\n\n#+begin_example\n%s\n#+end_example\n"
                           name
                           args-heading
                           args-json
                           (replace-regexp-in-string "^\\([*#]\\)" ",\\1" content))))))
     tool-calls
     tool-results)
    ;; 3. Fold Think heading (bounded by *** Tools at same level)
    (when think-marker
      (save-excursion
        (goto-char think-marker)
        (org-fold-hide-subtree)))
    ;; 4. Fold each **** tool heading in reverse order
    (dolist (pos tool-heading-positions)
      (save-excursion
        (goto-char pos)
        (org-fold-hide-subtree)))))

(defun ollama-buddy--extend-thinking-fold (heading-marker)
  "Re-fold the subtree at HEADING-MARKER to cover newly inserted text.
Called after inserting thinking tokens so appended text stays hidden.
If the user has manually unfolded the heading, the subtree is left visible."
  (when (and heading-marker (marker-buffer heading-marker))
    (save-excursion
      (goto-char (marker-position heading-marker))
      (end-of-line)
      ;; Only re-fold if the subtree is currently folded;
      ;; if the user has unfolded with TAB, leave it visible.
      (when (and (< (point) (point-max))
                 (invisible-p (1+ (point))))
        (org-fold-hide-subtree)))))

;; Function to check if text contains a reasoning marker
(defun ollama-buddy--find-reasoning-marker (text)
  "Check if TEXT contain a reasoning marker."
  (let ((found-marker nil))
    (dolist (marker-pair ollama-buddy-reasoning-markers found-marker)
      (when (and (not found-marker)
                 (string-match-p (regexp-quote (car marker-pair)) text))
        (setq found-marker (cons 'start marker-pair)))
      (when (and (not found-marker)
                 (string-match-p (regexp-quote (cdr marker-pair)) text))
        (setq found-marker (cons 'end marker-pair))))
    found-marker))

(defun ollama-buddy-beginning-of-prompt ()
  "Move to the beginning of the prompt, or to the real beginning of line on repeat.
Behaves like smart C-a: first go to prompt start (if it exists),
second go to column 0."
  (interactive)
  (let* ((prompt-pos
          (save-excursion
            (beginning-of-line)
            (when (re-search-forward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):" (line-end-position) t)
              (forward-char 1)
              (point)))))
    (cond
     ;; If point is already at prompt start → go to col 0
     ((and prompt-pos (eq (point) prompt-pos))
      (beginning-of-line))

     ;; If prompt exists → go to the prompt
     (prompt-pos
      (goto-char prompt-pos))

     ;; Otherwise fallback to regular C-a behavior
     ;; Use beginning-of-visual-line so that on a folded org heading
     ;; (e.g. *** Think...) we land at the heading start, not at the
     ;; start of a hidden line inside the fold.
     (t
      (beginning-of-visual-line)))))

(defcustom ollama-buddy-at-commands
  '(("search" "@search(%s)" "Search the web and attach results")
    ("rag"    "@rag(%s)"   "Search RAG indexes and attach context")
    ("skill"  "@skill(%s)" "Inject a 'skill' category user prompt")
    ("file"   "@file(%s)"  "Attach a file inline"))
  "Alist of inline `@' commands and their syntax templates.
Each entry is (NAME TEMPLATE DESCRIPTION)."
  :type '(alist :key-type string :value-type (list string string))
  :group 'ollama-buddy)

(defun ollama-buddy--at-complete ()
  "Complete an inline `@' command in the prompt area.
When point is in the prompt area, offer `completing-read' with
available `@' commands and insert the chosen syntax template.
Outside the prompt area, insert a literal `@'.
Cancelling with \\[keyboard-quit] does nothing; use \\[quoted-insert] @ for a literal `@' in the prompt."
  (interactive)
  (let ((in-prompt
         (save-excursion
           (beginning-of-line)
           (re-search-forward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):" (line-end-position) t))))
    (if (not in-prompt)
        (self-insert-command 1)
      (let* ((candidates
              ollama-buddy-at-commands)
             (names (mapcar #'car candidates))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (s)
                   (let ((desc (caddr (assoc s candidates))))
                     (and desc (concat " -- " desc))))))
             (choice (condition-case nil
                         (completing-read "@ command: " names nil t)
                       (quit nil))))
        (when choice
          (let* ((template (cadr (assoc choice candidates)))
                 (parts (split-string template "%s"))
                 (value ""))
            ;; Special handling for commands that need a secondary completion
            (cond
             ((string= choice "skill")
              (let* ((prompts (ollama-buddy-user-prompts--get-prompts))
                     (formatted (mapcar #'ollama-buddy-user-prompts--format-for-completion prompts))
                     (prompt-alist (cl-mapcar #'cons formatted prompts))
                     (selected (completing-read "Select skill: " formatted nil t)))
                (setq value (plist-get (cdr (assoc selected prompt-alist)) :title))))
             ((string= choice "file")
              (setq value (read-file-name "Attach file: " nil nil t))))
            
            (insert (car parts))
            (insert value)
            (save-excursion
              (insert (cadr parts)))))))))

(defun ollama-buddy-copy-last-response ()
  "Copy the last assistant response to the kill ring."
  (interactive)
  (let* ((model ollama-buddy--current-model)
         (history (gethash model ollama-buddy--conversation-history-by-model nil))
         (last-assistant
          (cl-find-if (lambda (msg) (equal (alist-get 'role msg) "assistant"))
                      (reverse history))))
    (if last-assistant
        (let ((content (alist-get 'content last-assistant)))
          (kill-new content)
          (message "Last response copied to kill ring (%d chars)" (length content)))
      (message "No assistant response found in history"))))

(defun ollama-buddy-retry-last-prompt ()
  "Resend the last user prompt to the current model."
  (interactive)
  (if ollama-buddy--prompt-history
      (let ((last-prompt (car ollama-buddy--prompt-history))
            (model (or ollama-buddy--current-model
                       ollama-buddy-default-model
                       "Default:latest")))
        (ollama-buddy--send-backend last-prompt model))
    (message "No prompt history to retry")))

(defun ollama-buddy-rewind (&optional choose)
  "Rewind the conversation to a previous prompt.
When point is on or after a prompt heading, rewind to that prompt.
Otherwise, offer all prompts via `completing-read'.
With non-nil CHOOSE (or prefix argument), always use `completing-read'.
Everything from the selected prompt onward is removed, the
conversation history is truncated, and the prompt text is
pre-filled so you can edit and resend.

Typically invoked via `C-u C-u C-c C-c'."
  (interactive "P")
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let ((prompt-text nil)
          (prompt-pos nil))
      ;; Try to find prompt heading at or before point (skip when CHOOSE)
      (unless choose
        (save-excursion
          (beginning-of-line)
          (unless (looking-at "^\\* \\*.*\\*.*>> PROMPT: \\(.*\\)")
            (re-search-backward "^\\* \\*.*\\*.*>> PROMPT: " nil t))
          (when (looking-at "^\\* \\*.*\\*.*>> PROMPT: \\(.*\\)")
            (setq prompt-text (string-trim (match-string 1))
                  prompt-pos (match-beginning 0)))))
      ;; Fall back to completing-read if not on a prompt heading
      (unless prompt-pos
        (let ((prompts nil))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\* \\*.*\\*.*>> PROMPT: \\(.*\\)" nil t)
              (let ((text (string-trim (match-string 1)))
                    (pos (match-beginning 0)))
                (when (and text (not (string-empty-p text)))
                  (push (cons text pos) prompts)))))
          (setq prompts (nreverse prompts))
          (when prompts
            (let* ((numbered
                    (cl-loop for (text . pos) in prompts
                             for i from 1
                             collect (cons (format "%d: %s"
                                                  i (truncate-string-to-width text 80))
                                           pos)))
                   (choice (completing-read "Rewind to: "
                                            (mapcar #'car numbered) nil t))
                   (pos (cdr (assoc choice numbered))))
              (when pos
                (save-excursion
                  (goto-char pos)
                  (when (looking-at "^\\* \\*.*\\*.*>> PROMPT: \\(.*\\)")
                    (setq prompt-text (string-trim (match-string 1))
                          prompt-pos pos))))))))
      (if (not prompt-pos)
          (message "No prompts found to rewind to")
        (when (y-or-n-p (format "Rewind to: %s? "
                                (truncate-string-to-width prompt-text 60)))
          ;; Count prompt headings before the selected one
          (let ((prompt-index 0))
            (save-excursion
              (goto-char (point-min))
              (while (and (re-search-forward
                           "^\\* \\*.*\\*.*>> PROMPT: " nil t)
                          (< (match-beginning 0) prompt-pos))
                (cl-incf prompt-index)))
            ;; Truncate conversation history
            (let* ((model (or ollama-buddy--current-model
                              ollama-buddy-default-model))
                   (history (gethash model
                                     ollama-buddy--conversation-history-by-model nil))
                   (keep-count (* 2 prompt-index)))
              (when history
                (puthash model (seq-take history keep-count)
                         ollama-buddy--conversation-history-by-model)))
            ;; Truncate prompt history
            (let* ((all-prompts 0))
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "^\\* \\*.*\\*.*>> PROMPT: " nil t)
                  (cl-incf all-prompts)))
              (let ((prompts-to-remove (- all-prompts prompt-index)))
                (setq ollama-buddy--prompt-history
                      (nthcdr prompts-to-remove ollama-buddy--prompt-history))))
            ;; Delete buffer content from this prompt onward
            (let ((inhibit-read-only t))
              (goto-char prompt-pos)
              (skip-chars-backward "\n")
              (delete-region (point) (point-max))
              ;; Re-insert prompt area with old text pre-filled
              (ollama-buddy--prepare-prompt-area)
              (goto-char (point-max))
              (insert prompt-text))
            ;; Refresh header line, status, and imenu cache
            (setq imenu--index-alist nil)
            (ollama-buddy--update-status "Rewound")
            (ollama-buddy-update-mode-line)))))))


(defcustom ollama-buddy-slash-commands
  '(("model"      ollama-buddy--swap-model            "Switch the current LLM model")
    ("system"     ollama-buddy-user-prompts-load      "Load a saved system prompt")
    ("clear"      ollama-buddy-sessions-new           "Start a fresh chat session")
    ("save"       ollama-buddy-sessions-save          "Save the current session to a file")
    ("load"       ollama-buddy-sessions-load          "Load a saved chat session")
    ("tools"      ollama-buddy-tools-toggle           "Toggle LLM tool-calling capabilities")
    ("autoexec"   ollama-buddy-tools-toggle-auto-execute "Toggle tool auto-execute (skip confirmation)")
    ("unguarded"  ollama-buddy-tools-toggle-unguarded  "Toggle unguarded mode — bypass ALL safety prompts")
    ("context"    ollama-buddy-show-attachments       "View and manage attached context/files")
    ("help"       ollama-buddy--menu-help-assistant    "Show chat interface commands and help")
    ("copy"       ollama-buddy-copy-last-response     "Copy the last AI response to kill ring")
    ("retry"      ollama-buddy-retry-last-prompt      "Resend the last prompt to the model")
    ("tone"       ollama-buddy-set-tone               "Set the response tone/style")
    ("streaming"  ollama-buddy-toggle-streaming       "Toggle real-time response streaming")
    ("think"      ollama-buddy-toggle-thinking        "Toggle thinking/reasoning output")
    ("skill"      ollama-buddy-user-prompts-load      "Load a skill as system prompt")
    ("reset"      ollama-buddy-reset-system-prompt    "Clear the current system prompt")
    ("completion" ollama-buddy-completion-toggle      "Toggle inline code completions")
    ("new"        ollama-buddy-sessions-new           "Start a fresh chat session")
    ("exit"       ollama-buddy-exit                  "Close the chat buffer")
    ("bye"        ollama-buddy-exit                  "Close the chat buffer")
    ("unload"     ollama-buddy-unload-model           "Unload model from Ollama memory")
    ("manage"     ollama-buddy-manage-models          "Open the Model Management buffer")
    ("project"    ollama-buddy-project-attach-file    "Attach a file from the current project")
    ("cd"         ollama-buddy-project-switch-directory "Switch working directory and load project context")
    ("set"        ollama-buddy-params-edit            "Edit model generation parameters")
    ("show"       ollama-buddy-show-raw-model-info    "Show raw JSON model information")
    ("benchmark"  ollama-buddy-benchmark-models       "Benchmark all models with editable selection")
    ("init"       ollama-buddy-project-init           "Generate or load project summary")
    ("rename"     ollama-buddy-sessions-rename        "Rename the current session")
    ("login"      ollama-buddy-cloud-signin            "Sign in to Ollama cloud")
    ("logout"     ollama-buddy-cloud-signout           "Sign out from Ollama cloud")
    ("manual"     ollama-buddy-open-info              "Open the Ollama Buddy Info manual")
    ("export"     org-export-dispatch                 "Open org-export dispatcher for the chat buffer")
    ("backend"    ollama-buddy-switch-communication-backend "Switch between network-process and curl backends")
    ("launch"     ollama-buddy-launch                    "Launch a model in an external terminal agent (claude, codex, aider, ...)")
    ("format"     ollama-buddy-set-response-format   "Set response format (json/schema/off)")
    ("annotate"   ollama-buddy-annotate-apply-last-response "Apply annotations from last response to database")
    ("annotate-dir" ollama-buddy-annotate-directory         "Batch-annotate every source file in a directory")
    ("annotate-cancel" ollama-buddy-annotate-directory-cancel "Cancel a running annotate-directory batch")
    ("rewind"     (lambda () (interactive) (ollama-buddy-rewind t)) "Rewind conversation to a previous prompt")
    ("plan"       ollama-buddy-plan-start               "Start plan mode — LLM generates a structured plan")
    ("plan-next"  ollama-buddy-plan-execute-next         "Execute the next TODO step in the plan")
    ("plan-all"   ollama-buddy-plan-execute-all          "Execute all remaining plan steps")
    ("plan-done"  ollama-buddy-plan-mark-done            "Mark the current IN-PROGRESS step as DONE")
    ("plan-status" ollama-buddy-plan-status              "Show plan progress summary")
    ("plan-stop"  ollama-buddy-plan-stop                 "Deactivate plan mode"))
  "Alist of available `/' slash commands.
Each entry is (NAME FUNCTION DESCRIPTION) where FUNCTION is
called interactively."
  :type '(alist :key-type string :value-type (list function string))
  :group 'ollama-buddy)

(defun ollama-buddy--slash-complete ()
  "Complete a `/' slash command in the prompt area.
Slash-command completion is offered only when `/' is the first
non-whitespace character typed in the current prompt — i.e. nothing
but whitespace sits between the `>> PROMPT:' marker and point.  In
every other position (mid-prompt, paths, regex, outside the prompt
area) a literal `/' is inserted instead, so no special escape key
is ever needed."
  (interactive)
  (let* ((prompt-content-start
          (save-excursion
            (beginning-of-line)
            (when (re-search-forward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):"
                                     (line-end-position) t)
              (point))))
         (at-prompt-start
          (and prompt-content-start
               (>= (point) prompt-content-start)
               (string-blank-p
                (buffer-substring-no-properties prompt-content-start (point))))))
    (if (not at-prompt-start)
        (self-insert-command 1)
      (let* ((candidates
              (cl-remove-if-not
               (lambda (entry)
                 (pcase (car entry)
                   ("system" (featurep 'ollama-buddy-user-prompts))
                   ("tools" (featurep 'ollama-buddy-tools))
                   ("annotate" (featurep 'ollama-buddy-annotate))
                   ("annotate-dir" (featurep 'ollama-buddy-annotate))
                   ("annotate-cancel" (featurep 'ollama-buddy-annotate))
                   (_ t)))
               ollama-buddy-slash-commands))
             (names (mapcar #'car candidates))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (s)
                   (let ((desc (cl-caddr (assoc s candidates))))
                     (and desc (concat " -- " desc))))))
             (choice (condition-case nil
                         (completing-read "/ command: " names nil t)
                       (quit nil))))
        (when choice
          (let ((fn (cadr (assoc choice candidates))))
            (call-interactively fn)))))))

(defun ollama-buddy-history-search ()
  "Search through the prompt history using a `completing-read' interface."
  (interactive)
  (when ollama-buddy--prompt-history
    (let* ((prompt-data (ollama-buddy--get-prompt-content))
           (prompt-point (cdr prompt-data))
           ;; Create an alist with indices and history items for completing-read
           (history-items
            (cl-loop for item in ollama-buddy--prompt-history
                     for index from 0
                     collect (cons item index)))
           ;; Use completing-read to search through history
           (selected-item (completing-read "Search history: "
                                           (mapcar #'car history-items)
                                           nil t))
           ;; Find the selected item in our history
           (selected-index (cdr (assoc selected-item history-items))))
      
      ;; Store position for next cycle
      (put 'ollama-buddy--cycle-prompt-history 'history-position selected-index)
      
      ;; Replace current prompt with selected history item
      (when prompt-point
        (save-excursion
          (goto-char prompt-point)
          (search-forward ":")
          (delete-region (point) (point-max))
          (insert " " selected-item))))))

(defun ollama-buddy-display-token-stats ()
  "Display a visual graph and statistics of token usage."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Token Stats*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)
        (erase-buffer)

        (insert "#+title: Ollama Token Stats\n\n")
        (insert "Press =g= to refresh\n\n")
        (insert-text-button
         "[Reset Stats]"
         'action (lambda (_)
                   (ollama-buddy-reset-token-stats)
                   (ollama-buddy-display-token-stats))
         'help-echo "Clear all token usage history")
        (insert "\n\n")

        (if (null ollama-buddy--token-usage-history)
            (insert "No token usage data available yet.")

          ;; Calculate summary stats in a single pass
          (let* ((total-tokens 0)
                 (total-rate 0.0)
                 (count (length ollama-buddy--token-usage-history))
                 (_ (dolist (info ollama-buddy--token-usage-history)
                      (cl-incf total-tokens (plist-get info :tokens))
                      (cl-incf total-rate (plist-get info :rate))))
                 (avg-rate (/ total-rate (float count))))

            ;; Summary section
            (insert (format "Total tokens generated: *%d*  |  Average token rate: *%.2f* tokens/sec\n\n"
                            total-tokens avg-rate)))

          ;; Group by model
          (let ((model-data (make-hash-table :test 'equal))
                (wait-data (make-hash-table :test 'equal))
                (model-ranks (make-hash-table :test 'equal)))
            (dolist (info ollama-buddy--token-usage-history)
              (let* ((model (ollama-buddy--get-real-model-name (plist-get info :model)))
                     (tokens (plist-get info :tokens))
                     (rate (plist-get info :rate))
                     (model-stats (gethash model model-data nil)))
                (unless model-stats
                  (setq model-stats (list :tokens 0 :count 0 :rates nil)))
                (plist-put model-stats :tokens (+ (plist-get model-stats :tokens) tokens))
                (plist-put model-stats :count (1+ (plist-get model-stats :count)))
                (plist-put model-stats :rates
                           (cons rate (plist-get model-stats :rates)))
                (puthash model model-stats model-data)))

            ;; Gather wait times per model (used for rankings and TTFT chart)
            (dolist (info ollama-buddy--token-usage-history)
              (when-let* ((wt (plist-get info :wait-time)))
                (let* ((model (ollama-buddy--get-real-model-name (plist-get info :model)))
                       (entry (gethash model wait-data)))
                  (unless entry
                    (setq entry (list :waits nil))
                    (puthash model entry wait-data))
                  (plist-put entry :waits (cons wt (plist-get entry :waits))))))

            ;; Model Rankings (composite score across speed, responsiveness, throughput)
            (when (> (hash-table-count model-data) 1)
              (insert "* Model Rankings\n\n"
                      "Composite score: Speed 40% (avg tokens/sec), Responsiveness 30% (time to first token),\n"
                      "Throughput 30% (total tokens). Does not measure response accuracy or quality.\n\n")
              (let ((rankings nil)
                    (max-rate 0)
                    (max-wait 0)
                    (max-tokens 0)
                    (max-model-len 0))
                ;; Compute per-model averages and find maximums
                (maphash
                 (lambda (model stats)
                   (let* ((rates (plist-get stats :rates))
                          (avg-rate (if rates (/ (apply #'+ rates) (float (length rates))) 0))
                          (tokens (plist-get stats :tokens))
                          (waits (plist-get (gethash model wait-data) :waits))
                          (avg-wait (if waits (/ (apply #'+ waits) (float (length waits))) nil)))
                     (push (list :model model :avg-rate avg-rate :tokens tokens :avg-wait avg-wait)
                           rankings)
                     (setq max-rate (max max-rate avg-rate))
                     (when avg-wait (setq max-wait (max max-wait avg-wait)))
                     (setq max-tokens (max max-tokens tokens))
                     (setq max-model-len (max max-model-len (length model)))))
                 model-data)
                ;; Compute composite scores
                (let ((scored
                       (mapcar
                        (lambda (r)
                          (let* ((speed (if (> max-rate 0)
                                            (* (/ (plist-get r :avg-rate) max-rate) 100)
                                          0))
                                 (throughput (if (> max-tokens 0)
                                                 (* (/ (float (plist-get r :tokens)) max-tokens) 100)
                                               0))
                                 (avg-wait (plist-get r :avg-wait))
                                 (has-wait (and avg-wait (> max-wait 0)))
                                 (responsiveness (if has-wait
                                                     (* (- 1 (/ avg-wait max-wait)) 100)
                                                   nil))
                                 (score (if responsiveness
                                            (+ (* 0.4 speed) (* 0.3 responsiveness) (* 0.3 throughput))
                                          (+ (* 0.57 speed) (* 0.43 throughput)))))
                            (cons score r)))
                        rankings)))
                  ;; Sort by score descending
                  (setq scored (sort scored (lambda (a b) (> (car a) (car b)))))
                  ;; Display
                  (let ((rank 0)
                        (max-score (if scored (car (car scored)) 1)))
                    (when (< max-score 1) (setq max-score 1))
                    (insert (format (format "#  %%-%ds │ Score\n" max-model-len) "Model"))
                    (dolist (entry scored)
                      (setq rank (1+ rank))
                      (let* ((score (car entry))
                             (model (plist-get (cdr entry) :model))
                             (medal (pcase rank
                                      (1 "🥇")
                                      (2 "🥈")
                                      (3 "🥉")
                                      (_ (number-to-string rank))))
                             (rank-pad (make-string (max 0 (- 3 (string-width medal))) ?\s))
                             (bar-width (max 1 (round (* 50 (/ score max-score)))))
                             (bar (make-string bar-width ?█)))
                        (puthash model medal model-ranks)
                        (insert (format (format "%%s%%-%ds │ %%s %%.1f\n" max-model-len)
                                        (concat medal rank-pad) model bar score))))
                    (insert "\n")))))

            ;; Display token count graph
            (insert "* Token Count by Model\n\n")
            (let* ((models nil)
                   (max-tokens 0)
                   (max-model-len 0))

              ;; Gather data
              (maphash (lambda (model stats)
                         (push model models)
                         (setq max-tokens (max max-tokens (plist-get stats :tokens)))
                         (setq max-model-len (max max-model-len (length model))))
                       model-data)

              ;; Sort models by token count
              (setq models (sort models (lambda (a b)
                                          (> (plist-get (gethash a model-data) :tokens)
                                             (plist-get (gethash b model-data) :tokens)))))

              ;; Display bar chart
              (dolist (model models)
                (let* ((stats (gethash model model-data))
                       (tokens (plist-get stats :tokens))
                       (count (plist-get stats :count))
                       (bar-width (round (* 50 (/ (float tokens) max-tokens))))
                       (bar (make-string bar-width ?█))
                       (rs (gethash model model-ranks))
                       (rank-prefix (if rs
                                        (concat rs (make-string (max 0 (- 3 (string-width rs))) ?\s))
                                      "")))
                  (insert (format (format "%%s%%-%ds │ %%s %%d tokens (%%d responses)\n"
                                          max-model-len)
                                  rank-prefix
                                  model
                                  bar
                                  tokens count))))

              ;; Display token rate graph
              (insert "\n* Average Token Rate by Model\n\n")
              (let ((max-rate 0))
                ;; Find max rate
                (maphash (lambda (_model stats)
                           (let ((rates (plist-get stats :rates)))
                             (when rates
                               (let ((avg (/ (apply #'+ rates) (float (length rates)))))
                                 (setq max-rate (max max-rate avg))))))
                         model-data)

                ;; Sort models by average rate descending
                (let ((rate-sorted
                       (sort (copy-sequence models)
                             (lambda (a b)
                               (let ((ra (plist-get (gethash a model-data) :rates))
                                     (rb (plist-get (gethash b model-data) :rates)))
                                 (> (if ra (/ (apply #'+ ra) (float (length ra))) 0)
                                    (if rb (/ (apply #'+ rb) (float (length rb))) 0)))))))
                  ;; Display bar chart
                  (dolist (model rate-sorted)
                    (let* ((stats (gethash model model-data))
                           (rates (plist-get stats :rates))
                           (avg-rate (if rates (/ (apply #'+ rates) (float (length rates))) 0))
                           (bar-width (round (* 50 (/ avg-rate max-rate))))
                           (bar (make-string bar-width ?█))
                           (rs (gethash model model-ranks))
                           (rank-prefix (if rs
                                            (concat rs (make-string (max 0 (- 3 (string-width rs))) ?\s))
                                          "")))
                      (insert (format (format "%%s%%-%ds │ %%s %%.1f tokens/sec\n"
                                              max-model-len)
                                      rank-prefix
                                      model
                                      bar
                                      avg-rate)))))))

            ;; Average response wait time by model
            (when (> (hash-table-count wait-data) 0)
              (insert "\n* Average Time to First Token by Model\n\n")
              (let ((wait-models nil)
                    (max-wait 0)
                    (max-model-len 0))
                (maphash (lambda (model entry)
                           (let ((avg (/ (apply #'+ (plist-get entry :waits))
                                         (float (length (plist-get entry :waits))))))
                             (push (cons model avg) wait-models)
                             (setq max-wait (max max-wait avg))
                             (setq max-model-len (max max-model-len (length model)))))
                         wait-data)
                ;; Sort by average wait time ascending (fastest first)
                (setq wait-models (sort wait-models (lambda (a b) (< (cdr a) (cdr b)))))
                (when (> max-wait 0)
                  (dolist (entry wait-models)
                    (let* ((model (car entry))
                           (avg (cdr entry))
                           (count (length (plist-get (gethash model wait-data) :waits)))
                           (bar-width (max 1 (round (* 50 (/ avg max-wait)))))
                           (bar (make-string bar-width ?█))
                           (rs (gethash model model-ranks))
                           (rank-prefix (if rs
                                            (concat rs (make-string (max 0 (- 3 (string-width rs))) ?\s))
                                          "")))
                      (insert (format (format "%%s%%-%ds │ %%s %%.1fs (%%d samples)\n"
                                              max-model-len)
                                      rank-prefix model bar avg count)))))))

            ;; Recent interactions
            (insert "\n* Recent Interactions\n\n")
            (let ((recent (seq-take ollama-buddy--token-usage-history 10)))
              (dolist (info recent)
                (let ((model (ollama-buddy--get-real-model-name (plist-get info :model)))
                      (tokens (plist-get info :tokens))
                      (rate (plist-get info :rate))
                      (wait (plist-get info :wait-time))
                      (time (format-time-string "%Y-%m-%d %H:%M:%S"
                                                (plist-get info :timestamp))))
                  (insert (format "  *%s*: %d tokens (%.2f)%s at %s\n"
                                  model tokens rate
                                  (if wait (format " [wait %.1fs]" wait) "")
                                  time))))))))
      (goto-char (point-min))
      (visual-line-mode -1)
      (setq truncate-lines t)
      (view-mode 1)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "g") #'ollama-buddy-display-token-stats)
        (setq-local minor-mode-overriding-map-alist
                    (list (cons 'view-mode map)))))
    (display-buffer buf)))

(defun ollama-buddy-history-save ()
  "Save the edited history back to `ollama-buddy--conversation-history-by-model'."
  (interactive)
  (unless (and (boundp 'ollama-buddy-editing-history)
               ollama-buddy-editing-history)
    (user-error "Not in an Ollama history edit buffer"))
  
  (condition-case err
      (let ((edited-history (read (buffer-string)))
            (model (or ollama-buddy--current-model
                       ollama-buddy-default-model)))
        ;; Validate the edited history
        (unless (listp edited-history)
          (user-error "Invalid history format: must be an alist"))
        
        (puthash model edited-history ollama-buddy--conversation-history-by-model)
        
        ;; Provide feedback and clean up
        (message "History saved successfully")
        (ollama-buddy-history-edit-model)
        (ollama-buddy--update-status "History Updated"))
    (error
     (message "Error saving history: %s" (error-message-string err)))))

(defun ollama-buddy-history-edit-model ()
  "Edit the conversation history for a specific MODEL."
  (interactive)
  (let ((buf (get-buffer-create ollama-buddy--history-edit-buffer))
        (model (or ollama-buddy--current-model
                   ollama-buddy-default-model)))
    
    (with-current-buffer buf
      (org-mode)
      (setq-local org-hide-emphasis-markers t)
      (setq-local org-hide-leading-stars t)
      (erase-buffer)
      (visual-line-mode 1)
      (setq-local ollama-buddy--history-view-mode 'display)

      ;; Get this model's history
      (let ((model-history (gethash model ollama-buddy--conversation-history-by-model nil)))
        ;; Display the history in human-readable format initially
        (insert (format "#+title: Conversation History for model: *%s*\n\n" model))
        
        (if (null model-history)
            (insert "No conversation history available")
          (let ((history-count (/ (length model-history) 2)))
            (insert (format "History contains %d message pairs\n\n" history-count))
            ;; Display the history in chronological order
            (dolist (msg model-history)
              (let* ((role (alist-get 'role msg))
                     (content (alist-get 'content msg)))
                (insert (format "* [%s]\n\n" (upcase role)))
                (insert (format "%s\n\n" content)))))))
      
      (let ((map (copy-keymap org-mode-map)))  ;; Start with org-mode map
        (define-key map (kbd "C-x C-q")
                    (lambda () (interactive)
                      (ollama-buddy-history-toggle-edit-model model)))
        (define-key map (kbd "C-c C-k") (lambda ()
                                          (interactive)
                                          (kill-buffer)))
        (use-local-map map))
      
      ;; Set buffer-local variables
      (setq-local ollama-buddy-editing-history nil)
      (setq-local ollama-buddy-editing-model model)
      (setq header-line-format
            (format "History for %s - Press C-x C-q to edit, C-c C-k to cancel" model)))
    
    ;; Display the buffer
    (pop-to-buffer buf)
    (goto-char (point-min))
    (view-mode 1)
    (message "Press C-x C-q to edit history, C-c C-k to cancel")))

(defun ollama-buddy-history-toggle-edit-model (&optional model)
  "Toggle between viewing and editing modes for history."
  (interactive)
  (let ((inhibit-read-only t)
        (model (or model
                   ollama-buddy--current-model
                   ollama-buddy-default-model
                   (error "No default model set"))))
    (cond
     ;; Switch from display to edit mode
     ((eq ollama-buddy--history-view-mode 'display)
      (org-mode)
      (setq-local org-hide-emphasis-markers t)
      (setq-local org-hide-leading-stars t)
      (erase-buffer)
      (buffer-disable-undo)
      (buffer-enable-undo)
      (read-only-mode -1)
      (emacs-lisp-mode)
      (visual-line-mode 1)
      
      ;; Convert the hashtable to an alist for easier editing
      (let ((history-alist (gethash model ollama-buddy--conversation-history-by-model nil)))
        ;; Insert the pretty-printed history
        (let ((print-level nil)
              (print-length nil))
          (pp history-alist (current-buffer))))
      
      ;; Update mode and keys
      (setq-local ollama-buddy--history-view-mode 'edit)
      (setq-local ollama-buddy-editing-history t)
      (let ((map (copy-keymap org-mode-map)))  ;; Start with org-mode map
        (define-key map (kbd "C-c C-k") (lambda ()
                                          (interactive)
                                          (ollama-buddy-history-edit-model)))
        (define-key map (kbd "C-c C-c") 'ollama-buddy-history-save)
        (use-local-map map))
      (setq header-line-format "Edit history and press C-c C-c to save, C-c C-k to cancel")
      (message "Now in edit mode. Press C-c C-c to save, C-c C-k to cancel"))
     
     ;; Switch from edit to display mode
     ((eq ollama-buddy--history-view-mode 'edit)
      ;; If there are unsaved changes, confirm before switching back
      (when (and (buffer-modified-p)
                 (not (y-or-n-p "Discard unsaved changes? ")))
        (message "Edit mode maintained")
        (cl-return-from ollama-buddy-history-toggle-edit-mode))
      
      ;; Switch back to display mode
      (ollama-buddy-history-edit-model)
      
      ;; Update mode and header
      (setq-local ollama-buddy--history-view-mode 'display)
      (setq-local ollama-buddy-editing-history nil)
      (setq header-line-format "Press C-x C-q to edit, C-c C-k to cancel")
      (message "Viewing mode. Press C-x C-q to edit, C-c C-k to cancel")))
    (goto-char (point-min))))

;;;###autoload
(defun ollama-buddy-update-command-with-params (entry-name &rest props-and-params)
  "Update command ENTRY-NAME with properties and parameters.
PROPS-AND-PARAMS should be property-value pairs,
with an optional :parameters property followed by parameter-value pairs."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let ((current-plist (cdr entry))
          properties
          parameters)
      
      ;; Split into properties and parameters
      (let ((params-pos (cl-position :parameters props-and-params)))
        (if params-pos
            (progn
              (setq properties (cl-subseq props-and-params 0 params-pos))
              (when (< (+ params-pos 1) (length props-and-params))
                (setq parameters (nth (+ params-pos 1) props-and-params))))
          (setq properties props-and-params)))
      
      ;; Update properties
      (while properties
        (let ((prop (car properties))
              (value (cadr properties)))
          (setq current-plist (plist-put current-plist prop value))
          (setq properties (cddr properties))))
      
      ;; Update parameters if provided
      (when parameters
        (setq current-plist (plist-put current-plist :parameters parameters)))
      
      ;; Update the command definition
      (setf (cdr entry) current-plist)))
  ollama-buddy-command-definitions)

(defun ollama-buddy-add-parameters-to-command (entry-name &rest parameters)
  "Add specific parameters to ENTRY-NAME command.
PARAMETERS should be a plist with parameter names and values."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let* ((current-plist (cdr entry))
           (current-params (plist-get current-plist :parameters))
           (new-params (if current-params
                           current-params
                         (list))))
      
      ;; Process parameter pairs and add to list
      (cl-loop for (param value) on parameters by #'cddr do
               (push (cons param value) new-params))
      
      ;; Update the command definition
      (setf (cdr entry) (plist-put current-plist :parameters new-params))))
  ollama-buddy-command-definitions)

;;;###autoload
(defun ollama-buddy-update-menu-entry (entry-name &rest props)
  "Update menu entry ENTRY-NAME with property-value pairs in PROPS.
PROPS should be a sequence of property-value pairs."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (let ((current-plist (cdr entry)))
      (while props
        (let ((prop (car props))
              (value (cadr props)))
          (setq current-plist (plist-put current-plist prop value))
          (setq props (cddr props))))
      (setf (cdr entry) current-plist)))
  ollama-buddy-command-definitions)

(defun ollama-buddy-params-reset ()
  "Reset all parameters to default values and clear modification tracking."
  (interactive)
  (setq ollama-buddy-params-active (copy-tree ollama-buddy-params-defaults)
        ollama-buddy-params-modified nil)
  (ollama-buddy--update-status "Params Reset")
  (message "Ollama parameters reset to defaults"))

(defun ollama-buddy-params-edit (param)
  "Edit a specific parameter PARAM interactively."
  (interactive
   (list (intern (completing-read "Select parameter to edit: "
                                  (mapcar (lambda (pair) (symbol-name (car pair)))
                                          ollama-buddy-params-active)
                                  nil t))))
  (let* ((current-value (alist-get param ollama-buddy-params-active))
         (default-value (alist-get param ollama-buddy-params-defaults))
         (prompt (format "Set %s (default: %s): " param default-value))
         (new-value
          (cond
           ((or (booleanp current-value) (booleanp default-value))
            (y-or-n-p (format "Enable %s? " param)))
           ((integerp current-value)
            (read-number prompt current-value))
           ((floatp current-value)
            (read-number prompt current-value))
           ((vectorp current-value)
            (let ((items (split-string
                          (read-string
                           (format "Enter stop sequences (comma-separated): %s"
                                   (mapconcat #'identity current-value ","))
                           nil nil (mapconcat #'identity current-value ","))
                          "," t "\\s-*")))
              (vconcat [] items)))
           (t (read-string prompt (format "%s" current-value))))))
    
    ;; Track whether this parameter is being modified or reset to default
    (if (equal new-value default-value)
        (setq ollama-buddy-params-modified
              (delete param ollama-buddy-params-modified))
      (add-to-list 'ollama-buddy-params-modified param))
    
    ;; Update the parameter value
    (setf (alist-get param ollama-buddy-params-active) new-value)
    (ollama-buddy--update-status "Params changed")
    (message "Updated %s to %s%s"
             param
             new-value
             (if (equal new-value default-value)
                 " (default value)"
               ""))))

(defconst ollama-buddy--params-help-text
  '((temperature . "Controls randomness of generation (0.0-1.0+). Lower values make output more focused and deterministic, higher values make it more creative and varied.")
    (top_k . "Limits token selection to the top K most probable tokens. Lower values are more focused, higher values allow more variety.")
    (top_p . "Nucleus sampling threshold (0.0-1.0). Only tokens whose cumulative probability exceeds this threshold are considered. Works alongside top_k.")
    (min_p . "Minimum probability threshold for token selection. Tokens below this probability relative to the most likely token are filtered out.")
    (typical_p . "Controls how 'typical' responses are. Filters tokens based on how much they deviate from the expected information content.")
    (repeat_last_n . "Number of tokens to look back when applying repetition penalties. Set to 0 to disable, -1 for the full context.")
    (repeat_penalty . "Penalty applied to tokens that have already appeared (higher = less repetition). A value of 1.0 means no penalty.")
    (presence_penalty . "Penalises tokens that have appeared at all in the text so far, encouraging the model to explore new topics.")
    (frequency_penalty . "Penalises tokens proportionally to how often they have appeared, reducing frequent word repetition.")
    (mirostat . "Enable adaptive sampling to maintain a target perplexity. 0 = off, 1 = Mirostat, 2 = Mirostat 2.0.")
    (mirostat_tau . "Target entropy (perplexity) for Mirostat sampling. Lower values produce more focused text, higher values more diverse.")
    (mirostat_eta . "Learning rate for Mirostat. Controls how quickly the algorithm adjusts to maintain the target entropy.")
    (penalize_newline . "Whether to penalise newline tokens during generation. Disabling can help with structured or multi-line output.")
    (stop . "Sequences that will stop generation when produced. The model halts as soon as any stop sequence is emitted.")
    (num_keep . "Number of tokens from the initial prompt to retain when the context window is full and tokens must be discarded.")
    (seed . "Random seed for deterministic generation. Using the same seed with the same parameters produces identical output.")
    (num_predict . "Maximum number of tokens to generate in the response. Set to -1 for unlimited, -2 to fill the context window.")
    (numa . "Enable Non-Uniform Memory Access optimisation. Can improve performance on multi-socket server systems.")
    (num_ctx . "Context window size in tokens. Larger values allow longer conversations but use more memory.")
    (num_batch . "Number of tokens to process in parallel during prompt evaluation. Larger values use more memory but can be faster.")
    (num_gpu . "Number of GPU layers to offload. Set to 0 for CPU-only. Higher values offload more computation to the GPU.")
    (main_gpu . "Index of the primary GPU to use for computation when multiple GPUs are available.")
    (low_vram . "Optimise for systems with limited VRAM by reducing GPU memory usage at the cost of speed.")
    (vocab_only . "Load only the model vocabulary without weights. Useful for tokenisation tasks without generation.")
    (use_mmap . "Use memory-mapped files to load the model. Faster loading and allows sharing memory between processes.")
    (use_mlock . "Lock model weights in RAM to prevent swapping to disk. Requires sufficient available memory.")
    (num_thread . "Number of CPU threads to use for generation. Typically set to the number of physical cores for best performance."))
  "Help text for each Ollama API parameter.")

(defun ollama-buddy--params-format-value (value)
  "Format parameter VALUE for display."
  (cond
   ((vectorp value) (mapconcat #'identity value ", "))
   ((eq value t) "yes")
   ((null value) "no")
   (t (format "%s" value))))

(defun ollama-buddy--params-insert-section (params)
  "Insert PARAMS as second-level org headings with help text."
  (dolist (param params)
    (let* ((value (alist-get param ollama-buddy-params-active))
           (default-value (alist-get param ollama-buddy-params-defaults))
           (modifiedp (memq param ollama-buddy-params-modified))
           (val-str (ollama-buddy--params-format-value value))
           (def-str (ollama-buddy--params-format-value default-value))
           (help (alist-get param ollama-buddy--params-help-text)))
      (insert (format "** %s %s"
                      (if modifiedp (format "*%s %s*" param val-str)
                        (format "%s %s" param val-str))
                      (if modifiedp (format "(default: %s)" def-str) "")))
      (insert "\n\n")
      (when help
        (insert help)
        (insert "\n\n")))))

(defun ollama-buddy-params-display ()
  "Display the current Ollama parameter settings."
  (interactive)
  (let ((buf (get-buffer-create "*Ollama Parameters*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)

        (erase-buffer)

        (insert "#+title: Ollama API Parameters\n\n")
        (insert "Press =g= to refresh, =0= to reset all, =TAB= to expand for details\n\n")

        (let ((generation-params '(temperature top_k top_p min_p typical_p
                                               repeat_last_n repeat_penalty presence_penalty
                                               frequency_penalty mirostat mirostat_tau mirostat_eta
                                               penalize_newline stop))
              (resource-params '(num_keep seed num_predict numa num_ctx num_batch
                                          num_gpu main_gpu low_vram vocab_only use_mmap
                                          use_mlock num_thread)))

          (insert "* Generation Parameters\n\n")
          (ollama-buddy--params-insert-section generation-params)

          (insert "* Resource Parameters\n\n")
          (ollama-buddy--params-insert-section resource-params)))

      (goto-char (point-min))
      (org-content 2)
      (view-mode 1)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "g") (lambda () (interactive) (ollama-buddy-params-display)))
        (define-key map (kbd "0") (lambda () (interactive)
                                    (ollama-buddy-params-reset)
                                    (ollama-buddy-params-display)))
        (setq-local minor-mode-overriding-map-alist
                    (list (cons 'view-mode map)))))
    (display-buffer buf)))

(defun ollama-buddy-toggle-params-in-header ()
  "Toggle display of modified parameters in the header line."
  (interactive)
  (setq ollama-buddy-show-params-in-header
        (not ollama-buddy-show-params-in-header))
  (ollama-buddy--update-status ollama-buddy--status)
  (message "Parameters in header: %s"
           (if ollama-buddy-show-params-in-header "enabled" "disabled")))

(defun ollama-buddy-reset-all-prompts ()
  "Reset the system prompt to default (none)."
  (interactive)
  (setq ollama-buddy--current-system-prompt nil)
  
  ;; Update the UI to reflect the change
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (ollama-buddy--prepare-prompt-area t))
  
  ;; Update status
  (ollama-buddy--update-status "reset")
  (message "System prompt has been reset"))

(defun ollama-buddy-show-raw-model-info (&optional model)
  "Retrieve and display raw JSON information about the current default MODEL."
  (interactive)
  (let* ((model (or model
                    ollama-buddy--current-model
                    ollama-buddy-default-model
                    (error "No default model set")))
         (endpoint "/api/show")
         (payload (json-encode `((model . ,(ollama-buddy--get-real-model-name model))))))
    
    ;; Update status to show operation in progress
    (ollama-buddy--update-status (format "Fetching info for %s..." model))
    
    ;; Make API request asynchronously to get model info
    (ollama-buddy--make-request-async-backend
     endpoint
     "POST"
     payload
     (lambda (status result)
       (if (plist-get status :error)
           (progn
             (message "Error retrieving model info: %s" (cdr (plist-get status :error)))
             (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
               (pop-to-buffer (current-buffer))
               (goto-char (point-max))
               (insert (format "\n\n** [ERROR] Failed to retrieve info for model: %s\n\n" model))
               (insert (format "Error: %s\n\n" (cdr (plist-get status :error))))
               (ollama-buddy--prepare-prompt-area)
               (ollama-buddy--update-status "Error retrieving model info")))
         ;; Success path
         (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
           (pop-to-buffer (current-buffer))
           (goto-char (point-max))
           
           ;; Insert model info header
           (insert (format "[MODEL INFO REQUEST]\n\n** [MODEL INFO: %s]\n\n" model))

           ;; Pretty print the JSON response
           (insert "#+begin_src json\n")
           (let ((json-start (point)))
             ;; Insert JSON data
             (insert (json-encode result))
             ;; Pretty print the inserted JSON
             (json-pretty-print json-start (point)))
           (insert "\n#+end_src")
           
           ;; Add a prompt area after the information
           (ollama-buddy--prepare-prompt-area)
           (ollama-buddy--update-status "Model info displayed")))))))

(defun ollama-buddy-toggle-debug-mode ()
  "Toggle display of raw JSON messages in a debug buffer."
  (interactive)
  (setq ollama-buddy-debug-mode (not ollama-buddy-debug-mode))
  (if ollama-buddy-debug-mode
      (progn
        (with-current-buffer (get-buffer-create ollama-buddy--debug-buffer)
          (erase-buffer)
          (insert "=== Ollama Buddy Debug Mode ===\n")
          (insert "Raw JSON messages will appear here.\n\n")
          (special-mode))
        (display-buffer ollama-buddy--debug-buffer)
        (message "Debug mode enabled - raw JSON will be shown"))
    (when (get-buffer ollama-buddy--debug-buffer)
      (kill-buffer ollama-buddy--debug-buffer))
    (message "Debug mode disabled")))

(defun ollama-buddy-set-system-prompt ()
  "Set the current prompt as a system prompt."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data)))
    
    ;; Add to history if non-empty
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (add-to-history 'ollama-buddy--prompt-history prompt-text))
    
    ;; Set as system prompt with auto-generated title
    (setq ollama-buddy--current-system-prompt prompt-text)
    (ollama-buddy--update-system-prompt-display-info prompt-text)
    
    ;; Update the UI to reflect the change
    (ollama-buddy--prepare-prompt-area t t t)
    (ollama-buddy--prepare-prompt-area nil nil)
    
    ;; Update status to show system prompt is set
    (ollama-buddy--update-status "System prompt set")
    (message "System prompt set: %s" 
             (or ollama-buddy--current-system-prompt-title "Custom Prompt"))))

(defun ollama-buddy-reset-system-prompt ()
  "Reset the system prompt to default (none)."
  (interactive)
  (setq ollama-buddy--current-system-prompt nil)
  
  ;; Update the UI to reflect the change
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (ollama-buddy--prepare-prompt-area t))
  
  ;; Update status
  (ollama-buddy--update-status "reset")
  (message "System prompt has been reset"))

(defun ollama-buddy--get-prompt-content ()
  "Extract the current prompt content from the buffer.
Returns a cons cell (TEXT . POINT) with the prompt text and point position."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):" nil t)
        (let ((start-point (point)))
          (search-forward ":")
          (cons (string-trim (buffer-substring-no-properties
                              (point) (point-max)))
                start-point))
      (cons "" nil))))

(defun ollama-buddy--prepare-command-prompt (command-name &optional selected-text)
  "Prepare prompt for COMMAND-NAME with optional SELECTED-TEXT.
Returns the full prompt text ready to be sent."
  (let* ((cmd-prompt (ollama-buddy--get-command-prop command-name :prompt))
         (model (ollama-buddy--get-command-prop command-name :model))
         (content (or selected-text ""))
         (full-prompt (if cmd-prompt
                          (concat cmd-prompt "\n\n" content)
                        content)))
    
    ;; Temporarily switch model if command has its own model
    (when model
      (setq ollama-buddy--current-request-temporary-model ollama-buddy--current-model)
      (setq ollama-buddy--current-model model))
    
    ;; Prepare the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (pop-to-buffer (current-buffer))
      (ollama-buddy--prepare-prompt-area t nil)  ;; New prompt, no content
      (goto-char (point-max))
      (insert (string-trim full-prompt)))
    
    full-prompt))

(defun ollama-buddy-toggle-markdown-conversion ()
  "Toggle automatic conversion of markdown to `org-mode' format."
  (interactive)
  (setq ollama-buddy-convert-markdown-to-org
        (not ollama-buddy-convert-markdown-to-org))
  (ollama-buddy--update-status
   (if ollama-buddy-convert-markdown-to-org "Markdown conversion enabled" "Markdown conversion disabled"))
  (message "Markdown to Org conversion: %s"
           (if ollama-buddy-convert-markdown-to-org "enabled" "disabled")))

(defun ollama-buddy--ensure-sessions-directory ()
  "Create the ollama-buddy sessions directory if it doesn't exist."
  (unless (file-directory-p ollama-buddy-sessions-directory)
    (make-directory ollama-buddy-sessions-directory t)))

(defun ollama-buddy--autosave-transcript ()
  "Auto-save the chat buffer to a recovery file.
Called after each completed response to protect against data loss."
  (condition-case nil
      (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
        (ollama-buddy--ensure-sessions-directory)
        (let ((recovery-file (expand-file-name "~autosave.org"
                                               ollama-buddy-sessions-directory)))
          (with-current-buffer ollama-buddy--chat-buffer
            (write-region (point-min) (point-max) recovery-file nil 'quiet))))
    (error nil)))

(defun ollama-buddy--generate-session-name ()
  "Generate a session name from the first user message.
Filters stop words and returns up to 5 key words joined by hyphens."
  (when-let* ((history (gethash ollama-buddy--current-model
                                ollama-buddy--conversation-history-by-model nil))
              (first-msg (car history))
              (content (cdr (assoc 'content first-msg))))
    (let* ((stop-words '("the" "a" "an" "is" "are" "was" "were" "be" "been"
                         "being" "have" "has" "had" "do" "does" "did" "will"
                         "would" "could" "should" "may" "might" "can" "shall"
                         "to" "of" "in" "for" "on" "with" "at" "by" "from"
                         "as" "into" "about" "that" "this" "it" "i" "me" "my"
                         "we" "our" "you" "your" "he" "she" "they" "them"
                         "what" "which" "who" "how" "when" "where" "why"
                         "not" "no" "so" "if" "or" "and" "but" "just" "also"
                         "please" "help" "want" "need" "like" "think" "know"))
           (words (split-string (downcase content) "[^a-z0-9]+" t))
           (filtered (seq-remove (lambda (w) (or (member w stop-words)
                                                 (< (length w) 3)))
                                 words))
           (key-words (seq-take filtered 5)))
      (if key-words
          (string-join key-words "-")
        (string-join (seq-take (split-string content) 5) "-")))))

(defun ollama-buddy--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to an alist for serialization."
  (let ((alist '()))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

(defun ollama-buddy--alist-to-hash-table (alist)
  "Convert ALIST back to a hash table."
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (puthash (car pair) (cdr pair) hash-table))
    hash-table))

(defun ollama-buddy-sessions-directory ()
  "Jump to the session directory."
  (interactive)
  (ollama-buddy--ensure-sessions-directory)
  (dired-other-window ollama-buddy-sessions-directory))

(defun ollama-buddy-sessions-save ()
  "Save the current Ollama Buddy session including attachments."
  (interactive)
  (let* ((description (or ollama-buddy-current-session-name
                          (ollama-buddy--generate-session-name)))
         (default-name (concat (format-time-string "%F-%H%M%S")
                               (if (and description (not (string-empty-p description)))
                                   (concat "-" description)
                                 "")))
         (session-name (read-string "Session name/description: " default-name))
         (session-file (expand-file-name (concat session-name ".el") 
                                         ollama-buddy-sessions-directory))
         (org-file (expand-file-name (concat session-name ".org") 
                                     ollama-buddy-sessions-directory))
         ;; Convert hash table to alist for serialization
         (history-alist (ollama-buddy--hash-table-to-alist 
                         ollama-buddy--conversation-history-by-model))
         ;; Strip :results (embedding vectors) from RAG attachments for serialization
         (rag-attachments
          (when (and (featurep 'ollama-buddy-rag)
                     (boundp 'ollama-buddy-rag--current-results)
                     ollama-buddy-rag--current-results)
            (mapcar (lambda (r)
                      (list :query (plist-get r :query)
                            :index-name (plist-get r :index-name)
                            :content (plist-get r :content)
                            :tokens (plist-get r :tokens)
                            :timestamp (plist-get r :timestamp)))
                    ollama-buddy-rag--current-results)))
         (session-data
          `(:version "1.0"
                     :model ,(or ollama-buddy--current-model ollama-buddy-default-model)
                     :history ,history-alist
                     :attachments ,ollama-buddy--current-attachments
                     :system-prompt ,ollama-buddy--current-system-prompt
                     :params-active ,ollama-buddy-params-active
                     :params-modified ,ollama-buddy-params-modified
                     :rag-attachments ,rag-attachments
                     :created-time ,(current-time)
                     :session-name ,session-name
                     :default-directory ,(with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
                                           default-directory))))

    (ollama-buddy--ensure-sessions-directory)
    
    ;; Write session data
    (with-temp-file session-file
      (insert ";; Ollama Buddy session file\n")
      (insert (format ";; Session: %s\n" session-name))
      (insert (format ";; Created: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (let ((print-length nil)
            (print-level nil))
        (pp session-data (current-buffer))))
    
    ;; Save chat buffer to org file
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (write-region (point-min) (point-max) org-file))
    
    ;; Remove autosave file since we have a proper save now
    (let ((recovery-file (expand-file-name "~autosave.org"
                                           ollama-buddy-sessions-directory)))
      (when (file-exists-p recovery-file)
        (delete-file recovery-file)))

    (setq ollama-buddy-current-session-name description)
    (ollama-buddy-update-mode-line)
    (message "Session saved as %s" session-name)))


(defun ollama-buddy-update-mode-line ()
  "Update the mode line to show the current session name and tone."
  (let ((segment-name 'ollama-buddy-mode-line-segment))

    ;; Define a dynamic segment that reads session and tone at display time
    (setq ollama-buddy-mode-line-segment
          '(:eval
            (let* ((session (or ollama-buddy-current-session-name "No Session"))
                   (tone (and (boundp 'ollama-buddy--current-tone)
                              ollama-buddy--current-tone))
                   (show-tone (and tone (not (string= tone "Normal")))))
              (if show-tone
                  (format "[%s] %s" session tone)
                (format "[%s]" session)))))

    ;; Search and replace the existing segment in the mode line
    (setq mode-line-format
          (mapcar (lambda (segment)
                    (if (and (listp segment)
                             (eq (car segment) segment-name))
                        ollama-buddy-mode-line-segment
                      segment))
                  mode-line-format))

    ;; If the segment isn't already present, add it at the beginning
    (unless (member ollama-buddy-mode-line-segment mode-line-format)
      (setq mode-line-format
            (cons ollama-buddy-mode-line-segment mode-line-format))))

  ;; Force an update of the mode line
  (force-mode-line-update t))

(defun ollama-buddy-sessions-load ()
  "Load an Ollama Buddy session including attachments."
  (interactive)
  (let* ((session-files (directory-files ollama-buddy-sessions-directory t "\\.el$"))
         (session-names (mapcar #'file-name-base session-files))
         (chosen-session (completing-read "Choose a session to load: " session-names nil t))
         (session-file (expand-file-name (concat chosen-session ".el") 
                                         ollama-buddy-sessions-directory))
         (org-file (expand-file-name (concat chosen-session ".org") 
                                     ollama-buddy-sessions-directory)))
    
    ;; Read and parse session data
    (let ((session-data (with-temp-buffer
                          (insert-file-contents session-file)
                          (goto-char (point-min))
                          ;; Skip comments
                          (while (looking-at ";;")
                            (forward-line))
                          (read (current-buffer)))))
      
      ;; Restore model
      (setq ollama-buddy--current-model (plist-get session-data :model))
      
      ;; Restore conversation history (convert alist back to hash table)
      (setq ollama-buddy--conversation-history-by-model
            (ollama-buddy--alist-to-hash-table (plist-get session-data :history)))
      
      ;; Restore attachments
      (setq ollama-buddy--current-attachments
            (plist-get session-data :attachments))
      
      ;; Restore prompts
      (setq ollama-buddy--current-system-prompt
            (plist-get session-data :system-prompt))

      ;; Restore parameters if available
      (when (plist-get session-data :params-active)
        (setq ollama-buddy-params-active (plist-get session-data :params-active)))
      (when (plist-get session-data :params-modified)
        (setq ollama-buddy-params-modified (plist-get session-data :params-modified)))

      ;; Restore RAG attachments if available
      (when (and (featurep 'ollama-buddy-rag)
                 (boundp 'ollama-buddy-rag--current-results)
                 (plist-get session-data :rag-attachments))
        (setq ollama-buddy-rag--current-results
              (plist-get session-data :rag-attachments)))

      ;; Load org file contents
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (let ((inhibit-read-only t))
          ;; Restore default-directory if saved and still exists
          (let ((saved-dir (plist-get session-data :default-directory)))
            (when (and saved-dir (file-directory-p saved-dir))
              (setq default-directory saved-dir)))
          (pop-to-buffer (current-buffer))
          (erase-buffer)
          (when (file-exists-p org-file)
            (insert-file-contents org-file))
          (org-mode)
          (visual-line-mode 1)
          (ollama-buddy-mode 1)
          (ollama-buddy--fold-all-thinking-blocks)
          (goto-char (point-max))))

      (setq ollama-buddy-current-session-name chosen-session)
      (ollama-buddy-update-mode-line)
      (ollama-buddy--update-status (format "Session '%s' loaded" chosen-session))
      
      ;; Show information about loaded session
      (message "Session loaded: %s%s%s%s"
               chosen-session
               (if ollama-buddy--current-attachments
                   (format " (%d attachments)" (length ollama-buddy--current-attachments))
                 "")
               (if (and (featurep 'ollama-buddy-rag)
                        (boundp 'ollama-buddy-rag--current-results)
                        ollama-buddy-rag--current-results)
                   (format " (%d RAG)" (length ollama-buddy-rag--current-results))
                 "")
               (if ollama-buddy--current-system-prompt " [system prompt]" "")))))

(defun ollama-buddy-sessions-rename ()
  "Rename the current session."
  (interactive)
  (let* ((default (or ollama-buddy-current-session-name
                      (ollama-buddy--generate-session-name)
                      ""))
         (new-name (read-string "Session name: " default)))
    (when (and new-name (not (string-empty-p new-name)))
      (setq ollama-buddy-current-session-name new-name)
      (ollama-buddy-update-mode-line)
      (message "Session renamed to: %s" new-name))))

(defun ollama-buddy-sessions-new ()
  "Start a new session by clearing history and buffer."
  (interactive)

  (when (y-or-n-p "Are you sure? ")
    ;; Confirm if we have a current session
    (when (and ollama-buddy--current-session
               (not (yes-or-no-p
                     (format "Start new session? Current session '%s' will be discarded unless saved.  ?"
                             ollama-buddy--current-session))))
      (user-error "Operation cancelled"))
    
    ;; Clear current session
    (setq ollama-buddy--current-session nil)
    (setq ollama-buddy-current-session-name nil)

    ;; Clear current system prompt
    (setq ollama-buddy--current-system-prompt nil)
    
    ;; Clear all model histories
    (clrhash ollama-buddy--conversation-history-by-model)

    ;; Clear all attachments directly (no extra confirmation)
    (setq ollama-buddy--current-attachments nil)
    (when (boundp 'ollama-buddy-web-search--current-results)
      (setq ollama-buddy-web-search--current-results nil))
    (when (featurep 'ollama-buddy-rag)
      (ollama-buddy-rag-clear-attached))

    ;; Reset tools to disabled
    (when (featurep 'ollama-buddy-tools)
      (setq ollama-buddy-tools-enabled nil)
      (setq ollama-buddy-tools-auto-execute nil))

    ;; Clear the chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (let ((inhibit-read-only t))
        (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
        (erase-buffer)
        (ollama-buddy-mode 1)
        (insert (ollama-buddy--create-intro-message))
        (save-excursion
          (when (re-search-backward "^\\*\\* More Commands$" nil t)
            (org-fold-hide-subtree)))
        (ollama-buddy--prepare-prompt-area)))

    ;; Update status and mode line
    (ollama-buddy--update-status "New session started")
    (ollama-buddy-update-mode-line)
    (message "Started new session")))

(defun ollama-buddy-exit ()
  "Close the Ollama Buddy chat buffer and clear conversation history.
Prompts for confirmation before closing."
  (interactive)
  (when (yes-or-no-p "Close Ollama Buddy session? ")
    (when-let* ((buf (get-buffer ollama-buddy--chat-buffer)))
      (clrhash ollama-buddy--conversation-history-by-model)
      (setq ollama-buddy--current-attachments nil)
      (when (boundp 'ollama-buddy-web-search--current-results)
        (setq ollama-buddy-web-search--current-results nil))
      (when (featurep 'ollama-buddy-rag)
        (ollama-buddy-rag-clear-attached))
      (when (featurep 'ollama-buddy-tools)
        (setq ollama-buddy-tools-enabled nil)
        (setq ollama-buddy-tools-auto-execute nil))
      (quit-window nil (get-buffer-window buf))
      (kill-buffer buf))))

(defun ollama-buddy-unload-model ()
  "Unload running models from memory.
Offers a completing-read of currently running models plus an
\"[All]\" option to unload everything."
  (interactive)
  (let ((running (ollama-buddy--get-running-models)))
    (if (null running)
        (message "No models currently loaded")
      (let* ((candidates (cons "[All]" running))
             (choice (completing-read "Unload model: " candidates nil t)))
        (let ((to-unload (if (string= choice "[All]") running (list choice))))
          (dolist (model to-unload)
            (let ((payload (json-encode `((model . ,model) (keep_alive . 0)))))
              (ollama-buddy--make-request "/api/generate" "POST" payload))
            (message "Unloaded %s" model))
          (when (string= choice "[All]")
            (message "Unloaded all models")))))))

(defun ollama-buddy-clear-history (&optional all-models)
  "Clear the conversation history.
With prefix argument ALL-MODELS, clear history for all models."
  (interactive "P")
  (if all-models
      (progn
        (clrhash ollama-buddy--conversation-history-by-model)
        (ollama-buddy--update-status "All models' history cleared")
        (message "Ollama conversation history cleared for all models"))
    (let ((model ollama-buddy--current-model))
      (remhash model ollama-buddy--conversation-history-by-model)
      (ollama-buddy--update-status (format "History cleared for %s" model))
      (message "Ollama conversation history cleared for %s" model))))

(defun ollama-buddy-toggle-history ()
  "Toggle conversation history on/off."
  (interactive)
  (setq ollama-buddy-history-enabled (not ollama-buddy-history-enabled))
  (ollama-buddy--update-status
   (if ollama-buddy-history-enabled "History enabled" "History disabled"))
  (message "Ollama conversation history %s"
           (if ollama-buddy-history-enabled "enabled" "disabled")))

(defun ollama-buddy--update-token-rate-display ()
  "Update the token rate display in real-time."
  (when (and ollama-buddy--current-token-start-time
             (> ollama-buddy--current-token-count 0))
    (let* ((current-time (float-time))
           (total-rate (if (> (- current-time ollama-buddy--current-token-start-time) 0)
                           (/ ollama-buddy--current-token-count
                              (- current-time ollama-buddy--current-token-start-time))
                         0)))

      (cond
       ((and ollama-buddy-hide-reasoning
             ollama-buddy--in-reasoning-section)
        (ollama-buddy--update-status ollama-buddy--reasoning-status-message))
       ;; Show "Thinking..." during thinking phase (collapse mode)
       ((or ollama-buddy--thinking-api-active
            (and ollama-buddy-collapse-thinking
                 ollama-buddy--in-reasoning-section))
        (ollama-buddy--update-status
         (format "Thinking... [%d %.1f]"
                 ollama-buddy--current-token-count total-rate)))
       ;; Normal working
       (t
        (ollama-buddy--update-status
         (format "Working... [%d %.1f]"
                 ollama-buddy--current-token-count total-rate))))
      
      ;; Update tracking variables
      (setq ollama-buddy--last-token-count ollama-buddy--current-token-count
            ollama-buddy--last-update-time current-time))))

(defun ollama-buddy--model-average-wait-time (model)
  "Return the average wait time in seconds for MODEL, or nil if no data.
Compares model names after stripping provider prefixes for consistency."
  (let* ((real-name (ollama-buddy--get-real-model-name model))
         (wait-times
          (cl-loop for entry in ollama-buddy--token-usage-history
                   for entry-model = (plist-get entry :model)
                   when (and entry-model
                             (string= (ollama-buddy--get-real-model-name entry-model)
                                      real-name)
                             (plist-get entry :wait-time)
                             (numberp (plist-get entry :wait-time))
                             (> (plist-get entry :wait-time) 0))
                   collect (plist-get entry :wait-time))))
    (when wait-times
      (/ (apply #'+ wait-times) (float (length wait-times))))))

(defun ollama-buddy--clear-response-countdown ()
  "Remove countdown text from the RESPONSE header, if present."
  (when (and ollama-buddy--response-countdown-marker
             (marker-buffer ollama-buddy--response-countdown-marker)
             (buffer-live-p (marker-buffer ollama-buddy--response-countdown-marker)))
    (with-current-buffer (marker-buffer ollama-buddy--response-countdown-marker)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char ollama-buddy--response-countdown-marker)
          (when (search-forward "]" (line-end-position) t)
            (delete-region ollama-buddy--response-countdown-marker
                          (1- (point))))))))
  (when ollama-buddy--response-countdown-marker
    (set-marker ollama-buddy--response-countdown-marker nil))
  (setq ollama-buddy--response-countdown-marker nil
        ollama-buddy--response-avg-wait nil))

(defun ollama-buddy--trim-token-history ()
  "Trim token usage history to `ollama-buddy-token-history-max-size'."
  (when (> (length ollama-buddy--token-usage-history)
           ollama-buddy-token-history-max-size)
    (setcdr (nthcdr (1- ollama-buddy-token-history-max-size)
                    ollama-buddy--token-usage-history)
            nil)))

(defun ollama-buddy-reset-token-stats ()
  "Clear all token usage history."
  (interactive)
  (when (yes-or-no-p "Reset all token stats? ")
    (setq ollama-buddy--token-usage-history nil)
    (message "Token stats reset")))

(defun ollama-buddy--start-response-wait-timer (&optional model)
  "Start the response-wait elapsed timer if threshold is configured.
When MODEL is provided, compute the average wait time for countdown display."
  ;; Temporarily detach countdown state so that cancel (which clears any
  ;; previous countdown) does not destroy the marker the send function
  ;; just created for the *current* request.
  (let ((new-marker ollama-buddy--response-countdown-marker)
        (new-avg ollama-buddy--response-avg-wait))
    (setq ollama-buddy--response-countdown-marker nil
          ollama-buddy--response-avg-wait nil)
    (ollama-buddy--cancel-response-wait-timer)
    (setq ollama-buddy--response-countdown-marker new-marker
          ollama-buddy--response-avg-wait new-avg))
  (setq ollama-buddy--response-wait-duration nil)
  (setq ollama-buddy--response-wait-start (float-time))
  (when (and model (not ollama-buddy--response-avg-wait))
    (setq ollama-buddy--response-avg-wait
          (ollama-buddy--model-average-wait-time model)))
  (when (or ollama-buddy-response-wait-threshold
            ollama-buddy--response-countdown-marker)
    (setq ollama-buddy--response-wait-timer
          (run-with-timer 1 1 #'ollama-buddy--update-response-wait-display))))

(defun ollama-buddy--cancel-response-wait-timer ()
  "Cancel the response-wait timer and reset state.
Captures the elapsed wait duration before clearing."
  (when ollama-buddy--response-wait-timer
    (cancel-timer ollama-buddy--response-wait-timer)
    (setq ollama-buddy--response-wait-timer nil))
  (when ollama-buddy--response-wait-start
    (setq ollama-buddy--response-wait-duration
          (- (float-time) ollama-buddy--response-wait-start))
    (setq ollama-buddy--response-wait-start nil))
  (ollama-buddy--clear-response-countdown))

(defun ollama-buddy--update-response-wait-display ()
  "Update the status line and in-buffer countdown with elapsed wait time."
  (if (null ollama-buddy--response-wait-start)
      ;; First token already arrived — cancel ourselves
      (ollama-buddy--cancel-response-wait-timer)
    (let ((elapsed (round (- (float-time) ollama-buddy--response-wait-start))))
      (when (and ollama-buddy-response-wait-threshold
                 (>= elapsed ollama-buddy-response-wait-threshold))
        (ollama-buddy--update-status (format "Working... [%ds]" elapsed)))
      ;; Update in-buffer countdown
      (when (and ollama-buddy--response-countdown-marker
                 ollama-buddy--response-avg-wait
                 (marker-buffer ollama-buddy--response-countdown-marker)
                 (buffer-live-p (marker-buffer ollama-buddy--response-countdown-marker)))
        (with-current-buffer (marker-buffer ollama-buddy--response-countdown-marker)
          (let ((inhibit-read-only t)
                (remaining (round (- ollama-buddy--response-avg-wait elapsed))))
            (save-excursion
              (goto-char ollama-buddy--response-countdown-marker)
              (when (search-forward "]" (line-end-position) t)
                (delete-region ollama-buddy--response-countdown-marker
                              (1- (point)))
                (goto-char ollama-buddy--response-countdown-marker)
                (insert (if (> remaining 0)
                            (format " ~%ds" remaining)
                          (format " +%ds" (abs remaining))))))))))))

(defun ollama-buddy-toggle-show-history-indicator ()
  "Toggle display of token statistics after each response."
  (interactive)
  (setq ollama-buddy-show-history-indicator (not ollama-buddy-show-history-indicator))
  (ollama-buddy--update-status (concat "History display " (if ollama-buddy-show-history-indicator "enabled" "disabled")))
  (message "History display: %s"
           (if ollama-buddy-show-history-indicator "enabled" "disabled")))

(defun ollama-buddy-roles--get-available-roles ()
  "Scan the preset directory and extract role names from filenames."
  (if (not (file-directory-p ollama-buddy-roles-directory))
      (progn
        (message "Error: Ollama Buddy roles directory does not exist: %s"
                 ollama-buddy-roles-directory)
        nil)
    (let ((files (directory-files ollama-buddy-roles-directory nil "^ollama-buddy--preset__.*\\.el$"))
          roles)
      (if (null files)
          (progn
            (message "No role preset files found in directory: %s"
                     ollama-buddy-roles-directory)
            nil)
        (dolist (file files)
          (when (string-match "ollama-buddy--preset__\\(.*\\)\\.el$" file)
            (push (match-string 1 file) roles)))
        (sort roles #'string<)))))

(defun ollama-buddy-roles--load-role-preset (role)
  "Load the preset file for ROLE."
  (let ((preset-file (expand-file-name
                      (format "ollama-buddy--preset__%s.el" role)
                      ollama-buddy-roles-directory)))
    (if (file-exists-p preset-file)
        (progn
          (load-file preset-file)
          (setq ollama-buddy-roles--current-role role)
          (message "Loaded Ollama Buddy role: %s" role))
      (message "Role preset file not found: %s" preset-file))))


(defun ollama-buddy-roles-switch-role ()
  "Switch to a different ollama-buddy role."
  (interactive)
  (let ((roles (ollama-buddy-roles--get-available-roles)))
    (if (null roles)
        (message "No role presets available. Use C-c O → I to install extras, or create files in %s"
                 ollama-buddy-roles-directory)
      (let ((role (completing-read
                   (format "Select role (current: %s): " ollama-buddy-roles--current-role)
                   roles nil t)))
        (ollama-buddy-roles--load-role-preset role)))))

(defun ollama-buddy-role-creator--create-command ()
  "Create a new command interactively."
  (let* ((command-name (read-string "Command name (e.g., my-command): "))
         (key (read-char "Press key for menu shortcut: "))
         (description (read-string "Description: "))
         (use-model (y-or-n-p "Use specific model? "))
         (model (if use-model
                    (completing-read "Model: " (ollama-buddy--get-models) nil t)
                  nil))
         (use-prompt (y-or-n-p "Add a user prompt prefix? "))
         (prompt (if use-prompt
                     (read-string "User prompt prefix: ")
                   nil))
         (use-system (y-or-n-p "Add a system prompt/message? "))
         (system (if use-system
                     (read-string "System prompt/message: ")
                   nil))
         (symbol (intern command-name)))
    ;; Generate the command definition
    (list symbol
          :key key
          :description description
          :model model
          :prompt prompt
          :system system
          :action `(lambda ()
                     (ollama-buddy--send-with-command ',symbol)))))

(defun ollama-buddy-role-creator-generate-role-file (role-name commands &optional menu-columns)
  "Generate a role file for ROLE-NAME with COMMANDS.
Optional MENU-COLUMNS specifies the number of columns for the menu display."
  (let ((file-path (expand-file-name
                    (format "ollama-buddy--preset__%s.el" role-name)
                    ollama-buddy-roles-directory)))
    ;; Create directory if it doesn't exist
    (unless (file-directory-p ollama-buddy-roles-directory)
      (make-directory ollama-buddy-roles-directory t))
    ;; Generate the file content
    (with-temp-file file-path
      (insert (format ";; ollama-buddy preset for role: %s\n" role-name))
      (insert ";; Generated by ollama-buddy-role-creator\n\n")
      (insert "(require 'ollama-buddy)\n\n")
      ;; Insert menu columns setting if specified
      (when menu-columns
        (insert (format ";; Menu display columns for this role\n"))
        (insert (format "(setq ollama-buddy-menu-columns %d)\n\n" menu-columns)))
      (insert "(setq ollama-buddy-command-definitions\n")
      (insert "  '(\n")
      ;; Insert the standard commands first
      (insert "    ;; Standard commands\n")
      (dolist (cmd '(open-chat show-models switch-role create-role open-roles-directory swap-model help send-region))
        (when-let ((cmd-def (ollama-buddy--get-command-def cmd)))
          (insert (format "    %S\n" cmd-def))))
      ;; Insert custom commands
      (insert "\n    ;; Custom commands for this role\n")
      (dolist (cmd commands)
        (insert (format "    %S\n" cmd)))
      ;; Close the list and provide call
      (insert "    ))\n\n"))
    ;; Return the file path
    file-path))

;;;###autoload
(defun ollama-buddy-role-creator-create-new-role ()
  "Create a new role interactively."
  (interactive)
  ;; Ensure the directory exists
  (ollama-buddy-roles-create-directory)
  (let ((role-name (read-string "Role name: "))
        (menu-columns (read-number "Menu columns (default 2): " 2))
        (commands '())
        (continue t))
    ;; Main command creation loop
    (while continue
      (message "Adding command %d..." (1+ (length commands)))
      (push (ollama-buddy-role-creator--create-command) commands)
      (setq continue (y-or-n-p "Add another command? ")))
    ;; Generate the role file
    (let ((file-path (ollama-buddy-role-creator-generate-role-file
                      role-name commands menu-columns)))
      (message "Role saved to %s" file-path)
      ;; Ask to load the new role
      (when (y-or-n-p "Load this role now? ")
        (ollama-buddy-roles--load-role-preset role-name)))))

;; Helper function to create the roles directory
(defun ollama-buddy-roles-create-directory ()
  "Create the ollama-buddy roles directory if it doesn't exist."
  (interactive)
  (if (file-exists-p ollama-buddy-roles-directory)
      (message "Ollama Buddy roles directory already exists: %s"
               ollama-buddy-roles-directory)
    (if (yes-or-no-p
         (format "Create Ollama Buddy roles directory at %s? "
                 ollama-buddy-roles-directory))
        (progn
          (make-directory ollama-buddy-roles-directory t)
          (message "Created Ollama Buddy roles directory: %s"
                   ollama-buddy-roles-directory))
      (message "Directory creation cancelled."))))

;; Function to open the roles directory in dired
(defun ollama-buddy-roles-open-directory ()
  "Open the ollama-buddy roles directory in Dired."
  (interactive)
  (if (not (file-directory-p ollama-buddy-roles-directory))
      (if (yes-or-no-p
           (format "Roles directory doesn't exist.  Create it at %s? "
                   ollama-buddy-roles-directory))
          (progn
            (make-directory ollama-buddy-roles-directory t)
            (dired ollama-buddy-roles-directory))
        (message "Directory not created."))
    (dired-other-window ollama-buddy-roles-directory)))

(defun ollama-buddy--extras-missing-p ()
  "Return non-nil if presets or user prompts directories are missing."
  (or (not (file-directory-p ollama-buddy-roles-directory))
      (not (file-directory-p ollama-buddy-user-prompts-directory))))

(defun ollama-buddy--install-extras-from-dir (src-dir presets-needed prompts-needed)
  "Install extras from SRC-DIR.
PRESETS-NEEDED and PROMPTS-NEEDED control which directories to copy."
  (when presets-needed
    (let ((src (expand-file-name "ollama-buddy-presets" src-dir)))
      (when (file-directory-p src)
        (copy-directory src ollama-buddy-roles-directory nil t t)
        (message "Installed presets to %s"
                 (abbreviate-file-name ollama-buddy-roles-directory)))))
  (when prompts-needed
    (let ((src (expand-file-name "ollama-buddy-user-prompts" src-dir)))
      (when (file-directory-p src)
        (copy-directory src ollama-buddy-user-prompts-directory nil t t)
        (message "Installed user prompts to %s"
                 (abbreviate-file-name ollama-buddy-user-prompts-directory)))))
  (message "Ollama Buddy extras installed successfully!"))

(defun ollama-buddy--install-extras-from-github (presets-needed prompts-needed)
  "Download extras from GitHub and install.
PRESETS-NEEDED and PROMPTS-NEEDED control which directories to install."
  (unless (executable-find "tar")
    (error "tar command not found; cannot extract downloaded archive"))
  (let ((tmp-tar (make-temp-file "ollama-buddy-" nil ".tar.gz"))
        (tmp-dir (make-temp-file "ollama-buddy-extract-" t))
        (url "https://github.com/captainflasmr/ollama-buddy/archive/refs/heads/main.tar.gz"))
    (unwind-protect
        (progn
          (message "Downloading ollama-buddy extras from GitHub...")
          (url-copy-file url tmp-tar t)
          (message "Extracting...")
          (unless (zerop (call-process "tar" nil nil nil "xzf" tmp-tar "-C" tmp-dir))
            (error "Failed to extract archive"))
          (let ((extracted (car (directory-files tmp-dir t "^[^.]"))))
            (unless extracted
              (error "No directory found in extracted archive"))
            (ollama-buddy--install-extras-from-dir
             extracted presets-needed prompts-needed)))
      (ignore-errors (delete-file tmp-tar))
      (ignore-errors (delete-directory tmp-dir t)))))

(defun ollama-buddy-install-extras ()
  "Install presets and user prompts to Emacs directory.
Copies from the local package directory if the bundled directories
are present, otherwise downloads from GitHub."
  (interactive)
  (let* ((presets-needed (not (file-directory-p ollama-buddy-roles-directory)))
         (prompts-needed (not (file-directory-p ollama-buddy-user-prompts-directory)))
         (items (delq nil
                      (list (when presets-needed "presets")
                            (when prompts-needed "user prompts")))))
    (if (null items)
        (message "Presets and user prompts are already installed.")
      (let* ((pkg-dir (file-name-directory (locate-library "ollama-buddy")))
             (local-available
              (and pkg-dir
                   (or (and presets-needed
                            (file-directory-p
                             (expand-file-name "ollama-buddy-presets" pkg-dir)))
                       (and prompts-needed
                            (file-directory-p
                             (expand-file-name "ollama-buddy-user-prompts" pkg-dir)))))))
        (when (yes-or-no-p
               (format "%s %s to %s? "
                       (if local-available "Install" "Download and install")
                       (string-join items " and ")
                       (abbreviate-file-name user-emacs-directory)))
          (if local-available
              (ollama-buddy--install-extras-from-dir
               pkg-dir presets-needed prompts-needed)
            (ollama-buddy--install-extras-from-github
             presets-needed prompts-needed)))))))

(defun ollama-buddy--initialize-chat-buffer ()
  "Initialize the chat buffer and check Ollama status."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (when (= (buffer-size) 0)
      (org-mode)
      (setq-local org-hide-emphasis-markers t)
      (setq-local org-hide-leading-stars t)
      (with-no-warnings
        (if (boundp 'org-fold-catch-invisible-edits)
            (setq-local org-fold-catch-invisible-edits nil)
          (setq-local org-catch-invisible-edits nil)))
      (visual-line-mode 1)
      (ollama-buddy-mode 1)
      (ollama-buddy--check-status)
      (insert (ollama-buddy--create-intro-message))
      ;; Fold the "More Commands" heading so it doesn't clutter the welcome screen
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\*\\* More Commands$" nil t)
          (beginning-of-line)
          (org-fold-hide-subtree)))
      ;; now set up default model if none exist
      (setq ollama-buddy--current-model ollama-buddy-default-model)
      (when (not ollama-buddy-default-model)
        ;; just get the first model
        (let ((model (car (ollama-buddy--get-models))))
          (if model
              (progn
                (setq ollama-buddy--current-model model)
                (setq ollama-buddy-default-model model)
                (insert (format "\n\n* NO DEFAULT MODEL : Using best guess : %s" model)))
            (insert "\n\n* OFFLINE : Ollama server is not running — start it with =ollama serve="))))
      ;; Auto-load project summary if available (before prompt area setup)
      (when (featurep 'ollama-buddy-project)
        (ollama-buddy-project-auto-load-summary))
      (ollama-buddy--prepare-prompt-area)
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1))
    (ollama-buddy--update-status (if ollama-buddy--current-model "Idle" "OFFLINE"))
    (ollama-buddy-update-mode-line)
    ;; Fetch capabilities asynchronously so Emacs doesn't block
    (when (and ollama-buddy--current-model
               (ollama-buddy--ollama-running))
      (ollama-buddy--fetch-model-context-size-async
       ollama-buddy--current-model
       (lambda ()
         (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
           (with-current-buffer ollama-buddy--chat-buffer
             (ollama-buddy--prepare-prompt-area t t))))))))

(defun ollama-buddy--stream-filter (_proc output)
  "Process stream OUTPUT while preserving cursor position.
Accumulates partial data in `ollama-buddy--stream-pending' and processes
complete newline-delimited JSON lines.  This ensures no data is lost when
TCP packets split a JSON object across multiple filter calls."

  ;; Save match data at the start to prevent clobbering
  (save-match-data
    ;; Log raw output to debug buffer if enabled
    (when ollama-buddy-debug-mode
      (with-current-buffer (get-buffer-create ollama-buddy--debug-buffer)
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (start-point (point)))
          (insert (format "\n=== MESSAGE %s ===\n"
                          (format-time-string "%H:%M:%S.%3N")))
          (insert (replace-regexp-in-string "\r" "" output) "\n")
          (save-excursion
            (save-match-data  ; Save match data for nested operations
              (goto-char start-point)
              (when (search-forward "{" nil t)
                (goto-char (match-beginning 0))
                (let ((json-start (point)))
                  (when (ignore-errors (forward-sexp) t)
                    (let ((json-end (point)))
                      (json-pretty-print json-start json-end))))))))))

    ;; Accumulate output into pending buffer and process complete lines
    (setq ollama-buddy--stream-pending
          (concat ollama-buddy--stream-pending output))

    ;; Strip HTTP headers if still present (first chunk contains them).
    ;; Capture the status code so non-2xx responses can be handled specially.
    (when (string-match "^HTTP/[0-9.]+ \\([0-9]+\\)" ollama-buddy--stream-pending)
      (let ((status (string-to-number (match-string 1 ollama-buddy--stream-pending))))
        (when (string-match "\r?\n\r?\n" ollama-buddy--stream-pending)
          (setq ollama-buddy--stream-pending
                (substring ollama-buddy--stream-pending (match-end 0)))
          (unless (and (>= status 200) (< status 300))
            (setq ollama-buddy--stream-http-status status)))))

    ;; Non-2xx: the error body is a single pretty-printed JSON object, not
    ;; newline-delimited.  Try to parse the whole body at once; ignore-errors
    ;; handles the case where the body has not fully arrived yet (next chunk
    ;; retries).  On success, display the error and tear down the process.
    ;;
    ;; Strip any leading non-`{' bytes first so a chunked-transfer chunk size
    ;; (e.g. `82\r\n') doesn't get parsed as the integer 82 and crash
    ;; downstream alist-get calls.  We also require the parse to yield a
    ;; cons (alist) so a stray scalar can't masquerade as an error object.
    (when ollama-buddy--stream-http-status
      (let* ((body (replace-regexp-in-string
                    "\\`[^{]*" "" ollama-buddy--stream-pending))
             (error-json (and (> (length body) 0)
                              (ignore-errors
                                (json-read-from-string body)))))
        (when (consp error-json)
          (let ((status-str (ollama-buddy--handle-http-error
                             ollama-buddy--stream-http-status error-json)))
            (ollama-buddy--update-status status-str))
          (setq ollama-buddy--stream-pending "")
          ;; Delete the process; sentinel fires with "deleted" and skips
          ;; inserting the normal completion/interrupted message.
          (when (and ollama-buddy--active-process
                     (process-live-p ollama-buddy--active-process))
            (delete-process ollama-buddy--active-process)))))

    ;; Process all complete newline-delimited JSON lines.
    ;; Skipped when we are in HTTP-error mode to avoid spurious parse attempts.
    (unless ollama-buddy--stream-http-status
      (while (string-match "\\([^\n]*\\)\n" ollama-buddy--stream-pending)
        (let* ((line (match-string 1 ollama-buddy--stream-pending)))
          (setq ollama-buddy--stream-pending
                (substring ollama-buddy--stream-pending (match-end 0)))
          ;; Strip any non-JSON prefix (e.g. chunk encoding) and parse
          (let* ((json-str (replace-regexp-in-string "^[^{]*" "" line))
                 (json-data (when (and (stringp json-str)
                                       (> (length json-str) 0))
                              (ignore-errors
                                (json-read-from-string json-str)))))
            (when json-data
              (ollama-buddy--stream-process-json json-data))))))))

(defun ollama-buddy--stream-process-json (json-data)
  "Process a single parsed JSON-DATA object from the Ollama stream."
  (save-match-data
    (let* ((error-msg (alist-get 'error json-data))
           (message-data (alist-get 'message json-data))
           (text (when message-data (alist-get 'content message-data)))
           ;; thinking field: used by models like deepseek-r1 instead of <think> tags
           (thinking-text (when message-data (alist-get 'thinking message-data)))
           (tool-calls-raw (when message-data (alist-get 'tool_calls message-data)))
           (tool-calls (when tool-calls-raw
                         (if (vectorp tool-calls-raw)
                             (append tool-calls-raw nil)
                           tool-calls-raw))))

      ;; Check for errors in JSON response (e.g. "prompt too long", auth errors)
      (when error-msg
        (let ((is-auth-error (or (string-match-p "unauthorized\\|authentication\\|sign.?in\\|not.?logged" error-msg)
                                 (string-match-p "401\\|403" error-msg))))
          ;; Update auth cache if this is an auth error
          (when is-auth-error
            (ollama-buddy--set-cloud-auth-status nil))
          (with-current-buffer ollama-buddy--chat-buffer
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-max))
                (if is-auth-error
                    (insert (format "\n\n*Authentication Error:* %s\n\nPlease sign in using =C-c A= or =M-x ollama-buddy-cloud-signin=" error-msg))
                  (insert (format "\n\n*Error:* %s" error-msg)))
                (ollama-buddy--prepare-prompt-area))))
          ;; Clean up timers — the request is over
          (when ollama-buddy--token-update-timer
            (cancel-timer ollama-buddy--token-update-timer)
            (setq ollama-buddy--token-update-timer nil))
          (ollama-buddy--cancel-response-wait-timer)
          (ollama-buddy--update-status (if is-auth-error "Auth Required" "Error"))))

      ;; Handle thinking tokens from the dedicated API field (e.g. deepseek-r1).
      ;; These models stream thinking in message.thinking with content="" rather
      ;; than embedding <think>...</think> tags in message.content.
      (when (and thinking-text (not (string-empty-p thinking-text)))
        ;; Start token timer on first token
        (unless ollama-buddy--current-token-start-time
          (ollama-buddy--cancel-response-wait-timer)
          (setq ollama-buddy--current-token-start-time (float-time)
                ollama-buddy--last-token-count 0
                ollama-buddy--last-update-time nil)
          (when ollama-buddy--token-update-timer
            (cancel-timer ollama-buddy--token-update-timer))
          (setq ollama-buddy--token-update-timer
                (run-with-timer 0 ollama-buddy--token-update-interval
                                #'ollama-buddy--update-token-rate-display)))
        (setq ollama-buddy--current-token-count (1+ ollama-buddy--current-token-count))
        (with-current-buffer ollama-buddy--chat-buffer
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (cond
               ;; Collapse: accumulate + insert folded (peekable via TAB)
               (ollama-buddy-collapse-thinking
                (unless ollama-buddy--thinking-api-active
                  (setq ollama-buddy--thinking-api-active t
                        ollama-buddy--thinking-arrow-marker (ollama-buddy--insert-thinking-header)
                        ollama-buddy--thinking-block-start  (copy-marker (point) t)))
                ;; Accumulate thinking tokens
                (setq ollama-buddy--thinking-content-accumulator
                      (concat ollama-buddy--thinking-content-accumulator thinking-text))
                ;; Always insert into buffer; extend fold so text stays hidden
                (insert thinking-text)
                (ollama-buddy--extend-thinking-fold
                 ollama-buddy--thinking-arrow-marker))
               ;; Hide: silently discard
               (ollama-buddy-hide-reasoning
                (setq ollama-buddy--thinking-api-active t))
               ;; Show: insert raw
               (t
                (insert thinking-text)))))))

      ;; Rest of the function remains the same...
      (when text
        ;; Set start time if this is the first token and start the update timer
        (unless ollama-buddy--current-token-start-time
          (ollama-buddy--cancel-response-wait-timer)
          (setq ollama-buddy--current-token-start-time (float-time)
                ollama-buddy--last-token-count 0
                ollama-buddy--last-update-time nil)

          ;; Start the real-time update timer
          (when ollama-buddy--token-update-timer
            (cancel-timer ollama-buddy--token-update-timer))
          
          (setq ollama-buddy--token-update-timer
                (run-with-timer 0 ollama-buddy--token-update-interval
                                #'ollama-buddy--update-token-rate-display)))
        
        ;; Increment token count when text is received
        (when (not (string-empty-p text))
          (setq ollama-buddy--current-token-count (1+ ollama-buddy--current-token-count)))
        
        (with-current-buffer ollama-buddy--chat-buffer
          (let* ((inhibit-read-only t)
                 (window (get-buffer-window ollama-buddy--chat-buffer t))
                 (old-point (and window (window-point window)))
                 (old-window-start (and window (window-start window)))
                 (response-start (if (markerp ollama-buddy--response-start-position)
                                     (marker-position ollama-buddy--response-start-position)
                                   ollama-buddy--response-start-position))
                 (completed nil))
            (save-excursion
              (goto-char (point-max))

              (setq ollama-buddy--reasoning-marker-found nil)

              ;; Close thinking-API block when content tokens start arriving.
              ;; deepseek-r1 style: thinking came via message.thinking, content="" until now.
              (when (and ollama-buddy--thinking-api-active
                         (not (string-empty-p text)))
                (setq ollama-buddy--thinking-api-active nil
                      ollama-buddy--reasoning-skip-newlines t)
                (when (and ollama-buddy-collapse-thinking
                           ollama-buddy--thinking-block-start)
                  (ollama-buddy--finalize-thinking-block
                   ollama-buddy--thinking-arrow-marker)
                  (set-marker ollama-buddy--thinking-block-start nil)
                  (setq ollama-buddy--thinking-block-start  nil
                        ollama-buddy--thinking-arrow-marker nil)
                  ;; Re-anchor point to the real end of buffer: finalize
                  ;; deleted the raw thinking region and re-inserted content
                  ;; + `*** Response', which collapses the outer goto-char
                  ;; marker into the new content.  Without this, the first
                  ;; response token is inserted inside the thinking block.
                  (goto-char (point-max))))

              (cond
               ;; --- Collapse mode: stream content visibly, then fold heading ---
               (ollama-buddy-collapse-thinking
                (setq ollama-buddy--reasoning-marker-found
                      (ollama-buddy--find-reasoning-marker text))
                (cond
                 ;; Start marker: insert header as real text, record content start
                 ((and ollama-buddy--reasoning-marker-found
                       (eq (car ollama-buddy--reasoning-marker-found) 'start))
                  (setq ollama-buddy--in-reasoning-section t
                        ollama-buddy--thinking-arrow-marker (ollama-buddy--insert-thinking-header)
                        ollama-buddy--thinking-block-start  (copy-marker (point) t)))
                 ;; End marker: finalize the *** Think heading
                 ((and ollama-buddy--reasoning-marker-found
                       (eq (car ollama-buddy--reasoning-marker-found) 'end)
                       ollama-buddy--in-reasoning-section)
                  (setq ollama-buddy--in-reasoning-section nil
                        ollama-buddy--reasoning-skip-newlines t)
                  (when ollama-buddy--thinking-block-start
                    (ollama-buddy--finalize-thinking-block
                     ollama-buddy--thinking-arrow-marker)
                    (set-marker ollama-buddy--thinking-block-start nil)
                    (setq ollama-buddy--thinking-block-start  nil
                          ollama-buddy--thinking-arrow-marker nil)
                    ;; Re-anchor point to the real end of buffer (see
                    ;; matching comment in the API-style path above).
                    (goto-char (point-max))))))

               ;; --- Hide mode: show status message then delete block ---
               (ollama-buddy-hide-reasoning
                (setq ollama-buddy--reasoning-marker-found
                      (ollama-buddy--find-reasoning-marker text))
                (cond
                 ((and ollama-buddy--reasoning-marker-found
                       (eq (car ollama-buddy--reasoning-marker-found) 'start))
                  (setq ollama-buddy--in-reasoning-section t
                        ollama-buddy--reasoning-status-message
                        (format "%s..."
                                (capitalize
                                 (replace-regexp-in-string
                                  "[<>]" "" (car (cdr ollama-buddy--reasoning-marker-found))))))
                  (setq ollama-buddy--start-point (point))
                  (insert ollama-buddy--reasoning-status-message))
                 ((and ollama-buddy--reasoning-marker-found
                       (eq (car ollama-buddy--reasoning-marker-found) 'end))
                  (setq ollama-buddy--in-reasoning-section nil
                        ollama-buddy--reasoning-status-message nil
                        ollama-buddy--reasoning-skip-newlines t)
                  (when ollama-buddy--start-point
                    (delete-region ollama-buddy--start-point (point-max))
                    (setq ollama-buddy--start-point nil))))))

              ;; Legacy: clean up stray start-point when neither mode is active
              (when (and (not ollama-buddy-hide-reasoning)
                         (not ollama-buddy-collapse-thinking)
                         ollama-buddy--start-point)
                (delete-region ollama-buddy--start-point (point-max))
                (setq ollama-buddy--start-point nil))

              ;; Insert text unless: hide-mode and inside block, or any marker chunk found
              (unless (or (and ollama-buddy-hide-reasoning
                               (not ollama-buddy-collapse-thinking)
                               ollama-buddy--in-reasoning-section)
                          ollama-buddy--reasoning-marker-found)

                ;; Conditional header insertion for text content
                (when (and (not ollama-buddy--header-inserted-p)
                           (not (string-empty-p (string-trim text))))
                  (let ((pos (ollama-buddy--insert-response-header
                              ollama-buddy--current-model
                              ollama-buddy--current-original-model
                              ollama-buddy--current-has-images)))
                    ;; Update response start marker if the header changed it
                    (when pos
                      (set-marker ollama-buddy--response-start-position pos)
                      (set-marker pos nil)
                      ;; Move point past the header so text inserts after it
                      (goto-char (point-max)))))

                ;; In collapse mode, accumulate thinking content (peekable via TAB)
                (if (and ollama-buddy-collapse-thinking
                         ollama-buddy--in-reasoning-section
                         ollama-buddy--thinking-content-accumulator)
                    (progn
                      (setq ollama-buddy--thinking-content-accumulator
                            (concat ollama-buddy--thinking-content-accumulator text))
                      ;; Always insert into buffer; extend fold so text stays hidden
                      (insert text)
                      (ollama-buddy--extend-thinking-fold
                       ollama-buddy--thinking-arrow-marker))
                  ;; Skip leading newlines immediately after a thinking block ends
                  (if (and ollama-buddy--reasoning-skip-newlines
                           (not ollama-buddy--in-reasoning-section)
                           (string-match "^[\n\r]+" text))
                      (let ((cleaned-text (replace-regexp-in-string "^[\n\r]+" "" text)))
                        (unless (string-empty-p cleaned-text)
                          (insert cleaned-text)
                          (setq ollama-buddy--reasoning-skip-newlines nil)))
                    (insert text))))
              
              ;; Track the complete response for history
              (when (boundp 'ollama-buddy--current-response)
                (setq ollama-buddy--current-response
                      (concat (or ollama-buddy--current-response "") text)))

              (unless (boundp 'ollama-buddy--current-response)
                (setq ollama-buddy--current-response text))

              ;; Accumulate tool calls from streaming chunks
              (when tool-calls
                (let* ((func (alist-get 'function (car tool-calls)))
                       (tool-name (when func (alist-get 'name func)))
                       (msg (if tool-name
                                (format "⚒ Preparing %s…" tool-name)
                              "⚒ Preparing tool call…")))
                  (ollama-buddy--update-status msg)
                  (unless ollama-buddy--current-tool-calls
                    (message "%s" msg)))
                (setq ollama-buddy--current-tool-calls
                      (nconc ollama-buddy--current-tool-calls
                             (copy-sequence tool-calls))))

              ;; Check if this response is complete
              (when (eq (alist-get 'done json-data) t)
                ;; Cancel the response wait timer immediately on completion
                (ollama-buddy--cancel-response-wait-timer)

                (let ((batch-start (if (markerp ollama-buddy--turn-start-position)
                                       (marker-position ollama-buddy--turn-start-position)
                                     (if (markerp ollama-buddy--response-start-position)
                                         (marker-position ollama-buddy--response-start-position)
                                       ollama-buddy--response-start-position))))

                  ;; Update cloud auth status on successful cloud model response
                  (when (and ollama-buddy--current-model
                             (ollama-buddy--cloud-model-p ollama-buddy--current-model))
                    (ollama-buddy--set-cloud-auth-status t))

                  ;; Add the user message to history
                  (ollama-buddy--add-to-history "user" ollama-buddy--current-prompt)
                  ;; Add the complete response to history
                  (ollama-buddy--add-to-history
                   "assistant"
                   (or ollama-buddy--current-response "")
                   ollama-buddy--current-tool-calls)

                  ;; Branch: tool calls vs normal completion
                  (if (and (featurep 'ollama-buddy-tools)
                           (bound-and-true-p ollama-buddy-tools-enabled)
                           ollama-buddy--current-tool-calls
                           (< ollama-buddy--tool-call-iteration
                              (if (boundp 'ollama-buddy-tools-max-iterations)
                                  ollama-buddy-tools-max-iterations 10)))

                      ;; === TOOL BATCH PATH ===
                      (let* ((thinking-content
                              (cond
                               ;; API-style thinking (deepseek-r1): accumulated in var
                               (ollama-buddy--thinking-content-accumulator
                                (prog1 ollama-buddy--thinking-content-accumulator
                                  (setq ollama-buddy--thinking-content-accumulator nil)))
                               ;; Tag-style thinking: extract from response text
                               ((and ollama-buddy--current-response
                                     (car (ollama-buddy--extract-thinking-from-response
                                           ollama-buddy--current-response)))
                                (car (ollama-buddy--extract-thinking-from-response
                                      ollama-buddy--current-response)))
                               (t nil)))
                             (response-text
                              (when ollama-buddy--current-response
                                (let ((extracted (ollama-buddy--extract-thinking-from-response
                                                  ollama-buddy--current-response)))
                                  (if (car extracted) (cdr extracted) ollama-buddy--current-response))))
                             ;; Convert response-text via md-to-org if enabled
                             (response-text
                              (if (and response-text
                                       (not (string-empty-p response-text))
                                       ollama-buddy-convert-markdown-to-org)
                                  (with-temp-buffer
                                    (insert response-text)
                                    (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
                                    (buffer-string))
                                response-text)))

                        (setq ollama-buddy--tool-call-iteration
                              (1+ ollama-buddy--tool-call-iteration))

                        ;; Execute tool calls
                        (let ((tool-results
                               (ollama-buddy-tools--process-tool-calls
                                ollama-buddy--current-tool-calls)))

                          ;; REBUILD: delete streaming artifacts, insert structured batch
                          (ollama-buddy--rebuild-tool-batch
                           batch-start
                           ollama-buddy--current-model
                           ollama-buddy--current-tool-calls
                           tool-results
                           thinking-content
                           response-text)

                          ;; Add tool results to history
                          (dolist (result tool-results)
                            (ollama-buddy--add-to-history-raw result))

                          ;; Reset state for next turn
                          (setq ollama-buddy--current-tool-calls nil
                                ollama-buddy--thinking-api-active nil
                                ollama-buddy--in-reasoning-section nil
                                ollama-buddy--thinking-content-accumulator nil)
                          (when ollama-buddy--thinking-block-start
                            (set-marker ollama-buddy--thinking-block-start nil)
                            (setq ollama-buddy--thinking-block-start nil))
                          (when ollama-buddy--thinking-arrow-marker
                            (setq ollama-buddy--thinking-arrow-marker nil))
                          (makunbound 'ollama-buddy--current-response)
                          (setq ollama-buddy--header-inserted-p nil)
                          (setq ollama-buddy--response-start-position (copy-marker (point-max)))

                          ;; Send continuation
                          (if (and (boundp 'ollama-buddy-tools--stop-after-batch)
                                   ollama-buddy-tools--stop-after-batch)
                              (progn
                                (setq ollama-buddy-tools--stop-after-batch nil)
                                (setq ollama-buddy--suppress-tools-once t)
                                (ollama-buddy--send "continue, I will manually apply the changes using ediff, please just supply a brief summary of what was changed making sure to mention that the file is available in the eregistry" ollama-buddy--current-model nil))
                            (ollama-buddy--send nil ollama-buddy--current-model t))))

                    ;; === NORMAL COMPLETION PATH ===

                    ;; If still in thinking-API phase at stream end, finalize
                    (when ollama-buddy--thinking-api-active
                      (setq ollama-buddy--thinking-api-active nil)
                      (when (and ollama-buddy-collapse-thinking
                                 ollama-buddy--thinking-block-start)
                        (ollama-buddy--finalize-thinking-block
                         ollama-buddy--thinking-arrow-marker)
                        (set-marker ollama-buddy--thinking-block-start nil)
                        (setq ollama-buddy--thinking-block-start  nil
                              ollama-buddy--thinking-arrow-marker nil)))

                    ;; If still in a marker-based reasoning section at stream end, force exit
                    (when ollama-buddy--in-reasoning-section
                      (setq ollama-buddy--in-reasoning-section nil)
                      (cond
                       ;; Collapse mode: finalize whatever arrived into the heading
                       (ollama-buddy-collapse-thinking
                        (when ollama-buddy--thinking-block-start
                          (ollama-buddy--finalize-thinking-block
                           ollama-buddy--thinking-arrow-marker)
                          (set-marker ollama-buddy--thinking-block-start nil)
                          (setq ollama-buddy--thinking-block-start  nil
                                ollama-buddy--thinking-arrow-marker nil)))
                       ;; Hide mode: delete the partial block
                       (ollama-buddy-hide-reasoning
                        (setq ollama-buddy--reasoning-status-message nil)
                        (when ollama-buddy--start-point
                          (delete-region ollama-buddy--start-point (point-max))
                          (setq ollama-buddy--start-point nil))
                        (insert "\n[Warning: Response ended with unclosed reasoning section]\n\n"))))

                    ;; Pulse the response region to indicate completion
                    (when (and ollama-buddy-pulse-response
                               ollama-buddy--response-start-position)
                      (ignore-errors
                        (pulse-momentary-highlight-region
                         ollama-buddy--response-start-position (point))))

                    ;; Run post-processing hooks on the streamed response region
                    ;; (runs BEFORE md-to-org conversion so downstream passes see
                    ;; normalised whitespace).  Used to fix models that emit
                    ;; numbered lists with no newlines between items.
                    (when (and ollama-buddy-response-post-process-functions
                               ollama-buddy--response-start-position)
                      (save-excursion
                        (let ((region-start (marker-position
                                             ollama-buddy--response-start-position))
                              (region-end (point-max)))
                          (run-hook-with-args
                           'ollama-buddy-response-post-process-functions
                           region-start region-end))))

                    ;; Convert the response from markdown to org format if enabled
                    (when ollama-buddy-convert-markdown-to-org
                      (let* ((clean-response
                              (let ((extracted (ollama-buddy--extract-thinking-from-response
                                               ollama-buddy--current-response)))
                                (if (car extracted) (cdr extracted) ollama-buddy--current-response)))
                             (converted-content (with-temp-buffer
                                                  (insert clean-response)
                                                  (ollama-buddy--md-to-org-convert-region (point-min) (point-max))
                                                  (buffer-string))))
                        (set-register ollama-buddy-default-register converted-content))

                      (when ollama-buddy--response-start-position
                        (let ((offset (if (save-excursion
                                            (goto-char ollama-buddy--response-start-position)
                                            (re-search-backward "^\\*\\*\\* Response$" nil t))
                                          3 2)))
                          (ollama-buddy--md-to-org-convert-region
                           ollama-buddy--response-start-position
                           (point-max)
                           offset))
                        ;; Reset the marker after conversion
                        (when (markerp ollama-buddy--response-start-position)
                          (set-marker ollama-buddy--response-start-position nil))
                        (setq ollama-buddy--response-start-position nil)))

                    (unless ollama-buddy-convert-markdown-to-org
                      (set-register ollama-buddy-default-register ollama-buddy--current-response))

                    (makunbound 'ollama-buddy--current-response)

                    ;; Cancel the update timer
                    (when ollama-buddy--token-update-timer
                      (cancel-timer ollama-buddy--token-update-timer)
                      (setq ollama-buddy--token-update-timer nil))

                    ;; Calculate final statistics
                    (let* ((elapsed-time (- (float-time) ollama-buddy--current-token-start-time))
                           (token-rate (if (> elapsed-time 0)
                                           (/ ollama-buddy--current-token-count elapsed-time)
                                         0))
                           (token-info (list :model ollama-buddy--current-model
                                             :tokens ollama-buddy--current-token-count
                                             :elapsed elapsed-time
                                             :rate token-rate
                                             :wait-time ollama-buddy--response-wait-duration
                                             :timestamp (current-time))))

                      ;; Insert property drawer on the response heading
                      (ollama-buddy--insert-response-properties
                       ollama-buddy--current-token-count
                       elapsed-time token-rate
                       ollama-buddy--response-wait-duration)

                      ;; Add to history
                      (push token-info ollama-buddy--token-usage-history)
                      (ollama-buddy--trim-token-history)

                      ;; Reset tracking variables
                      (setq ollama-buddy--current-token-count 0
                            ollama-buddy--current-token-start-time nil
                            ollama-buddy--last-token-count 0
                            ollama-buddy--last-update-time nil
                            ;; Reset reasoning variables
                            ollama-buddy--in-reasoning-section nil
                            ollama-buddy--reasoning-status-message nil
                            ollama-buddy--reasoning-skip-newlines nil))

                    ;; Re-fold the Think heading if one was finalized this turn.
                    ;; Post-streaming modifications (property drawer insertion,
                    ;; md-to-org conversion) trigger org-fold's fragility check,
                    ;; which can inadvertently reveal the fold.  Re-applying it
                    ;; here, after all those changes, ensures it stays collapsed.
                    (when (and ollama-buddy-collapse-thinking
                               ollama-buddy--last-think-heading-marker
                               (marker-buffer ollama-buddy--last-think-heading-marker))
                      (save-excursion
                        (goto-char ollama-buddy--last-think-heading-marker)
                        (org-fold-hide-subtree))
                      (set-marker ollama-buddy--last-think-heading-marker nil)
                      (setq ollama-buddy--last-think-heading-marker nil))

                    ;; Warn if tool calls were present but iteration limit reached
                    (when (and (featurep 'ollama-buddy-tools)
                               (bound-and-true-p ollama-buddy-tools-enabled)
                               ollama-buddy--current-tool-calls
                               (>= ollama-buddy--tool-call-iteration
                                   (if (boundp 'ollama-buddy-tools-max-iterations)
                                       ollama-buddy-tools-max-iterations 10)))
                      (save-excursion
                        (goto-char (point-max))
                        (insert (format "\n\n*** ⚠ Tool Limit Reached\nStopped after %d iterations (max: =ollama-buddy-tools-max-iterations=).\n"
                                        (if (boundp 'ollama-buddy-tools-max-iterations)
                                            ollama-buddy-tools-max-iterations 10))))
                      (message "Tool calling stopped: reached maximum of %d iterations"
                               (if (boundp 'ollama-buddy-tools-max-iterations)
                                   ollama-buddy-tools-max-iterations 10)))

                    ;; reset the current model if from external
                    (when ollama-buddy--current-request-temporary-model
                      (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model)
                      (setq ollama-buddy--current-request-temporary-model nil))

                    ;; Handle multishot progression here
                    (if ollama-buddy--multishot-sequence
                        (progn
                          (ollama-buddy--multishot-cancel-timer)
                          ;; Increment progress
                          (if (< ollama-buddy--multishot-progress
                                 (length ollama-buddy--multishot-sequence))
                              (progn
                                ;; Process next model after a short delay
                                (run-with-timer 0.5 nil #'ollama-buddy--send-next-in-sequence))
                            (progn
                              (ollama-buddy--update-status "Multi Finished")
                              (ollama-buddy--prepare-prompt-area))))
                      ;; Not in multishot mode, just show the prompt
                      (progn
                        (ollama-buddy--prepare-prompt-area)
                        (ollama-buddy--update-status (format "Finished [%d %.1f]"
                                                             (plist-get (car ollama-buddy--token-usage-history) :tokens)
                                                             (plist-get (car ollama-buddy--token-usage-history) :rate)))
                        ;; Auto-save transcript
                        (ollama-buddy--autosave-transcript)
                        ;; Check for pending project summary save
                        (when (bound-and-true-p ollama-buddy-project--pending-save-path)
                          (run-with-timer
                           0.5 nil
                           (let ((buf (current-buffer)))
                             (lambda ()
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (ollama-buddy-project--maybe-save-summary)))))))
                        ;; Fire post-response hook (annotate-directory, etc.)
                        (run-hook-with-args 'ollama-buddy-post-response-hook
                                            ollama-buddy--current-model)))))
                (setq completed t))) ; closes when-done AND save-excursion
            ;; Window state management (must be outside save-excursion)
            (when window
              (cond
               ;; On completion with visible response, move to prompt
               ((and completed (ollama-buddy--maybe-goto-prompt window response-start))
                nil)
               ;; Auto-scroll enabled, follow output
               (ollama-buddy-auto-scroll
                (set-window-point window (point-max)))
               ;; Otherwise restore original position
               (t
                (set-window-point window old-point)
                (set-window-start window old-window-start t))))))))))

(defun ollama-buddy--stream-sentinel (_proc event)
  "Handle stream completion EVENT."
  ;; If we already handled an HTTP error in the filter, just reset the
  ;; status flag and clean up timers — the error message and prompt area
  ;; were already written; no need for the usual "[Stream …]" completion notice.
  (if ollama-buddy--stream-http-status
      (progn
        (setq ollama-buddy--stream-http-status nil)
        (when ollama-buddy--token-update-timer
          (cancel-timer ollama-buddy--token-update-timer)
          (setq ollama-buddy--token-update-timer nil))
        (ollama-buddy--cancel-response-wait-timer)
        (setq ollama-buddy--stream-pending ""))
  (let ((cancelled ollama-buddy--request-cancelled))
    (setq ollama-buddy--request-cancelled nil)
  (when-let* ((status (cond (cancelled "Cancelled")
                            ((string-match-p "finished" event) "Completed")
                            ((string-match-p "\\(?:deleted\\|connection broken\\)" event) "Interrupted")))
              (msg (if cancelled "\n\n*** CANCELLED" (format "\n\n[Stream %s]" status))))
    ;; Clean up multishot variables but ensure we don't create out-of-range conditions
    (setq ollama-buddy--multishot-prompt nil)

    ;; Only set sequence to nil if we're done with it or interrupted
    (when (or (string= status "Interrupted")
              (string= status "Cancelled")
              (not ollama-buddy--multishot-sequence)
              (>= ollama-buddy--multishot-progress (length ollama-buddy--multishot-sequence)))
      (setq ollama-buddy--multishot-sequence nil
            ollama-buddy--multishot-progress 0))
    
    ;; Clean up token tracking
    (when ollama-buddy--token-update-timer
      (cancel-timer ollama-buddy--token-update-timer)
      (setq ollama-buddy--token-update-timer nil))

    ;; Clean up response wait timer
    (ollama-buddy--cancel-response-wait-timer)

    ;; Reset stream buffer
    (setq ollama-buddy--stream-pending "")

    ;; Reset the current model if from external
    (when ollama-buddy--current-request-temporary-model
      (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model)
      (setq ollama-buddy--current-request-temporary-model nil))
    
    (with-current-buffer ollama-buddy--chat-buffer
      (let* ((inhibit-read-only t)
             (window (get-buffer-window ollama-buddy--chat-buffer t))
             (old-point (and window (window-point window)))
             (old-window-start (and window (window-start window))))

        ;; Preserve accumulated thinking content before inserting completion msg
        (ollama-buddy--finalize-pending-thinking)

        (let ((response-start (if (markerp ollama-buddy--response-start-position)
                                  (marker-position ollama-buddy--response-start-position)
                                ollama-buddy--response-start-position)))
          (save-excursion
            (goto-char (point-max))
            (insert msg)
            (ollama-buddy--prepare-prompt-area))
          ;; Window state management - same logic as stream filter
          (when window
            (unless (ollama-buddy--maybe-goto-prompt window response-start)
              (when (not ollama-buddy-auto-scroll)
                (set-window-point window old-point)
                (set-window-start window old-window-start t)))))))
    ;; Only show token stats in status if we completed successfully
    (if (string= status "Completed")
        (let ((last-info (car ollama-buddy--token-usage-history)))
          (if last-info
              (ollama-buddy--update-status
               (format "Stream %s [%d tokens, %.1f]"
                       status
                       (plist-get last-info :tokens)
                       (plist-get last-info :rate)))
            (ollama-buddy--update-status (concat "Stream " status))))
      (progn
        (when ollama-buddy-convert-markdown-to-org
          (save-excursion
            (goto-char (point-max))
            (beginning-of-line)
            (let ((end (point)))
              (when (re-search-backward ": RESPONSE" nil t)
                (search-forward "]")
                ;; Skip past thinking block if present, use deeper offset
                (let ((offset 2))
                  (when (re-search-forward "^\\*\\*\\* Response\n" end t)
                    (setq offset 3))
                  (ollama-buddy--md-to-org-convert-region
                   (point) end offset))))))
        (ollama-buddy--update-status (concat "Stream " status))))

    ;; Auto-save transcript (for sentinel-based completions)
    (ollama-buddy--autosave-transcript)))))

(defun ollama-buddy--format-model-size (bytes)
  "Format BYTES as a human-readable size string."
  (when (and bytes (> bytes 0))
    (cond
     ((>= bytes 1073741824)
      (format "%.1f GB" (/ (float bytes) 1073741824)))
     ((>= bytes 1048576)
      (format "%.0f MB" (/ (float bytes) 1048576)))
     (t (format "%d B" bytes)))))

(defun ollama-buddy--model-annotation (model)
  "Return annotation string for MODEL in completing-read.
Shows provider, capability indicators, running status, display name,
context window, parameter count, quantization, and disk size."
  (let* ((indicators "")
         (meta         (gethash model ollama-buddy--models-metadata-cache))
         (running      (member (ollama-buddy--get-real-model-name model)
                               (mapcar #'ollama-buddy--get-real-model-name
                                       (ollama-buddy--get-running-models))))
         ;; Local Ollama model fields
         (params       (when meta (alist-get 'parameter-size meta)))
         (quant        (when meta (alist-get 'quantization meta)))
         (size         (when meta (ollama-buddy--format-model-size (alist-get 'size meta))))
         ;; Remote provider fields
         (provider     (ollama-buddy--get-provider-label model))
         (display-name (when meta (alist-get 'display-name meta)))
         (ctx-str      (ollama-buddy--format-context-window
                        (ollama-buddy--get-context-window model))))
    ;; Provider label for remote models
    (when provider
      (setq indicators (concat indicators "  " provider)))
    ;; Capability indicators
    (when (ollama-buddy--cloud-model-p model)
      (setq indicators (concat indicators " ☁")))
    (when running
      (setq indicators (concat indicators " ▶")))
    (when (ollama-buddy--model-supports-tools model)
      (setq indicators (concat indicators " ⚒")))
    (when (ollama-buddy--model-supports-vision model)
      (setq indicators (concat indicators " ⊙")))
    (when (ollama-buddy--model-supports-thinking model)
      (setq indicators (concat indicators " ✦")))
    ;; Remote: human-readable display name (e.g. from Claude API)
    (when display-name
      (setq indicators (concat indicators "  " display-name)))
    ;; Context window for remote models (Gemini from API, others from static table)
    (when ctx-str
      (setq indicators (concat indicators "  " ctx-str)))
    ;; Local: parameter count, quantization, disk size
    (when params
      (setq indicators (concat indicators "  " params)))
    (when quant
      (setq indicators (concat indicators " " quant)))
    (when size
      (setq indicators (concat indicators "  " size)))
    indicators))

(defun ollama-buddy--swap-model ()
  "Swap ollama model, including remote and cloud models if available.
When airplane mode is active, only local Ollama models are offered."
  (interactive)
  (let* ((models (if (bound-and-true-p ollama-buddy-airplane-mode)
                     (ollama-buddy--get-models)
                   (ollama-buddy--get-models-with-others)))
         (new-model (completing-read "Model: "
                                     (lambda (string pred action)
                                       (if (eq action 'metadata)
                                           '(metadata (annotation-function . ollama-buddy--model-annotation))
                                         (complete-with-action action models string pred)))
                                     nil t)))
    (setq ollama-buddy-default-model new-model)
    (setq ollama-buddy--current-model new-model)
    (message "Switched to model: %s" new-model)
    (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
    (ollama-buddy--prepare-prompt-area t t)
    (goto-char (point-max))
    (ollama-buddy--update-status "Idle")
    ;; Fetch capabilities asynchronously — update prompt area when done
    (when (ollama-buddy--ollama-running)
      (ollama-buddy--fetch-model-context-size-async
       new-model
       (lambda ()
         (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
           (with-current-buffer ollama-buddy--chat-buffer
             (ollama-buddy--prepare-prompt-area t t))))))))

(defvar ollama-buddy--signin-url-opened nil
  "Flag to track whether we've already opened the signin URL.")

(defun ollama-buddy-cloud-signin ()
  "Sign in to Ollama cloud services.
This runs `ollama signin' which outputs a URL for authentication.
The URL will be automatically opened in your default browser."
  (interactive)
  (setq ollama-buddy--signin-url-opened nil)
  (let ((buffer-name "*ollama-signin*"))
    (message "Starting Ollama signin process...")
    (if (executable-find ollama-buddy-ollama-executable)
        (let ((proc (start-process "ollama-signin" buffer-name
                                   ollama-buddy-ollama-executable "signin")))
          ;; Set up filter to capture URL and auto-open browser
          (set-process-filter
           proc
           (lambda (process output)
             ;; Append output to process buffer
             (when (buffer-live-p (process-buffer process))
               (with-current-buffer (process-buffer process)
                 (goto-char (point-max))
                 (insert output)))
             ;; Check for already signed in message
             (when (string-match-p "already signed in" output)
               (message "Ollama cloud: Already signed in")
               (ollama-buddy--set-cloud-auth-status t))
             ;; Look for URL and auto-open browser
             (when (and (not ollama-buddy--signin-url-opened)
                        (string-match "https://[^\n\r\t ]+" output))
               (let ((url (match-string 0 output)))
                 (setq ollama-buddy--signin-url-opened t)
                 (message "Opening Ollama signin URL in browser...")
                 (browse-url url)))))
          (set-process-sentinel
           proc
           (lambda (_process event)
             (cond
              ((string-match-p "finished" event)
               (if ollama-buddy--signin-url-opened
                   (message "Ollama signin: Complete authentication in your browser")
                 (progn
                   (message "Ollama signin completed")
                   (ollama-buddy--set-cloud-auth-status t)))
               (ollama-buddy--update-status "Signed in"))
              ((string-match-p "exited abnormally" event)
               (message "Ollama signin failed. Check *ollama-signin* buffer for details")
               (pop-to-buffer buffer-name)))))
          (message "Ollama signin: Checking authentication status..."))
      (user-error "Cannot find ollama executable. Set `ollama-buddy-ollama-executable'"))))

(defun ollama-buddy-cloud-signout ()
  "Sign out from Ollama cloud services."
  (interactive)
  (if (executable-find ollama-buddy-ollama-executable)
      (let ((exit-code (call-process ollama-buddy-ollama-executable nil nil nil "signout")))
        (if (zerop exit-code)
            (progn
              (ollama-buddy--set-cloud-auth-status nil)
              (message "Signed out from Ollama cloud")
              (ollama-buddy--update-status "Signed out"))
          (message "Ollama signout failed with exit code %d" exit-code)))
    (user-error "Cannot find ollama executable. Set `ollama-buddy-ollama-executable'")))

(defun ollama-buddy-cloud-status ()
  "Check Ollama cloud authentication status.
Shows cached status. Use signin/signout to update or try a cloud model request."
  (interactive)
  (let ((status ollama-buddy--cloud-auth-status))
    (message "Ollama cloud: %s"
             (pcase status
               ('authenticated "Signed in")
               ('not-authenticated "Not signed in (use C-c A to sign in)")
               ('unknown "Unknown (try using a cloud model to verify)")))))

(defun ollama-buddy--launch-model (model &optional directory)
  "Launch MODEL in an external terminal with an AI agent.
MODEL is the raw model name (without display prefixes).
Prompts for agent if more than one is available on PATH.
Standard agents use `ollama launch'; direct agents (:direct t)
are invoked as standalone executables.
When DIRECTORY is non-nil, the terminal starts in that directory."
  (let* ((detected (unless ollama-buddy-launch-terminal
                     (ollama-buddy--detect-terminal)))
         (terminal (or ollama-buddy-launch-terminal
                      (car detected)))
         (flag (or ollama-buddy-launch-terminal-flag
                   (cdr detected)
                   "-e"))
         (agents (ollama-buddy--detect-available-agents)))
    (unless terminal
      (user-error "No terminal emulator found. Set `ollama-buddy-launch-terminal'"))
    (unless (executable-find terminal)
      (user-error "Cannot find terminal %s. Set `ollama-buddy-launch-terminal'" terminal))
    (unless agents
      (user-error "No external agents found on PATH"))
    (let* ((agent
            (if (= (length agents) 1)
                (car agents)
              (let* ((candidates
                      (mapcar (lambda (a)
                                (cons (format "%s  %s"
                                              (plist-get a :name)
                                              (propertize (plist-get a :label)
                                                          'face 'font-lock-comment-face))
                                      a))
                              agents))
                     (choice (completing-read "Agent: "
                                              (mapcar #'car candidates) nil t)))
                (cdr (assoc choice candidates))))))
      ;; Warn about small models that may struggle with agent system prompts
      (let ((param-size (ollama-buddy--extract-model-param-size model))
            (threshold ollama-buddy-launch-small-model-threshold))
        (when (and param-size threshold (< param-size threshold)
                   (not (ollama-buddy--cloud-model-p model)))
          (unless (yes-or-no-p
                   (format "%s is a %.0fB parameter model.  Coding agents prepend 20K+ tokens of system prompt to every request — small models may appear frozen or take many minutes to respond, especially without a dedicated GPU.  Continue anyway? "
                           model param-size))
            (user-error "Launch cancelled"))))
      (let* ((direct (plist-get agent :direct))
             (model-flag (if (plist-member agent :model-flag)
                             (plist-get agent :model-flag)
                           "--model"))
             (cmd (if direct
                      (append (split-string flag)
                              (list (plist-get agent :executable))
                              (when model-flag
                                (list model-flag model)))
                    (append (split-string flag)
                            (list ollama-buddy-ollama-executable
                                  "launch" (plist-get agent :name)
                                  "--model" model))))
             (default-directory (or directory default-directory)))
        (call-process-shell-command
         (concat (mapconcat #'shell-quote-argument (cons terminal cmd) " ")
                 " &")
         nil 0)
        (message "Launched %s with model %s in %s (dir: %s)"
                 (plist-get agent :label) model terminal
                 default-directory)))))

(defun ollama-buddy-launch ()
  "Launch an Ollama model in an external terminal with an AI agent.
Prompts for a model, then for an agent if multiple are available.
Auto-detects the terminal emulator unless `ollama-buddy-launch-terminal'
is explicitly set."
  (interactive)
  (let* ((models (ollama-buddy--get-models-with-others))
         (model (completing-read "Launch model: "
                                 (lambda (string pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (annotation-function . ollama-buddy--model-annotation))
                                     (complete-with-action action models string pred)))
                                 nil t))
         (real-model (ollama-buddy--get-real-model-name model)))
    (ollama-buddy--launch-model real-model)))

(defun ollama-buddy-launch-external ()
  "Launch an AI agent in an external terminal from the current directory.
Does not require the Ollama Buddy chat buffer.  Uses `default-directory'
as the working directory (in Dired this is the displayed directory).
Prompts for a model, then for an agent if multiple are available."
  (interactive)
  (let* ((dir default-directory)
         (models (ollama-buddy--get-models-with-others))
         (model (completing-read (format "Launch model (in %s): "
                                         (abbreviate-file-name dir))
                                 (lambda (string pred action)
                                   (if (eq action 'metadata)
                                       '(metadata (annotation-function . ollama-buddy--model-annotation))
                                     (complete-with-action action models string pred)))
                                 nil t))
         (real-model (ollama-buddy--get-real-model-name model)))
    (ollama-buddy--launch-model real-model dir)))

(defun ollama-buddy--fetch-cloud-usage ()
  "Fetch cloud usage stats from ollama.com/settings.
Returns an alist ((session . \"N.N%\") (weekly . \"N.N%\")) or nil on failure.
Uses `ollama-buddy-cloud-session-token' cookie for authentication via curl.
Results are cached for `ollama-buddy-cloud-usage-cache-seconds'."
  (when (and (stringp ollama-buddy-cloud-session-token)
             (not (string-empty-p ollama-buddy-cloud-session-token)))
    ;; Return cached value if still fresh
    (if (and ollama-buddy--cloud-usage-cache
             ollama-buddy--cloud-usage-cache-time
             (< (float-time (time-subtract (current-time)
                                           ollama-buddy--cloud-usage-cache-time))
                ollama-buddy-cloud-usage-cache-seconds))
        ollama-buddy--cloud-usage-cache
      ;; Fetch fresh data using curl with session cookie
      (condition-case err
          (let ((buf (generate-new-buffer " *ollama-cloud-usage*")))
            (unwind-protect
                (let ((exit-code
                       (call-process
                        ollama-buddy-curl-executable nil buf nil
                        "-s"
                        "-b" (concat "__Secure-session=" ollama-buddy-cloud-session-token)
                        "https://ollama.com/settings")))
                  (when (zerop exit-code)
                    (let ((html (with-current-buffer buf (buffer-string)))
                          session-pct weekly-pct session-reset weekly-reset)
                      ;; Extract session usage - look for width style after "Session usage"
                      (when (string-match "Session usage" html)
                        (let ((start (match-end 0)))
                          (when (string-match "width:\\s-*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)%" html start)
                            (setq session-pct (concat (match-string 1 html) "%")))
                          (when (string-match "data-time=\"\\([^\"]+\\)\"" html start)
                            (setq session-reset (match-string 1 html)))))
                      ;; Extract weekly usage - look for width style after "Weekly usage"
                      (when (string-match "Weekly usage" html)
                        (let ((start (match-end 0)))
                          (when (string-match "width:\\s-*\\([0-9]+\\(?:\\.[0-9]+\\)?\\)%" html start)
                            (setq weekly-pct (concat (match-string 1 html) "%")))
                          (when (string-match "data-time=\"\\([^\"]+\\)\"" html start)
                            (setq weekly-reset (match-string 1 html)))))
                      (when (or session-pct weekly-pct)
                        (let ((result `((session . ,(or session-pct "N/A"))
                                        (weekly . ,(or weekly-pct "N/A"))
                                        ,@(when session-reset
                                            `((session-reset . ,session-reset)))
                                        ,@(when weekly-reset
                                            `((weekly-reset . ,weekly-reset))))))
                          (setq ollama-buddy--cloud-usage-cache result
                                ollama-buddy--cloud-usage-cache-time (current-time))
                          result)))))
              (kill-buffer buf)))
        (error
         (message "Failed to fetch cloud usage: %s" (error-message-string err))
         nil)))))

(defun ollama-buddy--cloud-usage-bar (percentage &optional width)
  "Generate a text progress bar for PERCENTAGE string like \"45.2%\".
WIDTH is the total bar width in characters (default 10)."
  (let* ((w (or width 10))
         (pct (string-to-number (replace-regexp-in-string "%" "" percentage)))
         (filled (round (* w (/ pct 100.0))))
         (empty (- w filled)))
    (concat (make-string filled ?█)
            (make-string empty ?░))))

(defun ollama-buddy--cloud-usage-pie (percentage &optional size)
  "Generate an SVG pie chart image for PERCENTAGE string like \"45.2%\".
SIZE is the diameter in pixels (default 16).  Returns a propertized
string with the SVG image displayed inline, or a text fallback for
terminal Emacs."
  (let* ((sz (or size 16))
         (pct (/ (string-to-number
                  (replace-regexp-in-string "%" "" percentage))
                 100.0))
         (pct (max 0.0 (min 1.0 pct))))
    (if (not (display-graphic-p))
        ;; Terminal fallback: use the text bar
        (ollama-buddy--cloud-usage-bar percentage 5)
      (require 'svg)
      (let* ((cx (/ sz 2.0))
             (cy (/ sz 2.0))
             (r (* cx 0.875))
             ;; Colour based on usage level
             (fill-colour (cond
                           ((< pct 0.5) "#4CAF50")   ; green
                           ((< pct 0.75) "#FF9800")  ; amber
                           (t "#F44336")))            ; red
             (svg (svg-create sz sz)))
        ;; Background ring (unfilled, stroke only)
        (svg-circle svg cx cy r :fill "none" :stroke "#c0c0c0" :stroke-width 1)
        ;; Pie slice (skip if 0%, full circle if ~100%)
        (cond
         ((<= pct 0.0) nil)
         ((>= pct 0.995)
          (svg-circle svg cx cy r :fill fill-colour))
         (t
          (let* ((start-angle (- (/ float-pi 2))) ; 12 o'clock
                 (end-angle (+ start-angle (* 2 float-pi pct)))
                 (x1 (+ cx (* r (cos start-angle))))
                 (y1 (+ cy (* r (sin start-angle))))
                 (x2 (+ cx (* r (cos end-angle))))
                 (y2 (+ cy (* r (sin end-angle))))
                 (large-arc (if (> pct 0.5) 1 0)))
            (dom-append-child
             svg
             (dom-node 'path
                       `((d . ,(format "M %f,%f L %f,%f A %f %f 0 %d 1 %f,%f Z"
                                       cx cy x1 y1 r r large-arc x2 y2))
                         (fill . ,fill-colour)))))))
        (propertize " "
                    'display (svg-image svg :ascent 'center :scale 1.0))))))

(defun ollama-buddy--cloud-usage-pie-indicator (usage)
  "Build a header-line cloud usage indicator with pie charts from USAGE alist.
Returns a propertized string with two pie charts (session, weekly) and
percentage labels, or a text fallback for terminal Emacs."
  (let* ((session-pct (alist-get 'session usage))
         (weekly-pct (alist-get 'weekly usage))
         (session-str (ollama-buddy--round-pct session-pct))
         (weekly-str (ollama-buddy--round-pct weekly-pct)))
    (if (display-graphic-p)
        (concat " "
                (ollama-buddy--cloud-usage-pie session-pct)
                (propertize session-str 'face '(:height 0.9))
                " "
                (ollama-buddy--cloud-usage-pie weekly-pct)
                (propertize weekly-str 'face '(:height 0.9)))
      (format " %s %s" session-str weekly-str))))

(defun ollama-buddy--cloud-reset-time-string (iso-time)
  "Format ISO-TIME string as a human-readable \"resets in\" string.
ISO-TIME should be an ISO 8601 timestamp like \"2026-03-06T08:00:00Z\"."
  (condition-case nil
      (let* ((reset-time (encode-time (iso8601-parse iso-time)))
             (diff (float-time (time-subtract reset-time (current-time)))))
        (if (<= diff 0)
            "resetting now"
          (let ((minutes (floor (/ diff 60)))
                (hours (floor (/ diff 3600)))
                (days (floor (/ diff 86400))))
            (cond
             ((< minutes 60) (format "resets in %dm" minutes))
             ((< hours 24) (format "resets in %dh %dm" hours (% minutes 60)))
             (t (format "resets in %dd %dh" days (% hours 24)))))))
    (error "?")))

(defun ollama-buddy-cloud-refresh-usage ()
  "Clear cached cloud usage and re-fetch.
Refreshes the model management buffer if visible."
  (interactive)
  (setq ollama-buddy--cloud-usage-cache nil
        ollama-buddy--cloud-usage-cache-time nil)
  (let ((usage (ollama-buddy--fetch-cloud-usage)))
    (if usage
        (message "Cloud usage - Session: %s | Weekly: %s"
                 (alist-get 'session usage)
                 (alist-get 'weekly usage))
      (message "Could not fetch cloud usage (check API key)")))
  (when (get-buffer "*ollama-buddy-models*")
    (ollama-buddy-manage-models)))

;; Update buffer initialization to check status
(defun ollama-buddy--open-chat ()
  "Open chat buffer and initialize if needed.
Updates the chat buffer's `default-directory' to match the caller's
directory so that project.el and file-relative commands reflect the
buffer the user launched from."
  (interactive)
  (let ((caller-dir default-directory))
    (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
    (setq default-directory caller-dir)
    (ollama-buddy--initialize-chat-buffer)
    (goto-char (point-max))))

(defun ollama-buddy--menu-help-assistant ()
  "Show the help assistant."
  (interactive)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (goto-char (point-max))
  (when (re-search-backward ">> PROMPT:\\s-*" nil t)
    (beginning-of-line)
    (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert (ollama-buddy--create-intro-message))
  (save-excursion
    (when (re-search-backward "^\\*\\* More Commands$" nil t)
      (org-fold-hide-subtree)))
  (ollama-buddy--prepare-prompt-area))

(defun ollama-buddy--menu-custom-prompt ()
  "Show the custom prompt."
  (interactive)
  (when-let ((prompt (read-string "Enter prompt prefix on selection: " nil nil nil t)))
    (unless (use-region-p)
      (user-error "No region selected.  Select text to use with prompt"))
    (unless (not (string-empty-p prompt))
      (user-error "Input string is empty"))
    (let* ((prompt-with-selection (concat
                                   (when prompt (concat prompt "\n\n"))
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end)))))
      (ollama-buddy--open-chat)
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (insert (string-trim prompt-with-selection)))
      (ollama-buddy--send-backend (string-trim prompt-with-selection)))))

(defun ollama-buddy--menu-minibuffer-prompt ()
  "Show the custom minibuffer prompt."
  (interactive)
  (when-let ((prompt (read-string "Enter prompt: " nil nil nil t)))
    (unless (not (string-empty-p prompt))
      (user-error "Input string is empty"))
    (ollama-buddy--open-chat)
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (insert (string-trim prompt)))
    (ollama-buddy--send-backend (string-trim prompt))))

(defun ollama-buddy--send-with-command (command-name)
  "Send request using configuration from COMMAND-NAME."
  (let* ((prompt-text (ollama-buddy--get-command-prop command-name :prompt))
         (system-text (ollama-buddy--get-command-prop command-name :system))
         (selected-text (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))))
         (model (ollama-buddy--get-command-prop command-name :model))
         (params-alist (ollama-buddy--get-command-prop command-name :parameters)))

    ;; Verify requirements
    (when (and prompt-text (not selected-text))
      (user-error "This command requires selected text"))

    ;; --- In-buffer replace branch ---
    (let* ((dest (ollama-buddy--get-command-prop command-name :destination))
           (use-in-buffer
            (and (featurep 'ollama-buddy-rewrite)
                 selected-text
                 (cond
                  ((eq dest 'chat)      nil)
                  ((eq dest 'in-buffer) t)
                  (t (bound-and-true-p ollama-buddy-in-buffer-replace))))))
    (if use-in-buffer
        (let* ((source-buf      (current-buffer))
               (r-start         (region-beginning))
               (r-end           (region-end))
               (cmd-prompt      (ollama-buddy--get-command-prop command-name :prompt))
               (full-prompt     (if cmd-prompt
                                    (concat cmd-prompt "\n\n" selected-text)
                                  selected-text))
               (base-system     (or system-text
                                     ollama-buddy--current-system-prompt))
               (ib-tone-text    (cdr (assoc "In-Buffer" ollama-buddy-tone-alist)))
               (effective-system (if (and ib-tone-text
                                          (not (string-empty-p ib-tone-text)))
                                     (if (and base-system
                                              (not (string-empty-p base-system)))
                                         (concat ib-tone-text "\n\n" base-system)
                                       ib-tone-text)
                                   base-system))
               (effective-model  (or model
                                     ollama-buddy--current-model
                                     ollama-buddy-default-model)))
          (when params-alist
            (ollama-buddy--apply-command-parameters params-alist))
          (ollama-buddy-rewrite-from-command
           source-buf r-start r-end selected-text
           effective-system full-prompt effective-model)
          (when params-alist
            (ollama-buddy--restore-default-parameters)))

      ;; --- Normal branch (unchanged) ---
      (ollama-buddy--open-chat)

      ;; Apply command-specific parameters if provided
      (when params-alist
        (ollama-buddy--apply-command-parameters params-alist))

      ;; Display which system prompt will be used
      (when system-text
        (ollama-buddy--display-system-prompt system-text 3))

      ;; Prepare and send the prompt
      (let ((full-prompt (ollama-buddy--prepare-command-prompt command-name selected-text)))
        ;; Temporarily set system prompt if specified for this command
        (let ((old-system-prompt ollama-buddy--current-system-prompt))
          (when system-text
            (setq ollama-buddy--current-system-prompt system-text))

          (when (and prompt-text (not (string-empty-p prompt-text)))
            (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
            (add-to-history 'ollama-buddy--prompt-history prompt-text))

          ;; Send the request
          (ollama-buddy--send-backend (string-trim full-prompt) model)

          ;; Restore the original system prompt if we changed it
          (when system-text
            (setq ollama-buddy--current-system-prompt old-system-prompt))

          ;; Restore default parameters if we changed them
          (when params-alist
            (ollama-buddy--restore-default-parameters))))))))

(defun ollama-buddy--calculate-prompt-context-percentage ()
  "Calculate and return the context percentage for the current prompt."
  (let* ((model (or ollama-buddy--current-model
                    ollama-buddy-default-model))
         (total-tokens 0)
         (history-tokens 0)
         (max-context-size (ollama-buddy--get-model-context-size model))
         (history (ollama-buddy--get-history-for-request))
         (attachment-tokens
          (ollama-buddy--estimate-token-count
           (mapconcat
            (lambda (attachment)
              (let ((content (plist-get attachment :content)))
                (format "%s" content)))
            ollama-buddy--current-attachments
            " ")))
         ;; Web search tokens
         (web-search-tokens
          (if (and (featurep 'ollama-buddy-web-search)
                   (fboundp 'ollama-buddy-web-search-total-tokens))
              (ollama-buddy-web-search-total-tokens)
            0))
         ;; RAG tokens
         (rag-tokens
          (if (and (featurep 'ollama-buddy-rag)
                   (fboundp 'ollama-buddy-rag-total-tokens))
              (ollama-buddy-rag-total-tokens)
            0))
         (prompt-tokens
          (ollama-buddy--estimate-token-count
           (car (ollama-buddy--get-prompt-content))))
         (system-prompt-tokens
          (if ollama-buddy--current-system-prompt
              (ollama-buddy--estimate-token-count ollama-buddy--current-system-prompt)
            0)))

    ;; Calculate history tokens
    (when history
      (dolist (msg history)
        (let ((content (alist-get 'content msg)))
          (when content
            (setq total-tokens (+ total-tokens
                                  (ollama-buddy--estimate-token-count content)))))))

    (setq history-tokens total-tokens)

    ;; Add system prompt tokens if not already in history
    (when (and ollama-buddy--current-system-prompt
               (not (seq-find (lambda (msg)
                                (string= (alist-get 'role msg) "system"))
                              history)))
      (setq total-tokens (+ total-tokens system-prompt-tokens)))

    ;; and now the file attachments
    (when ollama-buddy--current-attachments
      (setq total-tokens (+ total-tokens attachment-tokens)))

    ;; and web search results
    (when (> web-search-tokens 0)
      (setq total-tokens (+ total-tokens web-search-tokens)))

    ;; and RAG context
    (when (> rag-tokens 0)
      (setq total-tokens (+ total-tokens rag-tokens)))

    (setq total-tokens (+ total-tokens prompt-tokens))

    ;; Calculate total tokens and percentage
    (let* ((context-percentage (/ (float total-tokens) max-context-size)))

      ;; Save the current percentage
      (setq ollama-buddy--current-context-percentage context-percentage)

      ;; Save the total token count
      (setq ollama-buddy--current-context-tokens total-tokens)

      ;; Save the maximum context size
      (setq ollama-buddy--current-context-max-size max-context-size)

      ;; Save the breakdown for detailed display
      (setq ollama-buddy--current-context-breakdown
            (list :history-tokens history-tokens
                  :prompt-tokens prompt-tokens
                  :system-tokens system-prompt-tokens
                  :attachment-tokens attachment-tokens
                  :web-search-tokens web-search-tokens
                  :rag-tokens rag-tokens
                  :total-tokens total-tokens))

      ;; Return the percentage
      context-percentage)))

(defun ollama-buddy-show-context-info-refresh ()
  "Refresh the context sizes buffer."
  (interactive)
  (ollama-buddy-show-context-info))

(defun ollama-buddy-show-context-info ()
  "Show detailed information about context sizes for all models."
  (interactive)
  (ollama-buddy--calculate-prompt-context-percentage)
  (let ((buf (get-buffer-create "*Ollama Context Sizes*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)
        (erase-buffer)

        (insert "#+title: Ollama Model Context Sizes\n\n")
        (insert "Press =g= to refresh\n\n")

        ;; Show current context info if available
        (if (not ollama-buddy--current-context-percentage)
            (insert "No context usage data yet — send a prompt first.\n")
          (let* ((current-model (or ollama-buddy--current-model "unknown"))
                 (source (ollama-buddy--get-model-context-source current-model))
                 (source-desc (pcase source
                                ('api "from Ollama API (accurate)")
                                ('fallback "from fallback mappings (estimate)")
                                ('manual "manually set")
                                (_ "unknown"))))
            (insert "* Current context usage:\n\n")
            (insert (format "  Model            : *%s*\n" current-model))
            (insert (format "  Context max size : %d tokens (%s)\n" 
                            (or ollama-buddy--current-context-max-size 4096)
                            source-desc))
            (insert (format "  Current usage    : %d tokens (%.1f%%)\n"
                            (or ollama-buddy--current-context-tokens 0)
                            (* 100 (or ollama-buddy--current-context-percentage 0))))
            
            ;; Show breakdown if available
            (when ollama-buddy--current-context-breakdown
              (let* ((breakdown ollama-buddy--current-context-breakdown)
                     (max-size (or ollama-buddy--current-context-max-size 4096))
                     (history-tok (plist-get breakdown :history-tokens))
                     (system-tok (plist-get breakdown :system-tokens))
                     (attach-tok (plist-get breakdown :attachment-tokens))
                     (web-tok (or (plist-get breakdown :web-search-tokens) 0))
                     (rag-tok (or (plist-get breakdown :rag-tokens) 0))
                     (prompt-tok (plist-get breakdown :prompt-tokens))
                     (total-tok (or ollama-buddy--current-context-tokens 0))
                     (free-tok (max 0 (- max-size total-tok))))
                ;; Context breakdown chart
                (let* ((bar-width 50)
                       (segments
                        (cl-remove-if
                         (lambda (s) (= (nth 1 s) 0))
                         `(("History"   ,history-tok "#4CAF50" "█")
                           ("System"    ,system-tok  "#2196F3" "▓")
                           ("Attach"    ,attach-tok  "#FF9800" "▒")
                           ("Web"       ,web-tok     "#9C27B0" "░")
                           ("RAG"       ,rag-tok     "#00BCD4" "▫")
                           ("Prompt"    ,prompt-tok  "#F44336" "▪")
                           ("Free"      ,free-tok    "#E0E0E0" "·"))))
                       (total (max 1 max-size)))
                  (insert "\n* Context breakdown\n\n")
                  (if (display-graphic-p)
                      ;; SVG pie chart for GUI Emacs
                      (let* ((sz 120)
                             (cx (/ sz 2.0))
                             (cy (/ sz 2.0))
                             (r (* cx 0.9))
                             (svg (svg-create sz sz))
                             (angle (- (/ float-pi 2)))) ; start at 12 o'clock
                        (require 'svg)
                        ;; Background ring (unfilled, stroke only)
                        (svg-circle svg cx cy r :fill "none" :stroke "#c0c0c0" :stroke-width 1)
                        ;; Draw pie slices (skip "Free" — it's the background)
                        (dolist (seg segments)
                          (let* ((tokens (nth 1 seg))
                                 (colour (nth 2 seg))
                                 (frac (/ (float tokens) total)))
                            (when (and (> frac 0.001)
                                       (not (string= (nth 0 seg) "Free")))
                              (let* ((sweep (* 2 float-pi frac))
                                     (end-angle (+ angle sweep))
                                     (x1 (+ cx (* r (cos angle))))
                                     (y1 (+ cy (* r (sin angle))))
                                     (x2 (+ cx (* r (cos end-angle))))
                                     (y2 (+ cy (* r (sin end-angle))))
                                     (large-arc (if (> frac 0.5) 1 0)))
                                (if (>= frac 0.995)
                                    (svg-circle svg cx cy r :fill colour)
                                  (dom-append-child
                                   svg
                                   (dom-node 'path
                                             `((d . ,(format "M %f,%f L %f,%f A %f,%f 0 %d 1 %f,%f Z"
                                                             cx cy x1 y1 r r large-arc x2 y2))
                                               (fill . ,colour)))))
                                (setq angle end-angle)))))
                        (insert "  ")
                        (insert (propertize " "
                                           'display (svg-image svg :ascent 'center :scale 1.0)))
                        (insert "\n\n"))
                    ;; Terminal fallback: text bar chart
                    (insert "  ")
                    (dolist (seg segments)
                      (let ((chars (max (if (> (nth 1 seg) 0) 1 0)
                                        (round (* bar-width (/ (float (nth 1 seg)) total))))))
                        (insert (make-string chars (string-to-char (nth 3 seg))))))
                    (insert "\n\n"))
                  ;; Legend (shown in both modes)
                  (dolist (seg segments)
                    (let ((pct (* 100.0 (/ (float (nth 1 seg)) total))))
                      (if (display-graphic-p)
                          (let* ((swatch-sz 10)
                                 (swatch (svg-create swatch-sz swatch-sz)))
                            (require 'svg)
                            (svg-rectangle swatch 0 0 swatch-sz swatch-sz
                                           :fill (nth 2 seg) :rx 2 :ry 2)
                            (insert "  ")
                            (insert (propertize " "
                                               'display (svg-image swatch :ascent 'center)))
                            (insert (format " %-8s %5d tokens (%4.1f%%)\n"
                                            (nth 0 seg) (nth 1 seg) pct)))
                        (insert (format "  %s %-8s %5d tokens (%4.1f%%)\n"
                                        (nth 3 seg) (nth 0 seg)
                                        (nth 1 seg) pct))))))))))

        (goto-char (point-min))
        (view-mode 1)
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "g") #'ollama-buddy-show-context-info-refresh)
          (setq-local minor-mode-overriding-map-alist
                      (list (cons 'view-mode map))))))
    (display-buffer buf)))

(defun ollama-buddy-toggle-context-percentage ()
  "Toggle display of context percentage in the status bar."
  (interactive)
  (setq ollama-buddy-show-context-percentage
        (not ollama-buddy-show-context-percentage))
  (ollama-buddy--update-status
   (concat "Context percentage "
           (if ollama-buddy-show-context-percentage "shown" "hidden")))
  (message "Ollama context percentage display: %s"
           (if ollama-buddy-show-context-percentage "enabled" "disabled")))

(defun ollama-buddy--check-context-before-send ()
  "Check context size before sending and warn if it's too large.
Returns nil if user cancels, t otherwise."
  (let* ((percentage (ollama-buddy--calculate-prompt-context-percentage))
         (red-threshold (nth 1 ollama-buddy-context-size-thresholds)))
    
    (if (>= percentage red-threshold)
        ;; Context exceeds limit, ask for confirmation
        (yes-or-no-p
         (format "Warning: Your prompt exceeds the model's context limit (%.0f%%).  Send anyway? "
                 (* 100 percentage)))
      
      ;; Context is within limits
      t)))

;; ---------------------------------------------------------------------------
;; Shared request helpers (used by both network-process and curl backends)
;; ---------------------------------------------------------------------------

(defun ollama-buddy--validate-send-request (prompt tool-continuation-p)
  "Validate that a send request can proceed.
PROMPT is the user text, TOOL-CONTINUATION-P is non-nil for tool follow-ups.
Signals `user-error' if validation fails."
  (unless (ollama-buddy--check-status)
    (ollama-buddy--update-status "OFFLINE")
    (user-error "Ensure Ollama is running"))
  (unless (or tool-continuation-p (> (length prompt) 0))
    (user-error "Ensure prompt is defined"))
  (when ollama-buddy-show-context-percentage
    (unless (ollama-buddy--check-context-before-send)
      (user-error "Context too far over limit to send"))))

(defun ollama-buddy--process-inline-prompt (prompt)
  "Process inline delimiters in PROMPT and return the modified text.
Handles @search(), @rag(), @file(), and @skills() syntax."
  (when (and prompt (not ollama-buddy--skip-inline-processing))
    (when (and (featurep 'ollama-buddy-web-search)
               (fboundp 'ollama-buddy-web-search-process-inline))
      (setq prompt (ollama-buddy-web-search-process-inline prompt)))
    (setq prompt (ollama-buddy-rag-process-inline prompt))
    (setq prompt (ollama-buddy--file-process-inline prompt))
    (setq prompt (ollama-buddy--skills-process-inline prompt)))
  prompt)

(defun ollama-buddy--process-inline-prompt-async (prompt callback)
  "Process inline delimiters in PROMPT asynchronously.
Handles @search() and @rag() asynchronously to avoid blocking Emacs.
Handles @file() and @skills() synchronously (local operations).
Calls CALLBACK with the modified prompt when all processing is complete."
  (if (not (and prompt (not ollama-buddy--skip-inline-processing)))
      (funcall callback prompt)
    ;; Web search (async) → RAG (async) → file/skills (sync) → callback
    (ollama-buddy--process-inline-web-search-async
     prompt
     (lambda (p)
       (ollama-buddy--process-inline-rag-async
        p
        (lambda (p2)
          (setq p2 (ollama-buddy--file-process-inline p2))
          (setq p2 (ollama-buddy--skills-process-inline p2))
          (funcall callback p2)))))))

(defun ollama-buddy--process-inline-web-search-async (prompt callback)
  "Process @search() patterns in PROMPT asynchronously.
Calls CALLBACK with modified prompt."
  (if (and (featurep 'ollama-buddy-web-search)
           (fboundp 'ollama-buddy-web-search-process-inline-async))
      (ollama-buddy-web-search-process-inline-async prompt callback)
    ;; Module not loaded — pass through
    (funcall callback prompt)))

(defun ollama-buddy--process-inline-rag-async (prompt callback)
  "Process @rag() patterns in PROMPT asynchronously.
Calls CALLBACK with modified prompt."
  (if (and (featurep 'ollama-buddy-rag)
           (fboundp 'ollama-buddy-rag-process-inline-async))
      (ollama-buddy-rag-process-inline-async prompt callback)
    ;; Module not loaded — pass through
    (funcall callback prompt)))

(defun ollama-buddy--build-chat-payload (prompt specified-model tool-continuation-p)
  "Build the JSON payload and metadata for a chat API request.
PROMPT is the user message text (may be nil for TOOL-CONTINUATION-P).
SPECIFIED-MODEL overrides the default model.
TOOL-CONTINUATION-P non-nil means this follows tool execution.

Returns a plist with keys:
  :payload        - JSON string ready to send
  :model          - resolved model name
  :original-model - display model name (before real-name resolution)
  :has-images     - non-nil when vision images are attached
  :prompt         - the final prompt string"
  ;; Default prompt for tool continuations
  (when (and tool-continuation-p (not prompt))
    (setq prompt ""))

  (let* ((model-info (ollama-buddy--get-valid-model specified-model))
         (model (car model-info))
         (original-model (cdr model-info))
         (_ (ollama-buddy--ensure-cloud-model-available model))
         ;; Vision
         (supports-vision (and ollama-buddy-vision-enabled
                               (ollama-buddy--model-supports-vision model)))
         (image-files (when supports-vision
                        (ollama-buddy--detect-image-files prompt)))
         (has-images (and supports-vision image-files (not (null image-files))))
         ;; History & system prompt
         (history (ollama-buddy--get-history-for-request))
         (effective-system-prompt (ollama-buddy--effective-system-prompt))
         (messages-with-system
          (if effective-system-prompt
              (append `(((role . "system")
                         (content . ,effective-system-prompt)))
                      history)
            history))
         ;; Context: file attachments
         (attachment-context
          (when ollama-buddy--current-attachments
            (concat "\n\n## Attached Files Context:\n\n"
                    (mapconcat
                     (lambda (attachment)
                       (let ((file (plist-get attachment :file))
                             (content (plist-get attachment :content)))
                         (format "### File: %s\n### Path: %s\n\n#+end_src%s\n%s\n#+begin_src \n\n"
                                 (file-name-nondirectory file)
                                 file
                                 (or (plist-get attachment :type) "")
                                 content)))
                     ollama-buddy--current-attachments
                     ""))))
         ;; Context: web search
         (web-search-context
          (when (and (featurep 'ollama-buddy-web-search)
                     (fboundp 'ollama-buddy-web-search-get-context))
            (ollama-buddy-web-search-get-context)))
         ;; Context: RAG
         (rag-context (ollama-buddy-rag-get-context))
         ;; Combined context
         (combined-context
          (let ((contexts (delq nil (list attachment-context web-search-context rag-context))))
            (when contexts
              (concat "\n\n" (mapconcat #'identity contexts "\n\n")))))
         ;; Current message
         (current-message (if has-images
                              (ollama-buddy--create-vision-message prompt image-files)
                            `((role . "user")
                              (content . ,(if combined-context
                                              (concat prompt combined-context)
                                            prompt)))))
         ;; All messages (skip user message for tool continuations)
         (messages-all (if tool-continuation-p
                           messages-with-system
                         (append messages-with-system (list current-message))))
         ;; Parameters
         (modified-options (ollama-buddy-params-get-for-request))
         ;; Base payload
         (base-payload (append
                        `((model . ,(ollama-buddy--get-real-model-name model))
                          (messages . ,(vconcat [] messages-all))
                          (stream . ,(if ollama-buddy-streaming-enabled t :json-false)))
                        (when (and ollama-buddy-thinking-enabled
                                   (ollama-buddy--model-supports-thinking model))
                          '((think . t)))
                        (when ollama-buddy-keepalive
                          `((keep_alive . ,ollama-buddy-keepalive)))
                        (when ollama-buddy--response-format
                          `((format . ,ollama-buddy--response-format)))))
         ;; Add tools schema if applicable
         (with-tools (let* ((suppress (and (boundp 'ollama-buddy--suppress-tools-once)
                                           ollama-buddy--suppress-tools-once))
                            (_ (when suppress
                                 (setq ollama-buddy--suppress-tools-once nil)))
                            (schema (when (and (not suppress)
                                               (featurep 'ollama-buddy-tools)
                                               (bound-and-true-p ollama-buddy-tools-enabled)
                                               (ollama-buddy--model-supports-tools model))
                                      (ollama-buddy-tools--generate-schema))))
                       (if schema
                           (append base-payload `((tools . ,schema)))
                         base-payload)))
         ;; Add system prompt
         (with-system (if effective-system-prompt
                          (append with-tools `((system . ,effective-system-prompt)))
                        with-tools))
         ;; Add modified parameters
         (final-payload (if modified-options
                            (append with-system `((options . ,modified-options)))
                          with-system))
         (payload (json-encode final-payload)))

    (list :payload payload
          :model model
          :original-model original-model
          :has-images has-images
          :prompt (or prompt ""))))

(defun ollama-buddy--setup-chat-send (request-plist tool-continuation-p)
  "Set up chat buffer state and shared pre-send work.
REQUEST-PLIST is the result of `ollama-buddy--build-chat-payload'.
TOOL-CONTINUATION-P non-nil means this follows tool execution."
  (let ((model (plist-get request-plist :model))
        (original-model (plist-get request-plist :original-model))
        (has-images (plist-get request-plist :has-images))
        (prompt (plist-get request-plist :prompt)))

    ;; Clear any stale cancellation flag
    (setq ollama-buddy--request-cancelled nil)

    ;; Reset register
    (unless ollama-buddy--multishot-sequence
      (set-register ollama-buddy-default-register ""))

    ;; Set current state
    (setq ollama-buddy--current-model model)
    (setq ollama-buddy--current-prompt prompt)
    (setq ollama-buddy--current-tool-calls nil)
    (unless tool-continuation-p
      (setq ollama-buddy--tool-call-iteration 0))

    ;; Setup chat buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (unless tool-continuation-p
        (pop-to-buffer (current-buffer)))
      (goto-char (point-max))

      (unless (> (buffer-size) 0)
        (insert (ollama-buddy--create-intro-message))
        (save-excursion
          (when (re-search-backward "^\\*\\* More Commands$" nil t)
            (org-fold-hide-subtree))))

      (setq ollama-buddy--current-original-model original-model)
      (setq ollama-buddy--current-has-images has-images)
      (setq ollama-buddy--response-start-position (copy-marker (point)))
      (setq ollama-buddy--turn-start-position (copy-marker (point)))

      ;; Insert response header eagerly for immediate feedback with countdown
      (let ((pos (ollama-buddy--insert-response-header model original-model has-images)))
        (when pos
          (set-marker ollama-buddy--response-start-position pos)
          (set-marker pos nil)))
      (setq ollama-buddy--header-inserted-p t)

      (visual-line-mode 1))

    ;; Loading message for non-streaming mode
    (when (not ollama-buddy-streaming-enabled)
      (with-current-buffer ollama-buddy--chat-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (setq ollama-buddy--start-point (point))
          (insert "Loading response..."))))

    ;; Status + response wait timer
    (ollama-buddy--update-status (if has-images
                                     "Working... [vision]"
                                   "Working...")
                                 original-model model)
    (ollama-buddy--start-response-wait-timer model)

    ;; Kill existing process cleanly
    (when (and ollama-buddy--active-process
               (process-live-p ollama-buddy--active-process))
      (set-process-sentinel ollama-buddy--active-process nil)
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil))))

(defun ollama-buddy--send (&optional prompt specified-model tool-continuation-p)
  "Send PROMPT with optional SPECIFIED-MODEL.
When PROMPT contains image file paths and the model supports vision,
those images will be included in the request.
Cloud models are proxied through the local Ollama server which handles
authentication via `ollama signin'.
When TOOL-CONTINUATION-P is non-nil, this is a follow-up after tool execution
and no new user message is added."

  ;; If the user is sending a new (non-continuation) message, clear any
  ;; paused-session state left by an interactive tool (e.g. ediff).
  (unless tool-continuation-p
    (when (and (boundp 'ollama-buddy-tools--session-paused)
               ollama-buddy-tools--session-paused)
      (setq ollama-buddy-tools--session-paused nil)))

  ;; Validate request
  (ollama-buddy--validate-send-request prompt tool-continuation-p)

  ;; Process inline delimiters asynchronously, then send
  (ollama-buddy--process-inline-prompt-async
   prompt
   (lambda (processed-prompt)
     (ollama-buddy--send-payload processed-prompt specified-model tool-continuation-p))))

(defun ollama-buddy--send-payload (prompt specified-model tool-continuation-p)
  "Build and send the chat payload for PROMPT.
SPECIFIED-MODEL and TOOL-CONTINUATION-P are passed through
from `ollama-buddy--send'."
  ;; Build payload and setup shared state
  (let* ((request (ollama-buddy--build-chat-payload prompt specified-model tool-continuation-p))
         (payload (plist-get request :payload)))
    (ollama-buddy--setup-chat-send request tool-continuation-p)

    ;; --- Network-process transport ---
    (setq ollama-buddy--stream-pending "")

    (condition-case err
        (setq ollama-buddy--active-process
              (make-network-process
               :name "ollama-chat-stream"
               :buffer nil
               :host ollama-buddy-host
               :service ollama-buddy-port
               :coding 'utf-8
               :filter #'ollama-buddy--stream-filter
               :sentinel #'ollama-buddy--stream-sentinel))
      (error
       ;; Invalidate status cache so next check re-probes
       (setq ollama-buddy--last-status-check nil
             ollama-buddy--status-cache nil)
       (ollama-buddy--update-status "OFFLINE")
       (let ((msg (error-message-string err)))
         (if (string-match-p "Connection refused\\|connection refused\\|make client process failed" msg)
             (user-error "Ollama server is not running (%s:%d) — start it with `ollama serve'"
                         ollama-buddy-host ollama-buddy-port)
           (user-error "Failed to connect to Ollama: %s" msg)))))

    (condition-case err
        (process-send-string
         ollama-buddy--active-process
         (concat "POST /api/chat HTTP/1.1\r\n"
                 (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
                 "Content-Type: application/json\r\n"
                 (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
                 payload))
      (error
       (ollama-buddy--update-status "OFFLINE - Send failed")
       (when (and ollama-buddy--active-process
                  (process-live-p ollama-buddy--active-process))
         (delete-process ollama-buddy--active-process))
       (error "Failed to send request to Ollama: %s" (error-message-string err))))))

(defun ollama-buddy--multishot-send (prompt sequences)
  "Send PROMPT to multiple models specified by SEQUENCES list of model references."
  ;; Store sequences and prompt for use across multiple calls
  (setq ollama-buddy--multishot-sequence sequences
        ollama-buddy--multishot-prompt prompt
        ollama-buddy--multishot-progress 0)
  
  ;; Reset register
  (set-register ollama-buddy-default-register "")
  
  (setq ollama-buddy--current-request-temporary-model ollama-buddy--current-model)
  (ollama-buddy--send-next-in-sequence))

(defun ollama-buddy--multishot-cancel-timer ()
  "Cancel the multishot per-model timeout timer if active."
  (when (timerp ollama-buddy--multishot-timer)
    (cancel-timer ollama-buddy--multishot-timer)
    (setq ollama-buddy--multishot-timer nil)))

(defun ollama-buddy--multishot-timeout-handler ()
  "Handle a multishot per-model timeout.
Kill the active process, insert a timeout notice, and advance to the next model."
  (setq ollama-buddy--multishot-timer nil)
  (let ((model (or ollama-buddy--current-model "unknown")))
    ;; Kill the active process.  Clear the sentinel first so it doesn't
    ;; run synchronously during delete-process and wipe multishot state.
    (when (and ollama-buddy--active-process
               (process-live-p ollama-buddy--active-process))
      (set-process-sentinel ollama-buddy--active-process nil)
      (delete-process ollama-buddy--active-process)
      (setq ollama-buddy--active-process nil))

    ;; Clean up token tracking
    (when ollama-buddy--token-update-timer
      (cancel-timer ollama-buddy--token-update-timer)
      (setq ollama-buddy--token-update-timer nil))
    (ollama-buddy--cancel-response-wait-timer)
    (setq ollama-buddy--current-token-count 0
          ollama-buddy--current-token-start-time nil
          ollama-buddy--last-token-count 0
          ollama-buddy--last-update-time nil
          ollama-buddy--stream-pending "")

    ;; Insert timeout notice in the buffer
    (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "\n\n*** TIMED OUT (%ds)" ollama-buddy-multishot-timeout)))))

    (message "Multishot: %s timed out after %ds, skipping"
             model ollama-buddy-multishot-timeout)

    ;; Restore the saved model before advancing
    (when ollama-buddy--current-request-temporary-model
      (setq ollama-buddy--current-model ollama-buddy--current-request-temporary-model
            ollama-buddy--current-request-temporary-model nil))

    ;; Advance to next model or finish
    (if (and ollama-buddy--multishot-sequence
             (< ollama-buddy--multishot-progress
                (length ollama-buddy--multishot-sequence)))
        (run-with-timer 0.5 nil #'ollama-buddy--send-next-in-sequence)
      (ollama-buddy--update-status "Multi Finished")
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (ollama-buddy--prepare-prompt-area))
      (setq ollama-buddy--multishot-sequence nil
            ollama-buddy--multishot-prompt nil
            ollama-buddy--multishot-progress 0))))

(defun ollama-buddy--send-next-in-sequence ()
  "Send prompt to next model in the multishot sequence."
  (when (and ollama-buddy--multishot-sequence
             ollama-buddy--multishot-prompt
             (< ollama-buddy--multishot-progress (length ollama-buddy--multishot-sequence)))

    ;; Cancel any previous per-model timer
    (ollama-buddy--multishot-cancel-timer)

    ;; Get the next model key from the list
    (let* ((current-key (nth ollama-buddy--multishot-progress ollama-buddy--multishot-sequence))
           (model (cdr (assoc current-key ollama-buddy--model-letters))))
      (when model
        ;; Set current model and prepare prompt
        (setq ollama-buddy--current-model model)
        (if (= ollama-buddy--multishot-progress 0) ;; First model
            (ollama-buddy--prepare-prompt-area t t)
          (progn
            (ollama-buddy--prepare-prompt-area)
            (let ((buf (get-buffer-create ollama-buddy--chat-buffer)))
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (insert ollama-buddy--multishot-prompt))))))

        ;; Increment progress counter BEFORE sending the prompt
        ;; This is crucial to avoid access errors during the stream filter
        (setq ollama-buddy--multishot-progress (1+ ollama-buddy--multishot-progress))

        ;; Start per-model timeout timer
        (when ollama-buddy-multishot-timeout
          (setq ollama-buddy--multishot-timer
                (run-with-timer ollama-buddy-multishot-timeout nil
                               #'ollama-buddy--multishot-timeout-handler)))

        ;; Send the prompt
        (ollama-buddy--send-backend ollama-buddy--multishot-prompt model)))))

(defun ollama-buddy--multishot-prepare ()
  "Prepare for a multishot sequence and return the prompt text."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data)))
    
    ;; Ensure we have content
    (when (string-empty-p prompt-text)
      (user-error "Please enter a prompt before starting multishot"))
    
    prompt-text))

(defun ollama-buddy--multishot-prompt ()
  "Prompt for and execute multishot sequence using comma separation."
  (interactive)
  (let* ((prompt-text (ollama-buddy--multishot-prepare))
         (model-alist ollama-buddy--model-letters)
         (letter-display
          (mapconcat
           (lambda (pair)
             (format "%s" (car pair)))
           model-alist
           ", "))
         (prompt (format "Enter model sequence (separate with commas) - %s\nSequence: " letter-display))
         (input-sequence (read-string prompt))
         ;; Split by commas for clear separation
         (sequence-parts (split-string input-sequence "," t "\\s-*"))
         (valid-sequences '()))
    
    ;; Process each part of the input sequence
    (dolist (part sequence-parts)
      (let ((trimmed (string-trim part)))
        (when (member trimmed (mapcar 'car model-alist))
          (push trimmed valid-sequences))))
    
    ;; Reverse the list since we pushed elements
    (setq valid-sequences (nreverse valid-sequences))
    
    (when valid-sequences
      (let ((model-names (mapcar (lambda (key) (cdr (assoc key model-alist))) valid-sequences)))
        (message "Running multishot with %d models: %s"
                 (length valid-sequences)
                 (mapconcat 'identity model-names ", "))
        
        ;; Start the multishot sequence
        (ollama-buddy--multishot-send prompt-text valid-sequences)))))

(defun ollama-buddy-benchmark-models ()
  "Benchmark models by sending a prompt to each via multishot.
Uses `ollama-buddy-benchmark-prompt' as the prompt text.  Embedding models
are excluded by default.  Presents an editable comma-separated list of
model letters so you can remove models (e.g. cloud models) before running.
Results are recorded in `ollama-buddy--token-usage-history'."
  (interactive)
  (ollama-buddy--assign-model-letters (ollama-buddy--get-models))
  (let* ((model-alist ollama-buddy--model-letters)
         ;; Build default list excluding embedding models
         (default-sequences
          (cl-remove-if
           (lambda (key)
             (let ((model (cdr (assoc key model-alist))))
               (and model (string-match-p "embed" model))))
           (mapcar #'car model-alist)))
         ;; Let user edit the letter list
         (default-input (mapconcat #'identity default-sequences ","))
         (input (read-string "Benchmark models (edit letters, comma-separated): "
                             default-input))
         ;; Parse and validate
         (chosen-parts (split-string input "," t "\\s-*"))
         (valid-sequences
          (cl-remove-if-not
           (lambda (key) (assoc key model-alist))
           (mapcar #'string-trim chosen-parts))))
    (unless valid-sequences
      (user-error "No valid models selected"))
    (let ((model-names (mapcar (lambda (key) (cdr (assoc key model-alist)))
                               valid-sequences)))
      ;; Confirm
      (when (yes-or-no-p
             (format "Benchmark %d models: %s\nPrompt: \"%s\"\nProceed? "
                     (length valid-sequences)
                     (mapconcat #'identity model-names ", ")
                     ollama-buddy-benchmark-prompt))
        (message nil)
        ;; Ensure chat buffer exists and insert the benchmark prompt
        (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
          (ollama-buddy--prepare-prompt-area t t)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert ollama-buddy-benchmark-prompt)))
        (ollama-buddy--multishot-send ollama-buddy-benchmark-prompt valid-sequences)))))

(defun ollama-buddy--cycle-prompt-history (direction)
  "Cycle through prompt history in DIRECTION (1=forward, -1=backward)."
  (interactive)
  (when ollama-buddy--prompt-history
    (let* ((prompt-data (ollama-buddy--get-prompt-content))
           (prompt-point (cdr prompt-data))
           (current-pos (or (get 'ollama-buddy--cycle-prompt-history 'history-position) 0))
           (history-length (length ollama-buddy--prompt-history))
           (new-pos (+ current-pos direction))
           (new-pos (if (< new-pos -1) -1
                      (min new-pos (1- history-length))))
           (new-content (if (= new-pos -1)
                            "" ; Clear prompt when moving past the end
                          (nth new-pos ollama-buddy--prompt-history))))

      ;; Store position for next cycle
      (put 'ollama-buddy--cycle-prompt-history 'history-position new-pos)
      
      (when prompt-point
        (save-excursion
          (goto-char prompt-point)
          (search-forward ": ")
          (delete-region (point) (point-max))
          (insert new-content))))))

(defun ollama-buddy-previous-history ()
  "Navigate to previous item in prompt history."
  (interactive)
  (ollama-buddy--cycle-prompt-history 1))

(defun ollama-buddy-next-history ()
  "Navigate to next item in prompt history."
  (interactive)
  (ollama-buddy--cycle-prompt-history -1))

;;;###autoload
(defun ollama-buddy-menu ()
  "Display Ollama Buddy menu with support for prefixed model references."
  (interactive)
  (let ((ollama-status (ollama-buddy--check-status))
        (inhibit-message t)
        (url-show-status nil))
    (ollama-buddy--update-status
     (if ollama-status "Menu opened - Ready" "Menu opened - Ollama offline"))
    (when-let* ((items (mapcar (lambda (cmd-def)
                                 (let* ((key (plist-get (cdr cmd-def) :key))
                                        (desc (plist-get (cdr cmd-def) :description))
                                        (model (plist-get (cdr cmd-def) :model))
                                        (action (plist-get (cdr cmd-def) :action))
                                        ;; Add model indicator if a specific model is used
                                        (desc-with-model
                                         (if model
                                             (concat desc " "
                                                     (propertize (concat model)
                                                                 'face `(:inherit bold)))
                                           desc)))
                                   (cons key (list desc-with-model action))))
                               ollama-buddy-command-definitions))
                (formatted-items
                 (mapcar (lambda (item)
                           ;; Updated format to handle characters vs. strings
                           (if (stringp (car item))
                               (format "[%s] %s" (car item) (cadr item))
                             (format "[%c] %s" (car item) (cadr item))))
                         items))
                (total (length formatted-items))
                (rows (ceiling (/ total (float ollama-buddy-menu-columns))))
                ;; Ensure menu is fully visible by setting max-mini-window-height
                ;; Add 2 for header line and some padding
                (max-mini-window-height (+ rows 2))
                (padded-items (append formatted-items
                                      (make-list (- (* rows
                                                       ollama-buddy-menu-columns)
                                                    total)
                                                 "")))
                (format-string
                 (mapconcat
                  (lambda (width) (format "%%-%ds" (+ width 2)))
                  (butlast
                   (cl-loop for col below ollama-buddy-menu-columns collect
                            (cl-loop for row below rows
                                     for idx = (+ (* col rows) row)
                                     when (< idx total)
                                     maximize (length (nth idx padded-items)))))
                  ""))
                
                (model (or ollama-buddy--current-model
                           ollama-buddy-default-model "NONE"))
                (prompt
                 (format "%s %s%s\n%s"
                         (if ollama-status "RUNNING" "NOT RUNNING")
                         (propertize model 'face `(:weight bold))
                         (if (use-region-p) "" " (NO SELECTION)")
                         (mapconcat
                          (lambda (row)
                            (if format-string
                                (apply #'format (concat format-string "%s") row)
                              (car row)))
                          (cl-loop for row below rows collect
                                   (cl-loop for col below ollama-buddy-menu-columns
                                            for idx = (+ (* col rows) row)
                                            when (< idx (length padded-items))
                                            collect (nth idx padded-items)))
                          "\n")))
                (key (read-key prompt))
                (cmd (assoc key items)))
      (funcall (caddr cmd)))))

;;;###autoload
(defun ollama-buddy-add-model-to-menu-entry (entry-name model-name)
  "Add :model property with MODEL-NAME to ENTRY-NAME in the menu variable.
Modifies the variable in place."
  (when-let ((entry (assq entry-name ollama-buddy-command-definitions)))
    (setf (cdr entry)
          (append (cdr entry) (list :model model-name))))
  ollama-buddy-command-definitions)

(defun ollama-buddy--send-prompt ()
  "Send the current prompt to a LLM with support for system prompt."
  (interactive)
  (let ((model (or ollama-buddy--current-model
                   ollama-buddy-default-model
                   "Default:latest")))
    (if (and ollama-buddy-airplane-mode
             (ollama-buddy--internet-model-p model))
        (run-with-timer 0 nil (lambda (m) (message "✈ Airplane mode: %s requires internet" m)) model)
      (let* ((current-prefix-arg-val (prefix-numeric-value current-prefix-arg))
             (prompt-data (ollama-buddy--get-prompt-content))
             (prompt-text (car prompt-data)))

        ;; Handle prefix arguments
        (cond
         ;; C-u (4) - Continue (send "continue" without needing to type it)
         ((= current-prefix-arg-val 4)
          (ollama-buddy--send-backend "continue" model))

         ;; C-u C-u (16) - Rewind conversation
         ((= current-prefix-arg-val 16)
          (ollama-buddy-rewind))

         ;; No prefix - Regular prompt
         (t
          ;; Add to history if non-empty
          (when (and prompt-text (not (string-empty-p prompt-text)))
            (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
            (add-to-history 'ollama-buddy--prompt-history prompt-text))

          ;; Reset multishot variables
          (setq ollama-buddy--multishot-sequence nil
                ollama-buddy--multishot-prompt nil)

          ;; Send with system prompt support
          (ollama-buddy--send-backend prompt-text model)))))))

(defun ollama-buddy--cancel-request ()
  "Cancel the current request and clean up resources."
  (interactive)
  (if ollama-buddy--active-process
      (progn
        (setq ollama-buddy--request-cancelled t)
        (delete-process ollama-buddy--active-process)
        (setq ollama-buddy--active-process nil)

        ;; Clean up token tracking
        (when ollama-buddy--token-update-timer
          (cancel-timer ollama-buddy--token-update-timer)
          (setq ollama-buddy--token-update-timer nil))

        ;; Cancel response wait timer (countdown + elapsed display)
        (ollama-buddy--cancel-response-wait-timer)

        ;; Reset token tracking variables
        (setq ollama-buddy--current-token-count 0
              ollama-buddy--current-token-start-time nil
              ollama-buddy--last-token-count 0
              ollama-buddy--last-update-time nil)

        ;; Reset stream buffer
        (setq ollama-buddy--stream-pending "")

        ;; Cancel multishot timer and reset multishot variables
        (ollama-buddy--multishot-cancel-timer)
        (setq ollama-buddy--multishot-prompt nil)
        ;; Only reset sequence if we were using it
        (when ollama-buddy--multishot-sequence
          (setq ollama-buddy--multishot-sequence nil
                ollama-buddy--multishot-progress 0))
        
        (ollama-buddy--update-status "Cancelled"))

    (progn
      ;; otherwise regenerate/reset the prompt
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (with-current-buffer ollama-buddy--chat-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (ollama-buddy--prepare-prompt-area t))))))

(defun ollama-buddy-ensure-modelfile-directory ()
  "Create the ollama-buddy modelfile directory if it doesn't exist."
  (unless (file-directory-p ollama-buddy-modelfile-directory)
    (make-directory ollama-buddy-modelfile-directory t)))

(defun ollama-buddy-import-gguf-file (file-path)
  "Import a GGUF file at FILE-PATH into Ollama."
  (interactive "fSelect GGUF file: ")
  (unless (file-exists-p file-path)
    (user-error "File does not exist: %s" file-path))
  
  (unless (string-match-p "\\.gguf$" file-path)
    (user-error "File does not appear to be a GGUF file (missing .gguf extension): %s" file-path))
  
  ;; Ensure the modelfile directory exists
  (ollama-buddy-ensure-modelfile-directory)
  
  ;; Get the base name without extension for default model name
  (let* ((file-name (file-name-nondirectory file-path))
         (base-name (replace-regexp-in-string "\\.gguf$" "" file-name))
         ;; Prompt for model name, suggesting a sanitized version of the filename
         (model-name (read-string "Model name to create: "
                                  (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" base-name)))
         ;; Prompt for model parameters
         (parameters (read-string "Model parameters (optional): " ""))
         ;; Create a temporary Modelfile
         (modelfile-path (expand-file-name (format "Modelfile-%s" model-name)
                                           ollama-buddy-modelfile-directory))
         ;; Buffer for output
         (output-buffer (get-buffer-create "*Ollama Import*"))
         (default-directory (file-name-directory file-path)))
    
    ;; Generate Modelfile content
    (with-temp-file modelfile-path
      (insert (format "FROM %s\n" file-path))
      (when (not (string-empty-p parameters))
        (insert (format "PARAMETER %s\n" parameters))))
    
    ;; Show the buffer
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Creating Ollama model '%s' from file: %s\n\n" model-name file-path))
        (insert "Modelfile content:\n")
        (insert-file-contents modelfile-path)
        (insert "\n\nRunning ollama create command...\n\n")
        (display-buffer (current-buffer))))
    
    ;; Run the ollama create command
    (let ((process (start-process "ollama-create" output-buffer
                                  "ollama" "create" model-name "-f" modelfile-path)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (let ((status (string-trim event)))
           (with-current-buffer (process-buffer proc)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (cond
                ((string-prefix-p "finished" status)
                 (insert "\nSuccessfully created model: " model-name)
                 (ollama-buddy--assign-model-letters (ollama-buddy--get-models))
                 ;; Ask if user wants to use this model now
                 (when (y-or-n-p (format "Model '%s' created.  Use it now? " model-name))
                   (setq ollama-buddy--current-model model-name)
                   (message "Switched to model: %s" model-name)))
                (t
                 (insert "\nError creating model: " status)))
               (insert "\n")))))))))

(defun ollama-buddy-manage-models-refresh ()
  "Refresh the model management buffer, clearing all caches."
  (interactive)
  (setq ollama-buddy--cloud-usage-cache nil
        ollama-buddy--cloud-usage-cache-time nil
        ollama-buddy--models-cache-timestamp nil
        ollama-buddy--running-models-cache-timestamp nil)
  (ollama-buddy-manage-models))

(defun ollama-buddy-manage-models ()
  "Update the model management interface to include unload capabilities."
  (interactive)
  (let* ((available-models (ollama-buddy--get-models))
         (running-models (ollama-buddy--get-running-models))
         (_letters (ollama-buddy--assign-model-letters available-models))
         (cloud-display-models (mapcar #'ollama-buddy--get-full-cloud-model-name
                                       ollama-buddy-cloud-models))
         (launch-available (and (ollama-buddy--detect-available-agents)
                                (or ollama-buddy-launch-terminal
                                    (ollama-buddy--detect-terminal))))
         (buf (get-buffer-create "*Ollama Models Management*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)
        (setq truncate-lines t)
        (erase-buffer)

        (insert "#+title: Model Management")
        (when-let ((version (ollama-buddy--get-version)))
          (insert (format " (Ollama %s)" version)))
        (insert "\n\n")
        (insert "Press =g= to refresh\n\n")

        ;; Show running models with individual unload buttons
        (when running-models
          (insert (format "* Running Models: %d  " (length running-models)))
          (insert-text-button
           "[Unload All]"
           'action (lambda (_)
                     (ollama-buddy-unload-all-models)
                     (run-with-timer 3 nil #'ollama-buddy-manage-models))
           'help-echo "Unload all running models to free up resources")
          (insert "\n\n")
          (dolist (model running-models)
            (insert "- ")
            (insert-text-button
             model
             'action `(lambda (_)
                        (ollama-buddy-select-model ,model))
             'help-echo (format "Select %s" model))
            (insert "  ")
            (insert-text-button
             "Unload"
             'action `(lambda (_)
                        (ollama-buddy--unload-single-model ,model)
                        (run-with-timer 3 nil #'ollama-buddy-manage-models))
             'help-echo (format "Unload %s to free up resources" model))
            (insert "\n"))
          (insert "\n"))

        ;; Actions at bottom
        (insert "* Actions:\n\n")
        (insert-text-button
         "[Import GGUF File]"
         'action (lambda (_) (call-interactively #'ollama-buddy-import-gguf-file))
         'help-echo "Import a GGUF file to create a new model")
        (insert "  ")
        (insert-text-button
         "[Refresh List]"
         'action (lambda (_) (ollama-buddy-manage-models))
         'help-echo "Refresh model list")
        (insert "  ")
        (insert-text-button
         "[Custom Pull Model]"
         'action (lambda (_) (call-interactively #'ollama-buddy-pull-model))
         'help-echo "Pull a model from Ollama Hub")
        
        ;; List of models with status and actions
        (insert "\n\n* Available Models\n\n")

        ;; Cloud models section (hidden in airplane mode)
        (when (and ollama-buddy-cloud-models
                   (not ollama-buddy-airplane-mode))
          (insert (format "** ☁ Cloud Models %s\n\n"
                          (ollama-buddy--cloud-auth-status-indicator)))
          ;; Cloud usage stats
          (if (eq ollama-buddy--cloud-auth-status 'not-authenticated)
              (insert "  *Not signed in* — use =C-c A= or =M-x ollama-buddy-cloud-signin= to sign in\n\n")
            (let ((usage (ollama-buddy--fetch-cloud-usage)))
              (if usage
                  (let ((session (alist-get 'session usage))
                        (weekly (alist-get 'weekly usage))
                        (session-reset (alist-get 'session-reset usage))
                        (weekly-reset (alist-get 'weekly-reset usage)))
                    (insert (format "Session: %s %s" (ollama-buddy--cloud-usage-pie session 36) session))
                    (when session-reset
                      (insert (format " (%s)" (ollama-buddy--cloud-reset-time-string session-reset))))
                    (insert (format "  |  Weekly: %s %s" (ollama-buddy--cloud-usage-pie weekly 36) weekly))
                    (when weekly-reset
                      (insert (format " (%s)" (ollama-buddy--cloud-reset-time-string weekly-reset))))
                    (insert "\n\n"))
                (when (or (not (stringp ollama-buddy-cloud-session-token))
                          (string-empty-p ollama-buddy-cloud-session-token))
                  (insert "  (Set ollama-buddy-cloud-session-token for usage stats)\n\n")))))
          (dolist (model ollama-buddy-cloud-models)
            (let* ((display-model (ollama-buddy--get-full-cloud-model-name model))
                   (letter (ollama-buddy--get-model-letter display-model)))
              (insert (format "- (%s) "
                              (or letter " ")))
              ;; Select button
              (insert-text-button
               display-model
               'action `(lambda (_)
                          (ollama-buddy-select-model ,(ollama-buddy--get-full-cloud-model-name model)))
               'help-echo (format "Select cloud model %s" model))
              (insert " ☁")
              (when (ollama-buddy--model-supports-tools display-model)
                (insert "⚒"))
              (when (ollama-buddy--model-supports-vision display-model)
                (insert "⊙"))
              (when (ollama-buddy--model-supports-thinking display-model)
                (insert "✦"))
              (insert "  ")
              ;; Pull manifest button
              (insert-text-button
               "Pull Manifest"
               'action `(lambda (_)
                          (ollama-buddy--ensure-cloud-model-available ,display-model)
                          (ollama-buddy-manage-models))
               'help-echo (format "Pull manifest for %s (required before first use)" model))
              (when launch-available
                (insert "  ")
                (insert-text-button
                 "Launch"
                 'action `(lambda (_)
                            (ollama-buddy--launch-model ,model))
                 'help-echo (format "Launch %s in external terminal" model)))
              (insert "\n"))))

        (insert "\n** Local\n\n")

        (dolist (model available-models)
          (let* ((is-running (member model running-models))
                 (letter (ollama-buddy--get-model-letter model)))

            (insert (format "- (%s) "
                            (or letter " ")))

            ;; Select button
            (insert-text-button
             model
             'action `(lambda (_)
                        (ollama-buddy-select-model ,model))
             'help-echo "Select this model")
            (when (ollama-buddy--model-supports-tools model)
              (insert " ⚒"))
            (when (ollama-buddy--model-supports-vision model)
              (insert " ⊙"))
            (when (ollama-buddy--model-supports-thinking model)
              (insert " ✦"))

            (insert "  ")

            ;; Info button
            (insert-text-button
             "Info"
             'action `(lambda (_)
                        (ollama-buddy-show-raw-model-info ,model))
             'help-echo "Show model information")

            (insert "  ")

            ;; Add Unload button for running models
            (if is-running
                (progn
                  (insert-text-button
                   "Unload"
                   'action `(lambda (_)
                              (ollama-buddy--unload-single-model ,model)
                              (run-with-timer 3 nil #'ollama-buddy-manage-models))
                   'help-echo "Unload this model to free up resources")
                  (insert "  "))
              ;; Pull button for non-running models
              (progn
                (insert-text-button
                 "Pull"
                 'action `(lambda (_)
                            (ollama-buddy-pull-model ,model))
                 'help-echo "Pull/update this model")
                (insert "  ")))

            ;; Copy
            (insert-text-button
             "Copy"
             'action `(lambda (_)
                        (ollama-buddy-copy-model ,model)
                        (ollama-buddy-manage-models))
             'help-echo "Copy this model")

            (insert "  ")

            ;; Delete button with proper capture
            (insert-text-button
             "Delete"
             'action `(lambda (_)
                        (when (yes-or-no-p (format "Really delete model '%s'? " ,model))
                          ;; Refresh only after the async DELETE confirms,
                          ;; otherwise the list re-fetches before Ollama
                          ;; has actually removed the model.
                          (ollama-buddy-delete-model
                           ,model
                           (lambda ()
                             (when (get-buffer "*Ollama Models Management*")
                               (ollama-buddy-manage-models-refresh))))))
             'help-echo "Delete this model")

            (when launch-available
              (insert "  ")
              (insert-text-button
               "Launch"
               'action `(lambda (_)
                          (ollama-buddy--launch-model ,model))
               'help-echo (format "Launch %s in external terminal" model)))

            (insert "\n")))

        )
      (goto-char (point-min))
      (view-mode 1)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "g") #'ollama-buddy-manage-models-refresh)
        (setq-local minor-mode-overriding-map-alist
                    (list (cons 'view-mode map)))))
    (display-buffer buf)
    ;; Asynchronously fetch /api/show capabilities in the background.
    ;; The buffer renders immediately using static-list fallbacks;
    ;; when capabilities arrive and any indicator actually changed,
    ;; silently re-render with accurate data (no visible flash).
    (ollama-buddy--fetch-model-capabilities-async
     (append available-models cloud-display-models)
     (lambda ()
       (when (buffer-live-p buf)
         (let ((win (get-buffer-window buf))
               (saved-line (with-current-buffer buf
                             (line-number-at-pos (point))))
               (inhibit-redisplay t))
           (ollama-buddy-manage-models)
           (when win
             (with-current-buffer buf
               (goto-char (point-min))
               (forward-line (1- saved-line))
               (set-window-point win (point))))))))))

(defun ollama-buddy-recommended-models ()
  "Display recommended models from the Ollama Hub in a separate buffer.
Shows categorized models that are not yet installed locally."
  (interactive)
  (let* ((available-models (ollama-buddy--get-models))
         (models-to-pull
          (when (ollama-buddy--ollama-running)
            (let ((available-for-pull
                   (mapcar (lambda (model)
                             (if (ollama-buddy--should-use-marker-prefix)
                                 (concat ollama-buddy-marker-prefix model)
                               model))
                           (ollama-buddy--available-models-flat))))
              (cl-set-difference
               available-for-pull
               available-models
               :test #'string=))))
         (buf (get-buffer-create "*Ollama Recommended Models*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (setq-local org-hide-leading-stars t)
        (setq truncate-lines t)
        (erase-buffer)
        (insert "#+title: Recommended Models\n\n")
        (insert "Press =g= to refresh\n\n")
        (if (not models-to-pull)
            (insert "All recommended models are already installed.\n")
          ;; New models section
          (let ((new-pullable (cl-remove-if-not
                               (lambda (m) (member m models-to-pull))
                               (mapcar (lambda (model)
                                         (if (ollama-buddy--should-use-marker-prefix)
                                             (concat ollama-buddy-marker-prefix model)
                                           model))
                                       ollama-buddy-new-models))))
            (when new-pullable
              (insert "* New\n\n")
              (insert "Recently released models\n\n")
              (dolist (model new-pullable)
                (let ((display-model (if (ollama-buddy--should-use-marker-prefix)
                                         model
                                       (ollama-buddy--get-real-model-name model))))
                  (insert "- ")
                  (insert-text-button
                   display-model
                   'action `(lambda (_)
                              (ollama-buddy-pull-model ,model))
                   'help-echo (format "Pull %s from Ollama Hub" display-model))
                  (when (ollama-buddy--model-supports-tools display-model)
                    (insert " ⚒"))
                  (when (ollama-buddy--model-supports-vision display-model)
                    (insert " ⊙"))
                  (when (ollama-buddy--model-supports-thinking display-model)
                    (insert " ✦"))
                  (insert "\n")))
              (insert "\n")))
          ;; Category sections
          (dolist (category ollama-buddy-available-models)
            (let* ((cat-name (plist-get category :name))
                   (cat-desc (plist-get category :description))
                   (cat-models (plist-get category :models))
                   (pullable (cl-remove-if-not
                              (lambda (m) (member m models-to-pull))
                              (mapcar (lambda (model)
                                        (if (ollama-buddy--should-use-marker-prefix)
                                            (concat ollama-buddy-marker-prefix model)
                                          model))
                                      cat-models))))
              (when pullable
                (insert (format "* %s\n\n" cat-name))
                (when cat-desc
                  (insert (format "%s\n" cat-desc)))
                (insert "\n")
                (dolist (model pullable)
                  (let ((display-model (if (ollama-buddy--should-use-marker-prefix)
                                           model
                                         (ollama-buddy--get-real-model-name model))))
                    (insert "- ")
                    (insert-text-button
                     display-model
                     'action `(lambda (_)
                                (ollama-buddy-pull-model ,model))
                     'help-echo (format "Pull %s from Ollama Hub" display-model))
                    (when (ollama-buddy--model-supports-tools display-model)
                      (insert " ⚒"))
                    (when (ollama-buddy--model-supports-vision display-model)
                      (insert " ⊙"))
                    (when (ollama-buddy--model-supports-thinking display-model)
                      (insert " ✦"))
                    (insert "\n")))
                (insert "\n"))))))
      (goto-char (point-min))
      (view-mode 1)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "g")
          (lambda () (interactive) (ollama-buddy-recommended-models)))
        (setq-local minor-mode-overriding-map-alist
                    (list (cons 'view-mode map)))))
    (display-buffer buf)))

(defun ollama-buddy-select-model (model)
  "Set MODEL as the current model."
  (setq ollama-buddy-default-model model)
  (setq ollama-buddy--current-model model)
  (message "Selected model: %s" model)
  (pop-to-buffer (get-buffer-create ollama-buddy--chat-buffer))
  (ollama-buddy--prepare-prompt-area)
  (goto-char (point-max))
  (ollama-buddy--update-status "Idle")
  ;; Fetch capabilities asynchronously — update prompt area when done
  (when (ollama-buddy--ollama-running)
    (ollama-buddy--fetch-model-context-size-async
     model
     (lambda ()
       (when (buffer-live-p (get-buffer ollama-buddy--chat-buffer))
         (with-current-buffer ollama-buddy--chat-buffer
           (ollama-buddy--prepare-prompt-area t t)))))))

(defun ollama-buddy-pull-model (model)
  "Pull or update MODEL from Ollama Hub asynchronously.
When the operation completes, CALLBACK is called with no arguments if provided."
  (interactive
   (list (completing-read
          "Pull model: "
          #'ollama-buddy--pull-model-completion-table
          nil
          nil  ; Allow custom input
          nil
          nil
          (car (ollama-buddy--available-models-flat)))))

  (let* ((real-model (ollama-buddy--get-real-model-name model))
         (payload (json-encode `((model . ,real-model))))
         (operation-id (gensym "pull-"))
         (pending "")
         (headers-done nil)
         (finished nil)
         (pull-proc nil))

    (ollama-buddy--register-background-operation
     operation-id
     (format "Pulling %s" model))

    (condition-case err
        (let ((proc (make-network-process
                     :name (format "ollama-pull-%s" real-model)
                     :buffer nil
                     :host ollama-buddy-host
                     :service ollama-buddy-port
                     :coding 'utf-8
                     :filter
                     (lambda (_proc output)
                       (unless finished
                         (save-match-data
                           (setq pending (concat pending output))
                           ;; Skip HTTP headers
                           (unless headers-done
                             (when (string-match "\r\n\r\n" pending)
                               (setq pending (substring pending (match-end 0)))
                               (setq headers-done t)))
                           (when headers-done
                             ;; Process complete NDJSON lines
                             (let (json-lines)
                               ;; First collect all complete lines
                               (while (string-match "^\\([^\n]+\\)\n" pending)
                                 (push (match-string 1 pending) json-lines)
                                 (setq pending (substring pending (match-end 0))))
                               ;; Then process them (no match-data dependency)
                               (dolist (line (nreverse json-lines))
                                 (unless finished
                                   (condition-case nil
                                       (let* ((json (json-read-from-string line))
                                              (status-text (cdr (assq 'status json)))
                                              (total (cdr (assq 'total json)))
                                              (completed (cdr (assq 'completed json)))
                                              (err-text (cdr (assq 'error json))))
                                         (cond
                                          (err-text
                                           (setq finished t)
                                           (run-at-time 0 nil
                                                        (lambda ()
                                                          (message "Error pulling %s: %s" model err-text)
                                                          (ollama-buddy--complete-background-operation
                                                           operation-id
                                                           (format "Error: %s" err-text))
                                                          (when (and pull-proc (process-live-p pull-proc))
                                                            (delete-process pull-proc)))))
                                          ((equal status-text "success")
                                           (setq finished t)
                                           (run-at-time 0 nil
                                                        (lambda ()
                                                          (message "Successfully pulled model %s" model)
                                                          (ollama-buddy--complete-background-operation
                                                           operation-id
                                                           (format "Successfully pulled %s" model))
                                                          (when (and pull-proc (process-live-p pull-proc))
                                                            (delete-process pull-proc)))))
                                          ((and total (> total 0) completed)
                                           (let ((pct (/ (* 100 completed) total)))
                                             (ollama-buddy--update-background-operation
                                              operation-id
                                              (format "Pulling %s %d%%" model pct))))
                                          (status-text
                                           (ollama-buddy--update-background-operation
                                            operation-id
                                            (format "Pulling %s: %s" model status-text)))))
                                     (error nil)))))))))
                     :sentinel
                     (lambda (_proc _event)
                       (unless finished
                         (setq finished t)
                         (message "Error pulling %s: connection closed unexpectedly" model)
                         (ollama-buddy--complete-background-operation
                          operation-id
                          (format "Error pulling %s" model)))))))
          (setq pull-proc proc)
          (process-send-string
           proc
           (concat "POST /api/pull HTTP/1.1\r\n"
                   (format "Host: %s:%d\r\n" ollama-buddy-host ollama-buddy-port)
                   "Content-Type: application/json\r\n"
                   (format "Content-Length: %d\r\n\r\n" (string-bytes payload))
                   payload)))
      (error
       (message "Error pulling %s: %s" model (error-message-string err))
       (ollama-buddy--complete-background-operation
        operation-id
        (format "Error pulling %s" model))))))

(defun ollama-buddy-copy-model (model)
  "Copy MODEL in Ollama."
  (let* ((destination (read-string (format "New name for copy of %s: " model)))
         (payload (json-encode `((source . ,model)
                                 (destination . ,destination))))
         (operation-id (gensym "copy-")))

    (ollama-buddy--register-background-operation
     operation-id
     (format "Copying to %s" model))
    
    (ollama-buddy--make-request-async-backend
     "/api/copy"
     "POST"
     payload
     (lambda (status result)
       (cond
        ((plist-get status :error)
         (message "Error copying: %s" (cdr (plist-get status :error)))
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Error copying %s" model)))
        ((and result (cdr (assq 'error result)))
         (message "Error copying %s: %s" model (cdr (assq 'error result)))
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Error copying %s" model)))
        (t
         (message "Model %s successfully copied to %s" model destination)
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Successfully copied model %s" model))))))))

(defun ollama-buddy-delete-model (model &optional on-success)
  "Delete MODEL from Ollama.
If ON-SUCCESS is non-nil, call it (with no arguments) once the deletion
has been confirmed by the server.  This lets callers such as the Model
Management buffer refresh themselves only after the async DELETE has
actually completed, rather than racing it."
  (let ((payload (json-encode `((model . ,(ollama-buddy--get-real-model-name model)))))
        (operation-id (gensym "delete-")))

    (ollama-buddy--register-background-operation
     operation-id
     (format "Deleting %s" model))

    (ollama-buddy--make-request-async-backend
     "/api/delete"
     "DELETE"
     payload
     (lambda (status result)
       (cond
        ((plist-get status :error)
         (message "Error deleting: %s" (cdr (plist-get status :error)))
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Error deleting %s" model)))
        ((and result (cdr (assq 'error result)))
         (message "Error deleting %s: %s" model (cdr (assq 'error result)))
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Error deleting %s" model)))
        (t
         (message "Model %s successfully deleted" model)
         ;; Invalidate the models cache so the next listing actually
         ;; reflects the deletion rather than returning stale data.
         (setq ollama-buddy--models-cache-timestamp nil
               ollama-buddy--running-models-cache-timestamp nil)
         (ollama-buddy--complete-background-operation
          operation-id
          (format "Successfully deleted model %s" model))
         (when on-success
           (funcall on-success))))))))

(defun ollama-buddy-params-help ()
  "Display help for Ollama parameters.
This is now merged into `ollama-buddy-params-display'."
  (interactive)
  (ollama-buddy-params-display))

(defun ollama-buddy-set-model-context-size (model size)
  "Manually set the context size for MODEL to SIZE."
  (interactive
   (let* ((models (ollama-buddy--get-models))
          (model (completing-read "Model: " models nil t))
          (size (read-number "Context size: "
                             (or (gethash model ollama-buddy--model-context-sizes)
                                 4096))))
     (list model size)))

  (puthash model size ollama-buddy--model-context-sizes)

  (ollama-buddy--calculate-prompt-context-percentage)
  (ollama-buddy--update-status "CTX Applied")

  (message "Context size for %s set to %d" model size))

(defun ollama-buddy-set-max-history-length (length)
  "Set the LENGTH number of message pairs to keep in conversation history."
  (interactive
   (list
    (read-number (format "Set max history length (current: %d): "
                         ollama-buddy-max-history-length)
                 ollama-buddy-max-history-length)))
  
  ;; Validate the input
  (when (< length 0)
    (user-error "History length must be non-negative"))
  
  ;; Set the new value
  (setq ollama-buddy-max-history-length length)

  (ollama-buddy--update-status "Updated History Max")
  
  ;; Truncate existing histories if needed
  ;; (when (> length 0)
  ;;   (maphash (lambda (model history)
  ;;              (when (> (length history) (* 2 length))
  ;;                (puthash model
  ;;                         (seq-take history (* 2 length))
  ;;                         ollama-buddy--conversation-history-by-model)))
  ;;            ollama-buddy--conversation-history-by-model))
  
  ;; Provide feedback
  (if (= length 0)
      (message "History disabled (max length set to 0)")
    (message "Max history length set to %d message pairs" length)))

(defun ollama-buddy-toggle-context-display-type ()
  "Toggle between text and bar display for context usage."
  (interactive)
  (setq ollama-buddy-context-display-type
        (if (eq ollama-buddy-context-display-type 'text) 'bar 'text))
  (ollama-buddy--update-status
   (format "Context display: %s"
           (if (eq ollama-buddy-context-display-type 'bar) "bar" "text")))
  (message "Context display mode: %s"
           (if (eq ollama-buddy-context-display-type 'bar) "bar" "text")))

(defun ollama-buddy--is-supported-file-type (file)
  "Check if FILE is of a supported type."
  (cl-some (lambda (pattern)
             (string-match-p pattern file))
           ollama-buddy-supported-file-types))

(defun ollama-buddy--read-file-safely (file)
  "Read FILE content safely with size and encoding checks."
  (when (file-exists-p file)
    (let ((size (file-attribute-size (file-attributes file))))
      (when (> size ollama-buddy-max-file-size)
        (user-error "File too large: %s (max %d bytes)" file ollama-buddy-max-file-size))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (condition-case err
              (insert-file-contents file)
            (error
             (error "Failed to read file %s: %s" file (error-message-string err)))))
        (list :content (buffer-string)
              :size size
              :type (file-name-extension file))))))

(defun ollama-buddy-attach-file (file)
  "Attach FILE to the current conversation."
  (interactive "fAttach file: ")
  (unless (ollama-buddy--is-supported-file-type file)
    (unless (y-or-n-p "This file type may not be well-supported. Attach anyway? ")
      (user-error "File attachment cancelled")))
  
  (let* ((file-info (ollama-buddy--read-file-safely file))
         (attachment (list :file (expand-file-name file)
                           :content (plist-get file-info :content)
                           :size (plist-get file-info :size)
                           :type (plist-get file-info :type)
                           :attachment-time (current-time))))
    
    ;; Check if already attached
    (when (cl-find file ollama-buddy--current-attachments
                   :test #'string= :key (lambda (a) (plist-get a :file)))
      (user-error "File already attached: %s" file))
    
    ;; Add to current attachments
    (push attachment ollama-buddy--current-attachments)
    
    ;; Add to attachment history
    (push attachment ollama-buddy--attachment-history)

    (let ((start (point)))
      ;; Update status and display
      (ollama-buddy--update-status 
       (format "Attached: %s (%d files total)" 
               (file-name-nondirectory file)
               (length ollama-buddy--current-attachments)))
      
      ;; Show attachment in chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "\n\n- Attached: [[file:%s][%s]] (%d bytes)"
                          file
                          (file-name-nondirectory file)
                          (plist-get attachment :size)))
          (goto-char start)
          (while (re-search-forward "\n\n\n+" nil t)
            (replace-match "\n\n")))))
    
    (message "File attached: %s" file)))

;; Inline @file() support

(defconst ollama-buddy--file-inline-regexp
  "@file(\\([^)]+\\))"
  "Regexp to match inline file attachment delimiters: @file(path).")

(defun ollama-buddy--file-extract-inline-paths (text)
  "Extract all inline file paths from TEXT.
Returns list of path strings found in @file(path) delimiters."
  (let ((paths nil)
        (start 0))
    (while (string-match ollama-buddy--file-inline-regexp text start)
      (push (string-trim (match-string 1 text)) paths)
      (setq start (match-end 0)))
    (nreverse paths)))

;; Inline @skill() support

(defconst ollama-buddy--skills-inline-regexp
  "@skill(\\([^)]+\\))"
  "Regexp to match inline skill delimiters: @skill(name).")

(defun ollama-buddy--skills-process-inline (text)
  "Process TEXT for inline @skill(name) patterns.
Replaces @skill(name) with the actual skill content from user prompts."
  (let ((result text)
        (start 0)
        (prompts (ollama-buddy-user-prompts--get-prompts)))
    (while (string-match ollama-buddy--skills-inline-regexp result start)
      (let* ((name (match-string 1 result))
             ;; Look for prompt in 'skills' category first, then any category
             (skill (or (cl-find-if (lambda (p) 
                                     (and (string= (plist-get p :category) "skills")
                                          (string= (plist-get p :title) name)))
                                   prompts)
                        (cl-find name prompts :test #'string= :key (lambda (p) (plist-get p :title)))))
             (content (if skill 
                          (ollama-buddy-user-prompts--strip-org-headers 
                           (ollama-buddy-user-prompts--read-prompt-content (plist-get skill :file)))
                        (format "[Skill '%s' not found]" name))))
        (setq result (replace-match content t t result))
        (setq start (+ (match-beginning 0) (length content)))))
    result))

(defun ollama-buddy--file-remove-inline-delimiters (text)
  "Replace inline file delimiters with just the path text.
@file(path) becomes path, preserving the path in the prompt."
  (replace-regexp-in-string ollama-buddy--file-inline-regexp "\\1" text))

(defun ollama-buddy--file-process-inline (text)
  "Process TEXT for inline @file(path) patterns.
Extracts paths, attaches files to the conversation context.
Returns the text with @file() delimiters removed."
  (let ((paths (ollama-buddy--file-extract-inline-paths text)))
    (when paths
      (dolist (path paths)
        (let ((expanded (expand-file-name path)))
          (cond
           ((not (file-exists-p expanded))
            (message "Inline @file: file not found: %s" expanded))
           ((cl-find expanded ollama-buddy--current-attachments
                     :test #'string= :key (lambda (a) (plist-get a :file)))
            (message "Inline @file: already attached: %s" (file-name-nondirectory expanded)))
           (t
            (message "Inline @file: attaching %s" (file-name-nondirectory expanded))
            (let* ((file-info (ollama-buddy--read-file-safely expanded))
                   (attachment (list :file expanded
                                     :content (plist-get file-info :content)
                                     :size (plist-get file-info :size)
                                     :type (plist-get file-info :type)
                                     :attachment-time (current-time))))
              (push attachment ollama-buddy--current-attachments)
              (push attachment ollama-buddy--attachment-history)
              (message "Attached: %s (%d bytes, %d files total)"
                       (file-name-nondirectory expanded)
                       (plist-get file-info :size)
                       (length ollama-buddy--current-attachments)))))))))
  ;; Return text with delimiters removed
  (ollama-buddy--file-remove-inline-delimiters text))

(defun ollama-buddy-show-attachments ()
  "Display currently attached files and web searches."
  (interactive)
  (let ((has-files ollama-buddy--current-attachments)
        (has-searches (and (featurep 'ollama-buddy-web-search)
                           (boundp 'ollama-buddy-web-search--current-results)
                           ollama-buddy-web-search--current-results))
        (has-rag (ollama-buddy-rag-attached-p)))
    (if (and (null has-files) (null has-searches) (null has-rag))
        (message "No files, web searches, or RAG context attached to current conversation")
      (let ((buf (get-buffer-create "*Ollama Attachments*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (org-mode)
            (setq-local org-hide-emphasis-markers t)
            (setq-local org-hide-leading-stars t)
            (erase-buffer)

            (insert "#+title: Current Context Attachments\n\n")

            ;; File attachments section
            (when has-files
              (insert (format "* File Attachments (%d)\n" (length ollama-buddy--current-attachments)))
              (dolist (attachment ollama-buddy--current-attachments)
                (let ((file (plist-get attachment :file))
                      (size (plist-get attachment :size))
                      (type (plist-get attachment :type)))
                  (insert (format "\n** %s\n" (file-name-nondirectory file)))
                  (insert (format ":PROPERTIES:\n:PATH: %s\n:SIZE: %d bytes\n:TYPE: %s\n:END:\n"
                                  file size (or type "unknown")))
                  (insert "#+begin_example\n")
                  (insert (replace-regexp-in-string
                           "^\\*" ",*"
                           (plist-get attachment :content)))
                  (insert "\n#+end_example\n"))))

            ;; Web search attachments section
            (when has-searches
              (insert (format "\n* Web Searches (%d)\n" (length ollama-buddy-web-search--current-results)))
              (dolist (search ollama-buddy-web-search--current-results)
                (let ((query (plist-get search :query))
                      (results (plist-get search :results))
                      (content-map (plist-get search :content-map))
                      (tokens (plist-get search :tokens))
                      (size (plist-get search :size))
                      (timestamp (plist-get search :timestamp)))
                  (insert (format "\n** %s\n" query))
                  (insert (format ":PROPERTIES:\n:RESULTS: %d\n:TOKENS: ~%d\n:SIZE: %d bytes\n:TIME: %s\n:END:\n"
                                  (length results) tokens (or size 0)
                                  (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
                  ;; Show each result as a sub-heading with full content
                  (let ((idx 0))
                    (dolist (result results)
                      (cl-incf idx)
                      (let* ((title (or (alist-get 'title result) "Untitled"))
                             (url (or (alist-get 'url result) (alist-get 'link result) ""))
                             ;; Try content-map first (eww mode), then API content directly
                             (content (or (when content-map (cdr (assoc url content-map)))
                                          (when (fboundp 'ollama-buddy-web-search--get-api-content)
                                            (ollama-buddy-web-search--get-api-content result)))))
                        (insert (format "\n*** %d. %s\n" idx title))
                        (when (not (string-empty-p url))
                          (insert (format ":PROPERTIES:\n:URL: %s\n:END:\n" url)))
                        (when (and content (not (string-empty-p content)))
                          (insert "#+begin_example\n")
                          (insert (if (fboundp 'ollama-buddy-web-search--org-escape)
                                      (ollama-buddy-web-search--org-escape content)
                                    content))
                          (insert "\n#+end_example\n"))))))))

            ;; RAG attachments section
            (when has-rag
              (let ((rag-results (symbol-value 'ollama-buddy-rag--current-results)))
                (insert (format "\n* RAG Context (%d searches, ~%d tokens)\n"
                                (length rag-results)
                                (ollama-buddy-rag-total-tokens)))
                (dolist (result rag-results)
                  (let ((query (plist-get result :query))
                        (index-name (plist-get result :index-name))
                        (matches (plist-get result :results))
                        (tokens (plist-get result :tokens))
                        (timestamp (plist-get result :timestamp)))
                    (insert (format "\n** \"%s\" (from %s)\n" query index-name))
                    (insert (format ":PROPERTIES:\n:RESULTS: %d\n:TOKENS: ~%d\n:TIME: %s\n:END:\n"
                                    (length matches) (or tokens 0)
                                    (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
                    (dolist (match matches)
                      (let ((file (plist-get match :file))
                            (score (plist-get match :score))
                            (line-start (plist-get match :line-start))
                            (line-end (plist-get match :line-end))
                            (content (plist-get match :content)))
                        (insert (format "\n*** %s:%d-%d (%.2f)\n"
                                        (file-name-nondirectory (or file "unknown"))
                                        (or line-start 0) (or line-end 0)
                                        (or score 0.0)))
                        (insert "#+begin_example\n")
                        (insert (replace-regexp-in-string
                                 "^\\*" ",*"
                                 (truncate-string-to-width (or content "") 1000 nil nil "...")))
                        (insert "\n#+end_example\n")))))))

            (goto-char (point-min))
            (view-mode 1)
            (org-content)))
        (display-buffer buf)))))

(defun ollama-buddy-detach-file (file)
  "Remove FILE from current attachments."
  (interactive
   (list (completing-read "Detach file: "
                          (mapcar (lambda (a) (plist-get a :file))
                                  ollama-buddy--current-attachments)
                          nil t)))
  (setq ollama-buddy--current-attachments
        (cl-remove file ollama-buddy--current-attachments
                   :test #'string= :key (lambda (a) (plist-get a :file))))
  (ollama-buddy--update-status 
   (format "Detached: %s (%d files remaining)" 
           (file-name-nondirectory file)
           (length ollama-buddy--current-attachments)))
  (message "File detached: %s" file))

(defun ollama-buddy-clear-attachments ()
  "Clear all current attachments including file attachments, web searches, and RAG."
  (interactive)
  (let ((has-attachments ollama-buddy--current-attachments)
        (has-web-search (and (featurep 'ollama-buddy-web-search)
                             (boundp 'ollama-buddy-web-search--current-results)
                             ollama-buddy-web-search--current-results))
        (has-rag (> (ollama-buddy-rag-count) 0)))
    (when (or (and (not has-attachments) (not has-web-search) (not has-rag))
              (yes-or-no-p "Clear all attachments, web searches, and RAG context? "))
      (setq ollama-buddy--current-attachments nil)
      (when (boundp 'ollama-buddy-web-search--current-results)
        (setq ollama-buddy-web-search--current-results nil))
      (ollama-buddy-rag-clear-attached)
      (ollama-buddy--update-status "All attachments cleared")
      (message "All attachments cleared"))))

(defalias 'ollama-buddy-clear-all-context 'ollama-buddy-clear-attachments
  "Alias for `ollama-buddy-clear-attachments'.")

;; dired integration

(defun ollama-buddy-dired-attach-marked-files ()
  "Attach all marked files in current Dired buffer to Ollama chat."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (user-error "This command only works in Dired mode"))
  
  (let ((marked-files (dired-get-marked-files)))
    (if (null marked-files)
        (message "No files marked")
      (dolist (file marked-files)
        (when (file-regular-p file)
          (condition-case err
              (ollama-buddy-attach-file file)
            (error (message "Failed to attach %s: %s" 
                            file (error-message-string err))))))
      (message "Attached %d files to Ollama chat" 
               (length (cl-remove-if-not #'file-regular-p marked-files))))))

(defvar ollama-buddy-mode-map
  (let ((map (make-sparse-keymap)))
    
    ;; Primary Transient Menu access
    (define-key map (kbd "C-c O") #'ollama-buddy-transient-menu)
    ;; Convenient access to transient menu from chat buffer
    (define-key map (kbd "C-c .") #'ollama-buddy-transient-menu)
    (define-key map (kbd "C-c M") #'ollama-buddy-manage-models)
    (define-key map (kbd "C-c ?") #'ollama-buddy-open-info)
    (define-key map (kbd "C-c C-u") #'ollama-buddy-unload-all-models)
    (define-key map (kbd "C-c a") #'ollama-buddy-transient-attachment-menu)
    (define-key map (kbd "C-c P") #'ollama-buddy-transient-project-menu)
    (define-key map (kbd "C-c A") #'ollama-buddy-transient-auth-menu)
    
    ;; Chat section keybindings from transient
    (define-key map (kbd "C-c C-c") #'ollama-buddy--send-prompt)
    (define-key map (kbd "C-c RET") #'ollama-buddy--send-prompt)
    (define-key map (kbd "C-c h") #'ollama-buddy--menu-help-assistant)
    (define-key map (kbd "C-c C-k") #'ollama-buddy--cancel-request)
    (define-key map (kbd "C-c x") #'ollama-buddy-toggle-streaming)
    (define-key map (kbd "C-c !") #'ollama-buddy-toggle-airplane-mode)
    ;; Prompts section keybindings
    (define-key map (kbd "C-c l") #'ollama-buddy-pull-model)
    (define-key map (kbd "C-c s") #'ollama-buddy-transient-user-prompts-menu)
    (define-key map (kbd "C-c C-s") #'ollama-buddy-show-system-prompt-info)
    (define-key map (kbd "C-c C-r") #'ollama-buddy-reset-system-prompt)
    (define-key map (kbd "C-c y") #'ollama-buddy-transient-system-prompts-menu)

    ;; Model section keybindings
    (define-key map (kbd "C-c m") #'ollama-buddy--swap-model)
    (define-key map (kbd "C-c i") #'ollama-buddy-show-raw-model-info)
    (define-key map (kbd "C-c U") #'ollama-buddy--multishot-prompt)

    ;; Roles & Patterns keybindings
    (define-key map (kbd "C-c E") #'ollama-buddy-tools-toggle-auto-execute)
    (define-key map (kbd "C-c D") #'ollama-buddy-roles-open-directory)
    ;; Tools keybindings
    (define-key map (kbd "C-c SPC") #'ollama-buddy-tools-toggle)
    (define-key map (kbd "C-c G") #'ollama-buddy-tools-toggle-unguarded)
    (define-key map (kbd "C-c Q") #'ollama-buddy-tools-info)
    
    ;; RAG keybindings
    (define-key map (kbd "C-c r") #'ollama-buddy-transient-rag-menu)
    
    ;; Display Options keybindings
    (define-key map (kbd "C-c +") #'ollama-buddy-transient-settings-menu)
    (define-key map (kbd "C-c B") #'ollama-buddy-toggle-debug-mode)
    (define-key map (kbd "C-c >") #'ollama-buddy-toggle-show-history-indicator)
    (define-key map (kbd "C-c #") #'ollama-buddy-display-token-stats)
    (define-key map (kbd "C-c C-o") #'ollama-buddy-toggle-markdown-conversion)
    (define-key map (kbd "C-c <") #'ollama-buddy-toggle-global-system-prompt)
    (define-key map (kbd "C-c ~") #'ollama-buddy-set-tone)

    ;; History keybindings
    (define-key map (kbd "C-c J") #'ollama-buddy-toggle-history)
    (define-key map (kbd "C-c X") #'ollama-buddy-clear-history)
    (define-key map (kbd "C-c H") #'ollama-buddy-history-edit-model)
    (define-key map (kbd "C-c Y") #'ollama-buddy-set-max-history-length)
    (define-key map (kbd "M-p") #'ollama-buddy-previous-history)
    (define-key map (kbd "M-n") #'ollama-buddy-next-history)
    (define-key map (kbd "M-r") #'ollama-buddy-history-search)
    
    ;; Session keybindings
    (define-key map (kbd "C-c N") #'ollama-buddy-sessions-new)
    (define-key map (kbd "C-c f") #'ollama-buddy-sessions-load)
    (define-key map (kbd "C-c S") #'ollama-buddy-sessions-save)
    (define-key map (kbd "C-c w") #'ollama-buddy-sessions-rename)
    (define-key map (kbd "C-c L") #'ollama-buddy-recommended-models)
    (define-key map (kbd "C-c Z") #'ollama-buddy-sessions-directory)
    
    ;; Parameter keybindings
    (define-key map (kbd "C-c p") #'ollama-buddy-transient-parameter-menu)
    (define-key map (kbd "C-c t") #'ollama-buddy-params-display)

    ;; Context keybindings
    (define-key map (kbd "C-c $") #'ollama-buddy-set-model-context-size)
    (define-key map (kbd "C-c %") #'ollama-buddy-toggle-context-percentage)
    (define-key map (kbd "C-c C") #'ollama-buddy-show-context-info)
    (define-key map (kbd "C-c &") #'ollama-buddy-toggle-context-display-type)

    ;; file attachments
    (define-key map (kbd "C-c C-a") #'ollama-buddy-attach-file)
    (define-key map (kbd "C-c C-d") #'ollama-buddy-detach-file)
    (define-key map (kbd "C-c 0") #'ollama-buddy-clear-attachments)

    ;; annotate project
    (define-key map (kbd "C-c A") #'ollama-buddy-annotate-apply-last-response)

    ;; web search
    (define-key map (kbd "C-c /") #'ollama-buddy-transient-web-search-menu)

    (define-key map (kbd "C-c g") #'ollama-buddy-toggle-auto-scroll)
    (define-key map (kbd "C-c e") #'ollama-buddy-switch-communication-backend)
    (define-key map (kbd "C-c K") #'ollama-buddy-exit)

    (define-key map [remap move-beginning-of-line] #'ollama-buddy-beginning-of-prompt)
    (define-key map "@" #'ollama-buddy--at-complete)
    (define-key map "/" #'ollama-buddy--slash-complete)
    map)
  "Keymap for ollama-buddy mode.")

;;;###autoload
(define-minor-mode ollama-buddy-mode
  "Minor mode for ollama-buddy keybindings."
  :lighter " OB"
  :keymap ollama-buddy-mode-map
  (when ollama-buddy-mode
    (setq-local imenu-create-index-function #'ollama-buddy--imenu-create-index)
    (setq-local imenu-auto-rescan t)
    (setq-local imenu-sort-function nil)
    (setq-local org-refile-targets '((nil :maxlevel . 2)))
    (setq-local org-goto-interface 'outline-path-completion)))

(defun ollama-buddy--file-truename-safe (orig-fun file &rest args)
  "Advice around `file-truename' to handle nil FILE in non-file buffers.
`org-goto' calls `file-truename' on nil in buffers without a file name."
  (if file (apply orig-fun file args) nil))

(advice-add 'file-truename :around #'ollama-buddy--file-truename-safe)

(push 'ollama-buddy--prompt-history savehist-additional-variables)

(defun ollama-buddy-unload-function ()
  "Clean up when `ollama-buddy' is unloaded."
  (advice-remove 'file-truename #'ollama-buddy--file-truename-safe)
  (setq savehist-additional-variables
        (delq 'ollama-buddy--prompt-history savehist-additional-variables))
  nil)

(provide 'ollama-buddy)
;;; ollama-buddy.el ends here
