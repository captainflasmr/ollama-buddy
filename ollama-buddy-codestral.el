;;; ollama-buddy-codestral.el --- Mistral Codestral integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Mistral Codestral integration for the ollama-buddy package.
;; It allows users to interact with Mistral's Codestral language models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based Mistral Codestral model.

;;; Usage:
;;
;; (require 'ollama-buddy-codestral)
;; (setq ollama-buddy-codestral-api-key (getenv "CODESTRAL_API_KEY"))

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

;; Forward declarations
(declare-function ollama-buddy-escape-unicode "ollama-buddy-core")
(declare-function ollama-buddy--create-intro-message "ollama-buddy-core")
(declare-function ollama-buddy--update-status "ollama-buddy-core")
(declare-function ollama-buddy--add-to-history "ollama-buddy-core")
(declare-function ollama-buddy--prepare-prompt-area "ollama-buddy-core")
(declare-function ollama-buddy--md-to-org-convert-region "ollama-buddy-core")
(declare-function ollama-buddy-register-model-handler "ollama-buddy-core")

(defgroup ollama-buddy-codestral nil
  "Mistral Codestral integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-codestral-")

(defcustom ollama-buddy-codestral-marker-prefix "s:"
  "Prefix to indicate that a model is from Mistral Codestral rather than Ollama."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-api-key ""
  "API key for accessing Mistral Codestral services."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-default-model "codestral-latest"
  "Default Mistral Codestral model to use."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-api-endpoint "https://api.mistral.ai/v1/chat/completions"
  "Endpoint for Mistral Codestral API."
  :type 'string
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-temperature 0.7
  "Temperature setting for Mistral Codestral requests (0.0-2.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-codestral)

(defcustom ollama-buddy-codestral-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-codestral)

;; Internal variables

(defvar ollama-buddy-codestral--current-token-count 0
  "Counter for tokens in the current Mistral Codestral response.")

(defvar ollama-buddy-remote-models nil
  "List of available remote models.")

;; Helper functions

(defun ollama-buddy-codestral--is-codestral-model (model)
  "Check if MODEL is a Mistral Codestral model based on prefix."
  (and model
       (string-match-p (concat "^" (regexp-quote ollama-buddy-codestral-marker-prefix)) model)))

(defun ollama-buddy-codestral--get-full-model-name (model)
  "Get the full model name with prefix for MODEL."
  (concat ollama-buddy-codestral-marker-prefix model))

(defun ollama-buddy-codestral--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (if (ollama-buddy-codestral--is-codestral-model model)
      (string-trim (substring model (length ollama-buddy-codestral-marker-prefix)))
    model))

;; API interaction functions

(defun ollama-buddy-codestral--verify-api-key ()
  "Verify that the API key is set."
  (if (string-empty-p ollama-buddy-codestral-api-key)
      (progn
        (customize-variable 'ollama-buddy-codestral-api-key)
        (error "Please set your Mistral Codestral API key"))
    t))

(defun ollama-buddy-codestral--send (prompt &optional model)
  "Send PROMPT to Mistral Codestral's API using MODEL or default model asynchronously."
  (when (ollama-buddy-codestral--verify-api-key)
    ;; Set up the current model
    (setq ollama-buddy--current-model
          (or model
              ollama-buddy--current-model
              (ollama-buddy-codestral--get-full-model-name
               ollama-buddy-codestral-default-model)))

    ;; Initialize token counter
    (setq ollama-buddy-codestral--current-token-count 0)

    ;; Get history and system prompt
    (let* ((history (when ollama-buddy-history-enabled
                      (gethash ollama-buddy--current-model
                               ollama-buddy--conversation-history-by-model
                               nil)))
           (system-prompt ollama-buddy--current-system-prompt)
           (attachment-context
            (when ollama-buddy--current-attachments
              (concat "\n\n## Attached Files Context:\n\n"
                      (mapconcat
                       (lambda (attachment)
                         (let ((file (plist-get attachment :file))
                               (content (plist-get attachment :content)))
                           (format "### File: %s\n\n#+end_src%s\n%s\n#+begin_src \n\n"
                                   (file-name-nondirectory file)
                                   (or (plist-get attachment :type) "")
                                   content)))
                       ollama-buddy--current-attachments
                       ""))))
           (messages (vconcat []
                              (append
                               (when (and system-prompt (not (string-empty-p system-prompt)))
                                 `(((role . "system") (content . ,system-prompt))))
                               history
                               `(((role . "user")
                                  (content . ,(if attachment-context
                                                  (concat prompt attachment-context)
                                                prompt)))))))
           (max-tokens (or ollama-buddy-codestral-max-tokens 4096))
           (json-payload
            `((model . ,(ollama-buddy-codestral--get-real-model-name
                         ollama-buddy--current-model))
              (messages . ,messages)
              (temperature . ,ollama-buddy-codestral-temperature)
              (max_tokens . ,max-tokens)))
           (json-str (let ((json-encoding-pretty-print nil))
                       (ollama-buddy-escape-unicode (json-encode json-payload)))))

      ;; Prepare chat buffer
      (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
        (pop-to-buffer (current-buffer))
        (goto-char (point-max))

        (unless (> (buffer-size) 0)
          (insert (ollama-buddy--create-intro-message)))

        ;; Show any attached files
        (when ollama-buddy--current-attachments
          (insert (format "\n\n[Including %d attached file(s) in context]"
                          (length ollama-buddy--current-attachments))))

        (let (start-point
              (inhibit-read-only t))

          (insert (format "\n\n** [%s: RESPONSE]\n\n" ollama-buddy--current-model))

          (setq start-point (point))

          (insert "Loading response...")
          (ollama-buddy--update-status "Sending request to Mistral Codestral...")

          (set-register ollama-buddy-default-register "")

          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " ollama-buddy-codestral-api-key))))
                 ;; Set JSON as raw data - let url library handle encoding
                 (url-request-data json-str))

            ;; Setting this makes url.el use binary for the request
            (let ((url-mime-charset-string "utf-8")
                  (url-mime-language-string nil)
                  (url-mime-encoding-string nil)
                  (url-mime-accept-string "application/json"))

              (url-retrieve
               ollama-buddy-codestral-api-endpoint
               (lambda (status)
                 (if (plist-get status :error)
                     (progn
                       (with-current-buffer ollama-buddy--chat-buffer
                         (let ((inhibit-read-only t))
                           (goto-char start-point)
                           (delete-region start-point (point-max))
                           (insert "Error: URL retrieval failed\n")
                           (insert "Details: " (prin1-to-string (plist-get status :error)) "\n")
                           (insert "\n\n*** FAILED")
                           (ollama-buddy--prepare-prompt-area)
                           (ollama-buddy--update-status "Failed - URL retrieval error"))))

                   ;; Success - process the response
                   (progn
                     (goto-char (point-min))
                     (when (re-search-forward "\n\n" nil t)
                       (let* ((json-response-raw (buffer-substring (point) (point-max)))
                              (json-response-decoded (decode-coding-string json-response-raw 'utf-8))
                              (json-object-type 'alist)
                              (json-array-type 'vector)
                              (json-key-type 'symbol))

                         (condition-case err
                             (let* ((json-response (json-read-from-string json-response-decoded))
                                    (error-message (alist-get 'error json-response))
                                    (content "")
                                    (choices (alist-get 'choices json-response)))

                               ;; Extract the message content
                               (if error-message
                                   (setq content (format "Error: %s" (alist-get 'message error-message)))
                                 (when choices
                                   (setq content (alist-get 'content (alist-get 'message (aref choices 0))))))

                               ;; Update the chat buffer
                               (with-current-buffer ollama-buddy--chat-buffer
                                 (let ((inhibit-read-only t))
                                   (goto-char start-point)
                                   (delete-region start-point (point-max))

                                   ;; Insert the content
                                   (insert content)

                                   ;; Convert markdown to org if enabled
                                   (when ollama-buddy-convert-markdown-to-org
                                     (ollama-buddy--md-to-org-convert-region start-point (point-max)))

                                   ;; Write to register
                                   (let* ((reg-char ollama-buddy-default-register)
                                          (current (get-register reg-char))
                                          (new-content (concat (if (stringp current) current "") content)))
                                     (set-register reg-char new-content))

                                   ;; Add to history
                                   (when ollama-buddy-history-enabled
                                     (ollama-buddy--add-to-history "user" prompt)
                                     (ollama-buddy--add-to-history "assistant" content))

                                   ;; Calculate token count
                                   (setq ollama-buddy-codestral--current-token-count
                                         (length (split-string content "\\b" t)))

                                   ;; Show token stats if enabled
                                   (when ollama-buddy-display-token-stats
                                     (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                                     ollama-buddy-codestral--current-token-count)))

                                   (insert "\n\n*** FINISHED")
                                   (ollama-buddy--prepare-prompt-area)
                                   (ollama-buddy--update-status
                                    (format "Finished [%d tokens]"
                                            ollama-buddy-codestral--current-token-count)))))
                           (error
                            (with-current-buffer ollama-buddy--chat-buffer
                              (let ((inhibit-read-only t))
                                (goto-char start-point)
                                (delete-region start-point (point-max))
                                (insert "Error: Failed to parse Mistral Codestral response\n")
                                (insert "Details: " (error-message-string err) "\n")
                                (insert "\n\n*** FAILED")
                                (ollama-buddy--prepare-prompt-area)
                                (ollama-buddy--update-status "Failed - JSON parse error"))))))))))))))))))


(defun ollama-buddy-codestral--fetch-models ()
  "Fetch available models from Mistral Codestral API and register them with ollama-buddy."
  (let ((chat-models '("codestral-latest")))
    (let ((prefixed-models (mapcar (lambda (model-name)
                                     (concat ollama-buddy-codestral-marker-prefix model-name))
                                   chat-models)))
      (when (fboundp 'ollama-buddy-register-model-handler)
        (ollama-buddy-register-model-handler
         ollama-buddy-codestral-marker-prefix
         #'ollama-buddy-codestral--send))
      (setq ollama-buddy-remote-models (append ollama-buddy-remote-models prefixed-models)))))

(ollama-buddy-codestral--fetch-models)

(provide 'ollama-buddy-codestral)
;;; ollama-buddy-codestral.el ends here
