;;; ollama-buddy-copilot.el --- GitHub Copilot Chat integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides GitHub Copilot Chat integration for the ollama-buddy package.
;; It allows users to interact with GitHub Copilot models using the same interface
;; as ollama-buddy, providing seamless switching between local Ollama models and
;; cloud-based GitHub Copilot models.
;;
;; Requirements:
;; - Active GitHub Copilot subscription
;;
;; Setup:
;; 1. Run M-x ollama-buddy-copilot-login
;; 2. Follow the prompts to authenticate with GitHub
;; 3. Your OAuth token will be saved for future sessions
;;
;; Usage:
;; (require 'ollama-buddy-copilot)
;; Models will appear with the "p:" prefix (e.g., p:gpt-4o)

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

(defgroup ollama-buddy-copilot nil
  "GitHub Copilot Chat integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-copilot-")

(defcustom ollama-buddy-copilot-marker-prefix "p:"
  "Prefix to indicate that a model is from GitHub Copilot rather than Ollama."
  :type 'string
  :group 'ollama-buddy-copilot)

(defcustom ollama-buddy-copilot-token-file
  (expand-file-name "ollama-buddy-copilot-token.json" user-emacs-directory)
  "File to store the GitHub OAuth token for Copilot."
  :type 'file
  :group 'ollama-buddy-copilot)

;; GitHub OAuth App Client ID for Copilot (VS Code's client ID)
(defconst ollama-buddy-copilot--client-id "Iv1.b507a08c87ecfe98"
  "GitHub OAuth App Client ID for Copilot authentication.")

(defcustom ollama-buddy-copilot-default-model "gpt-4o"
  "Default GitHub Copilot model to use."
  :type 'string
  :group 'ollama-buddy-copilot)

(defcustom ollama-buddy-copilot-api-endpoint "https://api.githubcopilot.com/chat/completions"
  "Endpoint for GitHub Copilot chat completions API."
  :type 'string
  :group 'ollama-buddy-copilot)

(defcustom ollama-buddy-copilot-available-models
  '("gpt-4o" "gpt-4" "gpt-3.5-turbo")
  "List of available Copilot models.
These are the models available through GitHub Copilot Chat."
  :type '(repeat string)
  :group 'ollama-buddy-copilot)

(defcustom ollama-buddy-copilot-temperature 0.7
  "Temperature setting for Copilot requests (0.0-2.0).
Lower values make the output more deterministic, higher values more creative."
  :type 'float
  :group 'ollama-buddy-copilot)

(defcustom ollama-buddy-copilot-max-tokens nil
  "Maximum number of tokens to generate in the response.
Use nil for API default behavior (adaptive)."
  :type '(choice integer (const nil))
  :group 'ollama-buddy-copilot)

;; Internal variables

(defvar ollama-buddy-copilot--current-token-count 0
  "Counter for tokens in the current Copilot response.")

(defvar ollama-buddy-copilot--oauth-token nil
  "Cached GitHub OAuth token for Copilot.")

(defvar ollama-buddy-copilot--access-token nil
  "Cached Copilot access token obtained from OAuth token.")

(defvar ollama-buddy-copilot--token-expiry nil
  "Expiry time for the cached Copilot access token.")

(defvar ollama-buddy-copilot--device-code nil
  "Device code for pending authentication.")

(defvar ollama-buddy-copilot--poll-timer nil
  "Timer for polling during device flow authentication.")

;; Helper functions

(defun ollama-buddy-copilot--is-copilot-model (model)
  "Check if MODEL is a GitHub Copilot model based on prefix."
  (and model
       (string-prefix-p ollama-buddy-copilot-marker-prefix model)))

(defun ollama-buddy-copilot--get-full-model-name (model)
  "Get the full model name with prefix for MODEL."
  (concat ollama-buddy-copilot-marker-prefix model))

(defun ollama-buddy-copilot--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (if (ollama-buddy-copilot--is-copilot-model model)
      (string-trim (substring model (length ollama-buddy-copilot-marker-prefix)))
    model))

;; Token persistence functions

(defun ollama-buddy-copilot--save-oauth-token (token)
  "Save OAuth TOKEN to file for persistence."
  (let ((data (json-encode `((oauth_token . ,token)))))
    (with-temp-file ollama-buddy-copilot-token-file
      (insert data))
    (set-file-modes ollama-buddy-copilot-token-file #o600)))

(defun ollama-buddy-copilot--load-oauth-token ()
  "Load OAuth token from file if it exists."
  (when (file-exists-p ollama-buddy-copilot-token-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents ollama-buddy-copilot-token-file)
          (let* ((json-object-type 'alist)
                 (json-key-type 'symbol)
                 (data (json-read)))
            (alist-get 'oauth_token data)))
      (error nil))))

(defun ollama-buddy-copilot--ensure-oauth-token ()
  "Ensure we have an OAuth token, loading from file if needed."
  (unless ollama-buddy-copilot--oauth-token
    (setq ollama-buddy-copilot--oauth-token
          (ollama-buddy-copilot--load-oauth-token)))
  ollama-buddy-copilot--oauth-token)

;; Device flow authentication

(defun ollama-buddy-copilot-login ()
  "Start GitHub device flow authentication for Copilot."
  (interactive)
  (message "Starting GitHub Copilot authentication...")
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Accept" . "application/json")
            ("Content-Type" . "application/json")))
         (url-request-data
          (json-encode `((client_id . ,ollama-buddy-copilot--client-id)
                         (scope . "read:user")))))
    (url-retrieve
     "https://github.com/login/device/code"
     #'ollama-buddy-copilot--handle-device-code-response)))

(defun ollama-buddy-copilot--parse-response ()
  "Parse the HTTP response body, handling both JSON and form-urlencoded formats."
  (goto-char (point-min))
  ;; Skip past HTTP headers (look for blank line)
  (when (re-search-forward "^\r?\n" nil t)
    (let ((body (string-trim (buffer-substring-no-properties (point) (point-max)))))
      ;; Check if it's JSON (starts with {) or form-urlencoded
      (if (string-match-p "^{" body)
          ;; JSON response
          (let ((json-object-type 'alist)
                (json-key-type 'symbol))
            (json-read-from-string body))
        ;; Form-urlencoded response (parse key=value&key2=value2)
        (let ((pairs (split-string body "&"))
              (result nil))
          (dolist (pair pairs)
            (let ((trimmed (string-trim pair)))
              (when (string-match "\\([^=]+\\)=\\(.*\\)" trimmed)
                (push (cons (intern (match-string 1 trimmed))
                            (url-unhex-string (match-string 2 trimmed)))
                      result))))
          result)))))

(defun ollama-buddy-copilot--handle-device-code-response (status)
  "Handle the device code response from GitHub.
STATUS is the URL retrieval status."
  (if (plist-get status :error)
      (message "Failed to start authentication: %s"
               (prin1-to-string (plist-get status :error)))
    (let* ((response (ollama-buddy-copilot--parse-response))
           (device-code (alist-get 'device_code response))
           (user-code (alist-get 'user_code response))
           (verification-uri (alist-get 'verification_uri response))
           (interval-raw (alist-get 'interval response))
           (interval (if (stringp interval-raw)
                         (string-to-number interval-raw)
                       (or interval-raw 5))))

        (setq ollama-buddy-copilot--device-code device-code)

        ;; Show instructions to user
        (message "GitHub Copilot Authentication")
        (let ((buf (get-buffer-create "*Copilot Auth*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert "GitHub Copilot Authentication\n")
            (insert "==============================\n\n")
            (insert (format "1. Open: %s\n" verification-uri))
            (insert (format "2. Enter code: %s\n\n" user-code))
            (insert "Waiting for authentication...\n")
            (insert "(This buffer will close automatically when done)"))
          (pop-to-buffer buf))

        ;; Copy code to clipboard
        (kill-new user-code)
        (message "Code %s copied to clipboard. Opening browser..." user-code)

        ;; Try to open browser
        (browse-url verification-uri)

      ;; Start polling for token
      (ollama-buddy-copilot--start-polling interval))))

(defvar ollama-buddy-copilot--poll-interval 5
  "Polling interval in seconds.")

(defun ollama-buddy-copilot--start-polling (interval)
  "Start polling for OAuth token every INTERVAL seconds."
  ;; Use at least 6 seconds to avoid immediate rate limiting
  (setq ollama-buddy-copilot--poll-interval (max interval 6))
  (when ollama-buddy-copilot--poll-timer
    (cancel-timer ollama-buddy-copilot--poll-timer))
  ;; Schedule first poll
  (ollama-buddy-copilot--schedule-next-poll))

(defun ollama-buddy-copilot--schedule-next-poll ()
  "Schedule the next poll for OAuth token."
  (when ollama-buddy-copilot--device-code
    (setq ollama-buddy-copilot--poll-timer
          (run-at-time ollama-buddy-copilot--poll-interval nil
                       #'ollama-buddy-copilot--do-poll))))

(defun ollama-buddy-copilot--do-poll ()
  "Perform a poll and schedule the next one if still waiting."
  (ollama-buddy-copilot--poll-for-token))

(defun ollama-buddy-copilot--slow-down ()
  "Increase polling interval due to rate limiting."
  (setq ollama-buddy-copilot--poll-interval
        (+ ollama-buddy-copilot--poll-interval 5))
  (message "Rate limited, increasing interval to %d seconds..."
           ollama-buddy-copilot--poll-interval)
  (ollama-buddy-copilot--schedule-next-poll))

(defun ollama-buddy-copilot--poll-for-token ()
  "Poll GitHub for the OAuth access token."
  (condition-case err
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              '(("Accept" . "application/json")
                ("Content-Type" . "application/json")))
             (url-request-data
              (json-encode `((client_id . ,ollama-buddy-copilot--client-id)
                             (device_code . ,ollama-buddy-copilot--device-code)
                             (grant_type . "urn:ietf:params:oauth:grant-type:device_code")))))
        (url-retrieve
         "https://github.com/login/oauth/access_token"
         #'ollama-buddy-copilot--handle-poll-response
         nil t))
    (error
     (message "Error during polling: %s" (error-message-string err)))))

(defun ollama-buddy-copilot--handle-poll-response (status)
  "Handle the polling response from GitHub.
STATUS is the URL retrieval status."
  (condition-case err
      (progn
        (if (plist-get status :error)
            (progn
              (ollama-buddy-copilot--stop-polling)
              (message "Authentication failed: %s"
                       (prin1-to-string (plist-get status :error))))
          (let* ((response (ollama-buddy-copilot--parse-response))
                 (error-code (alist-get 'error response))
                 (error-desc (alist-get 'error_description response))
                 (access-token (alist-get 'access_token response)))

            (cond
             ;; Still waiting for user
             ((equal error-code "authorization_pending")
              (ollama-buddy-copilot--schedule-next-poll))

             ;; Rate limited - increase interval
             ((equal error-code "slow_down")
              (ollama-buddy-copilot--slow-down))

             ;; Token expired
             ((equal error-code "expired_token")
              (ollama-buddy-copilot--stop-polling)
              (message "Authentication expired. Please run M-x ollama-buddy-copilot-login again."))

             ;; User denied access
             ((equal error-code "access_denied")
              (ollama-buddy-copilot--stop-polling)
              (message "Authentication denied by user."))

             ;; Got the token!
             (access-token
              (ollama-buddy-copilot--stop-polling)
              (setq ollama-buddy-copilot--oauth-token access-token)
              (ollama-buddy-copilot--save-oauth-token access-token)
              (when (get-buffer "*Copilot Auth*")
                (kill-buffer "*Copilot Auth*"))
              (message "GitHub Copilot authentication successful! Token saved."))

             ;; Unknown error
             (error-code
              (ollama-buddy-copilot--stop-polling)
              (message "Authentication error: %s - %s" error-code (or error-desc "")))

             ;; No token and no error - unexpected response
             (t
              (ollama-buddy-copilot--stop-polling)
              (message "Unexpected response from GitHub: %S" response))))))
    (error
     (message "Error in poll response handler: %s" (error-message-string err)))))

(defun ollama-buddy-copilot--stop-polling ()
  "Stop polling for OAuth token."
  (when ollama-buddy-copilot--poll-timer
    (cancel-timer ollama-buddy-copilot--poll-timer)
    (setq ollama-buddy-copilot--poll-timer nil))
  (setq ollama-buddy-copilot--device-code nil))

(defun ollama-buddy-copilot-logout ()
  "Remove saved Copilot authentication."
  (interactive)
  (setq ollama-buddy-copilot--oauth-token nil)
  (setq ollama-buddy-copilot--access-token nil)
  (setq ollama-buddy-copilot--token-expiry nil)
  (when (file-exists-p ollama-buddy-copilot-token-file)
    (delete-file ollama-buddy-copilot-token-file))
  (message "Copilot authentication removed."))

(defun ollama-buddy-copilot-status ()
  "Check and display Copilot authentication status."
  (interactive)
  (if (ollama-buddy-copilot--ensure-oauth-token)
      (message "Copilot: Authenticated")
    (message "Copilot: Not authenticated. Run M-x ollama-buddy-copilot-login")))

;; API interaction functions

(defun ollama-buddy-copilot--get-access-token (callback)
  "Get Copilot access token using OAuth token, then call CALLBACK with it.
The token is cached until expiry."
  (let ((oauth-token (ollama-buddy-copilot--ensure-oauth-token)))
    (unless oauth-token
      (error "Not authenticated. Run M-x ollama-buddy-copilot-login first"))

    (if (and ollama-buddy-copilot--access-token
             ollama-buddy-copilot--token-expiry
             (time-less-p (current-time) ollama-buddy-copilot--token-expiry))
        ;; Use cached token
        (funcall callback ollama-buddy-copilot--access-token)
      ;; Fetch new token
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(concat "token " oauth-token))
                ("Accept" . "application/json")
                ("Editor-Version" . "Emacs/29.0")
                ("Editor-Plugin-Version" . "ollama-buddy/1.0.0")
                ("User-Agent" . "ollama-buddy"))))
        (url-retrieve
         "https://api.github.com/copilot_internal/v2/token"
         (lambda (status)
           (if (plist-get status :error)
               (progn
                 ;; Token might be invalid, clear it
                 (setq ollama-buddy-copilot--oauth-token nil)
                 (error "Failed to get Copilot access token. Try M-x ollama-buddy-copilot-login"))
             (goto-char (point-min))
             (when (re-search-forward "\n\n" nil t)
               (let* ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol)
                      (response (json-read)))
                 (setq ollama-buddy-copilot--access-token (alist-get 'token response))
                 (let ((expires-at (alist-get 'expires_at response)))
                   (when expires-at
                     (setq ollama-buddy-copilot--token-expiry
                           (seconds-to-time expires-at))))
                 (funcall callback ollama-buddy-copilot--access-token))))))))))

(defun ollama-buddy-copilot--send (prompt &optional model)
  "Send PROMPT to GitHub Copilot API using MODEL or default model asynchronously."
  ;; Set up the current model
  (setq ollama-buddy--current-model
        (or model
            ollama-buddy--current-model
            (ollama-buddy-copilot--get-full-model-name
             ollama-buddy-copilot-default-model)))

  ;; Initialize token counter
  (setq ollama-buddy-copilot--current-token-count 0)

  ;; Get access token and then send request
  (ollama-buddy-copilot--get-access-token
   (lambda (access-token)
     (ollama-buddy-copilot--send-with-token prompt access-token))))

(defun ollama-buddy-copilot--send-with-token (prompt access-token)
  "Send PROMPT to Copilot API using ACCESS-TOKEN."
  (let* ((history (when ollama-buddy-history-enabled
                    (gethash ollama-buddy--current-model
                             ollama-buddy--conversation-history-by-model
                             nil)))
         (system-prompt (ollama-buddy--effective-system-prompt))
         (attachment-context
          (when ollama-buddy--current-attachments
            (concat "\n\n## Attached Files Context:\n\n"
                    (mapconcat
                     (lambda (attachment)
                       (let ((file (plist-get attachment :file))
                             (content (plist-get attachment :content)))
                         (format "### File: %s\n\n```%s\n%s\n```\n\n"
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
         (max-tokens (or ollama-buddy-copilot-max-tokens 4096))
         (json-payload
          `((model . ,(ollama-buddy-copilot--get-real-model-name
                       ollama-buddy--current-model))
            (messages . ,messages)
            (temperature . ,ollama-buddy-copilot-temperature)
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

      (let ((start-point nil)
            (inhibit-read-only t))

        (insert (format "\n\n** [%s: RESPONSE]\n\n" ollama-buddy--current-model))

        (setq start-point (point))

        (insert "Loading response...")
        (ollama-buddy--update-status "Sending request to GitHub Copilot...")

        (set-register ollama-buddy-default-register "")

        (let* ((url-request-method "POST")
               (url-request-extra-headers
                `(("Content-Type" . "application/json")
                  ("Authorization" . ,(concat "Bearer " access-token))
                  ("Editor-Version" . "vscode/1.85.0")
                  ("Editor-Plugin-Version" . "copilot-chat/0.12.0")
                  ("Openai-Organization" . "github-copilot")
                  ("Openai-Intent" . "conversation-panel")
                  ("User-Agent" . "GitHubCopilotChat/0.12.0")))
               (url-request-data json-str)
               (url-mime-charset-string "utf-8")
               (url-mime-language-string nil)
               (url-mime-encoding-string nil)
               (url-mime-accept-string "application/json"))

          (url-retrieve
           ollama-buddy-copilot-api-endpoint
           (lambda (status)
             (ollama-buddy-copilot--handle-response status start-point prompt))))))))

(defun ollama-buddy-copilot--handle-response (status start-point prompt)
  "Handle the Copilot API response.
STATUS is the URL retrieval status, START-POINT is where to insert,
PROMPT is the original prompt for history."
  (if (plist-get status :error)
      (let ((error-body ""))
        ;; Try to get the response body for more details
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (setq error-body (buffer-substring-no-properties (point) (point-max))))
        (with-current-buffer ollama-buddy--chat-buffer
          (let ((inhibit-read-only t))
            (goto-char start-point)
            (delete-region start-point (point-max))
            (insert "Error: URL retrieval failed\n")
            (insert "Details: " (prin1-to-string (plist-get status :error)) "\n")
            (when (> (length error-body) 0)
              (insert "Response: " error-body "\n"))
            (insert "\n\n*** FAILED")
            (ollama-buddy--prepare-prompt-area)
            (ollama-buddy--update-status "Failed - URL retrieval error"))))
    ;; Success - process the response
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
                  (setq ollama-buddy-copilot--current-token-count
                        (length (split-string content "\\b" t)))

                  ;; Show token stats if enabled
                  (when ollama-buddy-display-token-stats
                    (insert (format "\n\n*** Token Stats\n[%d tokens]"
                                    ollama-buddy-copilot--current-token-count)))

                  (insert "\n\n*** FINISHED")
                  (ollama-buddy--prepare-prompt-area)
                  (ollama-buddy--update-status
                   (format "Finished [%d tokens]"
                           ollama-buddy-copilot--current-token-count)))))
          (error
           (with-current-buffer ollama-buddy--chat-buffer
             (let ((inhibit-read-only t))
               (goto-char start-point)
               (delete-region start-point (point-max))
               (insert "Error: Failed to parse Copilot response\n")
               (insert "Details: " (error-message-string err) "\n")
               (insert "\n\n*** FAILED")
               (ollama-buddy--prepare-prompt-area)
               (ollama-buddy--update-status "Failed - JSON parse error")))))))))

(defun ollama-buddy-copilot--register-models ()
  "Register Copilot models with ollama-buddy."
  (let ((prefixed-models (mapcar (lambda (model-name)
                                   (concat ollama-buddy-copilot-marker-prefix model-name))
                                 ollama-buddy-copilot-available-models)))
    ;; Register the handler
    (when (fboundp 'ollama-buddy-register-model-handler)
      (ollama-buddy-register-model-handler
       ollama-buddy-copilot-marker-prefix
       #'ollama-buddy-copilot--send))
    ;; Add to remote models list
    (setq ollama-buddy-remote-models
          (append ollama-buddy-remote-models prefixed-models))
    (ollama-buddy--update-status "Copilot models registered")))

;; Register models when loaded
(ollama-buddy-copilot--register-models)

(provide 'ollama-buddy-copilot)
;;; ollama-buddy-copilot.el ends here
