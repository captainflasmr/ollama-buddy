;;; ollama-buddy-core.el --- Core functionality for ollama-buddy -*- lexical-binding: t; -*-

;; Author: YourName
;; Keywords: local, tools
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; This file contains core functionality, shared variables, and utility functions
;; for the ollama-buddy package, which provides an interface to the Ollama API.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url)
(require 'cl-lib)
(require 'dired)
(require 'org)
(require 'savehist)
(require 'pulse)

;; Core Customization Groups
(defgroup ollama-buddy nil
  "Customization group for Ollama Buddy."
  :group 'applications
  :prefix "ollama-buddy-")

;; Forward declarations for functions defined in ollama-buddy.el
(declare-function ollama-buddy--calculate-prompt-context-percentage "ollama-buddy")
(declare-function ollama-buddy--send "ollama-buddy")
(declare-function ollama-buddy--stream-sentinel "ollama-buddy")
(declare-function ollama-buddy--stream-filter "ollama-buddy")
(declare-function ollama-buddy--create-vision-message "ollama-buddy")
(declare-function ollama-buddy--detect-image-files "ollama-buddy")
(declare-function ollama-buddy--model-supports-vision "ollama-buddy")
(declare-function ollama-buddy--model-supports-tools "ollama-buddy")
(declare-function ollama-buddy--model-supports-thinking "ollama-buddy")
(declare-function ollama-buddy--check-context-before-send "ollama-buddy")
(declare-function ollama-buddy-curl--validate-executable "ollama-buddy-curl")
(declare-function ollama-buddy-curl--test-connection "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request-direct "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request "ollama-buddy-curl")
(declare-function ollama-buddy-curl--make-request-async "ollama-buddy-curl")
(declare-function ollama-buddy-curl--process-filter "ollama-buddy-curl")
(declare-function ollama-buddy-curl--process-json-line "ollama-buddy-curl")
(declare-function ollama-buddy-curl--handle-content "ollama-buddy-curl")
(declare-function ollama-buddy-curl--handle-completion "ollama-buddy-curl")
(declare-function ollama-buddy-curl--sentinel "ollama-buddy-curl")
(declare-function ollama-buddy-curl--send "ollama-buddy-curl")
(declare-function ollama-buddy-curl--non-streaming-sentinel "ollama-buddy-curl")
(declare-function ollama-buddy-curl-test "ollama-buddy-curl")

;; Web search forward declarations
(declare-function ollama-buddy-web-search-count "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-get-context "ollama-buddy-web-search")
(declare-function ollama-buddy-web-search-total-tokens "ollama-buddy-web-search")

;; RAG forward declarations
(declare-function ollama-buddy-rag-count "ollama-buddy-rag")
(declare-function ollama-buddy-rag-get-context "ollama-buddy-rag")
(declare-function ollama-buddy-rag-clear-attached "ollama-buddy-rag")

;; Completion forward declarations
(declare-function ollama-buddy-completion-mode "ollama-buddy-completion")
(declare-function ollama-buddy-completion-trigger "ollama-buddy-completion")
(declare-function ollama-buddy-completion-toggle "ollama-buddy-completion")

(defgroup ollama-buddy-params nil
  "Customization group for Ollama API parameters."
  :group 'ollama-buddy
  :prefix "ollama-buddy-param-")

(defcustom ollama-buddy-communication-backend 'network-process
  "Communication backend to use for Ollama API requests.
- `network-process': Use Emacs built-in network process (default)
- `curl': Use external curl command for requests"
  :type '(choice (const :tag "Network Process (built-in)" network-process)
                 (const :tag "Curl (external)" curl))
  :group 'ollama-buddy)

(defcustom ollama-buddy-ollama-executable "ollama"
  "Path to the ollama executable.
Used for CLI commands like signin and signout."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-curl-executable "curl"
  "Path to the curl executable.
Only used when `ollama-buddy-communication-backend' is set to `curl'."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-curl-timeout 300
  "Timeout in seconds for curl requests.
Only used when `ollama-buddy-communication-backend' is set to `curl'."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-max-file-size (* 10 1024 1024) ; 10MB
  "Maximum size for attached files in bytes."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-supported-file-types
  '("\\.txt$" "\\.md$" "\\.org$" "\\.py$" "\\.js$" "\\.el$" "\\.cpp$" "\\.c$"
    "\\.java$" "\\.json$" "\\.xml$" "\\.html$" "\\.css$" "\\.sh$" "\\.sql$"
    "\\.yaml$" "\\.yml$" "\\.toml$" "\\.ini$" "\\.cfg$")
  "List of regex patterns for supported file types."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-context-display-type 'bar
  "How to display context usage in the status bar."
  :type '(choice (const :tag "Text (numbers)" text)
                 (const :tag "Visual bar" bar))
  :group 'ollama-buddy)

(defcustom ollama-buddy-context-bar-width 10
  "Width of the context progress bar in characters."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-context-bar-chars '(?█ ?░)
  "Characters used to draw the context progress bar.
First character is for filled portion, second for empty portion."
  :type '(list character character)
  :group 'ollama-buddy)

(defcustom ollama-buddy-fallback-context-sizes
  '(("llama3.2:1b" . 2048)
    ("llama3:8b" . 4096)
    ("tinyllama" . 2048)
    ("phi3:3.8b" . 4096)
    ("gemma3:1b" . 4096)
    ("gemma3:4b" . 8192)
    ("llama3.2:3b" . 8192)
    ("llama3.2:8b" . 8192)
    ("llama3.2:70b" . 8192)
    ("starcoder2:3b" . 8192)
    ("starcoder2:7b" . 8192)
    ("starcoder2:15b" . 8192)
    ("mistral:7b" . 8192)
    ("mistral:8x7b" . 32768)
    ("codellama:7b" . 8192)
    ("codellama:13b" . 8192)
    ("codellama:34b" . 8192)
    ("qwen2.5-coder:7b" . 8192)
    ("qwen2.5-coder:3b" . 8192)
    ("qwen3:0.6b" . 4096)
    ("qwen3:1.7b" . 8192)
    ("qwen3:4b" . 8192)
    ("qwen3:8b" . 8192)
    ("deepseek-r1:7b" . 8192)
    ("deepseek-r1:1.5b" . 4096))
  "Mapping of model names to their default context sizes.
Used as a fallback when context size can't be determined from the API."
  :type '(alist :key-type string :value-type integer)
  :group 'ollama-buddy)

(defcustom ollama-buddy-show-context-percentage t
  "Whether to show context percentage in the status bar."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-header-line-height 1.1
  "Relative height of the header line text.
A value of 1.0 is normal size, 1.2 is 20% larger, etc."
  :type 'float
  :group 'ollama-buddy)

(defcustom ollama-buddy-context-size-thresholds '(0.85 1.0)
  "Thresholds for context usage warnings.
First value (0.85) is the amber threshold (approaching limit).
Second value (1.0) is the red threshold (at or exceeding limit)."
  :type '(list (float :tag "Amber threshold")
               (float :tag "Red threshold"))
  :group 'ollama-buddy)

(defcustom ollama-buddy-vision-enabled t
  "Whether to enable vision support for models that support it."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-vision-models '("gemma3:4b" "llama3.2:3b" "llama3.2:8b")
  "List of models known to support vision capabilities."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-tools-models
  '("qwen3" "qwen3:32b" "qwen3:14b" "qwen3:8b"
    "qwen3-coder-next" "qwen3-coder:480b"
    "deepseek-v3.1:671b" "gpt-oss:120b" "gpt-oss:20b"
    "glm-4.7" "glm-4.7-flash"
    "llama3.1" "llama3.3" "mistral" "mistral-nemo"
    "command-r+" "granite4")
  "List of models known to support tool calling."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-thinking-models
  '("deepseek-r1" "deepseek-r1:1.5b" "deepseek-r1:7b" "deepseek-r1:8b"
    "deepseek-r1:14b" "deepseek-r1:32b" "deepseek-r1:70b" "deepseek-r1:671b"
    "qwen3" "qwen3:0.6b" "qwen3:1.7b" "qwen3:4b" "qwen3:8b"
    "qwen3:14b" "qwen3:30b" "qwen3:32b" "qwen3:235b"
    "phi4-mini-reasoning" "phi4-reasoning"
    "marco-o1" "skyfall" "deepthink")
  "List of models known to support thinking/reasoning capabilities.
These models emit extended reasoning in <think>...</think> blocks.
Exact name matches only; see `ollama-buddy-thinking-model-patterns'
for substring/prefix-based heuristics.
Auto-detection via Ollama's /api/show capabilities array also
supplements this list."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-thinking-model-patterns
  '("deepseek" "reasoning" "qwq" "think")
  "List of substrings used as a heuristic fallback for thinking model detection.
If any string in this list appears anywhere in the model name
\(case-insensitive), the model is assumed to support thinking/reasoning.
This allows broad families like all DeepSeek models to be recognised
without listing every variant.
Takes effect before the /api/show capabilities cache is populated."
  :type '(repeat string)
  :group 'ollama-buddy)

(defvar ollama-buddy-airplane-mode nil
  "When non-nil, restrict ollama-buddy to local Ollama models only.
All cloud models, external providers (OpenAI, Claude, Gemini, etc.) and
web search are blocked to prevent unintended internet access and token usage.
Use `ollama-buddy-toggle-airplane-mode' to toggle.")

(defvar ollama-buddy-in-buffer-replace nil
  "When non-nil, commands that operate on a region stream their response
back into the source buffer instead of the chat buffer.
Toggle with `ollama-buddy-toggle-in-buffer-replace' or the transient menu.")

(defcustom ollama-buddy-image-formats '("\\.png$" "\\.jpg$" "\\.jpeg$" "\\.webp$" "\\.gif$")
  "List of regular expressions matching supported image file formats."
  :type '(repeat string)
  :group 'ollama-buddy)

(defcustom ollama-buddy-collapse-thinking t
  "When non-nil, wrap thinking blocks in a collapsible overlay after streaming.
Content streams in visibly, then collapses to a `[✦ Think ▶]' header when
`</think>' is received.  Toggle with `C-c V' or by pressing RET on the header.
When nil, `ollama-buddy-hide-reasoning' controls the behaviour instead."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-hide-reasoning nil
  "When non-nil, hide reasoning/thinking blocks from the stream output.
Has no effect when `ollama-buddy-collapse-thinking' is non-nil."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-reasoning-markers
  '(("<think>" . "</think>")
    ("---" . "---")
    ("<thinking>" . "</thinking>")
    ;; Common XML-style tags
    ("<reasoning>" . "</reasoning>")
    ("<cot>" . "</cot>")  ; chain-of-thought
    ("<scratch>" . "</scratch>")
    ("<workings>" . "</workings>")
    ("<calculation>" . "</calculation>")
    ("<process>" . "</process>")
    ("<analysis>" . "</analysis>")
    ("<reflection>" . "</reflection>")
    ;; Common markdown patterns
    ("```thinking" . "```")
    ("```reasoning" . "```")
    ("```internal" . "```")
    ("```cot" . "```")
    ;; ASCII-style delimiters
    ("***THINKING***" . "***END THINKING***")
    ("===REASONING===" . "===END REASONING===")
    ;; More verbose patterns
    ("Let me think step by step:" . "Therefore:")
    ("Internal reasoning:" . "Conclusion:"))
  "List of marker pairs that encapsulate reasoning/thinking sections.
Each element is a cons cell (START . END) with the start and end markers."
  :type '(repeat (cons (string :tag "Start marker")
                       (string :tag "End marker")))
  :group 'ollama-buddy)

;; Core customization options
(defcustom ollama-buddy-default-register ?a
  "Default register to store the current response when not in multishot mode."
  :type 'character
  :group 'ollama-buddy)

(defcustom ollama-buddy-streaming-enabled t
  "Whether to use streaming mode for responses.
When enabled, responses appear token by token in real time.
When disabled, responses only appear after completion."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-keepalive nil
  "How long Ollama keeps the model loaded in memory after a request.
Accepts a duration string such as \"5m\", \"10m\", \"1h\", \"0\" (unload
immediately after the response), or \"-1\" (keep loaded indefinitely).
When nil (the default) the parameter is omitted and Ollama uses its
own default of five minutes."
  :type '(choice (const :tag "Use Ollama default (5m)" nil)
                 (string :tag "Duration (e.g. \"5m\", \"1h\", \"0\", \"-1\")"))
  :group 'ollama-buddy)

(defcustom ollama-buddy-auto-scroll nil
  "Whether to auto-scroll the chat buffer during streaming output.
When non-nil, the buffer scrolls to follow new output if the
cursor was at the end of the buffer.
When nil (default), the cursor stays in place and you can
manually scroll to view new output."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-pulse-response t
  "Whether to pulse/flash the response text when streaming completes.
When non-nil (default), the response region is briefly highlighted
to indicate completion."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-goto-prompt-on-visible-completion t
  "Whether to move point to the prompt when the response is wholly visible.
When non-nil (default), after a response completes and if the entire
response is visible in the window, point moves to the new prompt position.
When nil, point stays in its original position regardless of visibility."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-default-model nil
  "Default Ollama model to use."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-debug-mode nil
  "When non-nil, show raw JSON messages in a debug buffer."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-full-welcome-enabled t
  "Showing the full welcome screen."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-show-params-in-header t
  "Whether to show modified parameters in the header line."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-params-modified nil
  "Set of parameters that have been explicitly modified by the user.
These are the only parameters that will be sent to Ollama."
  :type '(set symbol)
  :group 'ollama-buddy-params)

(defcustom ollama-buddy-params-defaults
  '((num_keep . 5)
    (seed . 42)
    (num_predict . 100)
    (top_k . 20)
    (top_p . 0.9)
    (min_p . 0.0)
    (typical_p . 0.7)
    (repeat_last_n . 33)
    (temperature . 0.8)
    (repeat_penalty . 1.2)
    (presence_penalty . 1.5)
    (frequency_penalty . 1.0)
    (mirostat . 1)
    (mirostat_tau . 0.8)
    (mirostat_eta . 0.6)
    (penalize_newline . t)
    (stop . ["\n" "user:"])
    (numa . nil)
    (num_ctx . 1024)
    (num_batch . 2)
    (num_gpu . 1)
    (main_gpu . 0)
    (low_vram . nil)
    (vocab_only . nil)
    (use_mmap . t)
    (use_mlock . nil)
    (num_thread . 8))
  "Default values for Ollama API parameters."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'ollama-buddy-params)

(defcustom ollama-buddy-command-definitions
  '(
    ;; General Commands
    (open-chat
     :key ?o
     :description "Open chat buffer"
     :group "General"
     :action ollama-buddy--open-chat)

    (send-region
     :key ?l
     :description "Send region"
     :group "General"
     :action (lambda ()
               (let* ((selected-text (when (use-region-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end)))))
                 (when (not selected-text)
                   (user-error "This command requires selected text"))

                 (ollama-buddy--open-chat)
                 (insert selected-text))))

    (switch-role
     :key ?R
     :description "Switch roles"
     :group "General"
     :action ollama-buddy-roles-switch-role)

    ;; Custom commands
    (refactor-code
     :key ?r
     :description "Refactor code"
     :group "Custom"
     :prompt "Return only the refactored version of the following code, with no explanation or commentary:"
     :system "You are an expert software engineer. Return ONLY the refactored code with no preamble, explanation, or commentary. Improve readability, maintainability, and efficiency by applying clean code principles and design patterns."
     :parameters ((temperature . 0.2) (top_p . 0.7) (repeat_penalty . 1.3))
     :action (lambda () (ollama-buddy--send-with-command 'refactor-code))
     :destination in-buffer)

    (git-commit
     :key ?g
     :description "Git commit message"
     :group "Custom"
     :prompt "Write a git commit message for the following, returning only the commit message text:"
     :system "You are a version control expert. Return ONLY the commit message text with no explanation or preamble. Use imperative mood, keep the summary under 50 characters, explain the what and why of changes, and reference issue numbers where applicable."
     :action (lambda () (ollama-buddy--send-with-command 'git-commit))
     :destination chat)

    (describe-code
     :key ?c
     :description "Describe code"
     :group "Custom"
     :prompt "Describe the following code, returning only the description with no preamble:"
     :system "You are a technical documentation specialist. Return ONLY the description — no introductory phrase, no preamble. Provide a high-level summary covering main components, control flow, notable patterns, and any complex parts explained in accessible language."
     :action (lambda () (ollama-buddy--send-with-command 'describe-code))
     :destination chat)

    (dictionary-lookup
     :key ?d
     :description "Dictionary Lookup"
     :group "Custom"
     :prompt "Provide a dictionary definition for the following word, returning only the entry:"
     :system "You are a professional lexicographer. Return ONLY the dictionary entry with no preamble. Include pronunciation, all relevant parts of speech, etymology, examples of usage, and related synonyms and antonyms."
     :action (lambda () (ollama-buddy--send-with-command 'dictionary-lookup))
     :destination chat)

    (synonym
     :key ?s
     :description "Word synonym"
     :group "Custom"
     :prompt "List synonyms for the following word, returning only the synonyms:"
     :system "You are a linguistic expert. Return ONLY a concise list of synonyms with no preamble or explanation. Group by connotation or formality where helpful."
     :action (lambda () (ollama-buddy--send-with-command 'synonym))
     :destination chat)

    (proofread
     :key ?p
     :description "Proofread text"
     :group "Custom"
     :prompt "Proofread the following text and return only the corrected version, with no explanations or extra text:"
     :system "You are a professional editor. Only return the corrected text with all grammar, spelling, punctuation, and style errors corrected. Do not include explanations, lists, or any extra commentary."
     :action (lambda () (ollama-buddy--send-with-command 'proofread))
     :destination in-buffer)

    ;; System Commands
    (custom-prompt
     :key ?e
     :description "Custom prompt"
     :group "System"
     :action ollama-buddy--menu-custom-prompt)

    (minibuffer-prompt
     :key ?i
     :description "Minibuffer Prompt"
     :group "System"
     :action ollama-buddy--menu-minibuffer-prompt))
  "Comprehensive command definitions for Ollama Buddy.
Each command is defined with:
  :key - Character for menu selection
  :description - String describing the action
  :model - Specific Ollama model to use (nil means use default)
  :prompt - Optional user prompt prefix
  :system - Optional system prompt/message
  :parameters - Association list of Ollama API parameters
  :action - Function to execute
  :group - Optional group name for transient menu column layout"
  :type '(repeat
          (list :tag "Command Definition"
                (symbol :tag "Command Name")
                (plist :inline t
                       :options
                       ((:key (character :tag "Menu Key Character"))
                        (:description (string :tag "Command Description"))
                        (:model (choice :tag "Specific Model"
                                        (const :tag "Use Default" nil)
                                        (string :tag "Model Name")))
                        (:prompt (string :tag "Static Prompt Text"))
                        (:system (string :tag "System Prompt/Message"))
                        (:parameters (alist :key-type symbol :value-type sexp))
                        (:action (choice :tag "Action"
                                         (function :tag "Existing Function")
                                         (sexp :tag "Lambda Expression")))
                        (:group (string :tag "Menu Group Name"))
                        (:destination (choice :tag "Response Destination"
                                              (const :tag "Honour global toggle" nil)
                                              (const :tag "Always chat buffer" chat)
                                              (const :tag "Always in-buffer replace" in-buffer)))))))
  :group 'ollama-buddy)

(defcustom ollama-buddy-params-active
  (copy-tree ollama-buddy-params-defaults)
  "Currently active values for Ollama API parameters."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'ollama-buddy-params)

(defcustom ollama-buddy-params-profiles
  '(("Default" . nil)
    ("Creative" . ((temperature . 1.0)
                   (top_p . 0.95)
                   (repeat_penalty . 1.0)))
    ("Precise" . ((temperature . 0.2)
                  (top_p . 0.5)
                  (repeat_penalty . 1.5))))
  "Predefined parameter profiles for different usage scenarios."
  :type '(alist :key-type string :value-type (alist :key-type symbol :value-type sexp))
  :group 'ollama-buddy-params)

(defcustom ollama-buddy-convert-markdown-to-org t
  "Whether to automatically convert markdown to `org-mode' format in responses."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-global-system-prompt
  "Format responses in plain prose. Never use markdown tables. Use clear paragraphs and bullet points for structured information."
  "Global system prompt prepended to all requests for consistent formatting.
This prompt is combined with any session-specific system prompt to provide
baseline formatting instructions across all models and providers.
Set to an empty string to disable without toggling the enabled flag."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-global-system-prompt-enabled t
  "When non-nil, prepend `ollama-buddy-global-system-prompt' to all requests.
The global prompt provides consistent formatting instructions and is
combined with session-specific prompts (personas, roles, etc.)."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-tone-alist
  '(("Normal" . "")
    ("Concise" . "Be concise and direct. Give short, focused answers without unnecessary elaboration.")
    ("Learning" . "Explain concepts thoroughly as if teaching. Include context, examples and analogies to aid understanding.")
    ("Explanatory" . "Provide detailed explanations with reasoning. Break down complex topics step by step.")
    ("Formal" . "Use a formal, professional tone. Be precise and structured in your responses.")
    ("In-Buffer" . "Return only the requested content. No preamble, no introduction, no closing remarks, no commentary. Begin your output immediately with the content itself."))
  "Alist mapping tone names to system prompt modifier strings.
Each entry is (NAME . PROMPT-TEXT).  The selected tone text is
prepended to the global system prompt.  An empty string means no
modification (the default \"Normal\" tone).
The \"In-Buffer\" tone is automatically applied when
`ollama-buddy-in-buffer-replace' is active."
  :type '(alist :key-type string :value-type string)
  :group 'ollama-buddy)

(defvar ollama-buddy--current-tone "Normal"
  "Currently active tone name from `ollama-buddy-tone-alist'.")

(defcustom ollama-buddy-sessions-directory
  (expand-file-name "ollama-buddy-sessions" user-emacs-directory)
  "Directory containing ollama-buddy session files."
  :type 'directory
  :group 'ollama-buddy)

(defcustom ollama-buddy-host "localhost"
  "Host where Ollama server is running."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-port 11434
  "Port where Ollama server is running."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-menu-columns 2
  "Number of columns to display in the Ollama Buddy menu."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-roles-directory
  (expand-file-name "ollama-buddy-presets" user-emacs-directory)
  "Directory containing ollama-buddy role preset files."
  :type 'directory
  :group 'ollama-buddy)

(defcustom ollama-buddy-history-enabled t
  "Whether to use conversation history in Ollama requests."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-max-history-length 10
  "Maximum number of message pairs to keep in conversation history."
  :type 'integer
  :group 'ollama-buddy)

(defcustom ollama-buddy-show-history-indicator nil
  "Whether to show the history indicator in the header line."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-display-token-stats nil
  "Whether to display token usage statistics in responses."
  :type 'boolean
  :group 'ollama-buddy)

(defcustom ollama-buddy-modelfile-directory
  (expand-file-name "ollama-buddy-modelfiles" user-emacs-directory)
  "Directory for storing temporary Modelfiles."
  :type 'directory
  :group 'ollama-buddy)

(defcustom ollama-buddy-available-models
  '((:name "General Chat"
           :description "Everyday conversation, Q&A and general tasks"
           :models ("llama3.2:1b" "llama3.2:3b" "llama3.1:8b"))
    (:name "Reasoning"
           :description "Step-by-step problem solving and analysis"
           :models ("deepseek-r1:1.5b" "deepseek-r1:7b" "deepseek-r1:14b"))
    (:name "Efficient & Capable"
           :description "Balanced speed and quality from Google Gemma"
           :models ("gemma3:1b" "gemma3:4b" "gemma3:12b"))
    (:name "Coding"
           :description "Code generation, review and debugging"
           :models ("qwen2.5-coder:1.5b" "qwen2.5-coder:7b" "qwen3-coder:8b" "starcoder2:3b"))
    (:name "General Alternatives"
           :description "Other popular and versatile models"
           :models ("mistral:7b" "qwen3:4b" "qwen3:8b"))
    (:name "Embedding (RAG)"
           :description "Models for generating embeddings used by RAG search"
           :models ("nomic-embed-text" "mxbai-embed-large" "all-minilm"))
    (:name "Testing"
           :description "Tiny models for fast local testing - speed over accuracy"
           :models ("tinyllama:1.1b" "tinydolphin:1.1b" "qwen2.5:0.5b" "llama3.2:1b")))
  "Categorized list of recommended models from the Ollama Hub.
Each entry is a plist with :name, :description and :models keys."
  :type '(repeat (plist :options
                        ((:name (string :tag "Category name"))
                         (:description (string :tag "Category description"))
                         (:models (repeat (string :tag "Model name"))))))
  :group 'ollama-buddy)

(defun ollama-buddy--available-models-flat ()
  "Return a flat list of all model names from `ollama-buddy-available-models'."
  (mapcan (lambda (cat) (copy-sequence (plist-get cat :models)))
          ollama-buddy-available-models))

(defcustom ollama-buddy-marker-prefix "o:"
  "Prefix used to identify Ollama models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy)

(defcustom ollama-buddy-cloud-marker-prefix "cl:"
  "Prefix used to identify Ollama cloud models in the ollama-buddy interface."
  :type 'string
  :group 'ollama-buddy)

(defun ollama-buddy--should-use-marker-prefix ()
  "Determine if marker prefix should be used.
Returns non-nil if any remote provider models are available.
The `o:' prefix for local models and `cl:' prefix for cloud models
are only needed when external providers (OpenAI, Claude, Gemini, etc.)
are loaded to disambiguate."
  (and (boundp 'ollama-buddy-remote-models)
       ollama-buddy-remote-models))

(defun ollama-buddy--should-use-cloud-prefix ()
  "Determine if cloud marker prefix should be used.
Returns non-nil if any external provider models are available.
When only Ollama local and cloud models are used, no prefix is needed
since the cloud symbol (☁) in the UI provides sufficient indication."
  (ollama-buddy--should-use-marker-prefix))

(defun ollama-buddy--get-full-model-name (model)
  "Get the full display name for MODEL with prefix if needed."
  (if (ollama-buddy--should-use-marker-prefix)
      (concat ollama-buddy-marker-prefix model)
    model))

(defun ollama-buddy--get-full-cloud-model-name (model)
  "Get the full display name for cloud MODEL with prefix if needed."
  (if (ollama-buddy--should-use-cloud-prefix)
      (concat ollama-buddy-cloud-marker-prefix model)
    model))

(defun ollama-buddy--get-real-model-name (model)
  "Extract the actual model name from the prefixed MODEL string."
  (cond
   ((string-prefix-p ollama-buddy-marker-prefix model)
    (substring model (length ollama-buddy-marker-prefix)))
   ((string-prefix-p ollama-buddy-cloud-marker-prefix model)
    (substring model (length ollama-buddy-cloud-marker-prefix)))
   (t model)))

(defvar ollama-buddy--model-letters nil
  "Alist mapping letter keys to model names.
Each entry is (KEY . MODEL-NAME) where KEY is a string like \"a\"
or \"@a\" for models beyond the first 26, and MODEL-NAME is the
full display name including any prefix.")

(defvar ollama-buddy-cloud-models)

(defun ollama-buddy--assign-model-letters (local-models)
  "Assign letters to LOCAL-MODELS and cloud models.
LOCAL-MODELS should be the list already obtained from
`ollama-buddy--get-models'.  Cloud models from
`ollama-buddy-cloud-models' are appended with the `cl:' prefix
only when external providers are loaded.
Supports more than 26 models by using `@a', `@b', etc. for
additional models beyond the first 26.
Updates `ollama-buddy--model-letters'."
  (let* ((cloud-models (mapcar #'ollama-buddy--get-full-cloud-model-name
                               ollama-buddy-cloud-models))
         (all-models (append local-models cloud-models))
         (model-count (length all-models))
         (alphabet "abcdefghijklmnopqrstuvwxyz")
         (alphabet-length (length alphabet))
         letter-alist)
    ;; First 26 models get single letters a-z
    (dotimes (i (min model-count alphabet-length))
      (push (cons (char-to-string (aref alphabet i))
                  (nth i all-models))
            letter-alist))
    ;; Models beyond 26 get prefixed combinations @a, @b, etc.
    (when (> model-count alphabet-length)
      (let ((remaining-models (nthcdr alphabet-length all-models))
            (index 0))
        (dolist (model remaining-models)
          (when (< index alphabet-length)
            (push (cons (concat "@" (char-to-string (aref alphabet index)))
                        model)
                  letter-alist)
            (setq index (1+ index))))))
    (setq ollama-buddy--model-letters (nreverse letter-alist))))

(defun ollama-buddy--get-model-letter (model)
  "Return the letter key assigned to MODEL, or nil if none."
  (car (rassoc model ollama-buddy--model-letters)))

(defun ollama-buddy--get-model-by-letter (letter)
  "Return the model assigned to LETTER key, or nil if none."
  (cdr (assoc letter ollama-buddy--model-letters)))

(defcustom ollama-buddy-status-update-interval 1.0
  "Interval in seconds to update the status line with background operations."
  :type 'float
  :group 'ollama-buddy)

(defvar ollama-buddy--in-reasoning-section nil
  "Whether we are currently inside a reasoning section.")

(defvar ollama-buddy--current-response nil
  "The current response text being accumulated.")

(defvar ollama-buddy--current-tool-calls nil
  "Accumulated tool calls during streaming.")

(defvar ollama-buddy--tool-call-iteration 0
  "Current iteration count for tool-call loops.")

(defvar ollama-buddy--response-start-marker nil
  "Marker for the start of the current response, used for pulsing.")

(defvar ollama-buddy--current-system-prompt-title nil
  "Title/name of the current system prompt for display purposes.")

(defvar ollama-buddy--current-system-prompt-source nil
  "Source of the current system prompt (user, fabric, awesome, manual).")

(defvar ollama-buddy--system-prompt-registry (make-hash-table :test 'equal)
  "Registry mapping system prompt content to metadata (title, source).")

(defvar ollama-buddy--current-attachments nil
  "List of files attached to the current conversation.
Each element is a plist with :file, :content, :size, and :type.")

(defvar ollama-buddy--attachment-history nil
  "History of attached files across conversations.")

(defvar ollama-buddy--model-context-sizes (make-hash-table :test 'equal)
  "Hash table mapping model names to their maximum context window sizes.")

(defvar ollama-buddy--model-context-sources (make-hash-table :test 'equal)
  "Hash table mapping model names to their context size source.
Values are `api' (from Ollama API), `fallback' (static), or `manual'.")

(defvar ollama-buddy--current-context-percentage nil
  "The current context window percentage used.")

(defvar ollama-buddy--current-context-tokens nil
  "The current token count used in the context window.")

(defvar ollama-buddy--current-context-max-size nil
  "The maximum context size for the current model.")

(defvar ollama-buddy--current-context-breakdown nil
  "Breakdown of token counts by type (history, system prompt, current prompt).")

(defvar ollama-buddy-remote-models nil
  "List of available remote models.")

(defcustom ollama-buddy-cloud-models
  '("qwen3-coder-next:cloud"
    "qwen3-coder:480b-cloud"
    "qwen3.5:cloud"
    "kimi-k2.5:cloud"
    "deepseek-v3.1:671b-cloud"
    "gpt-oss:120b-cloud"
    "gpt-oss:20b-cloud"
    "glm-4.7:cloud"
    "glm-5:cloud"
    "minimax-m2.1:cloud")
  "List of available Ollama cloud models.
These models run on ollama.com infrastructure and require authentication
via `ollama signin`.  Use \\[universal-argument] with `ollama-buddy--swap-model'
to select from this list."
  :type '(repeat string)
  :group 'ollama-buddy)

(defvar ollama-buddy-current-session-name nil
  "The name of the currently loaded session.")

(defvar ollama-buddy--background-operations nil
  "Alist of active background operations.
Each entry is (OPERATION-ID . DESCRIPTION) where OPERATION-ID
is a unique identifier and DESCRIPTION is displayed in the status line.")

(defvar ollama-buddy--status-update-timer nil
  "Timer for updating the status line with background operations.")

(defvar ollama-buddy--running-models-cache nil
  "Cache for running Ollama models.")

(defvar ollama-buddy--running-models-cache-timestamp nil
  "Timestamp when running models cache was last updated.")

(defvar ollama-buddy--models-cache nil
  "Cache for available Ollama models.")

(defvar ollama-buddy--models-cache-timestamp nil
  "Timestamp when models cache was last updated.")

(defvar ollama-buddy--models-metadata-cache (make-hash-table :test 'equal)
  "Hash table mapping model name to metadata alist.
For local Ollama models the keys come from /api/tags with fields:
  size, parameter-size, quantization, family.
For remote models keys are the full prefixed name with fields:
  context-window, display-name.")

(defvar ollama-buddy--provider-labels
  '(("a:" . "OpenAI")
    ("c:" . "Anthropic")
    ("g:" . "Google")
    ("k:" . "Grok")
    ("p:" . "Copilot")
    ("s:" . "Mistral")
    ("d:" . "DeepSeek")
    ("r:" . "OpenRouter"))
  "Alist mapping model prefix string to provider display name.")

(defvar ollama-buddy--context-window-table
  '(;; OpenAI — longer/more-specific prefixes must come before shorter ones
    ("gpt-4.1-mini"         . 1047576)
    ("gpt-4.1-nano"         . 1047576)
    ("gpt-4.1"              . 1047576)
    ("gpt-4o-mini"          . 128000)
    ("gpt-4o"               . 128000)
    ("gpt-4-turbo"          . 128000)
    ("gpt-4"                . 8192)
    ("gpt-3.5-turbo"        . 16385)
    ("gpt-5"                . 1000000)
    ("o1-mini"              . 128000)
    ("o1"                   . 200000)
    ("o3-mini"              . 200000)
    ("o3"                   . 200000)
    ("o4-mini"              . 200000)
    ;; Anthropic Claude — newer naming (no "3-") first, then legacy
    ("claude-opus-4"        . 200000)
    ("claude-sonnet-4"      . 200000)
    ("claude-haiku-4"       . 200000)
    ("claude-haiku"         . 200000)
    ("claude-3-7-sonnet"    . 200000)
    ("claude-3-5-sonnet"    . 200000)
    ("claude-3-5-haiku"     . 200000)
    ("claude-3-opus"        . 200000)
    ("claude-3-haiku"       . 200000)
    ;; Grok (xAI)
    ("grok-4"               . 256000)
    ("grok-3-mini"          . 131072)
    ("grok-3"               . 131072)
    ("grok-2"               . 131072)
    ("grok-1"               . 8192)
    ;; DeepSeek
    ("deepseek-chat"        . 65536)
    ("deepseek-reasoner"    . 65536)
    ;; Mistral / Codestral
    ("codestral"            . 256000)
    ("mistral-large"        . 131072)
    ("mistral-small"        . 131072))
  "Static context window sizes (tokens) for well-known remote models.
Matched by prefix of the real model name (without provider prefix).
More specific prefixes must appear before less specific ones.")

(defun ollama-buddy--get-provider-label (model)
  "Return the provider display name for MODEL based on its prefix, or nil."
  (catch 'found
    (dolist (pair ollama-buddy--provider-labels)
      (when (string-prefix-p (car pair) model)
        (throw 'found (cdr pair))))))

(defun ollama-buddy--format-context-window (tokens)
  "Format TOKENS count as a compact context-window string.
Uses M suffix for >= 1M (e.g. \"1M ctx\"), k suffix for >= 1k (\"128k ctx\"),
and plain number below that."
  (when (and tokens (> tokens 0))
    (cond
     ((>= tokens 1000000)
      (format "%dM ctx" (round (/ (float tokens) 1000000))))
     ((>= tokens 1000)
      (format "%dk ctx" (/ tokens 1000)))
     (t (format "%d ctx" tokens)))))

(defun ollama-buddy--get-context-window (model)
  "Return context window size in tokens for MODEL, or nil if unknown.
Checks `ollama-buddy--models-metadata-cache' first, then the static
`ollama-buddy--context-window-table' matched by bare model name prefix.
Strips both local Ollama prefixes (o:, cl:) and remote provider prefixes
(a:, c:, g:, etc.) before doing the static table lookup."
  (let* ((meta (gethash model ollama-buddy--models-metadata-cache))
         (cached (when meta (alist-get 'context-window meta))))
    (or cached
        (let* (;; Strip local prefix (o: / cl:) first
               (after-local (ollama-buddy--get-real-model-name model))
               ;; Then strip any remote provider prefix (a:, c:, g: ...)
               (bare (or (catch 'stripped
                           (dolist (pair ollama-buddy--provider-labels)
                             (when (string-prefix-p (car pair) after-local)
                               (throw 'stripped
                                      (substring after-local
                                                 (length (car pair)))))))
                         after-local))
               (found nil))
          (dolist (pair ollama-buddy--context-window-table)
            (when (and (not found)
                       (string-prefix-p (car pair) bare))
              (setq found (cdr pair))))
          found))))

(defvar ollama-buddy--models-cache-ttl 5
  "Time-to-live for models cache in seconds.")

(defvar ollama-buddy-roles--current-role "default"
  "The currently active ollama-buddy role.")

(defvar ollama-buddy--history-edit-buffer "*Ollama History Edit*"
  "Buffer name for editing Ollama conversation history.")

(defvar ollama-buddy--saved-params-active nil
  "Saved copy of params-active before applying command-specific parameters.")

(defvar ollama-buddy--saved-params-modified nil
  "Saved copy of params-modified before applying command-specific parameters.")

(defvar ollama-buddy--current-system-prompt nil
  "The current system prompt if set.")

(defvar ollama-buddy--debug-buffer "*Ollama Debug*"
  "Buffer for showing raw JSON messages.")

(defvar ollama-buddy--current-request-temporary-model nil
  "For the current request don't make current model permanent.")

(defvar ollama-buddy--response-start-position nil
  "Marker for the start position of the current response.")

(defvar ollama-buddy--current-prompt nil
  "The current prompt.")

(defvar ollama-buddy--current-session nil
  "Name of the currently active session, or nil if none.")

(defvar ollama-buddy--conversation-history-by-model (make-hash-table :test 'equal)
  "Hash table mapping model names to their conversation histories.")

(defvar ollama-buddy--token-usage-history nil
  "History of token usage for ollama-buddy interactions.")

(defvar ollama-buddy--current-token-count 0
  "Counter for tokens in the current response.")

(defvar ollama-buddy--current-token-start-time nil
  "Timestamp when the current response started.")

(defvar ollama-buddy--token-update-interval 0.5
  "How often to update the token rate display, in seconds.")

(defvar ollama-buddy--token-update-timer nil
  "Timer for updating token rate display.")

(defvar ollama-buddy--last-token-count 0
  "Token count at last update interval.")

(defvar ollama-buddy--last-update-time nil
  "Timestamp of last token rate update.")

(defvar ollama-buddy--prompt-history nil
  "History of prompts used in ollama-buddy.")

(defvar ollama-buddy--last-status-check nil
  "Timestamp of last Ollama status check.")

(defvar ollama-buddy--status-cache nil
  "Cached status of Ollama connection.")

(defvar ollama-buddy--status-cache-ttl 5
  "Time in seconds before status cache expires.")

(defvar ollama-buddy--current-model nil
  "Current model being used for Ollama requests.")

(defvar ollama-buddy--chat-buffer "*Ollama Buddy Chat*"
  "Chat interaction buffer.")

(defvar ollama-buddy--active-process nil
  "Active Ollama process.")

(defvar ollama-buddy--stream-pending ""
  "Pending partial data from the stream not yet forming a complete JSON line.")

(defvar ollama-buddy--stream-http-status nil
  "Non-nil when the current stream received a non-2xx HTTP response.
Holds the integer status code (e.g. 429).  The filter accumulates the full
error body and displays it; the sentinel suppresses its normal completion
message while this is set.")

(defvar ollama-buddy--status "Idle"
  "Current status of the Ollama request.")

(defvar ollama-buddy--suppress-tools-once nil
  "When non-nil, omit the tools schema from the very next send request.
Cleared automatically after it has been consumed.")

(defvar-local ollama-buddy--header-line-remapped nil
  "Non-nil if the header-line face has been remapped in this buffer.")

(defvar ollama-buddy--model-letters nil
  "Alist mapping letters to model names.")

(defvar ollama-buddy--multishot-sequence nil
  "Current sequence of models for multishot execution.")

(defvar ollama-buddy--multishot-progress 0
  "Progress through current multishot sequence.")

(defvar ollama-buddy--multishot-prompt nil
  "The prompt being used for the current multishot sequence.")

(defvar ollama-buddy--model-handlers (make-hash-table :test 'equal)
  "Map of model prefixes to handler functions.")

;; Core utility functions
;; Backend detection and validation
(defun ollama-buddy--validate-curl-executable ()
  "Check if curl executable is available and working."
  (condition-case nil
      (zerop (call-process ollama-buddy-curl-executable nil nil nil "--version"))
    (error nil)))

(defun ollama-buddy--get-effective-backend ()
  "Get the effective communication backend, with fallback logic."
  (cond
   ;; If explicitly set to curl, validate it's available
   ((eq ollama-buddy-communication-backend 'curl)
    (if (and (featurep 'ollama-buddy-curl)
             (fboundp 'ollama-buddy-curl--validate-executable)
             (ollama-buddy-curl--validate-executable))
        'curl
      (progn
        (message "Warning: curl backend not available, falling back to network-process")
        'network-process)))
   ;; Default to network-process
   (t 'network-process)))

;; Modified backend dispatcher functions
(defun ollama-buddy--make-request-backend (endpoint method &optional payload)
  "Make request using the configured backend."
  (let ((backend (ollama-buddy--get-effective-backend)))
    (cond
     ((eq backend 'curl)
      (ollama-buddy-curl--make-request endpoint method payload))
     (t
      (ollama-buddy--make-request endpoint method payload)))))

(defun ollama-buddy--make-request-async-backend (endpoint method payload callback)
  "Make async request using the configured backend."
  (let ((backend (ollama-buddy--get-effective-backend)))
    (cond
     ((eq backend 'curl)
      (ollama-buddy-curl--make-request-async endpoint method payload callback))
     (t
      (ollama-buddy--make-request-async endpoint method payload callback)))))

(defun ollama-buddy--send-backend (prompt &optional specified-model)
  "Send prompt using the configured backend."
  (let* ((model (or specified-model
                    (bound-and-true-p ollama-buddy--current-model)
                    (bound-and-true-p ollama-buddy-default-model)))
         (backend (ollama-buddy--get-effective-backend)))
    (if (and ollama-buddy-airplane-mode
             (ollama-buddy--internet-model-p model))
        (message "✈ Airplane mode is active — %s requires internet access" model)
      (cond
       ((eq backend 'curl)
        (ollama-buddy-curl--send prompt specified-model))
       (t
        (ollama-buddy--send prompt specified-model))))))

;; Function to test communication backend
(defun ollama-buddy-test-communication-backend ()
  "Test the current communication backend."
  (interactive)
  (let ((backend (ollama-buddy--get-effective-backend)))
    (message "Testing %s backend..." backend)
    (condition-case err
        (if (ollama-buddy--make-request-backend "/api/tags" "GET")
            (message "%s backend working correctly!" (capitalize (symbol-name backend)))
          (message "%s backend failed to get response" (capitalize (symbol-name backend))))
      (error
       (message "%s backend failed: %s" (capitalize (symbol-name backend)) 
                (error-message-string err))))))

;; Function to switch backend interactively
(defun ollama-buddy-switch-communication-backend ()
  "Interactively switch communication backend."
  (interactive)
  (let ((current-backend ollama-buddy-communication-backend)
        (new-backend (intern (completing-read 
                              "Select communication backend: "
                              '("network-process" "curl") nil t))))
    (setq ollama-buddy-communication-backend new-backend)
    (message "Switched from %s to %s backend" 
             current-backend new-backend)
    (ollama-buddy--update-status new-backend)
    (ollama-buddy-test-communication-backend)))

(defun ollama-buddy--extract-title-from-content (content)
  "Extract a meaningful title from system prompt CONTENT."
  (when (and content (stringp content))
    (let ((content-clean (string-trim content))
          title)
      (cond
       ;; Pattern: "You are a/an [role]"
       ((string-match "^[Yy]ou are \\(?:a\\|an\\) \\([^.,:;!?\n]+\\)" content-clean)
        (setq title (capitalize (match-string 1 content-clean))))
       
       ;; Pattern: "Act as [role]"
       ((string-match "^[Aa]ct as \\(?:a\\|an\\|the\\)?\\s-*\\([^.,:;!?\n]+\\)" content-clean)
        (setq title (capitalize (match-string 1 content-clean))))
       
       ;; Pattern: "I want you to act as [role]"
       ((string-match "[Ii] want you to act as \\(?:a\\|an\\|the\\)?\\s-*\\([^.,:;!?\n]+\\)" content-clean)
        (setq title (capitalize (match-string 1 content-clean))))
       
       ;; Pattern: "Your role is [role]"
       ((string-match "[Yy]our role is \\(?:a\\|an\\|the\\)?\\s-*\\([^.,:;!?\n]+\\)" content-clean)
        (setq title (capitalize (match-string 1 content-clean))))
       
       ;; Fallback: Use first few words
       (t
        (let ((words (split-string content-clean)))
          (when words
            (setq title (mapconcat 'identity (seq-take words 3) " "))
            (when (> (length title) 30)
              (setq title (concat (substring title 0 27) "...")))))))
      
      ;; Clean up the title
      (when title
        (setq title (replace-regexp-in-string "\\s-+" " " title))
        (setq title (string-trim title))
        (when (> (length title) 40)
          (setq title (concat (substring title 0 37) "..."))))
      
      (or title "Custom Prompt"))))

(defun ollama-buddy--register-system-prompt (content title source)
  "Register system prompt CONTENT with TITLE and SOURCE metadata."
  (when (and content (stringp content) (not (string-empty-p content)))
    (let ((content-hash (secure-hash 'sha256 content)))
      (puthash content-hash
               (list :title title
                     :source source
                     :content content
                     :timestamp (current-time))
               ollama-buddy--system-prompt-registry)
      ;; Set current metadata
      (setq ollama-buddy--current-system-prompt-title title
            ollama-buddy--current-system-prompt-source source))))

(defun ollama-buddy--get-system-prompt-metadata (content)
  "Get metadata for system prompt CONTENT, generating title if needed."
  (when (and content (stringp content) (not (string-empty-p content)))
    (let* ((content-hash (secure-hash 'sha256 content))
           (metadata (gethash content-hash ollama-buddy--system-prompt-registry)))
      (unless metadata
        ;; Generate metadata if not found
        (let ((title (ollama-buddy--extract-title-from-content content)))
          (setq metadata (list :title title
                               :source "manual"
                               :content content
                               :timestamp (current-time)))
          (puthash content-hash metadata ollama-buddy--system-prompt-registry)))
      metadata)))

(defun ollama-buddy--update-system-prompt-display-info (content)
  "Update display information for the current system prompt CONTENT."
  (if (and content (not (string-empty-p content)))
      (let ((metadata (ollama-buddy--get-system-prompt-metadata content)))
        (setq ollama-buddy--current-system-prompt-title (plist-get metadata :title)
              ollama-buddy--current-system-prompt-source (plist-get metadata :source)))
    (setq ollama-buddy--current-system-prompt-title nil
          ollama-buddy--current-system-prompt-source nil)))

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

(defun ollama-buddy-set-system-prompt-with-title ()
  "Set the current prompt as a system prompt, allowing user to specify a title."
  (interactive)
  (let* ((prompt-data (ollama-buddy--get-prompt-content))
         (prompt-text (car prompt-data))
         (title (read-string "Title for this system prompt: "
                             (ollama-buddy--extract-title-from-content prompt-text))))
    
    ;; Add to history if non-empty
    (when (and prompt-text (not (string-empty-p prompt-text)))
      (put 'ollama-buddy--cycle-prompt-history 'history-position -1)
      (add-to-history 'ollama-buddy--prompt-history prompt-text))
    
    ;; Set as system prompt with metadata
    (setq ollama-buddy--current-system-prompt prompt-text)
    (ollama-buddy--register-system-prompt prompt-text title "manual")
    
    ;; Update the UI to reflect the change
    (ollama-buddy--prepare-prompt-area t t t)
    (ollama-buddy--prepare-prompt-area nil nil)
    
    ;; Update status to show system prompt is set
    (ollama-buddy--update-status "System prompt set")
    (message "System prompt set: %s" title)))

(defun ollama-buddy--get-system-prompt-display ()
  "Get display text for the current system prompt."
  (cond
   ((and ollama-buddy--current-system-prompt
         ollama-buddy--current-system-prompt-title)
    (let* ((source-indicator (cond
                              ((string= ollama-buddy--current-system-prompt-source "fabric") "F:")
                              ((string= ollama-buddy--current-system-prompt-source "awesome") "A:")
                              ((string= ollama-buddy--current-system-prompt-source "user") "U:")
                              (t "")))
           (title ollama-buddy--current-system-prompt-title))
      (format "[%s%s]" source-indicator title)))
   
   (ollama-buddy--current-system-prompt
    ;; Fallback for prompts without titles
    (let ((auto-title (ollama-buddy--extract-title-from-content 
                       ollama-buddy--current-system-prompt)))
      (ollama-buddy--update-system-prompt-display-info ollama-buddy--current-system-prompt)
      (format "[%s]" auto-title)))
   
   (t "")))

(defun ollama-buddy--set-system-prompt-with-metadata (content title source)
  "Set system prompt CONTENT with TITLE and SOURCE metadata."
  (setq ollama-buddy--current-system-prompt content)
  (ollama-buddy--register-system-prompt content title source)
  (ollama-buddy--update-status "System prompt set"))

(defun ollama-buddy--effective-system-prompt ()
  "Return the combined tone, global and session system prompts.
The tone modifier from `ollama-buddy-tone-alist' is prepended when
non-empty.  When `ollama-buddy-global-system-prompt-enabled' is
non-nil and `ollama-buddy-global-system-prompt' is non-empty, it
follows the tone.  The session-specific
`ollama-buddy--current-system-prompt' comes last.  Parts are
separated by two newlines when combined."
  (let* ((tone-text (or (cdr (assoc ollama-buddy--current-tone
                                    ollama-buddy-tone-alist))
                        ""))
         (tone (and (not (string-empty-p tone-text)) tone-text))
         (global (and ollama-buddy-global-system-prompt-enabled
                      (stringp ollama-buddy-global-system-prompt)
                      (not (string-empty-p ollama-buddy-global-system-prompt))
                      ollama-buddy-global-system-prompt))
         (session (and ollama-buddy--current-system-prompt
                       (not (string-empty-p ollama-buddy--current-system-prompt))
                       ollama-buddy--current-system-prompt))
         (parts (delq nil (list tone global session))))
    (when parts
      (mapconcat #'identity parts "\n\n"))))

(defun ollama-buddy-toggle-global-system-prompt ()
  "Toggle the global system prompt on or off."
  (interactive)
  (setq ollama-buddy-global-system-prompt-enabled
        (not ollama-buddy-global-system-prompt-enabled))
  (ollama-buddy--update-status
   (if ollama-buddy-global-system-prompt-enabled "Global System Prompt enabled" "Global System Prompt disabled"))
  (message "Global system prompt %s"
           (if ollama-buddy-global-system-prompt-enabled "enabled" "disabled")))

(defun ollama-buddy-set-tone ()
  "Select a response tone from `ollama-buddy-tone-alist'."
  (interactive)
  (let ((tone (completing-read "Tone: " (mapcar #'car ollama-buddy-tone-alist) nil t)))
    (setq ollama-buddy--current-tone tone)
    (ollama-buddy--update-status (format "Tone: %s" tone))
    (force-mode-line-update t)
    (message "Tone set to %s" tone)))

(defun ollama-buddy-show-system-prompt-info ()
  "Show detailed information about the current system prompt."
  (interactive)
  (if ollama-buddy--current-system-prompt
      (let* ((metadata (ollama-buddy--get-system-prompt-metadata 
                        ollama-buddy--current-system-prompt))
             (title (plist-get metadata :title))
             (source (plist-get metadata :source))
             (timestamp (plist-get metadata :timestamp))
             (buf (get-buffer-create "*System Prompt Info*")))
        
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            (setq-local org-hide-emphasis-markers t)
            (setq-local org-hide-leading-stars t)
            
            (insert "#+TITLE: System Prompt Information\n\n")
            (insert (format "* Title: %s\n\n" (or title "Untitled")))
            (insert (format "* Source: %s\n\n" (or source "Unknown")))
            (when timestamp
              (insert (format "* Set at: %s\n\n" 
                              (format-time-string "%Y-%m-%d %H:%M:%S" timestamp))))
            (insert "* Content:\n\n")
            (insert "#+begin_example\n")
            (insert ollama-buddy--current-system-prompt)
            (insert "\n#+end_example\n")
            
            (view-mode 1)
            (goto-char (point-min))))
        
        (display-buffer buf))
    (message "No system prompt is currently set")))

(defun ollama-buddy--extract-context-length-from-model-info (model-info)
  "Extract context length from MODEL-INFO returned by /api/show.
MODEL-INFO is the parsed JSON response containing model metadata.
The context length is stored in keys like `llama.context_length' or
`qwen3.context_length' depending on the model architecture."
  (when model-info
    (let ((context-length nil))
      ;; model_info contains architecture-specific keys like llama.context_length
      (dolist (key (mapcar #'car model-info))
        (when (and (not context-length)
                   (symbolp key)
                   (string-match-p "\\.context_length$" (symbol-name key)))
          (setq context-length (alist-get key model-info))))
      context-length)))

(defun ollama-buddy--fetch-model-context-size-sync (model)
  "Synchronously fetch context size for MODEL from Ollama API.
Returns the context size or nil if the API call fails.
As a side effect, caches the thinking capability in
`ollama-buddy--models-metadata-cache' when the capabilities array
from /api/show includes \"thinking\"."
  (condition-case nil
      (let* ((real-model (ollama-buddy--get-real-model-name model))
             (endpoint "/api/show")
             (payload (json-encode `((model . ,real-model))))
             (response (ollama-buddy--make-request endpoint "POST" payload)))
        (when response
          ;; Cache thinking capability detected via Ollama's capabilities array
          (let ((capabilities (append (alist-get 'capabilities response) nil)))
            (when (member "thinking" capabilities)
              (let ((cached-meta (or (gethash model ollama-buddy--models-metadata-cache) '())))
                (unless (alist-get 'thinking cached-meta)
                  (puthash model
                           (cons '(thinking . t) cached-meta)
                           ollama-buddy--models-metadata-cache)))))
          ;; Return context size
          (let ((model-info (alist-get 'model_info response)))
            (ollama-buddy--extract-context-length-from-model-info model-info))))
    (error nil)))

(defun ollama-buddy--get-fallback-context-size (model)
  "Get fallback context size for MODEL from static mappings.
Returns the size from `ollama-buddy-fallback-context-sizes' or 4096 as default."
  (if (null model)
      4096
    (let ((fallback-size nil))
      ;; First try exact match
      (setq fallback-size (cdr (assoc model ollama-buddy-fallback-context-sizes)))
      ;; Then try substring matches
      (unless fallback-size
        (dolist (entry ollama-buddy-fallback-context-sizes)
          (when (and (not fallback-size)
                     (string-match-p (car entry) model))
            (setq fallback-size (cdr entry)))))
      ;; Cloud/internet models have large context windows — use 128K as default.
      ;; Local models fall back to the conservative 4096.
      (or fallback-size
          (if (ollama-buddy--internet-model-p model) 131072 4096)))))

(defun ollama-buddy--get-model-context-size (model)
  "Get the context window size for MODEL.
Checks cache first, then Ollama API, then static fallback mappings.
Source is recorded in `ollama-buddy--model-context-sources'."
  (let* (;; Get base context size from cache, API, or fallback
         (base-size
          (or
           ;; First check if we have it cached
           (gethash model ollama-buddy--model-context-sizes)
           
           ;; If not cached, try to fetch from Ollama API
           (let ((api-size (ollama-buddy--fetch-model-context-size-sync model)))
             (when api-size
               ;; Cache the API result and record source
               (puthash model api-size ollama-buddy--model-context-sizes)
               (puthash model 'api ollama-buddy--model-context-sources)
               api-size))
           
           ;; Fall back to static mappings
           (let ((fallback-size (ollama-buddy--get-fallback-context-size model)))
             ;; Cache the fallback size and record source
             (puthash model fallback-size ollama-buddy--model-context-sizes)
             (puthash model 'fallback ollama-buddy--model-context-sources)
             fallback-size)))
         
         ;; Check if num_ctx parameter is set and modified
         (num-ctx (when (memq 'num_ctx ollama-buddy-params-modified)
                    (alist-get 'num_ctx ollama-buddy-params-active))))
    
    ;; Return the effective context size (base-size limited by num_ctx if set)
    (if (and num-ctx (numberp num-ctx) (> num-ctx 0))
        (min base-size num-ctx)
      base-size)))

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
  (puthash model 'manual ollama-buddy--model-context-sources)
  (message "Context size for %s set to %d" model size))

(defun ollama-buddy--get-model-context-source (model)
  "Get the source of the context size for MODEL.
Returns `api' if retrieved from Ollama API, `fallback' if from static mappings,
`manual' if set manually, or nil if not yet determined."
  (gethash model ollama-buddy--model-context-sources))

(defun ollama-buddy--estimate-token-count (text)
  "Estimate the number of tokens in TEXT.
This is a rough approximation based on word count."
  ;; Basic estimation: ~1.3 tokens per word for English
  (round (* 1.3 (length (split-string text)))))

(defun ollama-buddy-register-model-handler (prefix handler-function)
  "Register HANDLER-FUNCTION for models with PREFIX.
The handler function should accept the same arguments as `ollama-buddy--send`."
  (puthash prefix handler-function ollama-buddy--model-handlers))

(defun ollama-buddy--dispatch-to-handler (orig-fun prompt &optional specified-model tool-continuation-p)
  "Dispatch to appropriate handler based on model prefix.
ORIG-FUN is the original function being advised.
PROMPT, SPECIFIED-MODEL and TOOL-CONTINUATION-P are passed through."
  (let* ((model (or specified-model
                    ollama-buddy--current-model
                    ollama-buddy-default-model))
         (handler nil))
    ;; Find a matching handler based on model prefix
    (maphash (lambda (prefix func)
               (when (and (not handler)
                          (string-prefix-p prefix model))
                 (setq handler func)))
             ollama-buddy--model-handlers)
    ;; Call the handler or original function
    (if handler
        (funcall handler prompt model)
      (funcall orig-fun prompt specified-model tool-continuation-p))))

;; Apply the advice to ollama-buddy--send
(advice-add 'ollama-buddy--send :around #'ollama-buddy--dispatch-to-handler)


(defun ollama-buddy--count-models-with-prefix (prefix)
  "Count the number of models in `ollama-buddy-remote-models' with PREFIX."
  (if (and (boundp 'ollama-buddy-remote-models) ollama-buddy-remote-models)
      (length (seq-filter (lambda (m) (string-prefix-p prefix m))
                          ollama-buddy-remote-models))
    0))

(defun ollama-buddy--get-enabled-external-providers ()
  "Return a list of enabled external LLM provider names with info."
  (let (providers)
    (when (featurep 'ollama-buddy-openai)
      (push (format "a: OpenAI (%d)" (ollama-buddy--count-models-with-prefix "a:")) providers))
    (when (featurep 'ollama-buddy-claude)
      (push (format "c: Claude (%d)" (ollama-buddy--count-models-with-prefix "c:")) providers))
    (when (featurep 'ollama-buddy-gemini)
      (push (format "g: Gemini (%d)" (ollama-buddy--count-models-with-prefix "g:")) providers))
    (when (featurep 'ollama-buddy-grok)
      (push (format "k: Grok (%d)" (ollama-buddy--count-models-with-prefix "k:")) providers))
    (when (featurep 'ollama-buddy-copilot)
      (push (format "p: Copilot (%d)" (ollama-buddy--count-models-with-prefix "p:")) providers))
    (when (featurep 'ollama-buddy-codestral)
      (push (format "s: Codestral (%d)" (ollama-buddy--count-models-with-prefix "s:")) providers))
    (when (featurep 'ollama-buddy-deepseek)
      (push (format "d: DeepSeek (%d)" (ollama-buddy--count-models-with-prefix "d:")) providers))
    (when (featurep 'ollama-buddy-openrouter)
      (push (format "r: OpenRouter (%d)" (ollama-buddy--count-models-with-prefix "r:")) providers))
    (when (featurep 'ollama-buddy-openai-compat)
      (push (format "l: %s (%d)"
                    (if (boundp 'ollama-buddy-openai-compat-provider-name)
                        ollama-buddy-openai-compat-provider-name
                      "LocalAI")
                    (ollama-buddy--count-models-with-prefix
                     (if (boundp 'ollama-buddy-openai-compat-marker-prefix)
                         ollama-buddy-openai-compat-marker-prefix
                       "l:")))
            providers))
    (nreverse providers)))

(defvar ollama-buddy--cloud-auth-status 'unknown
  "Cached Ollama cloud authentication status.
Can be `unknown', `authenticated', or `not-authenticated'.
Updated when signin/signout is called, auth errors are detected,
or a successful cloud model response is received.")

(defun ollama-buddy--cloud-auth-status-p ()
  "Check if Ollama cloud is authenticated. Returns t if signed in, nil otherwise."
  (eq ollama-buddy--cloud-auth-status 'authenticated))

(defun ollama-buddy--set-cloud-auth-status (authenticated)
  "Set the cloud authentication status cache.
AUTHENTICATED should be t for authenticated, nil for not authenticated."
  (setq ollama-buddy--cloud-auth-status (if authenticated 'authenticated 'not-authenticated)))

(defun ollama-buddy--copilot-auth-status-p ()
  "Check if GitHub Copilot is authenticated. Returns t if logged in, nil otherwise."
  (when (featurep 'ollama-buddy-copilot)
    (and (boundp 'ollama-buddy-copilot--oauth-token)
         (or ollama-buddy-copilot--oauth-token
             (when (fboundp 'ollama-buddy-copilot--load-oauth-token)
               (ollama-buddy-copilot--load-oauth-token))))))

(defun ollama-buddy--get-browser-auth-status ()
  "Return a list of browser-auth providers with their status.
Each element is a plist with :name, :authenticated, and :enabled."
  (let (providers)
    ;; Ollama Cloud - always available if ollama is running
    (push (list :name "Ollama Cloud"
                :enabled t
                :authenticated (ollama-buddy--cloud-auth-status-p))
          providers)
    ;; GitHub Copilot - only if feature is loaded
    (when (featurep 'ollama-buddy-copilot)
      (push (list :name "GitHub Copilot"
                  :enabled t
                  :authenticated (ollama-buddy--copilot-auth-status-p))
            providers))
    (nreverse providers)))

(defun ollama-buddy--cloud-auth-status-indicator ()
  "Return the indicator for Ollama cloud auth status."
  (pcase ollama-buddy--cloud-auth-status
    ('authenticated "[✓]")
    ('not-authenticated "[✗]")
    ('unknown "[?]")))

(defun ollama-buddy--format-auth-status ()
  "Format browser-auth provider status for display."
  (let ((providers (ollama-buddy--get-browser-auth-status)))
    (when providers
      (mapconcat
       (lambda (p)
         (let ((name (plist-get p :name)))
           (format "%s %s"
                   name
                   (if (string= name "Ollama Cloud")
                       (ollama-buddy--cloud-auth-status-indicator)
                     (if (plist-get p :authenticated) "[✓]" "[✗]")))))
       providers
       " | "))))

;;; Tips

(require 'ollama-buddy-tips)

(defcustom ollama-buddy-show-tips t
  "When non-nil, display a random tip in the welcome screen."
  :type 'boolean
  :group 'ollama-buddy)

(defun ollama-buddy--get-random-tip ()
  "Return a random tip string from `ollama-buddy-tips'.
Returns nil when `ollama-buddy-show-tips' is nil or the list is empty."
  (when (and ollama-buddy-show-tips ollama-buddy-tips)
    (nth (random (length ollama-buddy-tips)) ollama-buddy-tips)))

(defun ollama-buddy--create-intro-message ()
  "Create minimal welcome message with essential commands in org format."
  (setq-local org-hide-emphasis-markers t)
  (setq-local org-hide-leading-stars t)
  (let* ((external-providers (ollama-buddy--get-enabled-external-providers))
         (auth-status (ollama-buddy--format-auth-status))
         (ollama-count (length (ollama-buddy--get-models)))
         (cloud-count (length ollama-buddy-cloud-models))
         (use-prefixes (ollama-buddy--should-use-marker-prefix))
         (provider-summary
          (let ((parts nil))
            ;; Only show "o: Ollama" with prefix when external providers are loaded
            (when (and (> ollama-count 0) use-prefixes)
              (push (format "o: Ollama (%d)" ollama-count) parts))
            (when external-providers
              (princ external-providers)
              (setq parts (append (nreverse parts) external-providers)
                    parts (nreverse parts)))
            ;; Only show "cl: Cloud" with prefix when external providers are loaded
            (when (and (> cloud-count 0) use-prefixes)
              (push (format "cl: Cloud (%d)" cloud-count) parts))
            (nreverse parts)))
         (message-text
          (concat
           (when (= (buffer-size) 0)
             (concat "#+TITLE: Ollama Buddy Chat"))
           "\n\n* Welcome to _OLLAMA BUDDY_\n\n"
           "#+begin_example\n"
           "┌───────────────────────────────────┐\n"
           "│  O L L A M A B U D D Y  [v3.1.0]  │\n"
           "└───────────────────────────────────┘\n"
           ;; "╔════════════════════════════════════════════════════════════╗\n"
           ;; "║  ▄▀▀▀▄ █   █   ▄▀▀▀▄ █▀▄▀█ ▄▀▀▀▄ █▀▀▄ █  █ █▀▀▄ █▀▀▄ █  █  ║\n"
           ;; "║  █   █ █   █   █▀▀▀█ █ █ █ █▀▀▀█ █▀▀▄ █  █ █  █ █  █ ▀█▀   ║\n"
           ;; "║   ▀▀▀  ▀▀▀ ▀▀▀ ▀   ▀ ▀   ▀ ▀   ▀ ▀▀▀  ▀▀▀▀ ▀▀▀  ▀▀▀   ▀    ║\n"
           ;; "╠════════════════════════════════════════════════════════════╣\n"
           ;; "║ > Model: Llama-3-8B                                        ║\n"
           ;; "╚════════════════════════════════════════════════════════════╝\n"
           ;; " ___ _ _      n _ n      ___       _   _ _ _\n"
           ;; "|   | | |__._|o(Y)o|__._| . |_ _ _| |_| | | |\n"
           ;; "| | | | | .  |2.8.1| .  | . | | | . | . |__ |\n"
           ;; "|___|_|_|__/_|_|_|_|__/_|___|___|___|___|___|\n"
           "#+end_example\n\n"
           (when (not (ollama-buddy--check-status))
             "** *THERE IS NO OLLAMA RUNNING*\n
please run =ollama serve=\n\n")
           (when provider-summary
             (concat
              (let* ((items provider-summary)
                     (col-width 28)
                     (lines nil))
                (while items
                  (let ((left (pop items))
                        (right (pop items)))
                    (push (if right
                              (format (format "%%-%ds %%s" col-width) left right)
                            left)
                          lines)))
                (mapconcat #'identity (nreverse lines) "\n"))
              "\n\n"))
           (when auth-status
             (concat "Auth: " auth-status "\n\n"))
           "- /Ask me anything!/       C-c C-c / C-c RET
- /Cancel request/         C-c C-k
- /Select model/           C-c m"
           (when ollama-buddy-full-welcome-enabled
             "
- /Main transient menu/    C-c O
- /Browse prompt history/  M-p/n/r
- /Manage models/          C-c M
- /ollama-buddy Manual/    C-c ?")
           (when-let ((tip (ollama-buddy--get-random-tip)))
             (concat "\n\n" tip))
           )))
    (add-face-text-property 0 (length message-text) '(:inherit bold) nil message-text)
    message-text))

(defun ollama-buddy-open-info ()
  "Open the Info manual for the ollama-buddy package."
  (interactive)
  (info "(ollama-buddy)"))

(defun ollama-buddy-escape-unicode (string)
  "Efficiently convert non-ASCII characters to Unicode escape sequences."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[^\x00-\x7F]" nil t)
      (let* ((char (char-before))
             (unicode-escape (format "\\u%04X" char)))
        (delete-char -1)
        (insert unicode-escape)))
    (buffer-string)))

(defun ollama-buddy--register-background-operation (operation-id description)
  "Register a new background OPERATION-ID with DESCRIPTION."
  ;; Start the timer if it's not already running
  (unless ollama-buddy--status-update-timer
    (setq ollama-buddy--status-update-timer
          (run-with-timer 0 ollama-buddy-status-update-interval
                          #'ollama-buddy--update-status-with-operations)))
  
  ;; Add the operation to the list
  (push (cons operation-id description) ollama-buddy--background-operations)
  
  ;; Immediately update the status
  (ollama-buddy--update-status-with-operations))

(defun ollama-buddy--update-background-operation (operation-id new-description)
  "Update OPERATION-ID with NEW-DESCRIPTION."
  (let ((entry (assq operation-id ollama-buddy--background-operations)))
    (when entry
      (setcdr entry new-description)
      (ollama-buddy--update-status-with-operations))))

(defun ollama-buddy--complete-background-operation (operation-id &optional completion-status)
  "Mark OPERATION-ID as completed with optional COMPLETION-STATUS."
  ;; Remove the operation from the list
  (setq ollama-buddy--background-operations
        (assq-delete-all operation-id ollama-buddy--background-operations))
  
  ;; Update status with completion message if provided
  (when completion-status
    (ollama-buddy--update-status completion-status))
  
  ;; Cancel the timer if no more operations
  (when (and (null ollama-buddy--background-operations)
             ollama-buddy--status-update-timer)
    (cancel-timer ollama-buddy--status-update-timer)
    (setq ollama-buddy--status-update-timer nil))
  
  ;; Update the status display
  (ollama-buddy--update-status-with-operations))

(defun ollama-buddy--update-status-with-operations ()
  "Update status line to show background operations."
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (let* ((regular-status ollama-buddy--status)
           (operations-text
            (when ollama-buddy--background-operations
              (mapconcat #'cdr ollama-buddy--background-operations " | ")))
           (combined-status
            (if operations-text
                (format "%s [%s...]" regular-status operations-text)
              regular-status)))
      
      ;; Call the original update status function with our combined status
      (let ((ollama-buddy--status combined-status))
        (ollama-buddy--update-status combined-status)))))

(defun ollama-buddy-toggle-streaming ()
  "Toggle streaming mode for Ollama responses.
When streaming is enabled, responses appear token by token in real time.
When disabled, responses only appear after completion."
  (interactive)
  (setq ollama-buddy-streaming-enabled (not ollama-buddy-streaming-enabled))
  (ollama-buddy--update-status
   (if ollama-buddy-streaming-enabled "Streaming enabled" "Streaming disabled"))
  (message "Ollama Buddy streaming mode: %s"
           (if ollama-buddy-streaming-enabled "enabled" "disabled")))

(defun ollama-buddy-set-keepalive ()
  "Set how long Ollama keeps the model loaded after a request.
Choose a preset or enter a custom duration string accepted by Ollama:
  \"5m\"  – five minutes (Ollama default)
  \"0\"   – unload immediately after each response
  \"-1\"  – keep loaded indefinitely
  \"default\" – omit the parameter (revert to Ollama default)"
  (interactive)
  (let* ((presets '("default" "0" "5m" "10m" "30m" "1h" "-1"))
         (choice (completing-read
                  (format "Keep-alive [current: %s]: "
                          (or ollama-buddy-keepalive "default"))
                  presets nil nil nil nil
                  (or ollama-buddy-keepalive "default")))
         (value (if (string= choice "default") nil choice)))
    (setq ollama-buddy-keepalive value)
    (ollama-buddy--update-status
     (format "Keep-alive: %s" (or value "default")))
    (message "Ollama keep-alive set to: %s"
             (or value "default (5m)"))))

(defun ollama-buddy--md-to-org-convert-region (start end)
  "Convert the region from START to END from Markdown to Org-mode format."

  (save-excursion
    (save-restriction
      (narrow-to-region start end)

      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match "\n\n"))
      
      ;; First, handle code blocks by temporarily protecting their content
      (goto-char (point-min))
      (let ((code-blocks nil)
            (counter 0)
            block-start block-end lang content placeholder)
        
        ;; IMPORTANT: Add save-match-data here
        (save-match-data
          ;; Find and replace code blocks with placeholders
          (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
            (setq lang (match-string 1)
                  content (match-string 2)
                  block-start (match-beginning 0)
                  block-end (match-end 0)
                  placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
            
            ;; Store the code block information for later restoration
            (push (list placeholder lang content) code-blocks)
            
            ;; Replace with placeholder
            (delete-region block-start block-end)
            (goto-char block-start)
            (insert placeholder)
            (setq counter (1+ counter))))
        
        ;; Apply regular Markdown to Org transformations - in individual save-match-data blocks
        ;; Lists: Translate `-`, `*`, or `+` lists to Org-mode lists
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
            (replace-match (concat (match-string 1) "- \\2"))))
        
        ;; Bold: `**bold**` -> `*bold*` only if directly adjacent
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
            (replace-match "*\\1*")))
        
        ;; Italics: `_italic_` -> `/italic/`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
            (replace-match "\\1/\\2/\\3")))
        
        ;; Links: `[text](url)` -> `[[url][text]]`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
            (replace-match "[[\\2][\\1]]")))
        
        ;; Inline code: `code` -> =code=
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "`\\(.*?\\)`" nil t)
            (replace-match "=\\1=")))
        
        ;; Horizontal rules: `---` or `***` -> `-----`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
            (replace-match "-----")))
        
        ;; Images: `![alt text](url)` -> `[[url]]`
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
            (replace-match "[[\\1]]")))
        
        ;; Headers: Adjust '#' - add 2 levels so MD H1 becomes org level 3
        ;; (below the ** [MODEL] RESPONSE heading)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "^\\(#+\\) " nil t)
            (replace-match (make-string (+ 2 (length (match-string 1))) ?*) nil nil nil 1)))
        
        ;; Any extra characters
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "—" nil t)
            (replace-match ", ")))
        
        ;; Restore code blocks with proper Org syntax
        (save-match-data
          (dolist (block (nreverse code-blocks))
            (let ((placeholder (nth 0 block))
                  (lang (nth 1 block))
                  (content (nth 2 block)))
              (goto-char (point-min))
              (when (search-forward placeholder nil t)
                (replace-match (format "#+begin_src %s\n%s#+end_src" lang content) t t)))))))))

(defun ollama-buddy--text-after-prompt ()
  "Get the text after the prompt:."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward ">> \\(?:PROMPT\\|SYSTEM PROMPT\\):" nil t)
        (progn
          (search-forward ":")
          (string-trim (buffer-substring-no-properties
                        (point) (point-max))))
      "")))

(defun ollama-buddy--get-command-def (command-name)
  "Get command definition for COMMAND-NAME."
  (assoc command-name ollama-buddy-command-definitions))

(defun ollama-buddy--get-command-prop (command-name prop)
  "Get property PROP from command COMMAND-NAME."
  (plist-get (cdr (ollama-buddy--get-command-def command-name)) prop))

(defun ollama-buddy--param-shortname (param)
  "Create a 4-character shortened name for PARAM by using first 2 and last 2 chars.
For parameters with 4 or fewer characters, returns the full name."
  (let* ((param-name (symbol-name param))
         (param-len (length param-name)))
    (if (<= param-len 4)
        param-name
      (concat (substring param-name 0 2)
              (substring param-name (- param-len 2) param-len)))))

(defun ollama-buddy--maybe-goto-prompt (window response-start)
  "Move point to prompt if response is wholly visible in WINDOW.
RESPONSE-START is the position where the response began.
Returns non-nil if point was moved.
Controlled by `ollama-buddy-goto-prompt-on-visible-completion'."
  (when (and ollama-buddy-goto-prompt-on-visible-completion
             window
             response-start
             (pos-visible-in-window-p response-start window)
             (pos-visible-in-window-p (point-max) window))
    (goto-char (point-max))
    (set-window-point window (point-max))
    t))

(defun ollama-buddy--prepare-prompt-area (&optional new-prompt keep-content system-prompt)
  "Prepare the prompt area in the buffer.
When NEW-PROMPT is non-nil, replace the existing prompt area.
When KEEP-CONTENT is non-nil, preserve the existing prompt content.
When SYSTEM-PROMPT is non-nil, mark as a system prompt."
  (let* ((model (or ollama-buddy--current-model
                    ollama-buddy-default-model
                    "Default:latest"))
         (existing-content (when keep-content (ollama-buddy--text-after-prompt)))
         (cloud-indicator (if (ollama-buddy--cloud-model-p model) "☁" ""))
         (tools-indicator (if (ollama-buddy--model-supports-tools model) "⚒" ""))
         (thinking-indicator (if (ollama-buddy--model-supports-thinking model) "✦" ""))
         (in-buffer-indicator (if (bound-and-true-p ollama-buddy-in-buffer-replace) "✎" ""))
         (indicators (string-trim (concat cloud-indicator tools-indicator
                                          thinking-indicator in-buffer-indicator))))

    (let ((buf (get-buffer-create ollama-buddy--chat-buffer)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          ;; Clean up existing prompt
          (goto-char (point-max))
          (when (re-search-backward "\\* .*>> \\(?:PROMPT\\|SYSTEM PROMPT\\):" nil t)
            (beginning-of-line)
            (if (or new-prompt
                    (not (string-match-p "[[:alnum:]]" (ollama-buddy--text-after-prompt))))
                ;; Either replacing prompt or current prompt is empty
                (progn
                  (skip-chars-backward "\n")
                  (delete-region (point) (point-max))
                  (goto-char (point-max)))
              ;; Keeping prompt with content
              (goto-char (point-max))))

          ;; Insert new prompt header
          (insert (format "\n\n* *%s* %s%s"
                          model
                          (if (string-empty-p indicators) "" (concat indicators " "))
                          (if system-prompt ">> SYSTEM PROMPT: " ">> PROMPT: ")))

          ;; Restore content if requested
          (when (and keep-content existing-content)
            (insert existing-content)))))))

;; API Interaction

(defun ollama-buddy--get-version ()
  "Return the Ollama server version string, or nil if unavailable."
  (condition-case nil
      (let ((response (ollama-buddy--make-request "/api/version" "GET")))
        (when response
          (alist-get 'version response)))
    (error nil)))

(defun ollama-buddy--make-request (endpoint method &optional payload)
  "Generic request function for ENDPOINT with METHOD and optional PAYLOAD."
  (when (ollama-buddy--ollama-running)
    (let ((url-request-method method)
          (url-request-extra-headers '(("Content-Type" . "application/json")
                                       ("Connection" . "close")))
          (url-show-status nil)
          (url (format "http://%s:%d%s"
                       ollama-buddy-host ollama-buddy-port endpoint)))
      (with-temp-buffer
        (if payload
            (let ((url-request-data (encode-coding-string payload 'utf-8)))
              (url-insert-file-contents url))
          (url-insert-file-contents url))
        (when (not (string-empty-p (buffer-string)))
          (json-read-from-string (buffer-string)))))))

(defun ollama-buddy--make-request-async (endpoint method payload callback)
  "Make an asynchronous request to ENDPOINT using METHOD with PAYLOAD.
When complete, CALLBACK is called with the status response and result."
  (when (ollama-buddy--ollama-running)
    (let ((url-request-method method)
          (url-request-extra-headers '(("Content-Type" . "application/json")
                                       ("Connection" . "close")))
          (url-request-data (when payload (encode-coding-string payload 'utf-8)))
          (url-show-status nil)
          (url (format "http://%s:%d%s"
                       ollama-buddy-host ollama-buddy-port endpoint)))
      (url-retrieve url
                    (lambda (status)
                      (let ((result nil))
                        (unless (plist-get status :error)
                          ;; Only try to parse JSON if there was no error and we have content
                          (goto-char (point-min))
                          (re-search-forward "^$" nil t) ;; Skip headers
                          (when (and (not (= (point) (point-max)))
                                     (not (string-empty-p (buffer-substring-no-properties (point) (point-max)))))
                            (condition-case err
                                (setq result (json-read-from-string
                                              (buffer-substring-no-properties (point) (point-max))))
                              (error
                               ;; If JSON parsing fails, just return the raw response
                               (message "Warning: Failed to parse JSON response: %s" (error-message-string err))))))
                        (funcall callback status result)))
                    nil t t))))

(defun ollama-buddy--ollama-running ()
  "Check if Ollama server is running using the configured backend."
  (let ((backend (ollama-buddy--get-effective-backend)))
    (cond
     ((and (eq backend 'curl)
           (featurep 'ollama-buddy-curl)
           (fboundp 'ollama-buddy-curl--test-connection))
      (ollama-buddy-curl--test-connection))
     (t
      (condition-case nil
          (progn
            (let ((url-show-status nil))
              (url-retrieve-synchronously
               (format "http://%s:%s/api/tags" ollama-buddy-host ollama-buddy-port)
               nil nil 2))
            t)
        (error nil))))))

(defun ollama-buddy--check-status ()
  "Check Ollama status with caching for better performance."
  (let ((current-time (float-time)))
    (when (or (null ollama-buddy--last-status-check)
              (> (- current-time ollama-buddy--last-status-check)
                 ollama-buddy--status-cache-ttl))
      (setq ollama-buddy--status-cache (ollama-buddy--ollama-running)
            ollama-buddy--last-status-check current-time))
    ollama-buddy--status-cache))

(defun ollama-buddy--get-models-with-others ()
  "Get all available models, including remote and cloud models."
  (let ((models '()))
    (dolist (model (ollama-buddy--get-models))
      (push model models))
    (setq models (append models ollama-buddy-remote-models))
    (setq models (append models
                         (mapcar #'ollama-buddy--get-full-cloud-model-name
                                 ollama-buddy-cloud-models)))
    models))

(defun ollama-buddy--get-models ()
  "Get available Ollama models with caching."
  (when (ollama-buddy--ollama-running)
    (let ((current-time (float-time)))
      (when (or (null ollama-buddy--models-cache-timestamp)
                (> (- current-time ollama-buddy--models-cache-timestamp)
                   ollama-buddy--models-cache-ttl))
        ;; Use backend dispatcher
        (when-let ((response (ollama-buddy--make-request-backend "/api/tags" "GET")))
          (setq ollama-buddy--models-cache
                (sort
                 (ollama-buddy--get-model-names-from-result response)
                 #'string<)
                ollama-buddy--models-cache-timestamp current-time)
          ;; (ollama-buddy--refresh-models-cache)
          ))
      ollama-buddy--models-cache)))

(defun ollama-buddy--refresh-models-cache ()
  "Refresh the models cache in the background."
  (ollama-buddy--make-request-async-backend
   "/api/tags"
   "GET"
   nil
   (lambda (status result)
     (unless (plist-get status :error)
       (when result
         (setq ollama-buddy--models-cache
               (sort
                (ollama-buddy--get-model-names-from-result result)
                #'string<)
               ollama-buddy--models-cache-timestamp (float-time)))))))

(defun ollama-buddy--get-model-names-from-result (result)
  "Extract model names from API RESULT, applying prefix if needed.
Cloud models (those with a `-cloud' suffix or in `ollama-buddy-cloud-models')
are excluded since they appear under the `cl:' prefix instead.
Also populates `ollama-buddy--models-metadata-cache' with size and detail info."
  (when result
    (clrhash ollama-buddy--models-metadata-cache)
    (cl-remove-if
     (lambda (name)
       (ollama-buddy--cloud-model-p
        (ollama-buddy--get-real-model-name name)))
     (mapcar (lambda (m)
               (let* ((raw-name (alist-get 'name m))
                      (full-name (ollama-buddy--get-full-model-name raw-name))
                      (details (alist-get 'details m))
                      (size (alist-get 'size m)))
                 (puthash full-name
                          `((size          . ,size)
                            (parameter-size . ,(alist-get 'parameter_size details))
                            (quantization   . ,(alist-get 'quantization_level details))
                            (family         . ,(alist-get 'family details)))
                          ollama-buddy--models-metadata-cache)
                 full-name))
             (alist-get 'models result)))))

(defun ollama-buddy--get-running-models ()
  "Get list of currently running Ollama models with caching."
  (when (ollama-buddy--ollama-running)
    (let ((current-time (float-time)))
      (when (or (null ollama-buddy--running-models-cache-timestamp)
                (> (- current-time ollama-buddy--running-models-cache-timestamp)
                   ollama-buddy--models-cache-ttl))
        ;; Use backend dispatcher
        (when-let ((response (ollama-buddy--make-request-backend "/api/ps" "GET")))
          (setq ollama-buddy--running-models-cache
                (mapcar (lambda (m)
                          (ollama-buddy--get-full-model-name (alist-get 'name m)))
                        (alist-get 'models response))
                ollama-buddy--running-models-cache-timestamp current-time)
          
          (ollama-buddy--refresh-running-models-cache)))
      ollama-buddy--running-models-cache)))

(defun ollama-buddy--refresh-running-models-cache ()
  "Refresh the running models cache in the background."
  (ollama-buddy--make-request-async-backend
   "/api/ps"
   "GET"
   nil
   (lambda (status result)
     (unless (plist-get status :error)
       (when result
         (setq ollama-buddy--running-models-cache
               (mapcar (lambda (m) (alist-get 'name m))
                       (alist-get 'models result))
               ollama-buddy--running-models-cache-timestamp (float-time)))))))

(defun ollama-buddy--ensure-cloud-model-available (model)
  "Ensure cloud MODEL has its manifest pulled locally.
MODEL may have a `cl:' or `o:' prefix.  If MODEL is not a cloud model,
return immediately.  Otherwise run `ollama pull' synchronously to fetch
the manifest.  The pull is idempotent and returns instantly when the
manifest is already present."
  (when (ollama-buddy--cloud-model-p model)
    (let ((raw (ollama-buddy--get-real-model-name model)))
      (message "Pulling cloud model manifest for %s..." raw)
      (let ((exit-code (call-process ollama-buddy-ollama-executable
                                     nil nil nil "pull" raw)))
        (if (zerop exit-code)
            (message "Pulling cloud model manifest for %s...done" raw)
          (user-error "Failed to pull cloud model manifest for %s (exit code %d)"
                      raw exit-code))))))

(defun ollama-buddy--cloud-model-p (model)
  "Return non-nil if MODEL is a cloud model.
Cloud models have a `-cloud' or `:cloud' suffix, `cl:' prefix,
or are in `ollama-buddy-cloud-models'."
  (when model
    (or (string-suffix-p "-cloud" model)
        (string-suffix-p ":cloud" model)
        (string-prefix-p ollama-buddy-cloud-marker-prefix model)
        (member model ollama-buddy-cloud-models))))

(defun ollama-buddy--internet-model-p (model)
  "Return non-nil if MODEL requires internet access.
This includes Ollama cloud models and all external provider models
\(OpenAI, Claude, Gemini, Grok, Copilot, Codestral, DeepSeek, OpenRouter)."
  (when model
    (or (ollama-buddy--cloud-model-p model)
        (seq-some (lambda (prefix) (string-prefix-p prefix model))
                  '("a:" "c:" "g:" "k:" "p:" "s:" "d:" "r:")))))

(defun ollama-buddy-toggle-airplane-mode ()
  "Toggle airplane mode on/off.
When enabled, only local Ollama models are accessible and web search
is disabled, preventing any internet access from this package."
  (interactive)
  (setq ollama-buddy-airplane-mode (not ollama-buddy-airplane-mode))
  (when (fboundp 'ollama-buddy--update-status)
    (ollama-buddy--update-status (or (bound-and-true-p ollama-buddy--status) "")))
  (message "Airplane mode %s"
           (if ollama-buddy-airplane-mode "enabled ✈ — local Ollama only" "disabled")))

(defun ollama-buddy-toggle-in-buffer-replace ()
  "Toggle in-buffer replacement mode on/off.
When enabled, commands that operate on a region stream their response
back into the source buffer instead of the chat buffer."
  (interactive)
  (setq ollama-buddy-in-buffer-replace (not ollama-buddy-in-buffer-replace))
  (when (fboundp 'ollama-buddy--update-status)
    (ollama-buddy--update-status (or (bound-and-true-p ollama-buddy--status) "")))
  (message "In-buffer replace %s"
           (if ollama-buddy-in-buffer-replace "ON (✎)" "OFF")))

(defun ollama-buddy--validate-model (model)
  "Validate MODEL availability.
Cloud models are always considered valid if Ollama is running."
  (when (and model (ollama-buddy--ollama-running))
    (when (or (member model (ollama-buddy--get-models-with-others))
              (ollama-buddy--cloud-model-p model))
      model)))

(defun ollama-buddy--get-valid-model (specified-model)
  "Get valid model from SPECIFIED-MODEL with fallback handling."
  (let* ((valid-model (or (ollama-buddy--validate-model specified-model)
                          (ollama-buddy--validate-model ollama-buddy-default-model))))
    (if valid-model
        (cons valid-model specified-model)
      (let ((models (ollama-buddy--get-models-with-others)))
        (if models
            (let ((selected (completing-read
                             (format "%s not available. Select model: "
                                     (or specified-model ""))
                             models nil t)))
              (setq ollama-buddy--current-model selected)
              (cons selected specified-model))
          (error "No Ollama models available"))))))

;; Parameter handling functions

(defun ollama-buddy--apply-command-parameters (params-alist)
  "Apply parameters from PARAMS-ALIST to the current Ollama request."
  ;; Save current parameters to restore later
  (setq ollama-buddy--saved-params-active (copy-tree ollama-buddy-params-active)
        ollama-buddy--saved-params-modified (copy-tree ollama-buddy-params-modified))
  
  ;; Apply new parameters
  (dolist (param-pair params-alist)
    (let ((param (car param-pair))
          (value (cdr param-pair)))
      (setf (alist-get param ollama-buddy-params-active) value)
      (add-to-list 'ollama-buddy-params-modified param))))

(defun ollama-buddy--restore-default-parameters ()
  "Restore parameters to their state before command execution."
  (when ollama-buddy--saved-params-active
    (setq ollama-buddy-params-active ollama-buddy--saved-params-active
          ollama-buddy-params-modified ollama-buddy--saved-params-modified)
    (setq ollama-buddy--saved-params-active nil
          ollama-buddy--saved-params-modified nil)))

(defun ollama-buddy-params-get-for-request ()
  "Get only the modified parameters formatted for the Ollama API request."
  (let ((params (make-hash-table)))
    ;; Only include explicitly modified parameters
    (dolist (param ollama-buddy-params-modified)
      (puthash param (alist-get param ollama-buddy-params-active)
               params))
    
    ;; Convert to an alist for the JSON encoding
    (let ((params-alist nil))
      (maphash (lambda (k v) (push (cons k v) params-alist)) params)
      params-alist)))

(defun ollama-buddy-apply-param-profile (profile-name)
  "Apply parameter PROFILE-NAME from `ollama-buddy-params-profiles'."
  (let ((profile (alist-get profile-name ollama-buddy-params-profiles nil nil #'string=)))
    (if (null profile)
        (message "Profile '%s' not found" profile-name)
      ;; Reset all parameters to defaults
      (setq ollama-buddy-params-active (copy-tree ollama-buddy-params-defaults)
            ollama-buddy-params-modified nil)
      ;; Apply profile-specific parameters
      (dolist (param-pair profile)
        (let ((param (car param-pair))
              (value (cdr param-pair)))
          (setf (alist-get param ollama-buddy-params-active) value)
          (add-to-list 'ollama-buddy-params-modified param)))
      (ollama-buddy--update-status "Profile Applied"))))

;; History-related functions

(defun ollama-buddy--add-to-history (role content &optional tool-calls)
  "Add message with ROLE and CONTENT to conversation history for current model.
Optional TOOL-CALLS includes tool call objects in the message."
  (when ollama-buddy-history-enabled
    (let* ((model ollama-buddy--current-model)
           (history (gethash model ollama-buddy--conversation-history-by-model nil)))

      ;; Create new history entry for this model if it doesn't exist
      (unless history
        (setq history nil))

      ;; Add the new message to this model's history
      ;; and put it at the end
      (let ((message (if tool-calls
                         `((role . ,role)
                           (content . ,content)
                           (tool_calls . ,(vconcat tool-calls)))
                       `((role . ,role)
                         (content . ,content)))))
        (setq history (append history (list message))))

      ;; Truncate history if needed - keep the MOST RECENT items
      ;; Calculate how many items to drop from the beginning
      (let ((max-items (* 2 ollama-buddy-max-history-length)))
        (when (> (length history) max-items)
          (setq history (seq-drop history (- (length history) max-items)))))

      ;; Update the hash table with the modified history
      (puthash model history ollama-buddy--conversation-history-by-model))))

(defun ollama-buddy--add-to-history-raw (message)
  "Add MESSAGE directly to conversation history without role/content wrapping.
Used for tool result messages which already have the correct structure."
  (when ollama-buddy-history-enabled
    (let* ((model ollama-buddy--current-model)
           (history (gethash model ollama-buddy--conversation-history-by-model nil)))
      (setq history (append history (list message)))
      (let ((max-items (* 2 ollama-buddy-max-history-length)))
        (when (> (length history) max-items)
          (setq history (seq-drop history (- (length history) max-items)))))
      (puthash model history ollama-buddy--conversation-history-by-model))))

(defun ollama-buddy--get-history-for-request ()
  "Get history for the current request."
  (if ollama-buddy-history-enabled
      (let* ((model ollama-buddy--current-model)
             (history (gethash model ollama-buddy--conversation-history-by-model nil)))
        history)
    nil))

;; Status update functions

(defun ollama-buddy--add-context-to-status-format ()
  "Calculate context percentage and display it according to preference."
  (if (and ollama-buddy-show-context-percentage
           ollama-buddy--current-context-percentage)
      (let* ((total-tokens ollama-buddy--current-context-tokens)
             (max-size ollama-buddy--current-context-max-size)
             (percentage ollama-buddy--current-context-percentage)
             (amber-threshold (nth 0 ollama-buddy-context-size-thresholds))
             (red-threshold (nth 1 ollama-buddy-context-size-thresholds))
             (status-face (cond
                           ((>= percentage red-threshold)
                            '(:inherit header-line
                                       :inverse-video t
                                       :weight bold))
                           ((>= percentage amber-threshold)
                            '(:inherit header-line
                                       :underline t
                                       :weight bold))
                           (t '(:inherit header-line)))))
        
        (cond
         ;; Text display
         ((eq ollama-buddy-context-display-type 'text)
          (let ((context-text
                 (propertize
                  (format "%d/%d"
                          (or total-tokens 0)
                          (or max-size 4096))
                  'face status-face)))
            (format "%s" context-text)))
         ;; Bar display
         ((eq ollama-buddy-context-display-type 'bar)
          (let* ((bar-width ollama-buddy-context-bar-width)
                 (filled-chars (round (* percentage bar-width)))
                 (filled-chars (min filled-chars bar-width))
                 (empty-chars (- bar-width filled-chars))
                 (filled-char (car ollama-buddy-context-bar-chars))
                 (empty-char (cadr ollama-buddy-context-bar-chars))
                 (bar-text (concat
                            (make-string filled-chars filled-char)
                            (make-string empty-chars empty-char))))
            bar-text
            ))))
    ""))

(defun ollama-buddy--update-status (status &optional original-model actual-model)
  "Update the Ollama status and refresh the display.
STATUS is the current operation status.
ORIGINAL-MODEL is the model that was requested.
ACTUAL-MODEL is the model being used instead."
  (setq ollama-buddy--status status)
  (when ollama-buddy-show-context-percentage
    (ollama-buddy--calculate-prompt-context-percentage))
  (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
    (unless ollama-buddy--header-line-remapped
      (face-remap-add-relative 'header-line :height ollama-buddy-header-line-height)
      (setq ollama-buddy--header-line-remapped t))
    (let* ((model (or ollama-buddy--current-model
                      ollama-buddy-default-model
                      "No Model"))
           (history (if (and ollama-buddy-show-history-indicator
                             ollama-buddy-history-enabled)
                        (let ((history-count (/ (length
                                                 (gethash model
                                                          ollama-buddy--conversation-history-by-model
                                                          nil))
                                                2)))
                          (format "H%d/%d"
                                  history-count ollama-buddy-max-history-length))
                      ""))
           (system-indicator (ollama-buddy--get-system-prompt-display))
           (params (when ollama-buddy-show-params-in-header
                     (let ((param-str
                            (mapconcat
                             (lambda (param)
                               (let ((value (alist-get param ollama-buddy-params-active)))
                                 (format "%s:%s"
                                         (ollama-buddy--param-shortname param)
                                         (cond
                                          ((floatp value) (format "%.1f" value))
                                          ((vectorp value) "...")
                                          (t value)))))
                             ollama-buddy-params-modified " ")))
                       (if (string-empty-p param-str)
                           ""
                         (format " [%s]" param-str)))))
           (airplane-indicator (if ollama-buddy-airplane-mode
                                   (propertize "✈" 'face '(:weight bold))
                                 ""))
           (cloud-indicator (if (ollama-buddy--cloud-model-p model) "☁" ""))
           (tools-indicator (if (and (boundp 'ollama-buddy-tools-enabled)
                                     ollama-buddy-tools-enabled
                                     (ollama-buddy--model-supports-tools model))
                                "⚒" ""))
           (vision-indicator (if (ollama-buddy--model-supports-vision model) "⊙" ""))
           (thinking-indicator (if (ollama-buddy--model-supports-thinking model) "✦" ""))
           (attachment-indicator (if ollama-buddy--current-attachments
                                     (propertize (format "≡%d" (length ollama-buddy--current-attachments))
                                                 'face '(:weight bold))
                                   ""))
           (web-search-indicator (if (and (featurep 'ollama-buddy-web-search)
                                          (fboundp 'ollama-buddy-web-search-count)
                                          (> (ollama-buddy-web-search-count) 0))
                                     (propertize (format "♁%d " (ollama-buddy-web-search-count))
                                                 'face '(:weight bold))
                                   ""))
           (rag-indicator (if (and (fboundp 'ollama-buddy-rag-count)
                                   (> (ollama-buddy-rag-count) 0))
                              (propertize (format "⊕%d " (ollama-buddy-rag-count))
                                          'face '(:weight bold))
                            ""))
           (in-buffer-indicator (if (bound-and-true-p ollama-buddy-in-buffer-replace) "✎" ""))
           (tone-indicator (let ((tone ollama-buddy--current-tone))
                             (if (or (null tone) (string= tone "Normal"))
                                 ""
                               (propertize (format "~%c" (aref tone 0))
                                           'face '(:weight bold))))))
      (setq header-line-format
            (concat
             (format "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s %s%s%s %s %s%s"
                     airplane-indicator
                     (if ollama-buddy-streaming-enabled "" "x")
                     (ollama-buddy--add-context-to-status-format)
                     (if ollama-buddy-global-system-prompt-enabled "" "<")
                     history
                     cloud-indicator
                     tools-indicator
                     vision-indicator
                     thinking-indicator
                     in-buffer-indicator
                     attachment-indicator
                     web-search-indicator
                     rag-indicator
                     (if (and ollama-buddy-hide-reasoning
                              (not ollama-buddy-collapse-thinking)) "V" "")
                     (if ollama-buddy-display-token-stats "T" "")
                     tone-indicator

                     (ollama-buddy--update-multishot-status)
                     (propertize (if (ollama-buddy--check-status) "" " OFFLINE")
                                 'face '(:weight bold))

                     (if (ollama-buddy--check-status)
                         (propertize model 'face `(:weight bold :box (:line-width 1 :style flat-button)))
                       (propertize model 'face `(:weight bold :inherit shadow :box (:line-width 1 :style flat-button))))
                     status
                     system-indicator
                     (or params ""))
             (when (and original-model actual-model (not (string= original-model actual-model)))
               (propertize (format " [Using %s instead of %s]" actual-model original-model)
                           'face '(:foreground "orange" :weight bold))))))))

(defun ollama-buddy--update-multishot-status ()
  "Update status line to show multishot progress.
Works with the list-based multishot sequence without using array operations."
  (if (not ollama-buddy--multishot-sequence)
      ""
    (let ((completed '())
          (remaining '())
          (i 0))
      ;; Build completed and remaining lists manually
      (dolist (key ollama-buddy--multishot-sequence)
        (if (< i ollama-buddy--multishot-progress)
            (push key completed)
          (push key remaining))
        (setq i (1+ i)))
      
      ;; Reverse the lists since we pushed elements
      (setq completed (nreverse completed))
      (setq remaining (nreverse remaining))
      
      ;; Format the status display
      (concat (propertize " Multishot: " 'face '(:weight bold))
              (if completed
                  (propertize (mapconcat 'upcase completed ",") 'face '(:weight bold))
                "")
              (when remaining
                (concat (if completed "," "")
                        (propertize (mapconcat 'identity remaining ",")
                                    'face '(:weight normal))))
              " "))))

;; Command handling functions
(defun ollama-buddy--display-system-prompt (system-prompt &optional timeout)
  "Display SYSTEM-PROMPT in the minibuffer for TIMEOUT seconds.
If TIMEOUT is nil, use a default of 2 seconds."
  (let ((timeout (or timeout 2))
        (message-text (if (string-empty-p system-prompt)
                          "No system prompt set"
                        (format "Using system prompt: %s"
                                (if (> (length system-prompt) 80)
                                    (concat (substring system-prompt 0 77) "...")
                                  system-prompt)))))
    ;; Display the message
    (message message-text)
    ;; Set a timer to clear it after timeout
    (run-with-timer timeout nil (lambda () (message nil)))))

(provide 'ollama-buddy-core)
;;; ollama-buddy-core.el ends here
