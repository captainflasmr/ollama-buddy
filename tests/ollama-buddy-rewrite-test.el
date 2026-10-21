;;; ollama-buddy-rewrite-test.el --- Tests for in-buffer rewrite -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>

;;; Commentary:

;; Offline ERT tests for the in-buffer rewrite request builder.  These
;; cover the remote-provider shapes so commands such as `proofread' work
;; with prefixed models (e.g. OpenCode Go's "n:deepseek-v4.1-flash").

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'ollama-buddy-provider)
(require 'ollama-buddy-rewrite)

(defun ollama-buddy-rewrite-test--provider (api-type endpoint)
  "Return a provider struct for API-TYPE using ENDPOINT."
  (ollama-buddy-provider--make
   :name "Test"
   :prefix "t:"
   :api-type api-type
   :api-key "test-key"
   :endpoint endpoint
   :temperature 0.5
   :max-tokens nil))

(defun ollama-buddy-rewrite-test--json (string)
  "Parse JSON STRING into an alist."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string string)))

(ert-deftest ollama-buddy-rewrite-test-openai-shape ()
  "Remote rewrite builds an OpenAI-compatible streaming request."
  (let* ((provider (ollama-buddy-rewrite-test--provider
                    'openai "https://example.com/v1/chat/completions"))
         (parts (ollama-buddy-rewrite--remote-request-parts
                 provider "t:test-model" "SYS" "USR"))
         (headers (nth 1 parts))
         (body (ollama-buddy-rewrite-test--json (nth 2 parts))))
    (should (equal (nth 0 parts) "https://example.com/v1/chat/completions"))
    (should (eq (nth 3 parts)
                #'ollama-buddy-remote--openai-extract-content))
    (should (equal (cdr (assoc "Authorization" headers))
                   "Bearer test-key"))
    (should (equal (alist-get 'model body) "test-model"))
    (should (eq (alist-get 'stream body) t))
    (should (= (length (alist-get 'messages body)) 2))
    (should (equal (alist-get 'role (aref (alist-get 'messages body) 0))
                   "system"))))

(ert-deftest ollama-buddy-rewrite-test-claude-shape ()
  "Remote rewrite builds an Anthropic streaming request."
  (let* ((provider (ollama-buddy-rewrite-test--provider
                    'claude "https://example.com/v1/messages"))
         (parts (ollama-buddy-rewrite--remote-request-parts
                 provider "t:test-model" "SYS" "USR"))
         (headers (nth 1 parts))
         (body (ollama-buddy-rewrite-test--json (nth 2 parts))))
    (should (eq (nth 3 parts)
                #'ollama-buddy-remote--claude-extract-content))
    (should (equal (cdr (assoc "anthropic-version" headers)) "2023-06-01"))
    (should (equal (alist-get 'system body) "SYS"))
    (should (eq (alist-get 'stream body) t))
    ;; Claude messages are a single user turn; the system prompt is top-level.
    (should (= (length (alist-get 'messages body)) 1))))

(ert-deftest ollama-buddy-rewrite-test-gemini-shape ()
  "Remote rewrite builds a Gemini streaming request."
  (let* ((provider (ollama-buddy-rewrite-test--provider
                    'gemini
                    "https://example.com/v1beta/models/%s:generateContent"))
         (parts (ollama-buddy-rewrite--remote-request-parts
                 provider "t:test-model" "SYS" "USR"))
         (endpoint (nth 0 parts))
         (body (ollama-buddy-rewrite-test--json (nth 2 parts))))
    (should (string-match-p
             (regexp-quote ":streamGenerateContent?alt=sse&key=test-key")
             endpoint))
    (should (string-match-p "test-model" endpoint))
    (should (eq (nth 3 parts)
                #'ollama-buddy-remote--gemini-extract-content))
    (should (alist-get 'systemInstruction body))
    (should (alist-get 'contents body))))

(ert-deftest ollama-buddy-rewrite-test-format-error ()
  "API error bodies are reduced to a readable message."
  (should (equal (ollama-buddy-rewrite--format-error
                  "{\"error\":{\"message\":\"Invalid credential\"}}")
                 "Invalid credential"))
  (should (equal (ollama-buddy-rewrite--format-error "plain failure")
                 "plain failure")))

(provide 'ollama-buddy-rewrite-test)
;;; ollama-buddy-rewrite-test.el ends here
