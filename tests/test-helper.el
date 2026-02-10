;;; test-helper.el --- Test helper for ollama-buddy tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>

;;; Commentary:

;; Common test utilities, fixtures, and setup for ollama-buddy tests.
;; Load this file before running any tests.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load-path for the main package files
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root))

;; Load the main package modules we're testing
(require 'ollama-buddy-core)

;;; Test Fixtures
;; ============================================================================

(defvar test-helper--sample-models
  '("llama3.2:1b" "llama3.2:3b" "qwen2.5-coder:3b" "tinyllama:latest")
  "Sample local models for testing.")

(defvar test-helper--sample-cloud-models
  '("qwen3-coder:480b-cloud" "deepseek-v3.1:671b-cloud" "custom-cloud")
  "Sample cloud models for testing.")

(defvar test-helper--sample-system-prompts
  '(("You are a helpful assistant." . "Helpful assistant")
    ("Act as a Python expert." . "Python expert")
    ("I want you to act as a code reviewer." . "Code reviewer")
    ("Your role is the senior developer." . "Senior developer")
    ("Just some random text here." . "Just some random"))
  "Sample system prompts with expected titles.")

;;; Test Utilities
;; ============================================================================

(defmacro with-ollama-buddy-test-env (&rest body)
  "Execute BODY with a clean ollama-buddy test environment.
Saves and restores relevant variables."
  (declare (indent 0) (debug t))
  `(let ((ollama-buddy-cloud-models test-helper--sample-cloud-models)
         (ollama-buddy-marker-prefix "o:")
         (ollama-buddy-cloud-marker-prefix "cl:")
         (ollama-buddy-remote-models nil)
         (ollama-buddy--current-model nil)
         (ollama-buddy-default-model "llama3.2:1b"))
     ,@body))

(defmacro with-temp-ollama-buffer (&rest body)
  "Execute BODY in a temporary buffer mimicking ollama-buddy chat buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq-local ollama-buddy--current-model "llama3.2:1b")
     ,@body))

(defun test-helper-make-response (content &optional done)
  "Create a mock API response with CONTENT and optionally DONE flag."
  `((model . "llama3.2:1b")
    (created_at . "2024-01-01T00:00:00Z")
    (response . ,content)
    (done . ,(or done :json-false))))

;;; ERT Test Tags
;; ============================================================================
;; Use these tags to categorize tests:
;;   :core     - Core functionality tests (pure functions, utilities)
;;   :api      - API interaction tests (may need mocking)
;;   :ui       - User interface tests (buffers, display)
;;   :provider - Provider-specific tests (openai, claude, etc.)

(provide 'test-helper)
;;; test-helper.el ends here
