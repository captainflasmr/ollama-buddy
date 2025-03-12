;;; savehist-mock.el --- Mock savehist functionality for testing -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides mock implementations of savehist functions
;; for testing purposes only.

;;; Code:

;; Define the variable that's referenced in ollama-buddy.el
(defvar savehist-additional-variables nil
  "List of variables to save in addition to `minibuffer-history-variable's value.")

(provide 'savehist)
;;; savehist-mock.el ends here
