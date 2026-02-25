;;; ollama-buddy-tips.el --- Tips for the ollama-buddy welcome screen -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This file contains the tip strings shown at random on the ollama-buddy
;; welcome screen.  Each string in `ollama-buddy-tips' is one tip.
;;
;; To add, edit or remove tips without touching any other file, just edit
;; this list.  Org-mode markup is supported in tips: =code=, /italic/, *bold*.
;;
;; To add your own tips from your Emacs config:
;;
;;   (with-eval-after-load 'ollama-buddy-tips
;;     (add-to-list 'ollama-buddy-tips "My custom tip" t))
;;
;; To disable tips entirely, set `ollama-buddy-show-tips' to nil.

;;; Code:

(defvar ollama-buddy-tips
  '(;; Navigation & basics
    "Press =C-c O= to open the full transient menu — it groups every feature by category so nothing stays hidden."
    "Type =/= at the prompt to open the slash-command list: model swap, session save/load, tone, tools and more — all without leaving the prompt."
    "Press =M-p= and =M-n= at the prompt to walk back and forward through your prompt history, just like a shell."
    "=C-c m= lets you switch models mid-conversation.  Each model keeps its own history, so context is never lost."
    "=C-c C-k= cancels an in-flight request immediately — useful when a model starts going off-track."
    "=C-c ?= opens the full ollama-buddy Info manual directly inside Emacs."

    ;; Context & attachments
    "Use =@file(/path/to/file)= inline in any prompt to attach a file directly to that message without leaving the chat."
    "Use =@search(your query)= inline to run a web search and fold the results into your prompt automatically."
    "Use =@rag(your query)= inline to search a local RAG index and attach the matching chunks to your prompt."
    "=C-c 0= clears all attachments — files, web search results, and RAG context — in one keystroke."
    "The =≡N= indicator in the header shows how many files are attached; =♁N= shows attached web-search results."

    ;; Models & providers
    "Load =ollama-buddy-openai=, =ollama-buddy-claude=, or any other provider and all their models appear in =C-c m= alongside local Ollama models."
    "The =☁= symbol in the model list marks Ollama cloud models that run on ollama.com infrastructure — no local GPU needed."
    "The =⊙= indicator means the current model supports vision — you can attach images with =C-c i=."
    "The =✦= indicator means the current model supports thinking/reasoning — look for collapsible =*** ✦ Think= sections in responses."
    "The =⚒= indicator in the header means tool calling is *enabled* for a model that supports it.  Toggle with =C-c SPC= or =/tools=."

    ;; Thinking models
    "Press =C-c V= to toggle all =*** ✦ Think= reasoning blocks in the buffer at once — expand them all or collapse them all."
    "Press =TAB= on any =*** ✦ Think= heading to fold or unfold just that one reasoning block."

    ;; In-buffer replace
    "Toggle in-buffer replace mode with =C-c W=.  Region-operating commands then stream their output back into the source buffer instead of the chat buffer."
    "After a rewrite streams in, press =C-c d= to see a word-level inline diff between the original and new text.  Press =C-c d= again to hide it."
    "=C-c C-c= accepts a rewrite (keeps new text); =C-c C-k= rejects it and restores the original — no undo required."

    ;; Sessions & history
    "Save and restore full conversations with =/save= and =/load= at the prompt, or via =C-c O= → Sessions."
    "History is per-model: switching models with =C-c m= gives you a fresh context for the new model, while the old model remembers where you left off."
    "Use =/retry= to instantly resend the last prompt — handy when a response was cut short or went wrong."
    "Use =/copy= to copy the last response to the kill ring without selecting it manually."

    ;; System prompts & tone
    "Use =/system= to load any of your saved user prompts as the system prompt for the current session."
    "Use =/tone= to set the response style — options include concise, detailed, formal, casual, and more."
    "Use =/reset= to restore the default system prompt after experimenting with roles or tones."
    "Presets (roles) set a full personality via a system prompt — load one from =C-c O= → Roles, or browse with =C-c O= → =P=."

    ;; RAG
    "=M-x ollama-buddy-rag-index-directory= indexes an entire directory of source code or documents for semantic search."
    "Set =ollama-buddy-rag-embedding-base-url= to point the RAG embedding engine at a separate server like embed-rerank instead of Ollama."
    "Set =ollama-buddy-rag-embedding-api-style= to =openai= to use any =/v1/embeddings=-compatible embedding service."

    ;; OpenAI-compat provider
    "=ollama-buddy-openai-compat= lets you connect to LM Studio, llama.cpp, vLLM or any OpenAI-API-compatible server — just set =ollama-buddy-openai-compat-base-url=."
    "=M-x ollama-buddy-openai-compat-refresh-models= re-discovers models from your local server after you load or switch a model in LM Studio or Jan."

    ;; Power user
    "=ollama-buddy-command-definitions= is a plain alist — add your own menu entries with =add-to-list= in your config."
    "=C-c != toggles airplane mode, blocking all internet access so every request stays on your local machine."
    "=C-c e= switches between the built-in network-process backend and the external curl backend on the fly."
    "Store API keys in =~/.authinfo.gpg= and retrieve them with =auth-source-pick-first-password= — keep secrets out of your init file."
    "Fabric patterns (=/fabric=) and Awesome ChatGPT prompts (=/awesome=) give you hundreds of ready-made system prompts for specialist tasks.")
  "List of tip strings shown at random on the ollama-buddy welcome screen.
Each element is a plain string; org-mode markup is supported.")

(provide 'ollama-buddy-tips)
;;; ollama-buddy-tips.el ends here
