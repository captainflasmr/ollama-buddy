;;; ollama-buddy-tips.el --- Tips for the ollama-buddy welcome screen -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "29.1"))

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
    "Thinking tokens stream under a folded =*** Thinking= heading — press =TAB= mid-stream to peek at the reasoning so far, =TAB= again to re-fold."
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
    "Press =C-u C-u C-c C-c= on a prompt heading to rewind the conversation to that point — history is truncated and the prompt is pre-filled for resending."
    "Use =S-TAB= to fold the buffer to just prompt headings, navigate to the one you want, then =C-u C-u C-c C-c= to rewind."

    ;; System prompts & tone
    "Use =/system= to load any of your saved user prompts as the system prompt for the current session."
    "Use =/tone= to set the response style — options include concise, detailed, formal, casual, and more."
    "Use =/reset= to restore the default system prompt after experimenting with roles or tones."
    "Presets (roles) set a full personality via a system prompt — load one from =C-c O= → Roles, or browse with =C-c O= → =P=."

    ;; RAG
    "=M-x ollama-buddy-rag-index-or-update-directory= indexes a directory for semantic search, or incrementally updates an existing index."
    "Set =ollama-buddy-rag-embedding-base-url= to point the RAG embedding engine at a separate server like embed-rerank instead of Ollama."
    "Set =ollama-buddy-rag-embedding-api-style= to =openai= to use any =/v1/embeddings=-compatible embedding service."

    ;; OpenAI-compat provider
    "=ollama-buddy-openai-compat= lets you connect to LM Studio, llama.cpp, vLLM or any OpenAI-API-compatible server — just set =ollama-buddy-openai-compat-base-url=."
    "=M-x ollama-buddy-openai-compat-refresh-models= re-discovers models from your local server after you load or switch a model in LM Studio or Jan."

    ;; Tool calling
    "=C-c Q= lists all available tools with descriptions — useful when asking a model to use a specific tool."
    "=C-c E= toggles tool auto-execute (=⚡=): safe tools run without confirmation, but unsafe tools like =execute_shell= always ask first."
    "Enable =ollama-buddy-tools-safe-mode= to restrict tool calling to read-only operations only — no file writes or shell commands."
    "When a tool returns a large result, you get a =(p)roceed/(t)runcate/(c)ancel= prompt — even during auto-execute — so huge outputs never silently flood the context."
    "Ask a tool-capable model to edit a file and it can use =propose_file_changes= to open an ediff session for interactive review before applying changes."

    ;; Model comparison & benchmarking
    "=C-c U= sends the same prompt to multiple models one after another — great for comparing how different models handle the same question."
    "=C-c u= or =/benchmark= runs a performance benchmark across selected models so you can compare speed and quality."

    ;; Web search
    "=C-c /= opens the web search menu: search and display results, search and attach to context, or clear search attachments."

    ;; Model management
    "=C-c M= opens the Model Management buffer — view installed models, pull new ones, copy, or delete models in one place."
    "=C-c L= shows a curated list of recommended models to try — a quick way to discover new models."
    "=C-c l= pulls a model directly from the Ollama hub without leaving Emacs."

    ;; Parameters
    "=C-c p= opens the parameter menu where you can tweak temperature, top_k, top_p, repeat_penalty and many more generation settings."
    "Quick parameter profiles: the parameter menu offers Default, Creative and Precise presets to switch generation style in one step."
    "=C-c G= displays all current parameter settings at a glance; =C-c V= resets them all to defaults."
    "=C-c F= toggles showing active parameters in the header line — see at a glance when you have non-default settings."

    ;; Context window
    "=C-c C= shows detailed context usage: how much of the model's context window is consumed by history, system prompt, and attachments."
    "=C-c %= toggles a context percentage display in the header — watch it grow as the conversation gets longer."
    "=C-c $= lets you set the context window size for the current model — useful when the default is too small for long conversations."

    ;; File attachments
    "=C-c C-a= attaches a file to the conversation context; =C-c C-d= detaches a specific file; =C-c C-w= shows all current attachments."
    "In Dired, mark files then run =M-x ollama-buddy-dired-attach-marked-files= to attach them all to the chat in one go."

    ;; Sessions
    "=C-c N= starts a fresh session; =C-c w= or =/rename= lets you give the current session a meaningful name."
    "=C-c Z= opens the sessions directory in a file manager — useful for browsing or cleaning up old saved conversations."

    ;; Display & settings
    "=C-c += opens the settings menu — debug mode, token stats, context info, markdown conversion and more, all in one transient."
    "=C-c #= displays token statistics for the current conversation: total tokens, rate and timing information."
    "=C-c C-o= toggles markdown-to-org conversion — turn it off to see raw markdown, or on to get proper org formatting."
    "=C-c g= toggles auto-scroll so the buffer follows streaming output as it arrives."
    "=C-c <= toggles global system prompt — when on, the same system prompt persists across model switches."

    ;; History
    "=M-r= searches your prompt history with completing-read — faster than =M-p= when you know what you are looking for."
    "=C-c H= toggles history tracking on or off; =C-c X= clears all history for the current model."

    ;; Authentication
    "=C-c A= opens the authentication menu — manage Ollama cloud login, GitHub Copilot tokens and other provider auth from one place."
    "Use =/login= and =/logout= at the prompt to sign in and out of Ollama cloud without touching the transient menu."

    ;; Project integration
    "=C-c P= opens the project menu — switch directories, attach project files, or view project info."
    "Use =/cd= to switch the working directory mid-session and automatically load that project's context."

    ;; Export & misc
    "Use =/export= to open the org-export dispatcher on the chat buffer — export your conversation to HTML, PDF, LaTeX or plain text."
    "The chat buffer is a full org-mode buffer — you can use any org commands: =C-c C-e= to export, =C-c C-l= to insert links, =C-c '= to edit source blocks."
    "=C-c C-s= shows the current system prompt info — useful to check what persona or instructions are active."
    "=C-c K= exits the chat session with confirmation — a clean way to close when you are done."

    ;; Power user
    "=ollama-buddy-command-definitions= is a plain alist — add your own menu entries with =add-to-list= in your config."
    "=C-c != toggles airplane mode, blocking all internet access so every request stays on your local machine."
    "=C-c e= switches between the built-in network-process backend and the external curl backend on the fly."
    "Store API keys in =~/.authinfo.gpg= and retrieve them with =auth-source-pick-first-password= — keep secrets out of your init file."
    "Use =/init= in a project to generate and cache a project summary — it auto-loads as context in future sessions so the model already knows your codebase."
    "=C-c j= jumps to any prompt in the conversation — handy for navigating long chat sessions. Also works via =M-x imenu=."
    "=C-c C-j= (=org-goto=) lets you browse and jump to any heading in the chat buffer — prompts, responses, and thinking blocks."
    "=C-c b= opens the role transient menu — switch between character personas with dedicated menus and keybindings."
    "=C-c v= sets the keep-alive duration — control how long Ollama keeps a model loaded in memory after the last request."
    "Press =C-c .= as a shortcut for =C-c O= — both open the main transient menu.")
  "List of tip strings shown at random on the ollama-buddy welcome screen.
Each element is a plain string; org-mode markup is supported.")

(provide 'ollama-buddy-tips)
;;; ollama-buddy-tips.el ends here
