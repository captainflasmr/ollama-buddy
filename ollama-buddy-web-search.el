;;; ollama-buddy-web-search.el --- Web search integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides web search integration for the ollama-buddy package.
;; It uses Ollama's web search API to fetch current information and attach
;; the results to conversations as context.
;;
;; Usage:
;;   M-x ollama-buddy-web-search           - Search and display results
;;   M-x ollama-buddy-web-search-attach    - Search and attach to context
;;   M-x ollama-buddy-web-search-detach    - Remove search results from context
;;
;; The search results appear in the status line as 🔍N (N = number of searches).

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'shr)
(require 'dom)
(require 'ollama-buddy-core)

;; Customization

(defgroup ollama-buddy-web-search nil
  "Web search integration for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-web-search-")

(defcustom ollama-buddy-web-search-api-key ""
  "API key for Ollama web search API.
Get your key from https://ollama.com/settings/keys."
  :type 'string
  :group 'ollama-buddy-web-search)

(defcustom ollama-buddy-web-search-api-endpoint "https://ollama.com/api/web_search"
  "Endpoint for Ollama web search API."
  :type 'string
  :group 'ollama-buddy-web-search)

(defcustom ollama-buddy-web-search-max-results 5
  "Maximum number of search results to include in context.
Lower values reduce context usage but may miss relevant information."
  :type 'integer
  :group 'ollama-buddy-web-search)

(defcustom ollama-buddy-web-search-snippet-length 500
  "Maximum characters per search result snippet.
Longer snippets provide more context but use more tokens.
Content is rendered via eww/shr for clean formatting."
  :type 'integer
  :group 'ollama-buddy-web-search)

(defcustom ollama-buddy-web-search-include-urls nil
  "Whether to include source URLs in the search context.
URLs help the model cite sources but add to token count.
Set to nil to maximize content and minimize noise."
  :type 'boolean
  :group 'ollama-buddy-web-search)

(defcustom ollama-buddy-web-search-show-token-estimate t
  "Whether to show estimated token count when attaching search results."
  :type 'boolean
  :group 'ollama-buddy-web-search)

;; Internal variables

(defvar ollama-buddy-web-search--current-results nil
  "List of current web search result attachments.
Each element is a plist with :query, :content, :results, :size, :timestamp.")

(defvar ollama-buddy-web-search--history nil
  "History of web search queries.")

;; Helper functions

(defun ollama-buddy-web-search--verify-api-key ()
  "Verify that the web search API key is set."
  (if (string-empty-p ollama-buddy-web-search-api-key)
      (progn
        (customize-variable 'ollama-buddy-web-search-api-key)
        (error "Please set your Ollama web search API key"))
    t))

(defun ollama-buddy-web-search--org-escape (text)
  "Escape org-mode special characters in TEXT.
Adds comma before lines starting with * or #+ to prevent org interpretation."
  (when text
    (replace-regexp-in-string "^\\([*#]\\)" ",\\1" text)))


(defun ollama-buddy-web-search--fetch-url-with-shr (url)
  "Fetch URL and render clean text using eww/shr."
  (condition-case err
      (with-temp-buffer
        (url-insert-file-contents url)
        (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
               ;; Try to find main content area
               (main (or (car (dom-by-tag dom 'article))
                         (car (dom-by-tag dom 'main))
                         (car (dom-by-class dom "content"))
                         (car (dom-by-class dom "main"))
                         dom)))
          (erase-buffer)
          (let ((shr-width 80)
                (shr-use-fonts nil))
            (shr-insert-document main))
          (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (error
     (message "Failed to fetch %s: %s" url (error-message-string err))
     nil)))

(defun ollama-buddy-web-search--get-result-content (result)
  "Get content for RESULT by fetching URL via eww/shr."
  (let ((url (or (alist-get 'url result)
                 (alist-get 'link result)
                 "")))
    (if (not (string-empty-p url))
        (progn
          (message "Fetching %s..." url)
          (or (ollama-buddy-web-search--fetch-url-with-shr url) ""))
      "")))

(defun ollama-buddy-web-search--format-results (results query)
  "Format search RESULTS for QUERY as context string."
  (let ((formatted-results
         (mapconcat
          (lambda (result)
            (let* ((title (or (alist-get 'title result) "Untitled"))
                   (content (ollama-buddy-web-search--get-result-content result))
                   (url (or (alist-get 'url result)
                            (alist-get 'link result)
                            ""))
                   (snippet (truncate-string-to-width
                             (or content "") ollama-buddy-web-search-snippet-length)))
              (concat
               (format "** %s\n" title)
               (when (and ollama-buddy-web-search-include-urls
                          (not (string-empty-p url)))
                 (format ":PROPERTIES:\n:URL: %s\n:END:\n" url))
               "#+begin_example\n"
               (ollama-buddy-web-search--org-escape snippet)
               "\n#+end_example")))
          results
          "\n\n")))
    (format "* Web search: \"%s\"\n\n%s" query formatted-results)))

(defun ollama-buddy-web-search--estimate-tokens (text)
  "Estimate token count for TEXT."
  ;; Rough estimate: ~1.3 tokens per word for English
  (round (* 1.3 (length (split-string text)))))

(defun ollama-buddy-web-search--fetch (query callback)
  "Fetch web search results for QUERY asynchronously.
CALLBACK is called with (success results-or-error)."
  (when (ollama-buddy-web-search--verify-api-key)
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " ollama-buddy-web-search-api-key))))
           (payload (json-encode `((query . ,query)
                                  (max_results . ,ollama-buddy-web-search-max-results))))
           (url-request-data (encode-coding-string payload 'utf-8)))

      (url-retrieve
       ollama-buddy-web-search-api-endpoint
       (lambda (status)
         (if (plist-get status :error)
             (funcall callback nil (format "Request failed: %s"
                                          (prin1-to-string (plist-get status :error))))
           ;; Parse response
           (goto-char (point-min))
           (if (re-search-forward "\n\n" nil t)
               (condition-case err
                   (let* ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-key-type 'symbol)
                          (response (json-read-from-string
                                    (buffer-substring (point) (point-max))))
                          (results (or (alist-get 'results response)
                                      (alist-get 'data response)
                                      (and (listp response) response))))
                     (if results
                         (funcall callback t results)
                       (funcall callback nil "No results found")))
                 (error
                  (funcall callback nil (format "Parse error: %s"
                                               (error-message-string err)))))
             (funcall callback nil "Invalid response format"))))
       nil t t))))

(defun ollama-buddy-web-search--fetch-sync (query)
  "Fetch web search results for QUERY synchronously.
Returns (success . results-or-error)."
  (when (ollama-buddy-web-search--verify-api-key)
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " ollama-buddy-web-search-api-key))))
           (payload (json-encode `((query . ,query)
                                  (max_results . ,ollama-buddy-web-search-max-results))))
           (url-request-data (encode-coding-string payload 'utf-8)))
      (condition-case err
          (with-temp-buffer
            (url-insert-file-contents ollama-buddy-web-search-api-endpoint)
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (response (json-read))
                   (results (or (alist-get 'results response)
                               (alist-get 'data response)
                               (and (listp response) response))))
              (if results
                  (cons t results)
                (cons nil "No results found"))))
        (error
         (cons nil (format "Request failed: %s" (error-message-string err))))))))

;; Public functions

(defun ollama-buddy-web-search (query)
  "Perform a web search for QUERY and display results."
  (interactive
   (list (read-string "Web search: " nil 'ollama-buddy-web-search--history)))
  (message "Searching for: %s..." query)
  (ollama-buddy-web-search--fetch
   query
   (lambda (success result)
     (if success
         (let ((buf (get-buffer-create "*Ollama Web Search*")))
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "#+TITLE: Web Search Results\n")
               (insert (format "#+SUBTITLE: Query: %s\n" query))
               (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
               (insert (format "Found *%d* results:\n\n" (length result)))
               (let ((idx 0))
                 (dolist (item result)
                   (cl-incf idx)
                   (let* ((title (or (alist-get 'title item) "Untitled"))
                          (content (ollama-buddy-web-search--get-result-content item))
                          (snippet (truncate-string-to-width
                                    (or content "") ollama-buddy-web-search-snippet-length))
                          (url (or (alist-get 'url item)
                                   (alist-get 'link item)
                                   "")))
                     (insert (format "* %d. %s\n" idx title))
                     (when (not (string-empty-p url))
                       (insert (format ":PROPERTIES:\n:URL: %s\n:END:\n" url)))
                     (insert "#+begin_example\n")
                     (insert (ollama-buddy-web-search--org-escape snippet))
                     (insert "\n#+end_example\n\n"))))
               (goto-char (point-min))
               (org-mode)
               (view-mode 1)))
           (display-buffer buf))
       (message "Web search failed: %s" result)))))

(defun ollama-buddy-web-search-attach (query)
  "Search for QUERY and attach results to the current conversation context."
  (interactive
   (list (read-string "Web search to attach: " nil 'ollama-buddy-web-search--history)))

  (message "Searching and attaching: %s..." query)

  ;; Check if this query is already attached
  (when (cl-find query ollama-buddy-web-search--current-results
                 :test #'string= :key (lambda (r) (plist-get r :query)))
    (if (y-or-n-p (format "Search for '%s' already attached. Replace? " query))
        (ollama-buddy-web-search-detach query)
      (user-error "Search attachment cancelled")))

  (let ((result (ollama-buddy-web-search--fetch-sync query)))
    (if (car result)
        (let* ((results (cdr result))
               (limited-results (seq-take results ollama-buddy-web-search-max-results))
               (formatted-content (ollama-buddy-web-search--format-results
                                  limited-results query))
               (token-estimate (ollama-buddy-web-search--estimate-tokens formatted-content))
               (search-attachment
                (list :query query
                      :content formatted-content
                      :results limited-results
                      :size (length formatted-content)
                      :tokens token-estimate
                      :timestamp (current-time))))

          ;; Add to search results
          (push search-attachment ollama-buddy-web-search--current-results)

          ;; Update display
          (ollama-buddy--update-status
           (format "Web search attached: \"%s\""
                   (if (> (length query) 30)
                       (concat (substring query 0 27) "...")
                     query)))

          ;; Show in chat buffer
          (with-current-buffer (get-buffer-create ollama-buddy--chat-buffer)
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "\n\n- 🔍 Web search attached: \"%s\" (%d results%s)\n"
                             query
                             (length limited-results)
                             (if ollama-buddy-web-search-show-token-estimate
                                 (format ", ~%d tokens" token-estimate)
                               "")))))

          (message "Web search attached: \"%s\" (%d results, ~%d tokens)"
                   query (length limited-results) token-estimate))

      (message "Web search failed: %s" (cdr result)))))

(defun ollama-buddy-web-search-detach (&optional query)
  "Remove web search results from context.
If QUERY is provided, remove that specific search; otherwise prompt."
  (interactive
   (list (when ollama-buddy-web-search--current-results
           (completing-read "Detach web search: "
                           (mapcar (lambda (r) (plist-get r :query))
                                   ollama-buddy-web-search--current-results)
                           nil t))))
  (if (null ollama-buddy-web-search--current-results)
      (message "No web search results attached")
    (setq ollama-buddy-web-search--current-results
          (cl-remove query ollama-buddy-web-search--current-results
                     :test #'string= :key (lambda (r) (plist-get r :query))))
    (ollama-buddy--update-status
     (format "Web search detached (%d remaining)"
             (length ollama-buddy-web-search--current-results)))
    (message "Web search detached: \"%s\"" query)))

(defun ollama-buddy-web-search-clear ()
  "Clear all web search results from context."
  (interactive)
  (when (or (null ollama-buddy-web-search--current-results)
            (y-or-n-p (format "Clear all %d web search result(s)? "
                             (length ollama-buddy-web-search--current-results))))
    (setq ollama-buddy-web-search--current-results nil)
    (ollama-buddy--update-status "All web searches cleared")
    (message "All web search results cleared")))

;; Context integration functions

(defun ollama-buddy-web-search-get-context ()
  "Get formatted web search context for inclusion in prompts.
Returns nil if no searches are attached."
  (when ollama-buddy-web-search--current-results
    (concat "## Web Search Context:\n\n"
            (mapconcat
             (lambda (search)
               (plist-get search :content))
             ollama-buddy-web-search--current-results
             "\n\n---\n\n"))))

(defun ollama-buddy-web-search-count ()
  "Return the number of attached web search results."
  (length ollama-buddy-web-search--current-results))

(defun ollama-buddy-web-search-total-tokens ()
  "Return estimated total tokens for all attached web searches."
  (apply #'+ (mapcar (lambda (s) (or (plist-get s :tokens) 0))
                     ollama-buddy-web-search--current-results)))

;; Inline search delimiter support

(defconst ollama-buddy-web-search--inline-regexp
  "@search(\\([^)]+\\))"
  "Regexp to match inline search delimiters: @search(query).")

(defun ollama-buddy-web-search-extract-inline-queries (text)
  "Extract all inline search queries from TEXT.
Returns list of query strings found in /search: query/ delimiters."
  (let ((queries nil)
        (start 0))
    (while (string-match ollama-buddy-web-search--inline-regexp text start)
      (push (string-trim (match-string 1 text)) queries)
      (setq start (match-end 0)))
    (nreverse queries)))

(defun ollama-buddy-web-search-remove-inline-delimiters (text)
  "Remove inline search delimiters from TEXT, returning cleaned text."
  (replace-regexp-in-string ollama-buddy-web-search--inline-regexp "" text))

(defun ollama-buddy-web-search-process-inline (text)
  "Process TEXT for inline search queries.
Extracts /search: query/ patterns, performs searches, attaches results.
Returns the text with search delimiters removed."
  (let ((queries (ollama-buddy-web-search-extract-inline-queries text)))
    (when queries
      (dolist (query queries)
        (message "Inline web search: %s" query)
        ;; Perform synchronous search and attach
        (let ((result (ollama-buddy-web-search--fetch-sync query)))
          (if (car result)
              (let* ((results (cdr result))
                     (limited-results (seq-take results ollama-buddy-web-search-max-results))
                     (formatted-content (ollama-buddy-web-search--format-results
                                         limited-results query))
                     (token-estimate (ollama-buddy-web-search--estimate-tokens formatted-content))
                     (search-attachment
                      (list :query query
                            :content formatted-content
                            :results limited-results
                            :size (length formatted-content)
                            :tokens token-estimate
                            :timestamp (current-time))))
                ;; Add to search results
                (push search-attachment ollama-buddy-web-search--current-results)
                (message "Attached: \"%s\" (%d results, ~%d tokens)"
                         query (length limited-results) token-estimate))
            (message "Search failed for: %s" query)))))
    ;; Return text with delimiters removed
    (ollama-buddy-web-search-remove-inline-delimiters text)))

(provide 'ollama-buddy-web-search)
;;; ollama-buddy-web-search.el ends here
