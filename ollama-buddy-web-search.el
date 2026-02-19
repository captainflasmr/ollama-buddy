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
;;   Use @search(query) inline in prompts  - Automatic search and attach
;;
;; Web searches are cleared with M-x ollama-buddy-clear-attachments.
;; The search results appear in the status line as ♁N (N = number of searches).
;;
;; Content Source:
;;   Set `ollama-buddy-web-search-content-source' to control how content is retrieved:
;;   - `eww' (default): Fetch each URL and render through eww/shr for clean text
;;   - `api': Use content returned directly from Ollama API (faster, less refined)

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

(defcustom ollama-buddy-web-search-content-source 'eww
  "Source for web search result content.
Controls how content is retrieved for search results:
  `eww' - Fetch each URL and render HTML through eww/shr (default).
          Produces cleaner text but slower (multiple HTTP requests).
  `api' - Use content returned directly from Ollama API.
          Faster (no extra requests) but content may be less refined."
  :type '(choice (const :tag "Fetch URLs via eww/shr" eww)
                 (const :tag "Use Ollama API content" api))
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


(defvar ollama-buddy-web-search--last-raw-results nil
  "Store last raw API results for debugging.")

(defun ollama-buddy-web-search--get-api-content (result)
  "Extract content directly from API RESULT.
Tries common field names: content, snippet, description, text, body."
  (or (alist-get 'content result)
      (alist-get 'snippet result)
      (alist-get 'description result)
      (alist-get 'text result)
      (alist-get 'body result)
      ""))

(defun ollama-buddy-web-search-debug-last-results ()
  "Display the raw API response structure for debugging.
Shows what fields are available in the last search results."
  (interactive)
  (if ollama-buddy-web-search--last-raw-results
      (let ((buf (get-buffer-create "*Ollama Web Search Debug*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "Last raw API results:\n\n")
          (insert (pp-to-string ollama-buddy-web-search--last-raw-results))
          (goto-char (point-min)))
        (display-buffer buf))
    (message "No search results cached. Run a search first.")))

(defun ollama-buddy-web-search--render-html-to-text (html-string)
  "Render HTML-STRING to plain text using shr."
  (with-temp-buffer
    (insert html-string)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (main (or (car (dom-by-tag dom 'article))
                     (car (dom-by-tag dom 'main))
                     (car (dom-by-class dom "content"))
                     (car (dom-by-class dom "main"))
                     dom)))
      (erase-buffer)
      (let ((shr-width 80)
            (shr-use-fonts nil))
        (shr-insert-document main))
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun ollama-buddy-web-search--fetch-url-async (url callback)
  "Fetch URL asynchronously and call CALLBACK with rendered text.
CALLBACK is called with (url content) where content may be nil on error."
  (condition-case err
      (url-retrieve
       url
       (lambda (status url callback)
         (if (plist-get status :error)
             (progn
               (message "Failed to fetch %s: %s" url (plist-get status :error))
               (funcall callback url nil))
           (goto-char (point-min))
           (if (re-search-forward "\r?\n\r?\n" nil t)
               (let* ((html (buffer-substring-no-properties (point) (point-max)))
                      (content (condition-case nil
                                   (ollama-buddy-web-search--render-html-to-text html)
                                 (error nil))))
                 (funcall callback url content))
             (funcall callback url nil)))
         (kill-buffer))
       (list url callback)
       t t)
    (error
     (message "Error starting fetch for %s: %s" url (error-message-string err))
     (funcall callback url nil))))

(defun ollama-buddy-web-search--fetch-all-urls-async (results callback)
  "Fetch all URLs from RESULTS asynchronously.
CALLBACK is called with results alist mapping URLs to content when all complete."
  (let* ((urls (delq nil (mapcar (lambda (r)
                                   (or (alist-get 'url r) (alist-get 'link r)))
                                 results)))
         (total (length urls))
         (completed 0)
         (content-map (make-hash-table :test 'equal)))
    (if (= total 0)
        (funcall callback content-map)
      (message "Fetching %d URLs..." total)
      (dolist (url urls)
        (ollama-buddy-web-search--fetch-url-async
         url
         (lambda (url content)
           (puthash url (or content "") content-map)
           (cl-incf completed)
           (message "Fetched %d/%d URLs" completed total)
           (when (= completed total)
             (funcall callback content-map))))))))

(defun ollama-buddy-web-search--get-result-content-from-map (result content-map)
  "Get content for RESULT from pre-fetched CONTENT-MAP."
  (let ((url (or (alist-get 'url result)
                 (alist-get 'link result)
                 "")))
    (if (not (string-empty-p url))
        (or (gethash url content-map) "")
      "")))

(defun ollama-buddy-web-search--fetch-url-sync (url)
  "Fetch URL synchronously and return rendered text."
  (condition-case err
      (with-temp-buffer
        (url-insert-file-contents url)
        (let ((html (buffer-substring-no-properties (point-min) (point-max))))
          (ollama-buddy-web-search--render-html-to-text html)))
    (error
     (message "Failed to fetch %s: %s" url (error-message-string err))
     "")))

(defun ollama-buddy-web-search--fetch-all-urls-sync (results)
  "Fetch all URLs from RESULTS synchronously.
Returns a hash-table mapping URLs to content."
  (let ((content-map (make-hash-table :test 'equal))
        (urls (delq nil (mapcar (lambda (r)
                                  (or (alist-get 'url r) (alist-get 'link r)))
                                results)))
        (idx 0)
        (total (length results)))
    (dolist (url urls)
      (cl-incf idx)
      (message "Fetching URL %d/%d..." idx total)
      (puthash url (ollama-buddy-web-search--fetch-url-sync url) content-map))
    content-map))

(defun ollama-buddy-web-search--format-results (results query &optional content-map)
  "Format search RESULTS for QUERY as context string.
CONTENT-MAP is a hash-table mapping URLs to pre-fetched content.
When `ollama-buddy-web-search-content-source' is `api', content-map is ignored
and content is taken directly from the API results."
  (let ((formatted-results
         (mapconcat
          (lambda (result)
            (let* ((title (or (alist-get 'title result) "Untitled"))
                   (content (if (eq ollama-buddy-web-search-content-source 'api)
                                (ollama-buddy-web-search--get-api-content result)
                              (if content-map
                                  (ollama-buddy-web-search--get-result-content-from-map result content-map)
                                "")))
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
  (if (bound-and-true-p ollama-buddy-airplane-mode)
      (message "✈ Airplane mode is active — web search is disabled")
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
                     ;; Store raw results for debugging
                     (setq ollama-buddy-web-search--last-raw-results results)
                     (if results
                         (funcall callback t results)
                       (funcall callback nil "No results found")))
                 (error
                  (funcall callback nil (format "Parse error: %s"
                                               (error-message-string err)))))
             (funcall callback nil "Invalid response format"))))
       nil t t)))))

(defun ollama-buddy-web-search--fetch-sync (query)
  "Fetch web search results for QUERY synchronously.
Returns (success . results-or-error)."
  (if (bound-and-true-p ollama-buddy-airplane-mode)
      (message "✈ Airplane mode is active — web search is disabled")
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
                ;; Store raw results for debugging
                (setq ollama-buddy-web-search--last-raw-results results)
                (if results
                    (cons t results)
                  (cons nil "No results found"))))
          (error
           (cons nil (format "Request failed: %s" (error-message-string err)))))))))

;; Public functions

(defun ollama-buddy-web-search--display-results (result query &optional content-map)
  "Display search RESULT for QUERY in a buffer.
CONTENT-MAP is used when `ollama-buddy-web-search-content-source' is `eww'."
  (let ((buf (get-buffer-create "*Ollama Web Search*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: Web Search Results\n")
        (insert (format "#+SUBTITLE: Query: %s\n" query))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "Found *%d* results (content source: %s):\n\n"
                        (length result) ollama-buddy-web-search-content-source))
        (let ((idx 0))
          (dolist (item result)
            (cl-incf idx)
            (let* ((title (or (alist-get 'title item) "Untitled"))
                   (content (if (eq ollama-buddy-web-search-content-source 'api)
                                (ollama-buddy-web-search--get-api-content item)
                              (ollama-buddy-web-search--get-result-content-from-map
                               item content-map)))
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
    (display-buffer buf)
    (message "Web search complete: %d results" (length result))))

(defun ollama-buddy-web-search (query)
  "Perform a web search for QUERY and display results."
  (interactive
   (list (read-string "Web search: " nil 'ollama-buddy-web-search--history)))
  (message "Searching for: %s..." query)
  (ollama-buddy-web-search--fetch
   query
   (lambda (success result)
     (if success
         (if (eq ollama-buddy-web-search-content-source 'api)
             ;; Use API content directly - no URL fetching needed
             (ollama-buddy-web-search--display-results result query)
           ;; Fetch all URLs asynchronously, then display
           (ollama-buddy-web-search--fetch-all-urls-async
            result
            (lambda (content-map)
              (ollama-buddy-web-search--display-results result query content-map))))
       (message "Web search failed: %s" result)))))

(defun ollama-buddy-web-search--attach-results (results query &optional content-map)
  "Attach search RESULTS for QUERY to the conversation context.
CONTENT-MAP is used when `ollama-buddy-web-search-content-source' is `eww'."
  (let* ((formatted-content (ollama-buddy-web-search--format-results
                             results query content-map))
         (token-estimate (ollama-buddy-web-search--estimate-tokens formatted-content))
         ;; Convert hash-table to alist for storage (if content-map provided)
         (content-alist (when content-map
                          (let (pairs)
                            (maphash (lambda (k v) (push (cons k v) pairs)) content-map)
                            pairs)))
         (search-attachment
          (list :query query
                :content formatted-content
                :results results
                :content-map content-alist
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
        (insert (format "\n\n- ♁ Web search attached: \"%s\" (%d results%s)\n"
                        query
                        (length results)
                        (if ollama-buddy-web-search-show-token-estimate
                            (format ", ~%d tokens" token-estimate)
                          "")))))

    (message "Web search attached: \"%s\" (%d results, ~%d tokens)"
             query (length results) token-estimate)))

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

  ;; Async fetch search results, then optionally fetch URLs, then attach
  (ollama-buddy-web-search--fetch
   query
   (lambda (success result)
     (if success
         (let ((limited-results (seq-take result ollama-buddy-web-search-max-results)))
           (if (eq ollama-buddy-web-search-content-source 'api)
               ;; Use API content directly - no URL fetching needed
               (ollama-buddy-web-search--attach-results limited-results query nil)
             ;; Fetch all URLs asynchronously
             (ollama-buddy-web-search--fetch-all-urls-async
              limited-results
              (lambda (content-map)
                (ollama-buddy-web-search--attach-results limited-results query content-map)))))
       (message "Web search failed: %s" result)))))

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

(defun ollama-buddy-web-search-detach (query)
  "Remove the attached web search result for QUERY."
  (setq ollama-buddy-web-search--current-results
        (cl-remove-if (lambda (r) (string= query (plist-get r :query)))
                      ollama-buddy-web-search--current-results)))

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
  "Replace inline search delimiters with just the query text.
@search(query) becomes query, preserving the search text in the prompt."
  (replace-regexp-in-string ollama-buddy-web-search--inline-regexp "\\1" text))

(defun ollama-buddy-web-search-process-inline (text)
  "Process TEXT for inline search queries.
Extracts @search(query) patterns, performs searches, attaches results.
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
                     ;; Only fetch URL content if using eww source
                     (content-map (unless (eq ollama-buddy-web-search-content-source 'api)
                                    (ollama-buddy-web-search--fetch-all-urls-sync limited-results)))
                     (formatted-content (ollama-buddy-web-search--format-results
                                         limited-results query content-map))
                     (token-estimate (ollama-buddy-web-search--estimate-tokens formatted-content))
                     ;; Convert hash-table to alist for storage
                     (content-alist (when content-map
                                      (let (pairs)
                                        (maphash (lambda (k v) (push (cons k v) pairs)) content-map)
                                        pairs)))
                     (search-attachment
                      (list :query query
                            :content formatted-content
                            :results limited-results
                            :content-map content-alist
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
