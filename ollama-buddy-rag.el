;;; ollama-buddy-rag.el --- RAG integration for ollama-buddy -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/ollama-buddy
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; This extension provides Retrieval-Augmented Generation (RAG) support for
;; the ollama-buddy package.  It allows indexing local documents and
;; retrieving relevant context to augment LLM prompts.
;;
;; Features:
;; - Index directories of source code, org files, markdown, etc.
;; - Chunk documents with configurable size and overlap
;; - Generate embeddings using Ollama's /api/embed endpoint
;; - Cosine similarity search for relevant document chunks
;; - Attach retrieved context to chat conversations
;;
;; Usage:
;;   M-x ollama-buddy-rag-index-directory  - Index files in a directory
;;   M-x ollama-buddy-rag-search           - Search and display results
;;   M-x ollama-buddy-rag-attach           - Search and attach to context
;;   M-x ollama-buddy-rag-list-indexes     - List available indexes
;;
;; RAG context is cleared with M-x ollama-buddy-clear-attachments or
;; M-x ollama-buddy-rag-clear-attached.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'ollama-buddy-core)

(declare-function ollama-buddy--register-background-operation "ollama-buddy-core")
(declare-function ollama-buddy--complete-background-operation "ollama-buddy-core")
(declare-function ollama-buddy--update-background-operation "ollama-buddy-core")

;;; Customization

(defgroup ollama-buddy-rag nil
  "RAG (Retrieval-Augmented Generation) settings for Ollama Buddy."
  :group 'ollama-buddy
  :prefix "ollama-buddy-rag-")

(defcustom ollama-buddy-rag-embedding-model "nomic-embed-text"
  "Ollama model to use for generating embeddings.
Popular options include:
- nomic-embed-text (768 dimensions, good general purpose)
- mxbai-embed-large (1024 dimensions, higher quality)
- all-minilm (384 dimensions, smaller/faster)"
  :type 'string
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-chunk-size 400
  "Target chunk size in words.
Roughly corresponds to ~500 tokens.  Smaller chunks are more
precise but may lose context.  Larger chunks preserve more
context but may dilute relevance."
  :type 'integer
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-chunk-overlap 50
  "Number of words to overlap between consecutive chunks.
Overlap helps preserve context across chunk boundaries."
  :type 'integer
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-top-k 5
  "Number of results to return from similarity search."
  :type 'integer
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-similarity-threshold 0.3
  "Minimum cosine similarity score (0.0-1.0) for results.
Results below this threshold are filtered out."
  :type 'float
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-index-directory
  (expand-file-name "ollama-buddy/rag-indexes" user-emacs-directory)
  "Directory to store RAG index files."
  :type 'directory
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-file-extensions
  '("el" "py" "js" "ts" "tsx" "jsx" "org" "md" "txt" "rs" "go" "c" "cpp" "h"
    "hpp" "java" "rb" "php" "swift" "kt" "scala" "clj" "hs" "ml" "r" "sql"
    "sh" "bash" "zsh" "yaml" "yml" "toml" "json" "xml" "html" "css" "scss")
  "File extensions to include when indexing directories.
Files with other extensions are skipped."
  :type '(repeat string)
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-max-file-size (* 1024 1024)
  "Maximum file size to index in bytes.
Files larger than this are skipped.  Default is 1MB."
  :type 'integer
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-show-token-estimate t
  "Whether to show estimated token count when attaching RAG results."
  :type 'boolean
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-batch-size 10
  "Number of chunks to embed in a single API call.
Larger batches are faster but use more memory."
  :type 'integer
  :group 'ollama-buddy-rag)

(defcustom ollama-buddy-rag-exclude-patterns
  '(".git" "node_modules" "__pycache__" ".venv" "venv" ".tox"
    "build" "dist" "target" ".cache" ".eggs" "*.egg-info")
  "Directory and file patterns to exclude from indexing.
Supports glob patterns."
  :type '(repeat string)
  :group 'ollama-buddy-rag)

;;; Internal Variables

(defvar ollama-buddy-rag--current-results nil
  "List of attached RAG search results.
Each element is a plist with keys:
  :query       - The search query string
  :index-name  - Name of the index searched
  :results     - List of matching chunks with scores
  :content     - Formatted content for prompt inclusion
  :tokens      - Estimated token count
  :timestamp   - Time of search")

(defvar ollama-buddy-rag--indexes (make-hash-table :test 'equal)
  "Cache of loaded indexes.
Keys are index names, values are index data plists.")

(defvar ollama-buddy-rag--search-history nil
  "History of RAG search queries.")

(defconst ollama-buddy-rag--version 1
  "Current version of the RAG index format.")

;;; Utility Functions

(defun ollama-buddy-rag--estimate-tokens (text)
  "Estimate token count for TEXT.
Uses roughly 1.3 tokens per word as approximation."
  (round (* 1.3 (length (split-string text)))))

(defun ollama-buddy-rag--generate-chunk-id ()
  "Generate a unique chunk ID."
  (format "%s-%s"
          (format-time-string "%Y%m%d%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 8)))

(defun ollama-buddy-rag--index-name-from-path (path)
  "Generate unique index name from PATH."
  (let* ((expanded (expand-file-name path))
         (name (file-name-nondirectory (directory-file-name expanded)))
         (hash (substring (md5 expanded) 0 8)))
    (format "%s-%s" name hash)))

(defun ollama-buddy-rag--index-file-path (index-name)
  "Return file path for INDEX-NAME."
  (expand-file-name (concat index-name ".rag")
                    ollama-buddy-rag-index-directory))

(defun ollama-buddy-rag--ensure-index-directory ()
  "Ensure the index directory exists."
  (unless (file-directory-p ollama-buddy-rag-index-directory)
    (make-directory ollama-buddy-rag-index-directory t)))

(defun ollama-buddy-rag--excluded-p (path)
  "Check if PATH matches any exclude pattern."
  (let ((name (file-name-nondirectory path)))
    (cl-some (lambda (pattern)
               (or (string-match-p (wildcard-to-regexp pattern) name)
                   (string-match-p (wildcard-to-regexp pattern) path)))
             ollama-buddy-rag-exclude-patterns)))

(defun ollama-buddy-rag--file-extension-p (file)
  "Check if FILE has an indexable extension."
  (let ((ext (file-name-extension file)))
    (and ext (member (downcase ext) ollama-buddy-rag-file-extensions))))

;;; Embedding Model Validation

(defun ollama-buddy-rag--embedding-model-available-p ()
  "Check if the configured embedding model is available in Ollama.
Returns non-nil if the model is pulled and ready."
  (condition-case nil
      (let* ((url-request-method "GET")
             (url (format "http://%s:%d/api/tags" ollama-buddy-host ollama-buddy-port)))
        (with-temp-buffer
          (url-insert-file-contents url)
          (goto-char (point-min))
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (response (json-read))
                 (models (alist-get 'models response)))
            (cl-some (lambda (m)
                       (string-prefix-p ollama-buddy-rag-embedding-model
                                        (alist-get 'name m)))
                     models))))
    (error nil)))

(defun ollama-buddy-rag--ensure-embedding-model ()
  "Signal an error if the embedding model is not available."
  (unless (ollama-buddy-rag--embedding-model-available-p)
    (user-error "Embedding model '%s' is not available. Run: ollama pull %s"
                ollama-buddy-rag-embedding-model
                ollama-buddy-rag-embedding-model)))

;;; Embedding API Functions

(defun ollama-buddy-rag--get-embedding-async (text callback)
  "Get embedding vector for TEXT using Ollama API asynchronously.
Calls CALLBACK with the embedding vector or nil on error."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (payload (json-encode `((model . ,ollama-buddy-rag-embedding-model)
                                 (input . ,text))))
         (url-request-data (encode-coding-string payload 'utf-8))
         (url (format "http://%s:%d/api/embed" ollama-buddy-host ollama-buddy-port)))
    (url-retrieve
     url
     (lambda (status cb)
       (if (plist-get status :error)
           (progn
             (message "Embedding error: %s" (plist-get status :error))
             (funcall cb nil))
         (condition-case err
             (progn
               (goto-char (point-min))
               (re-search-forward "\n\n")
               (let* ((json-object-type 'alist)
                      (json-array-type 'list)
                      (response (json-read))
                      (embeddings (alist-get 'embeddings response)))
                 (funcall cb (car embeddings))))
           (error
            (message "Embedding parse error: %s" (error-message-string err))
            (funcall cb nil)))))
     (list callback)
     t)))

(defun ollama-buddy-rag--get-embeddings-batch-async (texts callback)
  "Get embeddings for multiple TEXTS in one API call asynchronously.
Calls CALLBACK with list of embedding vectors or nil on error."
  (if (null texts)
      (funcall callback nil)
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (payload (json-encode `((model . ,ollama-buddy-rag-embedding-model)
                                   (input . ,(vconcat texts)))))
           (url-request-data (encode-coding-string payload 'utf-8))
           (url (format "http://%s:%d/api/embed" ollama-buddy-host ollama-buddy-port)))
      (url-retrieve
       url
       (lambda (status cb)
         (if (plist-get status :error)
             (progn
               (message "Batch embedding error: %s" (plist-get status :error))
               (funcall cb nil))
           (condition-case err
               (progn
                 (goto-char (point-min))
                 (re-search-forward "\n\n")
                 (let* ((json-object-type 'alist)
                        (json-array-type 'list)
                        (response (json-read)))
                   (funcall cb (alist-get 'embeddings response))))
             (error
              (message "Batch embedding parse error: %s" (error-message-string err))
              (funcall cb nil)))))
       (list callback)
       t))))

;;; Chunking Functions

(defun ollama-buddy-rag--split-into-words (text)
  "Split TEXT into words, preserving positions."
  (let ((words nil)
        (start 0))
    (while (string-match "\\S-+" text start)
      (push (list :word (match-string 0 text)
                  :start (match-beginning 0)
                  :end (match-end 0))
            words)
      (setq start (match-end 0)))
    (nreverse words)))

(defun ollama-buddy-rag--chunk-text (text)
  "Split TEXT into overlapping chunks.
Returns list of plists with :content :start-pos :end-pos."
  (let* ((words (ollama-buddy-rag--split-into-words text))
         (total-words (length words))
         (chunk-size ollama-buddy-rag-chunk-size)
         (overlap ollama-buddy-rag-chunk-overlap)
         (step (max 1 (- chunk-size overlap)))
         (chunks nil)
         (i 0))
    (while (< i total-words)
      (let* ((end-idx (min (+ i chunk-size) total-words))
             (chunk-words (cl-subseq words i end-idx))
             (start-pos (plist-get (car chunk-words) :start))
             (end-pos (plist-get (car (last chunk-words)) :end))
             (content (substring text start-pos end-pos)))
        (push (list :content content
                    :start-pos start-pos
                    :end-pos end-pos
                    :word-count (length chunk-words))
              chunks)
        (setq i (+ i step))))
    (nreverse chunks)))

(defun ollama-buddy-rag--count-lines-to-pos (text pos)
  "Count line number at POS in TEXT."
  (1+ (cl-count ?\n (substring text 0 (min pos (length text))))))

(defun ollama-buddy-rag--chunk-file (filepath)
  "Read and chunk FILEPATH.
Returns list of chunk plists with file metadata, or nil on error."
  (condition-case err
      (when (and (file-readable-p filepath)
                 (< (file-attribute-size (file-attributes filepath))
                    ollama-buddy-rag-max-file-size))
        (let* ((content (with-temp-buffer
                          (insert-file-contents filepath)
                          (buffer-string)))
               (chunks (ollama-buddy-rag--chunk-text content)))
          (mapcar (lambda (chunk)
                    (let ((start-pos (plist-get chunk :start-pos))
                          (end-pos (plist-get chunk :end-pos)))
                      (list :id (ollama-buddy-rag--generate-chunk-id)
                            :file filepath
                            :start-pos start-pos
                            :end-pos end-pos
                            :line-start (ollama-buddy-rag--count-lines-to-pos content start-pos)
                            :line-end (ollama-buddy-rag--count-lines-to-pos content end-pos)
                            :content (plist-get chunk :content)
                            :word-count (plist-get chunk :word-count))))
                  chunks)))
    (error
     (message "Error chunking %s: %s" filepath (error-message-string err))
     nil)))

;;; Vector Math Functions

(defun ollama-buddy-rag--dot-product (vec1 vec2)
  "Calculate dot product of VEC1 and VEC2."
  (cl-reduce #'+ (cl-mapcar #'* vec1 vec2)))

(defun ollama-buddy-rag--magnitude (vec)
  "Calculate magnitude (L2 norm) of VEC."
  (sqrt (cl-reduce #'+ (mapcar (lambda (x) (* x x)) vec))))

(defun ollama-buddy-rag--cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between VEC1 and VEC2.
Returns a value between -1 and 1, where 1 means identical."
  (let ((mag1 (ollama-buddy-rag--magnitude vec1))
        (mag2 (ollama-buddy-rag--magnitude vec2)))
    (if (or (zerop mag1) (zerop mag2))
        0.0
      (/ (ollama-buddy-rag--dot-product vec1 vec2)
         (* mag1 mag2)))))

(defun ollama-buddy-rag--search-chunks (query-embedding chunks top-k threshold)
  "Search CHUNKS for vectors similar to QUERY-EMBEDDING.
Returns top TOP-K results with similarity above THRESHOLD.
Each result is a plist with the original chunk data plus :score."
  (let* ((scored (mapcar
                  (lambda (chunk)
                    (let ((embedding (plist-get chunk :embedding)))
                      (when embedding
                        (cons (ollama-buddy-rag--cosine-similarity
                               query-embedding embedding)
                              chunk))))
                  chunks))
         (filtered (cl-remove-if-not
                    (lambda (item)
                      (and item (>= (car item) threshold)))
                    scored))
         (sorted (sort filtered (lambda (a b) (> (car a) (car b)))))
         (top-results (seq-take sorted top-k)))
    (mapcar (lambda (item)
              (append (list :score (car item)) (cdr item)))
            top-results)))

;;; Index Management Functions

(defun ollama-buddy-rag--save-index (index)
  "Save INDEX to disk."
  (ollama-buddy-rag--ensure-index-directory)
  (let* ((name (plist-get index :name))
         (filepath (ollama-buddy-rag--index-file-path name)))
    (with-temp-file filepath
      (let ((print-length nil)
            (print-level nil))
        (prin1 index (current-buffer))))
    (message "Index saved: %s" filepath)))

(defun ollama-buddy-rag--load-index (index-name)
  "Load index INDEX-NAME from disk.
Returns index plist or nil if not found."
  ;; Check cache first
  (or (gethash index-name ollama-buddy-rag--indexes)
      (let ((filepath (ollama-buddy-rag--index-file-path index-name)))
        (when (file-exists-p filepath)
          (condition-case err
              (with-temp-buffer
                (insert-file-contents filepath)
                (let ((index (read (current-buffer))))
                  ;; Cache the loaded index
                  (puthash index-name index ollama-buddy-rag--indexes)
                  index))
            (error
             (message "Error loading index %s: %s" index-name (error-message-string err))
             nil))))))

(defun ollama-buddy-rag--list-index-names ()
  "Return list of available index names."
  (ollama-buddy-rag--ensure-index-directory)
  (let ((files (directory-files ollama-buddy-rag-index-directory nil "\\.rag$")))
    (mapcar (lambda (f) (file-name-sans-extension f)) files)))

(defun ollama-buddy-rag--delete-index-file (index-name)
  "Delete the index file for INDEX-NAME."
  (let ((filepath (ollama-buddy-rag--index-file-path index-name)))
    (when (file-exists-p filepath)
      (delete-file filepath)
      (remhash index-name ollama-buddy-rag--indexes)
      t)))

;;; Formatting Functions

(defun ollama-buddy-rag--format-chunk-for-display (chunk &optional index)
  "Format CHUNK for display, optionally with INDEX number."
  (let ((file (plist-get chunk :file))
        (line-start (plist-get chunk :line-start))
        (line-end (plist-get chunk :line-end))
        (score (plist-get chunk :score))
        (content (plist-get chunk :content)))
    (concat
     (when index (format "** Result %d" index))
     (when score (format " (similarity: %.2f)" score))
     "\n"
     (format ":PROPERTIES:\n:FILE: %s\n:LINES: %d-%d\n:END:\n"
             (or file "unknown") (or line-start 0) (or line-end 0))
     "#+begin_example\n"
     (replace-regexp-in-string
      "^\\*" ",*"
      (truncate-string-to-width (or content "") 1000 nil nil "..."))
     "\n#+end_example\n")))

(defun ollama-buddy-rag--format-results-for-context (results query index-name)
  "Format search RESULTS for QUERY from INDEX-NAME for prompt context."
  (concat
   (format "* RAG Search: \"%s\" (from %s)\n\n" query index-name)
   (mapconcat
    (lambda (result)
      (let ((file (plist-get result :file))
            (line-start (plist-get result :line-start))
            (content (plist-get result :content)))
        (format "** %s:%d\n#+begin_example\n%s\n#+end_example\n"
                (file-name-nondirectory (or file "unknown"))
                (or line-start 0)
                (truncate-string-to-width (or content "") 800 nil nil "..."))))
    results
    "\n")))

;;; Async Batch Processing

(defvar ollama-buddy-rag--indexing-in-progress nil
  "Non-nil when an indexing operation is in progress.")

(defun ollama-buddy-rag--process-batches (all-chunks batch-size batch-num total-batches
                                                     index-name dir file-count chunk-count
                                                     operation-id)
  "Process embedding batches asynchronously.
Embeds chunks in ALL-CHUNKS starting from BATCH-NUM of TOTAL-BATCHES.
INDEX-NAME, DIR, FILE-COUNT, CHUNK-COUNT are used for the final index.
OPERATION-ID tracks this in the status line."
  (let* ((i (* batch-num batch-size))
         (batch-end (min (+ i batch-size) chunk-count))
         (batch-chunks (cl-subseq all-chunks i batch-end))
         (texts (mapcar (lambda (c) (plist-get c :content)) batch-chunks)))
    (ollama-buddy--update-background-operation
     operation-id
     (format "RAG indexing %d/%d" (1+ batch-num) total-batches))
    (ollama-buddy-rag--get-embeddings-batch-async
     texts
     (lambda (embeddings)
       (when embeddings
         (cl-loop for chunk in batch-chunks
                  for emb in embeddings
                  do (nconc chunk (list :embedding emb))))
       (let ((next-batch (1+ batch-num)))
         (if (< next-batch total-batches)
             ;; Process next batch
             (ollama-buddy-rag--process-batches
              all-chunks batch-size next-batch total-batches
              index-name dir file-count chunk-count operation-id)
           ;; All batches done - save index
           (let ((index (list :version ollama-buddy-rag--version
                              :name index-name
                              :source-path dir
                              :embedding-model ollama-buddy-rag-embedding-model
                              :created (format-time-string "%Y-%m-%dT%H:%M:%S")
                              :file-count file-count
                              :chunk-count chunk-count
                              :chunks all-chunks)))
             (ollama-buddy-rag--save-index index)
             (puthash index-name index ollama-buddy-rag--indexes)
             (setq ollama-buddy-rag--indexing-in-progress nil)
             (ollama-buddy--complete-background-operation
              operation-id
              (format "RAG indexed: %d files, %d chunks" file-count chunk-count)))))))))

;;; Public Commands

;;;###autoload
(defun ollama-buddy-rag-index-directory (directory)
  "Index all matching files in DIRECTORY for RAG retrieval.
Files are chunked, then embedded asynchronously via Ollama."
  (interactive "DDirectory to index: ")
  (ollama-buddy-rag--ensure-embedding-model)
  (when ollama-buddy-rag--indexing-in-progress
    (user-error "An indexing operation is already in progress"))
  (let* ((dir (expand-file-name directory))
         (index-name (ollama-buddy-rag--index-name-from-path dir))
         (files (directory-files-recursively
                 dir
                 (concat "\\." (regexp-opt ollama-buddy-rag-file-extensions) "$")))
         (filtered-files (cl-remove-if #'ollama-buddy-rag--excluded-p files))
         (total-files (length filtered-files))
         (all-chunks nil)
         (file-count 0)
         (chunk-count 0))

    (when (zerop total-files)
      (user-error "No indexable files found in %s" dir))

    (message "Indexing %d files from %s..." total-files dir)

    ;; Chunk all files (synchronous - fast, no network)
    (dolist (file filtered-files)
      (cl-incf file-count)
      (let ((chunks (ollama-buddy-rag--chunk-file file)))
        (when chunks
          (let ((relative-file (file-relative-name file dir)))
            (dolist (chunk chunks)
              (setq chunk (plist-put chunk :relative-file relative-file))
              (push chunk all-chunks)
              (cl-incf chunk-count))))))

    (setq all-chunks (nreverse all-chunks))
    (let ((total-batches (ceiling (/ (float chunk-count) ollama-buddy-rag-batch-size)))
          (operation-id (gensym "rag-index-")))
      (message "Created %d chunks from %d files. Generating embeddings (%d batches)..."
               chunk-count file-count total-batches)
      (setq ollama-buddy-rag--indexing-in-progress t)
      (ollama-buddy--register-background-operation
       operation-id
       (format "RAG indexing 1/%d" total-batches))
      (ollama-buddy-rag--process-batches
       all-chunks ollama-buddy-rag-batch-size 0 total-batches
       index-name dir file-count chunk-count operation-id))))

;;;###autoload
(defun ollama-buddy-rag-search (query)
  "Search RAG indexes for QUERY and display results."
  (interactive
   (list (read-string "RAG search: " nil 'ollama-buddy-rag--search-history)))
  (ollama-buddy-rag--ensure-embedding-model)
  (let* ((index-names (ollama-buddy-rag--list-index-names))
         (index-name (if (= 1 (length index-names))
                         (car index-names)
                       (completing-read "Search index: " index-names nil t)))
         (index (ollama-buddy-rag--load-index index-name)))

    (unless index
      (user-error "Index '%s' not found or could not be loaded" index-name))

    (let ((operation-id (gensym "rag-search-")))
      (message "Searching '%s' for: %s..." index-name query)
      (ollama-buddy--register-background-operation
       operation-id "RAG searching")

      (ollama-buddy-rag--get-embedding-async
       query
       (lambda (query-embedding)
         (let* ((chunks (plist-get index :chunks))
                (results (when query-embedding
                           (ollama-buddy-rag--search-chunks
                            query-embedding chunks
                            ollama-buddy-rag-top-k
                            ollama-buddy-rag-similarity-threshold))))
           (if (null results)
               (progn
                 (ollama-buddy--complete-background-operation
                  operation-id "RAG search: no results")
                 (message "No results found for: %s" query))
             (let ((buf (get-buffer-create "*Ollama RAG Search*")))
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert "#+TITLE: RAG Search Results\n")
                   (insert (format "#+SUBTITLE: Query: %s\n" query))
                   (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
                   (insert (format "#+INDEX: %s\n\n" index-name))
                   (insert (format "Found *%d* results:\n\n" (length results)))
                   (let ((idx 0))
                     (dolist (result results)
                       (cl-incf idx)
                       (insert (ollama-buddy-rag--format-chunk-for-display result idx))
                       (insert "\n")))
                   (goto-char (point-min))
                   (org-mode)
                   (view-mode 1)))
               (display-buffer buf)
               (ollama-buddy--complete-background-operation
                operation-id
                (format "RAG search: %d results" (length results)))))))))))

;;;###autoload
(defun ollama-buddy-rag-attach (query)
  "Search RAG indexes for QUERY and attach results to conversation context."
  (interactive
   (list (read-string "RAG search (attach): " nil 'ollama-buddy-rag--search-history)))
  (ollama-buddy-rag--ensure-embedding-model)
  (let* ((index-names (ollama-buddy-rag--list-index-names))
         (index-name (if (= 1 (length index-names))
                         (car index-names)
                       (completing-read "Search index: " index-names nil t)))
         (index (ollama-buddy-rag--load-index index-name)))

    (unless index
      (user-error "Index '%s' not found or could not be loaded" index-name))

    ;; Check for duplicate query
    (when (cl-find query ollama-buddy-rag--current-results
                   :test #'string= :key (lambda (r) (plist-get r :query)))
      (if (y-or-n-p (format "RAG search for '%s' already attached. Replace? " query))
          (setq ollama-buddy-rag--current-results
                (cl-remove-if (lambda (r) (string= query (plist-get r :query)))
                              ollama-buddy-rag--current-results))
        (user-error "RAG attachment cancelled")))

    (let ((operation-id (gensym "rag-attach-")))
      (message "Searching and attaching: %s..." query)
      (ollama-buddy--register-background-operation
       operation-id "RAG attaching")

      (ollama-buddy-rag--get-embedding-async
       query
       (lambda (query-embedding)
         (let* ((chunks (plist-get index :chunks))
                (results (when query-embedding
                           (ollama-buddy-rag--search-chunks
                            query-embedding chunks
                            ollama-buddy-rag-top-k
                            ollama-buddy-rag-similarity-threshold))))
           (if (null results)
               (progn
                 (ollama-buddy--complete-background-operation
                  operation-id "RAG attach: no results")
                 (message "No results found for: %s" query))
             (let* ((formatted-content (ollama-buddy-rag--format-results-for-context
                                        results query index-name))
                    (token-estimate (ollama-buddy-rag--estimate-tokens formatted-content))
                    (attachment (list :query query
                                      :index-name index-name
                                      :results results
                                      :content formatted-content
                                      :tokens token-estimate
                                      :timestamp (current-time))))

               ;; Add to results
               (push attachment ollama-buddy-rag--current-results)

               ;; Update chat buffer if it exists
               (when-let ((buf (get-buffer ollama-buddy--chat-buffer)))
                 (with-current-buffer buf
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "\n\n- RAG context attached: \"%s\" (%d results%s)\n"
                                     query
                                     (length results)
                                     (if ollama-buddy-rag-show-token-estimate
                                         (format ", ~%d tokens" token-estimate)
                                       ""))))))

               (ollama-buddy--complete-background-operation
                operation-id
                (format "RAG attached: %d results" (length results)))))))))))

;;;###autoload
(defun ollama-buddy-rag-list-indexes ()
  "List all available RAG indexes with their details."
  (interactive)
  (let ((index-names (ollama-buddy-rag--list-index-names)))
    (if (null index-names)
        (message "No RAG indexes found. Use M-x ollama-buddy-rag-index-directory to create one.")
      (let ((buf (get-buffer-create "*Ollama RAG Indexes*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "#+TITLE: RAG Indexes\n\n")
            (dolist (name index-names)
              (let ((index (ollama-buddy-rag--load-index name)))
                (insert (format "* %s\n" name))
                (when index
                  (insert (format "  - Source: %s\n" (plist-get index :source-path)))
                  (insert (format "  - Files: %d\n" (plist-get index :file-count)))
                  (insert (format "  - Chunks: %d\n" (plist-get index :chunk-count)))
                  (insert (format "  - Model: %s\n" (plist-get index :embedding-model)))
                  (insert (format "  - Created: %s\n" (plist-get index :created))))
                (insert "\n")))
            (goto-char (point-min))
            (org-mode)
            (view-mode 1)))
        (display-buffer buf)))))

;;;###autoload
(defun ollama-buddy-rag-delete-index (index-name)
  "Delete RAG index INDEX-NAME."
  (interactive
   (let ((names (ollama-buddy-rag--list-index-names)))
     (if (null names)
         (user-error "No RAG indexes found")
       (list (completing-read "Delete index: " names nil t)))))
  (when (yes-or-no-p (format "Really delete index '%s'? " index-name))
    (if (ollama-buddy-rag--delete-index-file index-name)
        (message "Index '%s' deleted" index-name)
      (message "Index '%s' not found" index-name))))

;;;###autoload
(defun ollama-buddy-rag-clear-attached ()
  "Clear all attached RAG context."
  (interactive)
  (let ((count (length ollama-buddy-rag--current-results)))
    (setq ollama-buddy-rag--current-results nil)
    (message "Cleared %d RAG attachment(s)" count)))

;;; Context Integration Functions

(defun ollama-buddy-rag-get-context ()
  "Get formatted RAG context for inclusion in prompts.
Returns nil if no RAG results are attached."
  (when ollama-buddy-rag--current-results
    (concat "## RAG Context (Retrieved Documents):\n\n"
            (mapconcat
             (lambda (result)
               (plist-get result :content))
             ollama-buddy-rag--current-results
             "\n\n---\n\n"))))

(defun ollama-buddy-rag-count ()
  "Return the number of attached RAG search results."
  (length ollama-buddy-rag--current-results))

(defun ollama-buddy-rag-total-tokens ()
  "Return estimated total tokens for all attached RAG context."
  (apply #'+ (mapcar (lambda (r) (or (plist-get r :tokens) 0))
                     ollama-buddy-rag--current-results)))

(defun ollama-buddy-rag-attached-p ()
  "Return non-nil if RAG context is attached."
  (not (null ollama-buddy-rag--current-results)))

(provide 'ollama-buddy-rag)
;;; ollama-buddy-rag.el ends here
