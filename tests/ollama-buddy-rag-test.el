;;; ollama-buddy-rag-test.el --- Tests for ollama-buddy-rag -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer <captainflasmr@gmail.com>

;;; Commentary:

;; ERT tests for ollama-buddy-rag.el
;; Tests cover offline functionality: chunking, vector math, index
;; management, formatting, and search.  No Ollama server required.
;;
;; Run with: make test-rag

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'ollama-buddy-rag)

;;; Test Fixtures
;; ============================================================================

(defvar test-rag--sample-text-short
  "The quick brown fox jumps over the lazy dog."
  "Short sample text for chunking tests.")

(defvar test-rag--sample-text-multi
  "Emacs is a powerful text editor with extensive customization.
It supports Lisp programming and has a rich ecosystem of packages.
Org mode provides outlining, planning, and authoring capabilities.
Magit is a complete Git porcelain inside Emacs.
TRAMP enables remote file editing transparently.
Dired is the directory editor built into Emacs.
ERC is an IRC client that runs within Emacs.
Gnus handles mail and news reading in Emacs.
Calc is an advanced calculator and math tool.
Ediff compares and merges files interactively."
  "Multi-line sample text for chunking and search tests.")

(defvar test-rag--recipe-text
  "Ingredients for chocolate cake: flour, sugar, cocoa powder, eggs, butter, milk.
Preheat the oven to 350 degrees Fahrenheit.
Mix the dry ingredients together in a large bowl.
In a separate bowl, beat the eggs and add the melted butter and milk.
Combine wet and dry ingredients and stir until smooth.
Pour the batter into a greased baking pan.
Bake for 30 to 35 minutes until a toothpick comes out clean.
Let the cake cool before frosting with chocolate ganache."
  "Sample recipe text for thematic search testing.")

(defvar test-rag--code-text
  "(defun fibonacci (n)
  \"Return the Nth Fibonacci number.\"
  (if (<= n 1)
      n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

(defun factorial (n)
  \"Return N factorial.\"
  (if (<= n 1)
      1
    (* n (factorial (- n 1)))))

(defun is-prime (n)
  \"Return t if N is a prime number.\"
  (when (> n 1)
    (cl-loop for i from 2 to (isqrt n)
             never (zerop (mod n i)))))"
  "Sample Elisp code for code search testing.")

;; Simple 3-dimensional test vectors for math tests
(defvar test-rag--vec-a '(1.0 0.0 0.0) "Unit vector along x-axis.")
(defvar test-rag--vec-b '(0.0 1.0 0.0) "Unit vector along y-axis.")
(defvar test-rag--vec-c '(1.0 1.0 0.0) "Diagonal vector in xy-plane.")
(defvar test-rag--vec-d '(1.0 0.0 0.0) "Identical to vec-a.")
(defvar test-rag--vec-zero '(0.0 0.0 0.0) "Zero vector.")

;;; Utility Macros
;; ============================================================================

(defmacro with-rag-test-env (&rest body)
  "Execute BODY with a clean RAG test environment using temp directory."
  (declare (indent 0) (debug t))
  `(let* ((test-dir (make-temp-file "rag-test-" t))
          (ollama-buddy-rag-index-directory
           (expand-file-name "indexes" test-dir))
          (ollama-buddy-rag-chunk-size 10)
          (ollama-buddy-rag-chunk-overlap 2)
          (ollama-buddy-rag-batch-size 5)
          (ollama-buddy-rag-top-k 3)
          (ollama-buddy-rag-similarity-threshold 0.1)
          (ollama-buddy-rag--indexes (make-hash-table :test 'equal))
          (ollama-buddy-rag--current-results nil))
     (make-directory ollama-buddy-rag-index-directory t)
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

(defmacro with-rag-test-files (file-specs &rest body)
  "Create temporary files from FILE-SPECS and execute BODY.
FILE-SPECS is a list of (FILENAME . CONTENT) pairs.
Binds `test-dir' to the directory containing the files."
  (declare (indent 1) (debug t))
  `(let ((test-dir (make-temp-file "rag-files-" t)))
     (dolist (spec ,file-specs)
       (let ((filepath (expand-file-name (car spec) test-dir)))
         (make-directory (file-name-directory filepath) t)
         (with-temp-file filepath
           (insert (cdr spec)))))
     (unwind-protect
         (progn ,@body)
       (delete-directory test-dir t))))

;;; Token Estimation Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-estimate-tokens-empty ()
  "Test token estimation with empty string."
  :tags '(rag)
  (should (= 0 (ollama-buddy-rag--estimate-tokens ""))))

(ert-deftest ollama-buddy-rag-test-estimate-tokens-sentence ()
  "Test token estimation with a typical sentence."
  :tags '(rag)
  ;; "The quick brown fox" = 4 words * 1.3 = 5 (rounded)
  (let ((result (ollama-buddy-rag--estimate-tokens "The quick brown fox")))
    (should (> result 3))
    (should (< result 8))))

(ert-deftest ollama-buddy-rag-test-estimate-tokens-longer ()
  "Test token estimation scales with text length."
  :tags '(rag)
  (let ((short-est (ollama-buddy-rag--estimate-tokens "hello world"))
        (long-est (ollama-buddy-rag--estimate-tokens test-rag--sample-text-multi)))
    (should (> long-est short-est))))

;;; Word Splitting Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-split-words-basic ()
  "Test word splitting returns correct count and structure."
  :tags '(rag)
  (let ((words (ollama-buddy-rag--split-into-words "hello world foo")))
    (should (= 3 (length words)))
    (should (equal "hello" (plist-get (nth 0 words) :word)))
    (should (equal "world" (plist-get (nth 1 words) :word)))
    (should (equal "foo" (plist-get (nth 2 words) :word)))))

(ert-deftest ollama-buddy-rag-test-split-words-positions ()
  "Test that word positions are tracked correctly."
  :tags '(rag)
  (let* ((text "abc def")
         (words (ollama-buddy-rag--split-into-words text)))
    (should (= 0 (plist-get (nth 0 words) :start)))
    (should (= 3 (plist-get (nth 0 words) :end)))
    (should (= 4 (plist-get (nth 1 words) :start)))
    (should (= 7 (plist-get (nth 1 words) :end)))))

(ert-deftest ollama-buddy-rag-test-split-words-empty ()
  "Test word splitting with empty string."
  :tags '(rag)
  (should (= 0 (length (ollama-buddy-rag--split-into-words "")))))

(ert-deftest ollama-buddy-rag-test-split-words-whitespace ()
  "Test word splitting handles various whitespace."
  :tags '(rag)
  (let ((words (ollama-buddy-rag--split-into-words "  hello\t\tworld\n  foo  ")))
    (should (= 3 (length words)))))

;;; Chunking Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-chunk-text-small ()
  "Test chunking text smaller than chunk size produces one chunk."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 100)
        (ollama-buddy-rag-chunk-overlap 10))
    (let ((chunks (ollama-buddy-rag--chunk-text test-rag--sample-text-short)))
      (should (= 1 (length chunks)))
      (should (equal test-rag--sample-text-short
                     (plist-get (car chunks) :content))))))

(ert-deftest ollama-buddy-rag-test-chunk-text-overlap ()
  "Test that chunks overlap correctly."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 5)
        (ollama-buddy-rag-chunk-overlap 2))
    (let ((chunks (ollama-buddy-rag--chunk-text test-rag--sample-text-short)))
      ;; With 9 words, chunk-size 5, step 3: should get multiple chunks
      (should (> (length chunks) 1))
      ;; Each chunk should have content
      (dolist (chunk chunks)
        (should (stringp (plist-get chunk :content)))
        (should (> (length (plist-get chunk :content)) 0))))))

(ert-deftest ollama-buddy-rag-test-chunk-text-has-positions ()
  "Test that chunks have start and end positions."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 5)
        (ollama-buddy-rag-chunk-overlap 1))
    (let ((chunks (ollama-buddy-rag--chunk-text test-rag--sample-text-short)))
      (dolist (chunk chunks)
        (should (numberp (plist-get chunk :start-pos)))
        (should (numberp (plist-get chunk :end-pos)))
        (should (numberp (plist-get chunk :word-count)))
        (should (>= (plist-get chunk :end-pos)
                    (plist-get chunk :start-pos)))))))

(ert-deftest ollama-buddy-rag-test-chunk-text-content-matches-positions ()
  "Test that chunk content matches the source text at given positions."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 4)
        (ollama-buddy-rag-chunk-overlap 1)
        (text test-rag--sample-text-short))
    (dolist (chunk (ollama-buddy-rag--chunk-text text))
      (let ((start (plist-get chunk :start-pos))
            (end (plist-get chunk :end-pos))
            (content (plist-get chunk :content)))
        (should (equal content (substring text start end)))))))

(ert-deftest ollama-buddy-rag-test-chunk-text-covers-all-content ()
  "Test that chunks collectively cover the entire text."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 4)
        (ollama-buddy-rag-chunk-overlap 1))
    (let* ((chunks (ollama-buddy-rag--chunk-text test-rag--sample-text-short))
           (first-start (plist-get (car chunks) :start-pos))
           (last-end (plist-get (car (last chunks)) :end-pos)))
      ;; First chunk starts at beginning
      (should (= 0 first-start))
      ;; Last chunk ends at or near end of text
      (should (>= last-end (- (length test-rag--sample-text-short) 1))))))

;;; File Chunking Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-chunk-file ()
  "Test chunking a real file with metadata."
  :tags '(rag)
  (let ((ollama-buddy-rag-chunk-size 100)
        (ollama-buddy-rag-chunk-overlap 10)
        (tmpfile (make-temp-file "rag-test-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert test-rag--code-text))
          (let ((chunks (ollama-buddy-rag--chunk-file tmpfile)))
            (should chunks)
            (should (> (length chunks) 0))
            ;; Each chunk has required metadata
            (dolist (chunk chunks)
              (should (plist-get chunk :id))
              (should (equal tmpfile (plist-get chunk :file)))
              (should (numberp (plist-get chunk :line-start)))
              (should (numberp (plist-get chunk :line-end)))
              (should (stringp (plist-get chunk :content))))))
      (delete-file tmpfile))))

(ert-deftest ollama-buddy-rag-test-chunk-file-too-large ()
  "Test that files exceeding max size are skipped."
  :tags '(rag)
  (let ((ollama-buddy-rag-max-file-size 10)
        (tmpfile (make-temp-file "rag-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "This text is longer than 10 bytes"))
          (should (null (ollama-buddy-rag--chunk-file tmpfile))))
      (delete-file tmpfile))))

(ert-deftest ollama-buddy-rag-test-chunk-file-unreadable ()
  "Test that unreadable files return nil."
  :tags '(rag)
  (should (null (ollama-buddy-rag--chunk-file "/nonexistent/file.txt"))))

;;; Line Counting Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-count-lines-start ()
  "Test line count at start of text."
  :tags '(rag)
  (should (= 1 (ollama-buddy-rag--count-lines-to-pos "hello\nworld" 0))))

(ert-deftest ollama-buddy-rag-test-count-lines-after-newline ()
  "Test line count after newlines."
  :tags '(rag)
  (should (= 2 (ollama-buddy-rag--count-lines-to-pos "hello\nworld" 6)))
  (should (= 3 (ollama-buddy-rag--count-lines-to-pos "a\nb\nc" 4))))

;;; Vector Math Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-dot-product ()
  "Test dot product calculation."
  :tags '(rag)
  ;; Orthogonal vectors
  (should (= 0.0 (ollama-buddy-rag--dot-product test-rag--vec-a test-rag--vec-b)))
  ;; Identical vectors
  (should (= 1.0 (ollama-buddy-rag--dot-product test-rag--vec-a test-rag--vec-d)))
  ;; General case
  (should (= 1.0 (ollama-buddy-rag--dot-product test-rag--vec-a test-rag--vec-c))))

(ert-deftest ollama-buddy-rag-test-magnitude ()
  "Test vector magnitude calculation."
  :tags '(rag)
  ;; Unit vector
  (should (= 1.0 (ollama-buddy-rag--magnitude test-rag--vec-a)))
  ;; Zero vector
  (should (= 0.0 (ollama-buddy-rag--magnitude test-rag--vec-zero)))
  ;; Diagonal vector: sqrt(1^2 + 1^2) = sqrt(2)
  (should (< (abs (- (sqrt 2.0) (ollama-buddy-rag--magnitude test-rag--vec-c)))
             0.0001)))

(ert-deftest ollama-buddy-rag-test-cosine-similarity-identical ()
  "Test cosine similarity of identical vectors is 1.0."
  :tags '(rag)
  (should (= 1.0 (ollama-buddy-rag--cosine-similarity
                   test-rag--vec-a test-rag--vec-d))))

(ert-deftest ollama-buddy-rag-test-cosine-similarity-orthogonal ()
  "Test cosine similarity of orthogonal vectors is 0.0."
  :tags '(rag)
  (should (= 0.0 (ollama-buddy-rag--cosine-similarity
                   test-rag--vec-a test-rag--vec-b))))

(ert-deftest ollama-buddy-rag-test-cosine-similarity-diagonal ()
  "Test cosine similarity of related vectors."
  :tags '(rag)
  ;; cos(45°) = 1/sqrt(2) ≈ 0.7071
  (let ((sim (ollama-buddy-rag--cosine-similarity
              test-rag--vec-a test-rag--vec-c)))
    (should (< (abs (- sim (/ 1.0 (sqrt 2.0)))) 0.0001))))

(ert-deftest ollama-buddy-rag-test-cosine-similarity-zero-vector ()
  "Test cosine similarity with zero vector returns 0."
  :tags '(rag)
  (should (= 0.0 (ollama-buddy-rag--cosine-similarity
                   test-rag--vec-a test-rag--vec-zero))))

;;; Search Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-search-chunks-basic ()
  "Test searching chunks returns results sorted by similarity."
  :tags '(rag)
  (let* ((chunks (list (list :content "chunk A" :embedding '(1.0 0.0 0.0))
                       (list :content "chunk B" :embedding '(0.0 1.0 0.0))
                       (list :content "chunk C" :embedding '(0.9 0.1 0.0))))
         (query-emb '(1.0 0.0 0.0))
         (results (ollama-buddy-rag--search-chunks query-emb chunks 3 0.0)))
    ;; Should return all 3
    (should (= 3 (length results)))
    ;; First result should be "chunk A" (exact match)
    (should (equal "chunk A" (plist-get (car results) :content)))
    ;; Results should be sorted by score descending
    (should (>= (plist-get (nth 0 results) :score)
                (plist-get (nth 1 results) :score)))
    (should (>= (plist-get (nth 1 results) :score)
                (plist-get (nth 2 results) :score)))))

(ert-deftest ollama-buddy-rag-test-search-chunks-threshold ()
  "Test that results below threshold are filtered out."
  :tags '(rag)
  (let* ((chunks (list (list :content "match" :embedding '(1.0 0.0 0.0))
                       (list :content "orthogonal" :embedding '(0.0 1.0 0.0))))
         (query-emb '(1.0 0.0 0.0))
         (results (ollama-buddy-rag--search-chunks query-emb chunks 5 0.5)))
    ;; Only the matching chunk should pass threshold
    (should (= 1 (length results)))
    (should (equal "match" (plist-get (car results) :content)))))

(ert-deftest ollama-buddy-rag-test-search-chunks-top-k ()
  "Test that only top-k results are returned."
  :tags '(rag)
  (let* ((chunks (list (list :content "a" :embedding '(1.0 0.0 0.0))
                       (list :content "b" :embedding '(0.9 0.1 0.0))
                       (list :content "c" :embedding '(0.8 0.2 0.0))
                       (list :content "d" :embedding '(0.7 0.3 0.0))))
         (query-emb '(1.0 0.0 0.0))
         (results (ollama-buddy-rag--search-chunks query-emb chunks 2 0.0)))
    (should (= 2 (length results)))))

(ert-deftest ollama-buddy-rag-test-search-chunks-no-embedding ()
  "Test that chunks without embeddings are skipped."
  :tags '(rag)
  (let* ((chunks (list (list :content "has emb" :embedding '(1.0 0.0 0.0))
                       (list :content "no emb")))
         (query-emb '(1.0 0.0 0.0))
         (results (ollama-buddy-rag--search-chunks query-emb chunks 5 0.0)))
    (should (= 1 (length results)))
    (should (equal "has emb" (plist-get (car results) :content)))))

(ert-deftest ollama-buddy-rag-test-search-chunks-empty ()
  "Test searching empty chunk list returns empty."
  :tags '(rag)
  (let ((results (ollama-buddy-rag--search-chunks '(1.0 0.0) nil 5 0.0)))
    (should (null results))))

;;; Index Management Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-index-name-from-path ()
  "Test index name generation from path."
  :tags '(rag)
  (let ((name (ollama-buddy-rag--index-name-from-path "/home/user/project")))
    ;; Should contain directory name
    (should (string-prefix-p "project-" name))
    ;; Should have a hash suffix
    (should (= (length name) (+ (length "project-") 8)))))

(ert-deftest ollama-buddy-rag-test-index-name-deterministic ()
  "Test that same path produces same index name."
  :tags '(rag)
  (let ((name1 (ollama-buddy-rag--index-name-from-path "/home/user/project"))
        (name2 (ollama-buddy-rag--index-name-from-path "/home/user/project")))
    (should (equal name1 name2))))

(ert-deftest ollama-buddy-rag-test-index-name-different-paths ()
  "Test that different paths produce different index names."
  :tags '(rag)
  (let ((name1 (ollama-buddy-rag--index-name-from-path "/home/user/project-a"))
        (name2 (ollama-buddy-rag--index-name-from-path "/home/user/project-b")))
    (should-not (equal name1 name2))))

(ert-deftest ollama-buddy-rag-test-save-and-load-index ()
  "Test saving and loading an index preserves data."
  :tags '(rag)
  (with-rag-test-env
    (let* ((chunks (list (list :id "test-1" :content "hello"
                               :embedding '(1.0 0.0 0.0))
                         (list :id "test-2" :content "world"
                               :embedding '(0.0 1.0 0.0))))
           (index (list :version 1
                        :name "test-index"
                        :source-path "/tmp/test"
                        :embedding-model "nomic-embed-text"
                        :created "2026-01-01T00:00:00"
                        :file-count 2
                        :chunk-count 2
                        :chunks chunks)))
      ;; Save
      (ollama-buddy-rag--save-index index)
      ;; Clear cache
      (clrhash ollama-buddy-rag--indexes)
      ;; Load
      (let ((loaded (ollama-buddy-rag--load-index "test-index")))
        (should loaded)
        (should (equal 1 (plist-get loaded :version)))
        (should (equal "test-index" (plist-get loaded :name)))
        (should (equal 2 (plist-get loaded :chunk-count)))
        ;; Verify chunks have embeddings
        (let ((loaded-chunks (plist-get loaded :chunks)))
          (should (= 2 (length loaded-chunks)))
          (should (equal '(1.0 0.0 0.0)
                         (plist-get (nth 0 loaded-chunks) :embedding)))
          (should (equal '(0.0 1.0 0.0)
                         (plist-get (nth 1 loaded-chunks) :embedding))))))))

(ert-deftest ollama-buddy-rag-test-list-index-names ()
  "Test listing available index names."
  :tags '(rag)
  (with-rag-test-env
    ;; Initially empty
    (should (null (ollama-buddy-rag--list-index-names)))
    ;; Save an index
    (let ((index (list :version 1 :name "my-index"
                       :chunks nil :file-count 0 :chunk-count 0)))
      (ollama-buddy-rag--save-index index))
    ;; Should find it
    (let ((names (ollama-buddy-rag--list-index-names)))
      (should (= 1 (length names)))
      (should (equal "my-index" (car names))))))

(ert-deftest ollama-buddy-rag-test-delete-index ()
  "Test deleting an index."
  :tags '(rag)
  (with-rag-test-env
    ;; Create and save
    (let ((index (list :version 1 :name "doomed"
                       :chunks nil :file-count 0 :chunk-count 0)))
      (ollama-buddy-rag--save-index index)
      (puthash "doomed" index ollama-buddy-rag--indexes))
    ;; Verify it exists
    (should (= 1 (length (ollama-buddy-rag--list-index-names))))
    ;; Delete it
    (should (ollama-buddy-rag--delete-index-file "doomed"))
    ;; Verify gone
    (should (null (ollama-buddy-rag--list-index-names)))
    (should (null (gethash "doomed" ollama-buddy-rag--indexes)))))

;;; File Extension Filter Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-file-extension-match ()
  "Test file extension matching for indexable files."
  :tags '(rag)
  (should (ollama-buddy-rag--file-extension-p "foo.el"))
  (should (ollama-buddy-rag--file-extension-p "bar.py"))
  (should (ollama-buddy-rag--file-extension-p "test.sh"))
  (should (ollama-buddy-rag--file-extension-p "README.md"))
  (should (ollama-buddy-rag--file-extension-p "config.json")))

(ert-deftest ollama-buddy-rag-test-file-extension-no-match ()
  "Test that non-indexable extensions are rejected."
  :tags '(rag)
  (should-not (ollama-buddy-rag--file-extension-p "image.png"))
  (should-not (ollama-buddy-rag--file-extension-p "archive.tar.gz"))
  (should-not (ollama-buddy-rag--file-extension-p "binary.exe")))

(ert-deftest ollama-buddy-rag-test-file-extension-no-ext ()
  "Test that files without extensions are rejected."
  :tags '(rag)
  (should-not (ollama-buddy-rag--file-extension-p "Makefile"))
  (should-not (ollama-buddy-rag--file-extension-p "script")))

;;; Exclude Pattern Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-excluded-git ()
  "Test that .git directory itself is excluded."
  :tags '(rag)
  (should (ollama-buddy-rag--excluded-p "/project/.git")))

(ert-deftest ollama-buddy-rag-test-excluded-node-modules ()
  "Test that node_modules directory is excluded."
  :tags '(rag)
  (should (ollama-buddy-rag--excluded-p "/project/node_modules")))

(ert-deftest ollama-buddy-rag-test-not-excluded ()
  "Test that normal paths are not excluded."
  :tags '(rag)
  (should-not (ollama-buddy-rag--excluded-p "/project/src/main.el"))
  (should-not (ollama-buddy-rag--excluded-p "/project/lib/utils.py")))

;;; Formatting Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-format-chunk-display ()
  "Test chunk display formatting has required elements."
  :tags '(rag)
  (let* ((chunk (list :file "/project/src/main.el"
                      :line-start 10 :line-end 25
                      :score 0.85
                      :content "(defun hello () \"world\")"))
         (output (ollama-buddy-rag--format-chunk-for-display chunk 1)))
    (should (string-match-p "Result 1" output))
    (should (string-match-p "0\\.85" output))
    (should (string-match-p ":FILE:.*main\\.el" output))
    (should (string-match-p ":LINES:.*10-25" output))
    (should (string-match-p "begin_example" output))
    (should (string-match-p "defun hello" output))))

(ert-deftest ollama-buddy-rag-test-format-chunk-escapes-org-headings ()
  "Test that org headings in content are escaped."
  :tags '(rag)
  (let* ((chunk (list :file "test.org" :line-start 1 :line-end 5
                      :score 0.5
                      :content "* Heading One\n** Sub Heading\nNormal text"))
         (output (ollama-buddy-rag--format-chunk-for-display chunk 1)))
    ;; Org headings in content should be escaped with comma prefix
    (should (string-match-p ",\\* Heading" output))
    (should (string-match-p ",\\*\\* Sub" output))))

(ert-deftest ollama-buddy-rag-test-format-context ()
  "Test context formatting for prompt inclusion."
  :tags '(rag)
  (let* ((results (list (list :file "main.el" :line-start 1
                              :content "hello world")))
         (output (ollama-buddy-rag--format-results-for-context
                  results "test query" "my-index")))
    (should (string-match-p "test query" output))
    (should (string-match-p "my-index" output))
    (should (string-match-p "hello world" output))))

;;; Context Integration Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-get-context-empty ()
  "Test that get-context returns nil when no results attached."
  :tags '(rag)
  (let ((ollama-buddy-rag--current-results nil))
    (should (null (ollama-buddy-rag-get-context)))))

(ert-deftest ollama-buddy-rag-test-get-context-with-results ()
  "Test that get-context returns formatted content."
  :tags '(rag)
  (let ((ollama-buddy-rag--current-results
         (list (list :query "test" :content "some context here"))))
    (let ((ctx (ollama-buddy-rag-get-context)))
      (should ctx)
      (should (string-match-p "RAG Context" ctx))
      (should (string-match-p "some context here" ctx)))))

(ert-deftest ollama-buddy-rag-test-count-and-tokens ()
  "Test count and token tracking functions."
  :tags '(rag)
  (let ((ollama-buddy-rag--current-results
         (list (list :query "q1" :tokens 100)
               (list :query "q2" :tokens 200))))
    (should (= 2 (ollama-buddy-rag-count)))
    (should (= 300 (ollama-buddy-rag-total-tokens)))))

(ert-deftest ollama-buddy-rag-test-attached-p ()
  "Test attached predicate."
  :tags '(rag)
  (let ((ollama-buddy-rag--current-results nil))
    (should-not (ollama-buddy-rag-attached-p)))
  (let ((ollama-buddy-rag--current-results '((:query "x"))))
    (should (ollama-buddy-rag-attached-p))))

(ert-deftest ollama-buddy-rag-test-clear-attached ()
  "Test clearing attached results."
  :tags '(rag)
  (let ((ollama-buddy-rag--current-results
         (list (list :query "q1") (list :query "q2"))))
    (ollama-buddy-rag-clear-attached)
    (should (null ollama-buddy-rag--current-results))))

;;; End-to-End Search Test (with mock embeddings)
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-save-load-search-roundtrip ()
  "Test complete flow: create index with embeddings, save, load, search."
  :tags '(rag)
  (with-rag-test-env
    ;; Create chunks with mock embeddings representing different topics
    (let* ((chunks (list
                    (list :id "1" :file "recipes.txt" :line-start 1 :line-end 5
                          :content "Mix flour and sugar for chocolate cake"
                          :embedding '(0.9 0.1 0.0 0.0))
                    (list :id "2" :file "recipes.txt" :line-start 6 :line-end 10
                          :content "Preheat oven to 350 degrees"
                          :embedding '(0.8 0.2 0.0 0.0))
                    (list :id "3" :file "code.el" :line-start 1 :line-end 8
                          :content "defun fibonacci recursive function"
                          :embedding '(0.0 0.0 0.9 0.1))
                    (list :id "4" :file "code.el" :line-start 9 :line-end 15
                          :content "defun factorial iterative loop"
                          :embedding '(0.0 0.0 0.8 0.2))))
           (index (list :version 1
                        :name "roundtrip-test"
                        :source-path "/tmp/test"
                        :embedding-model "test"
                        :created "2026-01-01"
                        :file-count 2
                        :chunk-count 4
                        :chunks chunks)))
      ;; Save and reload
      (ollama-buddy-rag--save-index index)
      (clrhash ollama-buddy-rag--indexes)
      (let ((loaded (ollama-buddy-rag--load-index "roundtrip-test")))
        (should loaded)
        ;; Search with a "recipe-like" query vector
        (let ((results (ollama-buddy-rag--search-chunks
                        '(0.85 0.15 0.0 0.0)
                        (plist-get loaded :chunks)
                        2 0.5)))
          ;; Should find recipe chunks, not code chunks
          (should (= 2 (length results)))
          (should (equal "recipes.txt"
                         (plist-get (car results) :file)))
          (should (> (plist-get (car results) :score) 0.9)))
        ;; Search with a "code-like" query vector
        (let ((results (ollama-buddy-rag--search-chunks
                        '(0.0 0.0 0.85 0.15)
                        (plist-get loaded :chunks)
                        2 0.5)))
          (should (= 2 (length results)))
          (should (equal "code.el"
                         (plist-get (car results) :file))))))))

;;; Inline @rag() Processing Tests
;; ============================================================================

(ert-deftest ollama-buddy-rag-test-inline-extract-single ()
  "Test extracting a single inline @rag() query."
  :tags '(rag)
  (let ((queries (ollama-buddy-rag-extract-inline-queries
                  "Tell me about @rag(giant squid attack) from the book.")))
    (should (= 1 (length queries)))
    (should (equal "giant squid attack" (car queries)))))

(ert-deftest ollama-buddy-rag-test-inline-extract-multiple ()
  "Test extracting multiple inline @rag() queries."
  :tags '(rag)
  (let ((queries (ollama-buddy-rag-extract-inline-queries
                  "Compare @rag(captain nemo) with @rag(ned land)")))
    (should (= 2 (length queries)))
    (should (equal "captain nemo" (car queries)))
    (should (equal "ned land" (cadr queries)))))

(ert-deftest ollama-buddy-rag-test-inline-extract-none ()
  "Test that text without @rag() returns empty list."
  :tags '(rag)
  (should (null (ollama-buddy-rag-extract-inline-queries "no rag here"))))

(ert-deftest ollama-buddy-rag-test-inline-remove-delimiters ()
  "Test removing @rag() delimiters preserving query text."
  :tags '(rag)
  (should (equal "Tell me about giant squid attack from the book."
                 (ollama-buddy-rag-remove-inline-delimiters
                  "Tell me about @rag(giant squid attack) from the book."))))

(ert-deftest ollama-buddy-rag-test-inline-remove-multiple ()
  "Test removing multiple @rag() delimiters."
  :tags '(rag)
  (should (equal "Compare captain nemo with ned land"
                 (ollama-buddy-rag-remove-inline-delimiters
                  "Compare @rag(captain nemo) with @rag(ned land)"))))

(ert-deftest ollama-buddy-rag-test-inline-process-no-queries ()
  "Test that process-inline returns text unchanged when no @rag() patterns."
  :tags '(rag)
  (should (equal "just a normal prompt"
                 (ollama-buddy-rag-process-inline "just a normal prompt"))))

(provide 'ollama-buddy-rag-test)
;;; ollama-buddy-rag-test.el ends here
