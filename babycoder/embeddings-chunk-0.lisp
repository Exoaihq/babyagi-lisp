(defun import-packages ()
  (ql:quickload '(:osicat :cl-csv :trivial-file-size :alexandria :cl-openai :pandas :numpy :cl-transformers :cl-dotenv :bordeaux-threads)))

(import-packages)

(defparameter *repository-path* (osicat:current-directory))

(defclass embeddings ()
  ((workspace-path :initarg :workspace-path)
   (doc-embeddings-model :initform "text-embedding-ada-002")
   (query-embeddings-model :initform "text-embedding-ada-002")
   (separator :initform "\n* ")
   (tokenizer :initform (transformers:make-gpt2-tokenizer-fast :model-name "gpt2"))
   (separator-len :initform (length (transformers:tokenize (slot-value (make-instance 'embeddings) 'tokenizer) (slot-value (make-instance 'embeddings) 'separator))))
   (document-embeddings :initform nil)))

(defun compute-repository-embeddings (embeddings-instance)
  (let ((playground-data-path (merge-pathnames "playground_data" (slot-value embeddings-instance 'workspace-path))))
    (delete-directory-and-files playground-data-path)
    (let ((info (extract-info *repository-path*)))
      (save-info-to-csv info embeddings-instance)
      (let ((df (pandas:read-csv (merge-pathnames "repository_info.csv" playground-data-path))))
        (setf (pandas:index df) (list "filePath" "lineCoverage"))
        (setf (slot-value embeddings-instance 'df) df)
        (let ((context-embeddings (compute-doc-embeddings embeddings-instance df)))
          (save-doc-embeddings-to-csv context-embeddings df (merge-pathnames "doc_embeddings.csv" playground-data-path))
          (setf (slot-value embeddings-instance 'document-embeddings) (load-embeddings (merge-pathnames "doc_embeddings.csv" playground-data-path))))))))

(defun extract-info (repository-path)
  (let ((info '()))
    (osicat:walk-directory repository-path
                            :directories nil
                            :test (lambda (file)
                                    (when (and (osicat:file file) (not (osicat:link file)))
                                      (with-open-file (in file :direction :input :if-does-not-exist nil)
                                        (let* ((lines (alexandria:read-file-into-lines in))
                                               (lines (remove-if #'null (mapcar #'string-trim lines)))
                                               (chunks (loop for i from 0 below (length lines) by 60
                                                             collect (subseq lines i (min (+ i 60) (length lines))))))
                                          (loop for chunk in chunks
                                                for first-line = (+ (* 60 (position chunk chunks)) 1)
                                                for last-line = (+ first-line (length chunk) -1)
                                                do (push (list file (cons first-line last-line) (format nil "~{~a~^~%~}" chunk)) info)))))))
    (reverse info)))

(defun save-info-to-csv (info embeddings-instance)
  (let ((csv-file (merge-pathnames "repository_info.csv" (merge-pathnames "playground_data" (slot-value embeddings-instance 'workspace-path)))))
    (ensure-directories-exist csv-file)
    (with-open-file (out csv-file :direction :output :if-exists :supersede)
      (let ((writer (csv:make-csv-writer out)))
        (csv:write-csv-row '("filePath" "lineCoverage" "content") writer)
        (loop for (file-path line-coverage content) in info
              do (csv:write-csv-row (list file-path (format nil "~a" line-coverage) content) writer))))))

(defun compute-doc-embeddings (embeddings-instance df)
  (let ((embeddings '()))
    (pandas:iterrows df
                    (lambda (index row)
                      (let ((content (pandas:getitem row "content")))
                        (push (compute-embedding embeddings-instance content) embeddings))))
    (reverse embeddings)))

(defun compute-embedding (embeddings-instance content)
  (let ((encoded-content (transformers:encode (slot-value embeddings-instance 'tokenizer) content)))
    (openai:compute-embedding (slot-value embeddings-instance 'doc-embeddings-model) encoded-content)))

(defun save-doc-embeddings-to-csv (context-embeddings df csv-file)
  (ensure-directories-exist csv-file)
  (with-open-file (out csv-file :direction :output :if-exists :supersede)
    (let ((writer (csv:make-csv-writer out)))
      (csv:write-csv-row '("filePath" "lineCoverage" "embedding") writer)
      (loop for index from 0
            for (file-path line-coverage) in (pandas:index df)
            for embedding in context-embeddings
            do (csv:write-csv-row (list file-path (format nil "~a" line-coverage) (format nil "~{~a~^,~}" embedding)) writer))))))

(defun load-embeddings (csv-file)
  (let ((embeddings '()))
    (with-open-file (in csv-file :direction :input :if-does-not-exist nil)
      (let ((reader (csv:make-csv-reader in)))
        (csv:read-csv-row reader) ; skip header row
        (loop for row = (csv:read-csv-row reader)
              while row
              do (push (coerce (mapcar #'parse-integer (split-sequence:split-sequence #\, (nth 2 row))) 'vector) embeddings))))
    (reverse embeddings)))

(defun delete-directory-and-files (directory)
  (when (osicat:directory directory)
    (osicat:walk-directory directory
                            :directories nil
                            :test (lambda (file)
                                    (when (osicat:file file)
                                      (delete-file file))))
    (delete-directory directory)))

(defun ensure-directories-exist (pathname)
  (let ((directory (pathname-directory pathname)))
    (unless (osicat:directory (make-pathname :directory directory))
      (ensure-directories-exist (make-pathname :directory (butlast directory)))
      (make-directory (make-pathname :directory directory)))))

(defun main ()
  (let ((embeddings-instance (make-instance 'embeddings :workspace-path *repository-path*)))
    (compute-repository-embeddings embeddings-instance)))

(main)