(defun get-relevant-code-chunks (self task-description task-context)
  (let ((query (concatenate 'string task-description "\n" task-context))
        (most-relevant-document-sections (order-document-sections-by-query-similarity query (slot-value self 'document-embeddings)))
        (selected-chunks '()))
    (loop for (_, section-index) in most-relevant-document-sections do
      (handler-case
          (let ((document-section (aref (slot-value self 'df) section-index)))
            (push (concatenate 'string (slot-value self 'separator) (substitute #\Space #\Newline (getf document-section 'content))) selected-chunks)
            (when (>= (length selected-chunks) 2)
              (return))))
        (error () (continue))))
    selected-chunks))

(defun get-embedding (text model)
  (let ((result (openai-embedding-create :model model :input text)))
    (getf (aref (getf result "data") 0) "embedding")))

(defun get-doc-embedding (self text)
  (get-embedding text (slot-value self 'doc-embeddings-model)))

(defun get-query-embedding (self text)
  (get-embedding text (slot-value self 'query-embeddings-model)))

(defun compute-doc-embeddings (self df)
  (let ((embeddings (make-hash-table)))
    (loop for idx across (array-dimensions df) for r in (array-row-major-contents df) do
      (sleep 1)
      (setf (gethash idx embeddings) (get-doc-embedding self (substitute #\Space #\Newline (getf r 'content)))))
    embeddings))

(defun save-doc-embeddings-to-csv (self doc-embeddings df csv-filepath)
  (when (zerop (hash-table-count doc-embeddings))
    (return-from save-doc-embeddings-to-csv))
  (let* ((embedding-dim (length (gethash (first (hash-table-keys doc-embeddings)) doc-embeddings)))
         (embeddings-df (make-array (list (array-dimension df 0) (+ 2 embedding-dim)))))
    (loop for idx across (array-dimensions df) for r in (array-row-major-contents df) do
      (let ((embedding (gethash idx doc-embeddings))
            (row (concatenate 'list (list (first idx) (second idx)) embedding)))
        (setf (aref embeddings-df (length embeddings-df)) row)))
    (write-csv embeddings-df csv-filepath)))

(defun vector-similarity (x y)
  (reduce #'+ (map 'list #'* x y)))

(defun order-document-sections-by-query-similarity (query contexts)
  (let ((query-embedding (get-query-embedding query)))
    (sort (loop for (doc-index doc-embedding) in contexts
                collect (cons (vector-similarity query-embedding doc-embedding) doc-index))
          #'> :key #'car)))

(defun load-embeddings (fname)
  (let ((df (read-csv fname)))
    (let ((max-dim (reduce #'max (loop for c across (array-dimensions df)
                                       when (and (not (string= c "filePath")) (not (string= c "lineCoverage")))
                                       collect (parse-integer c)))))
      (make-hash-table :test #'equal :size (array-dimension df 0)
                       :rehash-size (1+ (ceiling (array-dimension df 0) (array-dimension df 1)))
                       :rehash-threshold 1.0
                       :initargs (loop for r across (array-row-major-contents df)
                                       collect (cons (list (getf r 'filePath) (getf r 'lineCoverage))
                                                     (loop for i from 0 to max-dim
                                                           collect (getf r (format nil "~a" i)))))))))