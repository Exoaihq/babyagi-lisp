(defun getenv (key &optional default)
  (let ((value (uiop:getenv key)))
    (if value value default)))

(defparameter *openai-api-key* (getenv "OPENAI_API_KEY" ""))
(assert (not (string= *openai-api-key* "")) "OPENAI_API_KEY environment variable is missing from .env")

(defparameter *pinecone-api-key* (getenv "PINECONE_API_KEY" ""))
(assert (not (string= *pinecone-api-key* "")) "PINECONE_API_KEY environment variable is missing from .env")

(defparameter *pinecone-environment* (getenv "PINECONE_ENVIRONMENT" "us-east1-gcp"))
(assert (not (string= *pinecone-environment* "")) "PINECONE_ENVIRONMENT environment variable is missing from .env")

(defparameter *pinecone-table-name* (getenv "TABLE_NAME" ""))
(assert (not (string= *pinecone-table-name* "")) "TABLE_NAME environment variable is missing from .env")

(defun query-records (index query &optional (top-k 1000))
  (let ((results (pinecone:query index query :top-k top-k :include-metadata t)))
    (loop for task in (pinecone:matches results)
          collect (format nil "~A:~%~A~%------------------" (gethash "task" (pinecone:metadata task)) (gethash "result" (pinecone:metadata task))))))

(defun get-ada-embedding (text)
  (let ((text (substitute #\Space #\Newline text)))
    (first (gethash "embedding" (first (gethash "data" (openai:embedding-create :input (list text) :model "text-embedding-ada-002")))))))

(defun main ()
  (let* ((objective (or (getenv "OBJECTIVE" "") (format nil "~{~A~^ ~}" (uiop:command-line-arguments))))
         (query (get-ada-embedding objective))
         (retrieved-tasks (query-records *pinecone-table-name* query)))
    (dolist (task retrieved-tasks)
      (format t "~A~%" task))))

(main)