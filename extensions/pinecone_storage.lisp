(defun can-import (module-name)
  (handler-case
      (progn
        (asdf:load-system module-name)
        t)
    (asdf:missing-component () nil)))

(assert (can-import "pinecone") "Pinecone storage requires package pinecone-client. Install:  pip install -r extensions/requirements.txt")

(defclass pinecone-results-storage ()
  ((namespace :initarg :namespace :accessor namespace)
   (llm-model :initarg :llm-model :accessor llm-model)
   (llama-model-path :initarg :llama-model-path :accessor llama-model-path)
   (index :initarg :index :accessor index)))

(defun make-pinecone-results-storage (openai-api-key pinecone-api-key pinecone-environment llm-model llama-model-path results-store-name objective)
  (let ((namespace (remove-if (lambda (c) (not (<= #x00 c #x7F))) objective))
        (dimension (if (string-starts-with "llama" llm-model) 5120 1536))
        (metric "cosine")
        (pod-type "p1"))
    (setf openai:*api-key* openai-api-key)
    (pinecone:init :api-key pinecone-api-key :environment pinecone-environment)
    (unless (member results-store-name (pinecone:list-indexes))
      (pinecone:create-index results-store-name :dimension dimension :metric metric :pod-type pod-type))
    (let ((index (pinecone:index results-store-name))
          (index-stats-response (pinecone:describe-index-stats index)))
      (assert (= dimension (getf index-stats-response 'dimension)) "Dimension of the index does not match the dimension of the LLM embedding")
      (make-instance 'pinecone-results-storage :namespace namespace :llm-model llm-model :llama-model-path llama-model-path :index index))))

(defmethod add ((storage pinecone-results-storage) task result result-id)
  (let ((vector (get-embedding storage result)))
    (pinecone:upsert (index storage) :items `((,result-id ,vector :metadata (:task ,(getf task 'task_name) :result ,result))) :namespace (namespace storage))))

(defmethod query ((storage pinecone-results-storage) query top-results-num)
  (let* ((query-embedding (get-embedding storage query))
         (results (pinecone:query (index storage) query-embedding :top-k top-results-num :include-metadata t :namespace (namespace storage)))
         (sorted-results (sort (pinecone:matches results) #'> :key #'pinecone:score)))
    (mapcar (lambda (item) (getf (pinecone:metadata item) 'task)) sorted-results)))

(defmethod get-embedding ((storage pinecone-results-storage) text)
  (setf text (string-replace "\n" " " text))
  (if (string-starts-with "llama" (llm-model storage))
      (progn
        (require :llama)
        (let ((llm-embed (llama:make-llama :model-path (llama-model-path storage) :n-ctx 2048 :n-threads 4 :embedding t :use-mlock t)))
          (llama:embed llm-embed text)))
      (getf (first (getf (openai:embedding-create :input (list text) :model "text-embedding-ada-002") 'data)) 'embedding)))