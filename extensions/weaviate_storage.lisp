(defpackage :weaviate-results-storage
  (:use :cl)
  (:export :can-import
           :create-client
           :weaviate-results-storage
           :create-schema
           :add
           :query
           :get-embedding))

(in-package :weaviate-results-storage)

(defun can-import (module-name)
  (handler-case
      (progn
        (asdf:load-system module-name)
        t)
    (asdf:missing-component () nil)))

(assert (can-import "weaviate-client") "Weaviate storage requires package weaviate-client. Install:  pip install -r extensions/requirements.txt")

(defun create-client (weaviate-url weaviate-api-key weaviate-use-embedded)
  (if weaviate-use-embedded
      (weaviate:make-client :embedded-options (weaviate:make-embedded-options))
    (let ((auth-config (if weaviate-api-key
                           (weaviate:make-auth-api-key :api-key weaviate-api-key)
                         nil)))
      (weaviate:make-client :url weaviate-url :auth-client-secret auth-config))))

(defclass weaviate-results-storage ()
  ((client :initarg :client :accessor client)
   (index-name :initarg :index-name :accessor index-name)
   (llm-model :initarg :llm-model :accessor llm-model)
   (llama-model-path :initarg :llama-model-path :accessor llama-model-path)))

(defmethod initialize-instance :after ((self weaviate-results-storage) &key openai-api-key weaviate-url weaviate-api-key weaviate-use-embedded llm-model llama-model-path results-store-name objective)
  (setf (slot-value self 'client) (create-client weaviate-url weaviate-api-key weaviate-use-embedded))
  (setf (slot-value self 'llm-model) llm-model)
  (setf (slot-value self 'llama-model-path) llama-model-path)
  (create-schema self results-store-name))

(defmethod create-schema ((self weaviate-results-storage) results-store-name)
  (let ((valid-class-name (ppcre:create-scanner "^[A-Z][a-zA-Z0-9_]*$")))
    (unless (ppcre:scan valid-class-name results-store-name)
      (error "Invalid index name: ~A. Index names must start with a capital letter and contain only alphanumeric characters and underscores." results-store-name)))
  (let ((schema (list :class results-store-name
                      :properties (list (list :name "result_id" :dataType (list "string"))
                                        (list :name "task" :dataType (list "string"))
                                        (list :name "result" :dataType (list "text"))))))
    (if (weaviate:schema-contains (client self) schema)
        (format t "Index named ~A already exists. Reusing it." results-store-name)
      (progn
        (format t "Creating index named ~A" results-store-name)
        (weaviate:schema-create-class (client self) schema))))
  (setf (index-name self) results-store-name))

(defmethod add ((self weaviate-results-storage) task result result-id vector)
  (let ((enriched-result (list :data result)))
    (setf vector (get-embedding self (getf enriched-result :data)))
    (weaviate:with-batch (client self)
      (let ((data-object (list :result_id result-id
                               :task (getf task :task_name)
                               :result result)))
        (weaviate:batch-add-data-object (client self) :data-object data-object :class-name (index-name self) :vector vector)))))

(defmethod query ((self weaviate-results-storage) query top-results-num)
  (let ((query-embedding (get-embedding self query))
        (results (weaviate:query-get (client self) (index-name self) (list "task"))
                  :with-hybrid (list :query query :alpha 0.5 :vector query-embedding)
                  :with-limit top-results-num
                  :do))
    (_extract-tasks self results)))

(defmethod _extract-tasks ((self weaviate-results-storage) data)
  (let ((task-data (getf (getf (getf data :data) :Get) (index-name self))))
    (mapcar (lambda (item) (getf item :task)) task-data)))

(defmethod get-embedding ((self weaviate-results-storage) text)
  (setf text (ppcre:regex-replace-all "\\n" text " "))
  (if (str:starts-with? (llm-model self) "llama")
      (progn
        (ql:quickload "llama-cpp")
        (let ((llm-embed (llama-cpp:make-llama :model-path (llama-model-path self)
                                              :n-ctx 2048
                                              :n-threads 4
                                              :embedding t
                                              :use-mlock t)))
          (llama-cpp:embed llm-embed text)))
    (let ((embedding-result (openai:embedding-create :input (list text) :model "text-embedding-ada-002")))
      (getf (first (getf embedding-result :data)) :embedding))))