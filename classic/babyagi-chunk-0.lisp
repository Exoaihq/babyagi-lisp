Here's the code converted to Lisp:

```lisp
(defvar *openai-api-key* "")
(defvar *pinecone-api-key* "")
(defvar *pinecone-environment* "us-east1-gcp")
(defvar *your-table-name* "test-table")
(defvar *objective* "Solve world hunger.")
(defvar *your-first-task* "Develop a task list.")

(format t "~%~c[96m~c[1m*****OBJECTIVE*****~c[0m~c[0m~%" #\ESC #\ESC)
(format t "~a~%" *objective*)

; Configure OpenAI and Pinecone
; (openai.api-key *openai-api-key*)
; (pinecone.init :api-key *pinecone-api-key* :environment *pinecone-environment*)

; Create Pinecone index
; (pinecone.create-index *your-table-name* :dimension 1536 :metric "cosine" :pod-type "p1")

; Connect to the index
; (pinecone.index *your-table-name*)

(defvar *task-list* (make-queue))

(defun add-task (task)
  (enqueue task *task-list*))

(defun get-ada-embedding (text)
  ; Replace this function with the appropriate Lisp code to call the OpenAI API
  ; and get the embedding for the given text.
  )

(defun task-creation-agent (objective result task-description task-list)
  ; Replace this function with the appropriate Lisp code to call the OpenAI API
  ; and generate new tasks based on the given parameters.
  )

(defun prioritization-agent (this-task-id)
  ; Replace this function with the appropriate Lisp code to call the OpenAI API
  ; and prioritize the tasks in the task list.
  )

(defun execution-agent (objective task)
  ; Replace this function with the appropriate Lisp code to call the OpenAI API
  ; and execute the given task based on the objective.
  )

(defun context-agent (query index n)
  ; Replace this function with the appropriate Lisp code to call the Pinecone API
  ; and get the context for the given query.
  )

; Add the first task
(add-task (list :task-id 1 :task-name *your-first-task*))

; Main loop
(loop
  (when *task-list*
    ; Print the task list
    (format t "~%~c[95m~c[1m*****TASK LIST*****~c[0m~c[0m~%" #\ESC #\ESC)
    (dolist (t (queue-list *task-list*))
      (format t "~a: ~a~%" (getf t :task-id) (getf t :task-name)))

    ; Step 1: Pull the first task
    (let ((task (dequeue *task-list*)))
      (format t "~%~c[92m~c[1m*****NEXT TASK*****~c[0m~c[0m~%" #\ESC #\ESC)
      (format t "~a: ~a~%" (getf task :task-id) (getf task :task-name))

      ; Send to execution function to complete the task based on the context
      (let ((result (execution-agent *objective* (getf task :task-name))))
        (format t "~%~c[93m~c[1m*****TASK RESULT*****~c[0m~c[0m~%" #\ESC #\ESC)
        (format t "~a~%" result)

        ; Step 2: Create new tasks
        (let ((new-tasks (task-creation-agent *objective* result (getf task :task-name) (queue-list *task-list*))))
          (dolist (new-task new-tasks)
            (add-task new-task)))

        ; Step 3: Prioritize tasks
        (prioritization-agent (getf task :task-id))))))
```

Please note that the code above is a direct translation of the provided Python code, and some parts are commented out because they require specific Lisp libraries or API calls that are not available in standard Lisp. You will need to replace those parts with the appropriate Lisp code to call the OpenAI and Pinecone APIs.