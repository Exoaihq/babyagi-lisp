(defun openai-call (prompt max-tokens)
  ;; Call OpenAI API with the given prompt and max tokens
  ;; Returns the generated response
  (let ((response (call-openai-api prompt max-tokens)))
    (get-response-text response)))

(defun execution-agent (objective task)
  (let ((context (context-agent objective 5))
        (prompt (format nil "Perform one task based on the following objective: ~A.~%" objective)))
    (when context
      (setf prompt (concatenate 'string prompt "Take into account these previously completed tasks:" (format nil "~{~A~^~%~}" context))))
    (setf prompt (concatenate 'string prompt (format nil "~%Your task: ~A~%Response:" task)))
    (openai-call prompt 2000)))

(defun context-agent (query top-results-num)
  (let ((results (query-results-storage query top-results-num)))
    results))

(defun main ()
  (let ((loop t))
    (unless *join-existing-objective*
      (tasks-storage-append (list :task_id (tasks-storage-next-task-id)
                                  :task_name *initial-task*)))
    (while loop
      (if (not (tasks-storage-is-empty))
          (progn
            (format t "~c[95m~c[1m~%*****TASK LIST*****~%~c[0m~c[0m~%")
            (dolist (t (tasks-storage-get-task-names))
              (format t " • ~A~%" t))
            (let ((task (tasks-storage-popleft)))
              (format t "~c[92m~c[1m~%*****NEXT TASK*****~%~c[0m~c[0m~%")
              (format t "~A~%" (getf task :task_name))
              (let ((result (execution-agent *objective* (getf task :task_name))))
                (format t "~c[93m~c[1m~%*****TASK RESULT*****~%~c[0m~c[0m~%")
                (format t "~A~%" result)
                (let ((enriched-result (list :data result))
                      (result-id (format nil "result_~A" (getf task :task_id))))
                  (results-storage-add task result result-id)
                  (let ((new-tasks (task-creation-agent *objective* enriched-result (getf task :task_name) (tasks-storage-get-task-names))))
                    (format t "Adding new tasks to task_storage~%")
                    (dolist (new-task new-tasks)
                      (setf (getf new-task :task_id) (tasks-storage-next-task-id))
                      (format t "~A~%" new-task)
                      (tasks-storage-append new-task))
                    (unless *join-existing-objective*
                      (let ((prioritized-tasks (prioritization-agent)))
                        (when prioritized-tasks
                          (tasks-storage-replace prioritized-tasks)))))))
              (sleep 5)))
        (progn
          (format t "Done.~%")
          (setf loop nil))))))

(main)