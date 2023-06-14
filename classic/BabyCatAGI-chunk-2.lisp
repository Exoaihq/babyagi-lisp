(defun main ()
  (let ((objective "Research Untapped Capital and provide a summary of their investment thesis, portfolio, and team.")
        (task-id-counter 1)
        (task-list nil)
        (session-summary ""))
    
    (format t "~c[96m~c[1m*****OBJECTIVE*****~c[0m~c[0m~%" #\ESC #\ESC #\ESC #\ESC)
    (format t "~a~%" objective)

    (setf task-list (task-creation-agent objective))
    (print-tasklist task-list)

    (loop
      (if (null task-list) (return))
      (dolist (task task-list)
        (when (string= (gethash "status" task) "incomplete")
          (execute-task task task-list objective)
          (print-tasklist task-list)
          (return))))
    
    (format t "~c[96m~c[1m*****SESSION SUMMARY*****~c[0m~c[0m~%" #\ESC #\ESC #\ESC #\ESC)
    (format t "~a~%" session-summary)))

(defun task-creation-agent (prompt)
  (let ((response (openai-chat-completion-create
                   :model "gpt-4"
                   :messages `((:role "system" :content "You are a task creation AI.")
                               (:role "user" :content ,prompt))
                   :temperature 0
                   :max-tokens 1500
                   :top-p 1
                   :frequency-penalty 0
                   :presence-penalty 0))
        (result ""))
    
    (setf result (gethash "content" (gethash "message" (aref (gethash "choices" response) 0))))
    (handler-case
        (json:decode-json-from-string result)
      (error (err)
        (format t "~a~%" err)))))

(defun print-tasklist (task-list)
  (format t "~c[96m~c[1m*****TASK LIST*****~c[0m~c[0m~%" #\ESC #\ESC #\ESC #\ESC)
  (dolist (task task-list)
    (format t "~a~%" (json:encode-json-to-string task))))

(defun execute-task (task task-list objective)
  ;; Execute the task and update its status, result, and result_summary
  ;; ...
  (setf (gethash "status" task) "complete")
  (setf (gethash "result" task) "Task result")
  (setf (gethash "result_summary" task) "Task result summary"))

(main)