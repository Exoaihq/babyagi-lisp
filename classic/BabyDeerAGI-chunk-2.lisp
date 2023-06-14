(defun rape-tool (task)
  (cond
    ((string= (gethash "tool" task) "user-input")
     (setf (gethash "output" task) (user-input-tool (gethash "task" task))))
    (t (setf (gethash "output" task) (rape-tool (gethash "task" task))))
  )
)

(defun task-index (task task-list)
  (position-if (lambda (t) (= (gethash "id" t) (gethash "id" task))) task-list)
)

(defun mark-task-complete (task task-list)
  (let ((index (task-index task task-list)))
    (setf (gethash "status" (elt task-list index)) "complete")
    (setf (gethash "output" (elt task-list index)) (gethash "output" task))
  )
)

(defun print-task-output (task)
  (format t "~c[93m~c[1m~%Task Output (ID:~a):~c[0m~c[0m~%" #\ESC #\ESC (gethash "id" task))
  (format t "~a~%" (gethash "output" task))
)

(defun task-ready-to-run (task task-list)
  (every #'(lambda (dep-id) (string= (gethash "status" (get-task-by-id dep-id)) "complete")) (gethash "dependent_task_ids" task))
)

(defvar *task-list* nil)

(defun task-creation-agent (objective)
  ;; ... (rest of the code)
)

(defun main-loop (objective)
  (format t "~c[96m~c[1m~%*****OBJECTIVE*****~%~c[0m~c[0m~%" #\ESC #\ESC)
  (format t "~a~%" objective)

  (setq *task-list* (task-creation-agent objective))
  (print-tasklist)

  (let ((executor (make-instance 'thread-pool-executor)))
    (loop
      (setq tasks-submitted nil)
      (dolist (task *task-list*)
        (when (and (string= (gethash "status" task) "incomplete") (task-ready-to-run task *task-list*))
          (submit-task executor task *task-list* objective)
          (setf (gethash "status" task) "running")
          (setq tasks-submitted t)
        )
      )
      (unless tasks-submitted
        (when (every #'(lambda (task) (string= (gethash "status" task) "complete")) *task-list*)
          (return))
      )
      (sleep 5)
    )
  )
)

;; Call the main-loop function with the OBJECTIVE as an argument
(main-loop "Your objective here")