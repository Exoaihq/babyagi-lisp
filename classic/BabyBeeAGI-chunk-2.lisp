(defun add-task (task)
  (push task task-list))

(defun execute-task (task task-list objective)
  (let ((task-id (gethash "id" task))
        (task-tool (gethash "tool" task))
        (dependent-task-id (gethash "dependent_task_id" task)))
    (when (or (null dependent-task-id)
              (string= (gethash "status" (find dependent-task-id task-list :key (lambda (t) (gethash "id" t)))) "complete"))
      (setf (gethash "status" task) "complete")
      (setf (gethash "result" task) (format nil "Result for task ~a" task-id))
      (setf (gethash "result_summary" task) (format nil "Task ~a completed" task-id))
      (setf session-summary (update-session-summary objective task-list)))))

(defun update-session-summary (objective task-list)
  (let ((prompt (format nil "~a~%Updated session summary, which should describe all tasks in chronological order:" objective)))
    (setf response (openai-completion-create
                    :engine "text-davinci-003"
                    :prompt prompt
                    :temperature 0.5
                    :max_tokens 200
                    :top_p 1
                    :frequency_penalty 0
                    :presence_penalty 0))
    (string-trim response)))

(defvar task-list nil)
(defvar session-summary "")
(defvar task-id-counter 0)

(defvar first-task (list :id 1
                         :task "YOUR_FIRST_TASK"
                         :tool "text-completion"
                         :dependent_task_id nil
                         :status "incomplete"
                         :result ""
                         :result_summary ""))
(add-task first-task)

(defvar objective "YOUR_OBJECTIVE")
(format t "~c[96m~c[1m~%*****OBJECTIVE*****~%~c[0m~c[0m~%" #x1b #x1b #x1b)
(format t "~a~%" objective)

(loop while (some (lambda (task) (string= (gethash "status" task) "incomplete")) task-list)
      do (let ((incomplete-tasks (remove-if (lambda (task) (string= (gethash "status" task) "complete")) task-list)))
           (when incomplete-tasks
             (setf incomplete-tasks (sort incomplete-tasks (lambda (a b) (< (gethash "id" a) (gethash "id" b)))))
             (execute-task (first incomplete-tasks) task-list objective)
             (format t "~c[95m~c[1m~%*****TASK LIST*****~%~c[0m~%" #x1b #x1b #x1b)
             (dolist (t task-list)
               (let ((dependent-task (if (gethash "dependent_task_id" t) (format nil "~c[31m<dependency: #~a>~c[0m" (gethash "dependent_task_id" t) #x1b) ""))
                     (status-color (if (string= (gethash "status" t) "complete") "~c[32m" "~c[31m")))
                 (format t "~c[1m~a~c[0m: ~a ~a[~a]~c[0m ~c[93m[~a] ~a~c[0m~%" #x1b (gethash "id" t) #x1b (gethash "task" t) status-color (gethash "status" t) #x1b (gethash "tool" t) dependent-task #x1b)))
             (format t "~c[93m~c[1m~%*****SESSION SUMMARY*****~%~c[0m~c[0m~%" #x1b #x1b #x1b)
             (format t "~a~%" session-summary)
             (sleep 1))))

(when (every (lambda (task) (string/= (gethash "status" task) "incomplete")) task-list)
  (format t "~c[92m~c[1m~%*****ALL TASKS COMPLETED*****~%~c[0m~c[0m~%" #x1b #x1b #x1b)
  (dolist (task task-list)
    (format t "ID: ~a, Task: ~a, Result: ~a~%" (gethash "id" task) (gethash "task" task) (gethash "result" task))))