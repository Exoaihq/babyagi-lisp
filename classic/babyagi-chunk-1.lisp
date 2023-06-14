(defvar *objective* nil)
(defvar *task-name* nil)
(defvar *result* nil)
(defvar *this-task-id* nil)
(defvar *enriched-result* nil)
(defvar *result-id* nil)
(defvar *vector* nil)
(defvar *new-tasks* nil)
(defvar *new-task* nil)
(defvar *task-id-counter* 0)

(loop
  (setq *result* (execution-agent *objective* *task-name*))
  (setq *this-task-id* (parse-integer *task-id*))
  (format t "~c[93m~c[1m~%*****TASK RESULT*****~%~c[0m~c[0m~%" #\ESC #\ESC #\ESC)
  (format t "~a~%" *result*)

  (setq *enriched-result* (list (cons 'data *result*)))
  (setq *result-id* (concatenate 'string "result_" *task-id*))
  (setq *vector* (cdr (assoc 'data *enriched-result*)))
  (index-upsert (list (list *result-id* (get-ada-embedding *vector*) (list (cons 'task *task-name*) (cons 'result *result*)))))

  (setq *new-tasks* (task-creation-agent *objective* *enriched-result* *task-name* (mapcar (lambda (t) (cdr (assoc 'task-name t))) *task-list*)))

  (dolist (*new-task* *new-tasks*)
    (incf *task-id-counter*)
    (setf (getf *new-task* 'task-id) *task-id-counter*)
    (add-task *new-task*))
  (prioritization-agent *this-task-id*)

  (sleep 1))