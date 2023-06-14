(defvar *objectives* (make-hash-table :test 'equal))

(defun print-buffer (lines)
  (loop for line in lines do (format t "~A~%" line)))

(defun get-objective-names ()
  (let (names)
    (maphash (lambda (k v) (push k names)) *objectives*)
    names))

(defun get-task-names (objective)
  (let ((tasks (gethash objective *objectives*)))
    (if tasks
        (mapcar #'car tasks)
        '())))

(defun add-objective (name)
  (setf (gethash name *objectives*) '()))

(defun add-task (objective task)
  (let ((tasks (gethash objective *objectives*)))
    (push (cons task t) tasks)
    (setf (gethash objective *objectives*) tasks)))

(defun main ()
  (loop
     (let ((objectives-list (get-objective-names))
           (buffer '()))
       (if (null objectives-list)
           (setf buffer '("No objectives"))
           (dolist (objective objectives-list)
             (push "-----------------" buffer)
             (push (format nil "Objective: ~A" objective) buffer)
             (push "-----------------" buffer)
             (let ((tasks-list (get-task-names objective)))
               (push "Tasks:" buffer)
               (dolist (t tasks-list)
                 (push (format nil " * ~A" t) buffer))))
             (push "-----------------" buffer)))
       (print-buffer (reverse buffer))
       (sleep 30))))

(main)