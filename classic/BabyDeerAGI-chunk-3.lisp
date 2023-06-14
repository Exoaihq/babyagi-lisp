(defparameter *task-list* nil)
(defparameter *tasks-submitted* nil)

(defun check-tasks ()
  (loop
    (setf *tasks-submitted* nil)
    (dolist (task *task-list*)
      (if (string= (getf task :status) "in_progress")
          (setf *tasks-submitted* t)))
    (if (and (not *tasks-submitted*)
             (every (lambda (task) (string= (getf task :status) "complete")) *task-list*))
        (return))
    (sleep 5)))

(defun print-session-summary ()
  (format t "~c[96m~c[1m~%*****SAVING FILE...*****~%~c[0m~c[0m")
  (with-open-file (file (format nil "output/output_~a.txt" (format-timestring "%d_%m_%Y_%H_%M_%S" (get-universal-time)))
                        :direction :output
                        :if-exists :supersede)
    (write-line *session-summary* file))
  (format t "...file saved.~%END"))

(check-tasks)
(print-session-summary)