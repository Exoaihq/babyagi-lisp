(defvar *sys-path* (list))
(defvar *logging-level* 'fatal)
(defvar *ray-namespace* "babyagi")
(defvar *ignore-reinit-error* t)

(defun init-ray ()
  (handler-case
      (ray:init :address "auto" :namespace *ray-namespace* :logging-level *logging-level* :ignore-reinit-error *ignore-reinit-error*)
    (error (e)
      (ray:init :namespace *ray-namespace* :logging-level *logging-level* :ignore-reinit-error *ignore-reinit-error*))))

(defclass cooperative-task-list-storage-actor ()
  ((tasks :initform (make-instance 'queue))
   (task-id-counter :initform 0)))

(defmethod append ((self cooperative-task-list-storage-actor) task)
  (enqueue task (slot-value self 'tasks)))

(defmethod replace ((self cooperative-task-list-storage-actor) tasks)
  (setf (slot-value self 'tasks) (make-instance 'queue :initial-contents tasks)))

(defmethod popleft ((self cooperative-task-list-storage-actor))
  (dequeue (slot-value self 'tasks)))

(defmethod is-empty ((self cooperative-task-list-storage-actor))
  (zerop (length (queue-head (slot-value self 'tasks)))))

(defmethod next-task-id ((self cooperative-task-list-storage-actor))
  (incf (slot-value self 'task-id-counter)))

(defmethod get-task-names ((self cooperative-task-list-storage-actor))
  (mapcar (lambda (task) (gethash "task_name" task)) (queue-head (slot-value self 'tasks))))

(defclass cooperative-task-list-storage ()
  ((name :initarg :name)
   (actor :initform nil)))

(defmethod initialize-instance :after ((self cooperative-task-list-storage) &key)
  (setf (slot-value self 'actor) (ray:get-actor :name (slot-value self 'name) :namespace *ray-namespace*))
  (unless (slot-value self 'actor)
    (setf (slot-value self 'actor) (ray:remote (make-instance 'cooperative-task-list-storage-actor) :options '(:name (slot-value self 'name) :namespace *ray-namespace* :lifetime "detached"))))
  (let ((objectives (make-instance 'cooperative-objectives-list-storage)))
    (append objectives (slot-value self 'name))))

(defmethod append ((self cooperative-task-list-storage) task)
  (ray:remote (append (slot-value self 'actor) task)))

(defmethod replace ((self cooperative-task-list-storage) tasks)
  (ray:remote (replace (slot-value self 'actor) tasks)))

(defmethod popleft ((self cooperative-task-list-storage))
  (ray:get (ray:remote (popleft (slot-value self 'actor)))))

(defmethod is-empty ((self cooperative-task-list-storage))
  (ray:get (ray:remote (is-empty (slot-value self 'actor)))))

(defmethod next-task-id ((self cooperative-task-list-storage))
  (ray:get (ray:remote (next-task-id (slot-value self 'actor)))))

(defmethod get-task-names ((self cooperative-task-list-storage))
  (ray:get (ray:remote (get-task-names (slot-value self 'actor)))))

(init-ray)