(defvar *actor-name* "BabyAGI Objectives")

(defun init-ray ()
  (handler-case
      (ray:init :address "auto" :namespace "babyagi" :logging-level :fatal :ignore-reinit-error t)
    (error (e)
      (ray:init :namespace "babyagi" :logging-level :fatal :ignore-reinit-error t))))

(defclass cooperative-objectives-list-storage-actor ()
  ((objectives :initform (make-deque) :accessor objectives)))

(defmethod append ((self cooperative-objectives-list-storage-actor) objective)
  (unless (member objective (objectives self))
    (deque-append (objectives self) objective)))

(defmethod is-empty ((self cooperative-objectives-list-storage-actor))
  (null (objectives self)))

(defmethod get-objective-names ((self cooperative-objectives-list-storage-actor))
  (coerce (objectives self) 'list))

(defclass cooperative-objectives-list-storage ()
  ((actor :initform (progn
                      (handler-case
                          (ray:get-actor :name *actor-name* :namespace "babyagi")
                        (error (e)
                          (ray:remote (cooperative-objectives-list-storage-actor)
                                      :options (list :name *actor-name* :namespace "babyagi" :lifetime "detached"))))))))

(defmethod append ((self cooperative-objectives-list-storage) objective)
  (ray:remote (slot-value self 'actor) 'append objective))

(defmethod is-empty ((self cooperative-objectives-list-storage))
  (ray:get (ray:remote (slot-value self 'actor) 'is-empty)))

(defmethod get-objective-names ((self cooperative-objectives-list-storage))
  (ray:get (ray:remote (slot-value self 'actor) 'get-objective-names)))