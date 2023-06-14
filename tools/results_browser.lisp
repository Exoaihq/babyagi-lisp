(defun getenv (key &optional default)
  (let ((value (uiop:getenv key)))
    (if value value default)))

(defun assert-env (value message)
  (unless value (error message)))

(defparameter *openai-api-key* (getenv "OPENAI_API_KEY" ""))
(assert-env *openai-api-key* "OPENAI_API_KEY environment variable is missing from .env")

(defparameter *pinecone-api-key* (getenv "PINECONE_API_KEY" ""))
(assert-env *pinecone-api-key* "PINECONE_API_KEY environment variable is missing from .env")

(defparameter *pinecone-environment* (getenv "PINECONE_ENVIRONMENT" "us-east1-gcp"))
(assert-env *pinecone-environment* "PINECONE_ENVIRONMENT environment variable is missing from .env")

(defparameter *pinecone-table-name* (getenv "TABLE_NAME" ""))
(assert-env *pinecone-table-name* "TABLE_NAME environment variable is missing from .env")

(defun query-records (index query &optional (top-k 1000))
  (let ((results (pinecone:query index query :top-k top-k :include-metadata t)))
    (loop for task in (pinecone:matches results)
          collect (list :name (format nil "~a" (gethash "task" (pinecone:metadata task)))
                        :result (format nil "~a" (gethash "result" (pinecone:metadata task)))))))

(defun get-ada-embedding (text)
  (let ((response (openai:embedding-create :input (list text) :model "text-embedding-ada-002")))
    (gethash "embedding" (aref (gethash "data" response) 0))))

(defun draw-tasks (stdscr tasks scroll-pos selected)
  (let ((y 0)
        (h (curses:getmaxyx stdscr 'height))
        (w (curses:getmaxyx stdscr 'width)))
    (loop for idx from scroll-pos
          for task in tasks
          while (< y h) do
          (let ((task-name (format nil "~a" (gethash :name task)))
                (truncated-str (subseq task-name 0 (1- w))))
            (if (= idx selected)
                (curses:addstr stdscr y 0 truncated-str :attr (curses:A_REVERSE))
                (curses:addstr stdscr y 0 truncated-str))
            (incf y)))))

(defun draw-result (stdscr task)
  (let ((task-name (format nil "Task: ~a" (gethash :name task)))
        (task-result (format nil "Result: ~a" (gethash :result task)))
        (w (curses:getmaxyx stdscr 'width)))
    (loop for i from 0
          for line in (cl-ppcre:split "\\s{2,}" task-name)
          do (curses:addstr stdscr i 0 line))
    (multiple-value-bind (y _) (curses:getyx stdscr)
      (curses:addstr stdscr (1+ y) 0 "------------------")
      (curses:addstr stdscr (+ y 2) 0 task-result))))

(defun draw-summary (stdscr objective tasks start num)
  (curses:box stdscr)
  (let ((summary-text (format nil "~a tasks (~a-~a) | ~a" (length tasks) start num objective)))
    (curses:addstr stdscr 1 1 (subseq summary-text 0 (1- (curses:getmaxyx stdscr 'width))))))

(defun main (stdscr)
  (setf openai:*api-key* *openai-api-key*)
  (pinecone:init :api-key *pinecone-api-key*)
  (let* ((index (pinecone:index *pinecone-table-name*))
         (h (curses:getmaxyx stdscr 'height))
         (w (curses:getmaxyx stdscr 'width))
         (left-w (truncate w 2))
         (visible-lines (- h 3))
         (scroll-pos 0)
         (selected 0)
         (objective (or (uiop:command-line-arguments) (list (getenv "OBJECTIVE" ""))))
         (retrieved-tasks (query-records index (get-ada-embedding (format nil "~{~a~^ ~}" objective)))))
    (curses:curs-set 0)
    (curses:timeout stdscr 1000)
    (loop
      (curses:clear stdscr)
      (draw-tasks (curses:subwin stdscr (- h 3) left-w 0 0) retrieved-tasks scroll-pos selected)
      (draw-result (curses:subwin stdscr h (- w left-w) 0 left-w) (nth selected retrieved-tasks))
      (draw-summary (curses:subwin stdscr 3 left-w (- h 3) 0) objective retrieved-tasks (1+ scroll-pos) (+ scroll-pos h 3))
      (curses:refresh stdscr)
      (let ((key (curses:getch stdscr)))
        (cond
          ((or (= key (char-code #\q)) (= key 27)) (return))
          ((and (= key (curses:KEY_UP)) (> selected 0))
           (decf selected)
           (when (< selected scroll-pos) (decf scroll-pos)))
          ((and (= key (curses:KEY_DOWN)) (< selected (1- (length retrieved-tasks))))
           (incf selected)
           (when (>= (- selected scroll-pos) visible-lines) (incf scroll-pos))))))))

(curses:wrapper #'main)