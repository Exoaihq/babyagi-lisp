Here is the converted code in Lisp:

```lisp
(defvar *openai-api-key* (getenv "OPENAI_API_KEY"))
(defvar *openai-api-model* (getenv "OPENAI_API_MODEL"))

(defun print-colored-text (text color)
  (let ((color-mapping '((blue . "\033[34m")
                          (red . "\033[31m")
                          (yellow . "\033[33m")
                          (green . "\033[32m"))))
    (format t "~a~a\033[0m" (cdr (assoc (intern (string-upcase color)) color-mapping :test #'string=)) text)))

(defun print-char-by-char (text &optional (delay 0.00001) (chars-at-once 3))
  (loop for i from 0 below (length text) by chars-at-once
        do (let ((chunk (subseq text i (min (+ i chars-at-once) (length text)))))
             (format t "~a" chunk)
             (sleep delay))))

(defun openai-call (prompt &key (model *openai-api-model*) (temperature 0.5) (max-tokens 100))
  (let ((messages `((:role "user" :content ,prompt))))
    (multiple-value-bind (response error)
        (openai:chat-completion model messages :temperature temperature :max-tokens max-tokens)
      (if error
          (progn
            (format t "Error calling OpenAI: ~a" error)
            nil)
          (getf (first (getf response :choices)) :message.content)))))

(defun execute-command-string (command-string)
  (handler-case
      (uiop:run-program command-string :output '(:string :stripped))
    (error (e)
      (format nil "Error: ~a" e))))

(defun save-code-to-file (code file-path)
  (let ((full-path (merge-pathnames (make-pathname :directory '(:relative "playground") :name file-path))))
    (with-open-file (stream full-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format stream "~a~%" code))))

(defun refactor-code (modified-code file-path)
  (let ((full-path (merge-pathnames (make-pathname :directory '(:relative "playground") :name file-path))))
    (with-open-file (stream full-path :direction :input)
      (let ((lines (loop for line = (read-line stream nil)
                         while line
                         collect line)))
        (dolist (modification modified-code)
          (let ((start-line (getf modification :start_line))
                (end-line (getf modification :end_line))
                (modified-chunk (split-sequence:split-sequence #\newline (getf modification :modified_code))))
            (setf lines (append (subseq lines 0 start-line)
                                modified-chunk
                                (subseq lines end-line))))
          (with-open-file (stream full-path :direction :output :if-exists :supersede)
            (format stream "~{~a~%~}" lines)))))))
```

Please note that this conversion assumes you have the appropriate Lisp libraries for handling JSON, environment variables, and running external programs.