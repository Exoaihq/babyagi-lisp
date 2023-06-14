(defun main ()
  (let ((start_line 0)
        (end_line 0)
        (modified_chunk '())
        (lines '())
        (chunks '())
        (i 0))
    (with-open-file (stream "file_path" :direction :input)
      (setf lines (read-lines stream)))
    (loop for modification in modifications do
          (setf start_line (gethash "start_line" modification))
          (setf end_line (gethash "end_line" modification))
          (setf modified_chunk (split-sequence #\newline (gethash "modified_code" modification)))
          (setf lines (delete-sequence start_line end_line lines))
          (loop for line in modified_chunk for index from start_line to end_line do
                (setf lines (insert-at index line lines))))
    (with-open-file (stream "file_path" :direction :output :if-exists :supersede)
      (write-lines lines stream))
    (setf chunks (split_code_into_chunks "file_path" 50))
    (format t "~a" chunks)))

(defun read-lines (stream)
  (loop for line = (read-line stream nil)
        while line
        collect line))

(defun write-lines (lines stream)
  (loop for line in lines do
        (write-line line stream)))

(defun delete-sequence (start end sequence)
  (append (subseq sequence 0 start) (subseq sequence end)))

(defun insert-at (index element sequence)
  (append (subseq sequence 0 index) (list element) (subseq sequence index)))

(defun split_code_into_chunks (file_path chunk_size)
  (let ((lines '())
        (chunks '())
        (i 0))
    (with-open-file (stream file_path :direction :input)
      (setf lines (read-lines stream)))
    (loop for i from 0 to (length lines) by chunk_size do
          (let ((start_line (+ i 1))
                (end_line (min (+ i chunk_size) (length lines)))
                (chunk '()))
            (setf chunk (make-hash-table))
            (setf (gethash "start_line" chunk) start_line)
            (setf (gethash "end_line" chunk) end_line)
            (setf (gethash "code" chunk) (format nil "~{~a~^~%~}" (subseq lines i end_line)))
            (push chunk chunks)))
    (reverse chunks)))