(defun text-completion-tool (prompt)
  (let ((response (openai:Completion.create
                   :engine "text-davinci-003"
                   :prompt prompt
                   :temperature 0.5
                   :max_tokens 1500
                   :top_p 1
                   :frequency_penalty 0
                   :presence_penalty 0)))
    (string-trim (openai:choice-text (first (openai:completion-choices response))))))

(defun web-search-tool (query)
  (let ((search-params (list :engine "google"
                             :q query
                             :api_key SERPAPI_API_KEY
                             :num 3))
        (search-results (make-instance 'GoogleSearch :params search-params))
        (results (GoogleSearch:get-dict search-results)))
    (format nil "~a" (gethash "organic_results" results))))

(defun web-scrape-tool (url)
  (let ((response (requests:get url))
        (soup (BeautifulSoup:BeautifulSoup (requests:content response) "html.parser"))
        (result (BeautifulSoup:get-text soup :strip t))
        (links (BeautifulSoup:find-all soup 'a :attrs '((href . "^https://")))))
    (loop for link in links
          do (setf result (concatenate 'string result ", " (BeautifulSoup:get link 'href))))
    (concatenate 'string result "URLs: ")))

(defun execute-task (task task-list objective)
  (when (or (not (gethash "dependent_task_id" task))
            (string= (gethash "status" (get-task-by-id (gethash "dependent_task_id" task))) "complete"))
    (format t "~c[92m~c[1m~%*****NEXT TASK*****~%~c[0m~c[0m~%" #x1b #x1b #x1b #x1b)
    (format t "~a: ~a [~a]~%" (gethash "id" task) (gethash "task" task) (gethash "tool" task))
    (let* ((task-prompt (format nil "Complete your assigned task based on the objective: ~a. Your task: ~a~%Response:" objective (gethash "task" task)))
           (result (cond ((string= (gethash "tool" task) "text-completion") (text-completion-tool task-prompt))
                         ((string= (gethash "tool" task) "web-search") (web-search-tool task-prompt))
                         ((string= (gethash "tool" task) "web-scrape") (web-scrape-tool (gethash "task" task)))
                         (t "Unknown tool"))))
      (format t "~c[93m~c[1m~%*****TASK RESULT*****~%~c[0m~c[0m~%" #x1b #x1b #x1b #x1b)
      (format t "~a~%" (if (string= result (subseq result 0 2000))
                           result
                           (concatenate 'string (subseq result 0 2000) "...")))
      (setf (gethash "status" task) "complete")
      (setf (gethash "result" task) result)
      (setf (gethash "result_summary" task) (summarizer-agent result))
      (setf session-summary (overview-agent (gethash "id" task)))
      (incf task-id-counter)
      (task-manager-agent objective result (gethash "task" task) (mapcar (lambda (task) (gethash "task" task)) (remove-if (lambda (task) (string= (gethash "status" task) "complete")) task-list)) (gethash "id" task)))))