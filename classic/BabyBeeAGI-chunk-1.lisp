(defun task_manager_agent (current_task_id)
  (let ((original_task_list (copy-list task_list))
        (minified_task_list (mapcar (lambda (task) (remove-if (lambda (pair) (equal (car pair) "result")) task)) task_list)))
    (setf result (subseq result 0 4000))
    (setf prompt (concatenate 'string
                  "You are a task management AI tasked with cleaning the formatting of and reprioritizing the following tasks: "
                  (princ-to-string minified_task_list) ". "
                  "Consider the ultimate objective of your team: " (princ-to-string OBJECTIVE) ". "
                  "Do not remove any tasks. Return the result as a JSON-formatted list of dictionaries."
                  "Create new tasks based on the result of last task if necessary for the objective. Limit tasks types to those that can be completed with the available tools listed below. Task description should be detailed."
                  "The maximum task list length is 7. Do not add an 8th task."
                  "The last completed task has the following result: " result ". "
                  "Current tool option is [text-completion] " (princ-to-string websearch_var) " and [web-scrape] only."
                  "For tasks using [web-scrape], provide only the URL to scrape as the task description. Do not provide placeholder URLs, but use ones provided by a search step or the initial objective."
                  "For tasks using [web-search], provide the search query, and only the search query to use (eg. not 'research waterproof shoes, but 'waterproof shoes')"
                  "dependent_task_id should always be null or a number."
                  "Do not reorder completed tasks. Only reorder and dedupe incomplete tasks."
                  "Make sure all task IDs are in chronological order."
                  "Do not provide example URLs for [web-scrape]."
                  "Do not include the result from the last task in the JSON, that will be added after.."
                  "The last step is always to provide a final summary report of all tasks."
                  "An example of the desired output format is: "
                  "[{\"id\": 1, \"task\": \"https://untapped.vc\", \"tool\": \"web-scrape\", \"dependent_task_id\": null, \"status\": \"incomplete\", \"result\": null, \"result_summary\": null}, {\"id\": 2, \"task\": \"Analyze the contents of...\", \"tool\": \"text-completion\", \"dependent_task_id\": 1, \"status\": \"incomplete\", \"result\": null, \"result_summary\": null}, {\"id\": 3, \"task\": \"Untapped Capital\", \"tool\": \"web-search\", \"dependent_task_id\": null, \"status\": \"incomplete\", \"result\": null, \"result_summary\": null}]."))
    (print "Running task manager agent...")
    (setf response (openai:chat-completion-create
                    :model "gpt-4"
                    :messages `((:role "system" :content "You are a task manager AI.")
                                (:role "user" :content ,prompt))
                    :temperature 0.2
                    :max_tokens 1500
                    :top_p 1
                    :frequency_penalty 0
                    :presence_penalty 0))
    (setf result (gethash "content" (gethash "message" (aref (gethash "choices" response) 0))))
    (print "Done!")
    (handler-case
        (setf task_list (json:decode-json-from-string result))
      (error (err)
        (print err)))
    (loop for updated_task in task_list
          for original_task in original_task_list
          do (when (gethash "result" original_task)
               (setf (gethash "result" updated_task) (gethash "result" original_task))))
    (setf (gethash "result" (elt task_list current_task_id)) result)
    task_list))

(defun summarizer_agent (text)
  (setf text (subseq text 0 4000))
  (setf prompt (concatenate 'string "Please summarize the following text:\n" text "\nSummary:"))
  (setf response (openai:completion-create
                  :engine "text-davinci-003"
                  :prompt prompt
                  :temperature 0.5
                  :max_tokens 100
                  :top_p 1
                  :frequency_penalty 0
                  :presence_penalty 0))
  (string-trim " " (gethash "text" (aref (gethash "choices" response) 0))))

(defun overview_agent (last_task_id)
  (let ((completed_tasks (get_completed_tasks))
        (completed_tasks_text ""))
    (loop for task in completed_tasks
          do (setf completed_tasks_text (concatenate 'string completed_tasks_text
                                                     (princ-to-string (gethash "id" task)) ". "
                                                     (gethash "task" task) " - "
                                                     (gethash "result_summary" task) "\n")))
    (setf prompt (concatenate 'string
                  "Here is the current session summary:\n" session_summary "\n"
                  "The last completed task is task " (princ-to-string last_task_id) ". "
                  "Please update the session summary with the information of the last task:\n"
                  completed_tasks_text
                  "Updated session summary, which should describe all tasks in chronological order:"))
    (setf response (openai:completion-create
                    :engine "text-davinci-003"
                    :prompt prompt
                    :temperature 0.5
                    :max_tokens 100
                    :top_p 1
                    :frequency_penalty 0
                    :presence_penalty 0))
    (string-trim " " (gethash "text" (aref (gethash "choices" response) 0)))))