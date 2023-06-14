(defun try-weaviate ()
  (if (and (os-environ "WEAVIATE_URL") (os-environ "WEAVIATE_API_KEY"))
      (progn
        (print (concat "\nUsing results storage: " "\033[93m\033[1m" "Weaviate" "\033[0m\033[0m"))
        (weaviate-results-storage (os-environ "WEAVIATE_URL") (os-environ "WEAVIATE_API_KEY") OBJECTIVE))
    nil))

(defun try-pinecone ()
  (if (os-environ "PINECONE_API_KEY")
      (progn
        (print (concat "\nUsing results storage: " "\033[93m\033[1m" "Pinecone" "\033[0m\033[0m"))
        (pinecone-results-storage (os-environ "PINECONE_API_KEY") OBJECTIVE))
    nil))

(defun use-chroma ()
  (print (concat "\nUsing results storage: " "\033[93m\033[1m" "Chroma (Default)" "\033[0m\033[0m"))
  (default-results-storage))

(setq results-storage (or (try-weaviate) (try-pinecone) (use-chroma)))

(defclass single-task-list-storage ()
  ((tasks :initform (make-deque) :accessor tasks)
   (task-id-counter :initform 0 :accessor task-id-counter)))

(defmethod append ((self single-task-list-storage) task)
  (deque-append (tasks self) task))

(defmethod replace ((self single-task-list-storage) tasks)
  (setf (tasks self) (make-deque tasks)))

(defmethod popleft ((self single-task-list-storage))
  (deque-pop-left (tasks self)))

(defmethod is-empty ((self single-task-list-storage))
  (null (tasks self)))

(defmethod next-task-id ((self single-task-list-storage))
  (incf (task-id-counter self)))

(defmethod get-task-names ((self single-task-list-storage))
  (mapcar (lambda (t) (gethash "task_name" t)) (tasks self)))

(setq tasks-storage (single-task-list-storage))
(if (string= COOPERATIVE_MODE "l")
    (progn
      (if (can-import "extensions.ray_tasks")
          (progn
            (import sys)
            (import (pathlib Path))
            (sys.path.append (str (Path.resolve (Path "__file__"))))
            (import (extensions.ray_tasks CooperativeTaskListStorage))
            (setq tasks-storage (CooperativeTaskListStorage OBJECTIVE))
            (print (concat "\nReplacing tasks storage: " "\033[93m\033[1m" "Ray" "\033[0m\033[0m"))))))

(defun limit-tokens-from-string (string model limit)
  (let ((encoding (condition-case nil
                      (tiktoken.encoding-for-model model)
                    (error (tiktoken.encoding-for-model "gpt2")))))
    (let ((encoded (funcall encoding "encode" string)))
      (funcall encoding "decode" (subseq encoded 0 limit)))))

(defun openai-call (prompt &key (model LLM_MODEL) (temperature OPENAI_TEMPERATURE) (max-tokens 100))
  (loop
    (condition-case nil
        (progn
          (if (string-prefix-p "llama" model :ignore-case t)
              (let ((result (llm (substring prompt 0 CTX_MAX)
                                 :stop '("### Human")
                                 :echo nil
                                 :temperature 0.2
                                 :top-k 40
                                 :top-p 0.95
                                 :repeat-penalty 1.05
                                 :max-tokens 200)))
                (string-trim (gethash "text" (aref (gethash "choices" result) 0))))
            (if (string-prefix-p "human" model :ignore-case t)
                (user-input-await prompt)
              (if (not (string-prefix-p "gpt-" model :ignore-case t))
                  (let ((response (openai.Completion.create
                                   :engine model
                                   :prompt prompt
                                   :temperature temperature
                                   :max-tokens max-tokens
                                   :top-p 1
                                   :frequency-penalty 0
                                   :presence-penalty 0)))
                    (string-trim (gethash "text" (aref (gethash "choices" response) 0))))
                (let ((trimmed-prompt (limit-tokens-from-string prompt model (- 4000 max-tokens)))
                      (messages (list (list "role" "system" "content" trimmed-prompt)))
                      (response (openai.ChatCompletion.create
                                 :model model
                                 :messages messages
                                 :temperature temperature
                                 :max-tokens max-tokens
                                 :n 1
                                 :stop nil)))
                  (string-trim (gethash "content" (gethash "message" (aref (gethash "choices" response) 0))))))))
      (openai.error.RateLimitError
       (progn
         (print "   *** The OpenAI API rate limit has been exceeded. Waiting 10 seconds and trying again. ***")
         (sleep 10)))
      (openai.error.Timeout
       (progn
         (print "   *** OpenAI API timeout occurred. Waiting 10 seconds and trying again. ***")
         (sleep 10)))
      (openai.error.APIError
       (progn
         (print "   *** OpenAI API error occurred. Waiting 10 seconds and trying again. ***")
         (sleep 10)))
      (openai.error.APIConnectionError
       (progn
         (print "   *** OpenAI API connection error occurred. Check your network settings, proxy configuration, SSL certificates, or firewall rules. Waiting 10 seconds and trying again. ***")
         (sleep 10))))))