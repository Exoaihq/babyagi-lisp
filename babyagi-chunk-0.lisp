(defun main ()
  (let* ((llm-model (getenv "LLM_MODEL" (getenv "OPENAI_API_MODEL" "gpt-3.5-turbo")))
         (openai-api-key (getenv "OPENAI_API_KEY" ""))
         (results-store-name (getenv "RESULTS_STORE_NAME" (getenv "TABLE_NAME" "")))
         (instance-name (getenv "INSTANCE_NAME" (getenv "BABY_NAME" "BabyAGI")))
         (cooperative-mode "none")
         (join-existing-objective nil)
         (objective (getenv "OBJECTIVE" ""))
         (initial-task (getenv "INITIAL_TASK" (getenv "FIRST_TASK" "")))
         (openai-temperature (parse-float (getenv "OPENAI_TEMPERATURE" "0.0"))))
    (unless (or (string-starts-with llm-model "llama") (string-starts-with llm-model "human"))
      (assert openai-api-key "OPENAI_API_KEY environment variable is missing from .env"))
    (assert results-store-name "RESULTS_STORE_NAME environment variable is missing from .env")
    (assert objective "OBJECTIVE environment variable is missing from .env")
    (assert initial-task "INITIAL_TASK environment variable is missing from .env")
    ;; ... rest of the code ...
    ))