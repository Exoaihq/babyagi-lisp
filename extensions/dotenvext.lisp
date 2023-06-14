(defvar *dotenv-files* nil)

(defun load-dotenv-extensions (dotenv-files)
  (dolist (dotenv-file dotenv-files)
    (load-dotenv dotenv-file)))