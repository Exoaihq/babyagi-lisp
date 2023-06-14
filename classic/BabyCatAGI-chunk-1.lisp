Here's the code converted to Lisp:

```lisp
(defun fetch-url-content (url)
  (let ((headers '("User-Agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/94.0.4606.81 Safari/537.36")))
    (handler-case
        (let ((response (drakma:http-request url :headers headers :timeout 10)))
          (if (= 200 (drakma:http-response-status response))
              (babel:octets-to-string (drakma:http-response-body response))
              ""))
      (error (e)
        (format t "Error while fetching the URL: ~A" e)
        ""))))

(defun extract-links (content)
  (let ((soup (plump:parse content)))
    (loop for link in (plump:get-elements-by-tag-name soup "a")
          for href = (plump:attribute link "href")
          if (and href (uiop:starts-with-subseq "http" href))
          collect href)))

(defun extract-text (content)
  (let ((soup (plump:parse content)))
    (plump:serialize-text soup)))
```

Please note that this conversion requires the following Lisp libraries: `drakma`, `babel`, `plump`, `uiop`. Make sure to install these libraries before running the code.