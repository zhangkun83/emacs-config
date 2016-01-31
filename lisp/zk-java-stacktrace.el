;;; Detection of Java stack traces in compilation-mode and compilation-minor-mode
;;; TODO should also support .groovy
(defun zk-java-stacktrace-regexp-to-filename ()
  "Generates a relative filename from zk-java-stacktrace-regexp regexp match data."
  (let ((relative-path
         (concat (replace-regexp-in-string "\\." "/" (match-string 1))
                 (match-string 2))))
    (zk-java-stacktrace-find-path relative-path)))

(defun zk-java-stacktrace-find-path (relative-path)
  "Given a relative path of a Java source file, find a matching absolute path
and let user choose one if there are multiple matches"
  (let ((candidates
         ; e.g., bash -c 'find . | grep \'io/grpc/Metadata.java$\''
         (process-lines "bash" "-c" (concat "find . | grep '" relative-path "$'"))))
    (if (> (length candidates) 1)
        (completing-read "Select file: " candidates)
      candidates)))

(add-to-list 'compilation-error-regexp-alist 'zk-java-stacktrace-regexp)
(add-to-list 'compilation-error-regexp-alist-alist
  '(zk-java-stacktrace-regexp .
    ("^[[:space:]]*at \\(\\(?:[[:lower:]]+\\.\\)+\\)[^(]+(\\([[:alnum:]]+\\.java\\):\\([[:digit:]]+\\))"
     zk-java-stacktrace-regexp-to-filename 3)))

(provide 'zk-java-stacktrace)
