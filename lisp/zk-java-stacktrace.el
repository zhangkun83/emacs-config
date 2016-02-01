;;; Detection of Java stack traces in compilation-mode and compilation-minor-mode
(defun zk-java-stacktrace-regexp-to-filename ()
  "Generates a relative filename from zk-java-stacktrace-regexp regexp match data."
  (let ((relative-path
         (concat (replace-regexp-in-string "\\." "/" (match-string 1))
                 (match-string 2))))
    (zk-java-stacktrace-find-path relative-path)))

(require 'dash)

(defcustom zk-java-stacktrace-path-regex "src/"
  "*The regex for selecting the actual file paths for Java stack traces."
  :group 'zk
  :type 'regexp)

(defun zk-java-stacktrace-find-path (relative-path)
  "Given a relative path of a Java source file, find a matching absolute path
and let user choose one if there are multiple matches"
  (let ((candidates
	 ;; Must use string-match-p instead of string-match here, because the latter would change
	 ;; the current state which conflicts with the work of compilation-error-regexp-alist-alist.
	 (-filter (lambda (path) (string-match-p zk-java-stacktrace-path-regex path))
		  ;; e.g., bash -c 'find . | grep \'io/grpc/Metadata.java$\'; echo -n'
		  ;; The "echo -n" is to erase the return code 1 of grep when there is no match.
		  ;; Emacs is not happy with non-zero return codes.
		  (process-lines "bash" "-c" (concat "find . | grep '" relative-path "$' ; echo -n")))))
    (progn
      (if (> (length candidates) 1)
	  (warn "More than one candidates for %s: %s . The first one will be chosen.
 Please set zk-java-stacktrace-path-regex to prevent such case."
		relative-path candidates))
      (car candidates))))

(add-to-list 'compilation-error-regexp-alist 'java)

(defun zk-java-stacektrace-enable ()
  "Enable zk's Java stack trace detection for compilation mode.
 This may override existing such functionality from other sources."
  (interactive)
  ;; This regex does not only match .java, but all file extensions in similar pattern, e.g., .groovy
  ;; The 'java entry was there initially, we are replacing it.
  (setq compilation-error-regexp-alist-alist
	(assq-delete-all 'java compilation-error-regexp-alist-alist))
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(java .
	  ("^[[:space:]]*at \\(\\(?:[[:lower:]]+\\.\\)+\\)[^(]+(\\([[:alnum:]]+\\.[[:lower:]]+\\):\\([[:digit:]]+\\))"
	   zk-java-stacktrace-regexp-to-filename 3)))
  (message "Enabled zk's Java stacktrace detection"))

(provide 'zk-java-stacktrace)
