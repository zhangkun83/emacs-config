;;; Detection of Java stack traces in compilation-mode and compilation-minor-mode
(defun zk-java-stacktrace-regexp-to-filename ()
  "Generates a relative filename from zk-java-stacktrace-regexp regexp match data."
  (let ((relative-path
         (concat (replace-regexp-in-string "\\." "/" (match-string 1))
                 (match-string 2))))
    (zk-java-stacktrace-find-path relative-path)))

(require 'dash)
(require 'zk)

(defun zk-java-stacktrace-find-path (relative-path)
  "Given a relative path of a Java source file, find a matching absolute path
and let user choose one if there are multiple matches"
  (let ((candidates
	 ;; fgrep doesn't check if relative-path is the suffix of the full path.
	 ;; grep with regexp would be able to, but I don't want to convert relative-path to regexp-compatible.
	 ;; We need to do a filter ourselves.
	 (-filter (lambda (path) (string-suffix-p relative-path path))
		  ;; e.g., bash -c 'fgrep \'io/grpc/Metadata.java\' <project_root>/SRCFILES; echo -n'
		  ;; The "echo -n" is to erase the return code 1 of grep when there is no match.
		  ;; Emacs is not happy with non-zero return codes.
		  (process-lines "bash" "-c" (concat "fgrep '" relative-path "' '" zk-project-root "/SRCFILES'; echo -n")))))
    (progn
      (message "Candidates: %s" candidates)
      (if (> (length candidates) 1)
	  (warn "More than one candidates for %s: %s . The first one will be chosen.
 Double-check your generation rule of SRCFILES."
		relative-path candidates))
      (car candidates))))


(defun zk-java-stacektrace-enable ()
  "Enable zk's Java stack trace detection for compilation mode.
 This may override existing such functionality from other sources."
  (interactive)
  (add-to-list 'compilation-error-regexp-alist 'java)
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
