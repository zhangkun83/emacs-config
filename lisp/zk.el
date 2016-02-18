(defvar zk-project-root (expand-file-name command-line-default-directory)
  "The root directory of a project. TAGS and SRCFILES are located here.")

(defun zk-set-project-root(f)
  "Set project root where TAGS and SRCFILES are located."
  (interactive "DProject root: ")
  (setq zk-project-root f)
  (message "Project root set as %s" f)
  (setq tags-file-name (concat f "TAGS")))

(zk-set-project-root zk-project-root)

(defun zk-find-src-file-in-project(f)
  "Find a src file indexed in SRCFILES of this project."
  (interactive
   (list (completing-read "Find a src file: "
                          (process-lines "bash" "-c" (concat "cat '" zk-project-root "/SRCFILES'; echo -n")))))
  (find-file f))

(defun zk-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun zk-java-import-line< (line1 line2)
  "Decides whether one java import line should appear before the other.
They must not be equal, and must start with 'import '."
  (if (string-prefix-p "import static" line1)
      (if (string-prefix-p "import static " line2)
	  (string< line1 line2) t)
    (if (string-prefix-p "import static" line2)
	nil (string< line1 line2))))

(defun zk-insert-java-import(class-name)
  "Insert an import statement for a Java class."
  (interactive (list
		(let ((default-input (thing-at-point 'word)))
		  (read-string (format "Insert import for (%s): " default-input)
			     nil nil default-input))))
  (let ((result
	 (completing-read "Insert import: "
			  (process-lines "bash" "-c"
					 (concat "zk-find-java-import '" class-name "' '" zk-project-root "/SRCFILES'")))))
    (progn
      ;; Find the insertion point
      (push-mark)
      (goto-char (point-min))
      (let ((in-import-zone-p nil) (at-insert-point-p nil) current-line (continue-p t))
	(while continue-p
	  (setq current-line (zk-trim-string (thing-at-point 'line)))
	  (if (string= current-line result) ; import already there
	      (progn
		(setq continue-p nil)
		(message "Import already exists"))
	    (if (string-prefix-p "package " current-line)
		(setq in-import-zone-p t)
	      (if (string-prefix-p "import " current-line)
		  (if (zk-java-import-line< result current-line) (setq at-insert-point-p t))
		(if (not (string= "" current-line))
		    (if in-import-zone-p (setq at-insert-point-p t))))))
	  (if at-insert-point-p
	      (progn
		(insert result)
		(insert "\n")
		(message "Import inserted: %s" result)
		(setq continue-p nil))
	    (if (= 1 (forward-line 1))
		(setq continue-p nil))))))))

(provide 'zk)
