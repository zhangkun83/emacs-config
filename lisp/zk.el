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
  (let ((p1 (zk-extract-package-name-from-import line1))
        (p2 (zk-extract-package-name-from-import line2)))
    (if (string-prefix-p "import static" line1)
        (if (string-prefix-p "import static " line2)
            (zk-java-package-name< p1 p2) t)
      (if (string-prefix-p "import static" line2)
          nil (zk-java-package-name< p1 p2)))))

(defun zk-extract-package-name-from-import (line)
  "Extracts a java package name out of the import line,
which must start with 'import '. It doesn't remove the trailing semicolon."
  (replace-regexp-in-string "^import \\(static \\)?" "" line))

(defun zk-java-package-name< (package1 package2)
  "Decides whether one java package should be sorted before the other.
JDK packages, i.e., java.* and javax.* are the last. Other packages are
sorted in alphabetical order."
  (if (zk-jdk-package-p package1)
      (if (zk-jdk-package-p package2)
          (string< package1 package2) nil)
    (if (zk-jdk-package-p package2)
        t (string< package1 package2))))

(defun zk-jdk-package-p (package)
  "Decides whether a package name starts with 'java.' or 'javax.'"
  (or (string-prefix-p "java." package) (string-prefix-p "javax." package)))

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
                (forward-line -1)  ; Place the cursor on the inserted line
		(message "Import inserted: %s" result)
		(setq continue-p nil))
	    (if (= 1 (forward-line 1))
		(setq continue-p nil))))))))

(provide 'zk)
