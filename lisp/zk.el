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


;; TODO: automatically load the symbol under cursor
;; TODO: find and insert to the proper location
(defun zk-insert-java-import(class-name)
  "Insert an import statement for a Java class."
  (interactive "*sClass name: ")
  (let ((result
	 (completing-read "Insert: "
			  (process-lines "bash" "-c"
					 (concat "zk-find-java-import '" class-name "' '" zk-project-root "/SRCFILES'")))))
    (insert result)))

(provide 'zk)
