(defvar zk-project-root command-line-default-directory
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
                          (process-lines "bash" "-c" (concat "cat '" (expand-file-name zk-project-root) "/SRCFILES'; echo -n")))))
  (find-file f))

(provide 'zk)
