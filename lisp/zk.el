(defvar zk-project-root (expand-file-name command-line-default-directory)
  "The root directory of a project. TAGS and SRCFILES are located here.")

(defun zk-set-project-root(f)
  "Set project root where TAGS and SRCFILES are located."
  (interactive "DProject root: ")
  (setq zk-project-root f)
  (message "Project root set as %s" f)
  (setq tags-file-name (concat f "TAGS")))

(zk-set-project-root zk-project-root)

(defun zk-index ()
  "Regenerate SRCFILES and TAGS."
  (interactive)
  (shell-command "zkindex")
  (shell-command "zk-link-javasrc"))

(defun zk-grep (pattern)
  "Grep through the files from SRCFILES."
  (interactive "sGrep in src files: ")
  (grep-find (concat "zk-grep " pattern)))

(require 'dash)
(defun zk-find-src-file-in-project(f)
  "Find a src file indexed in SRCFILES of this project."
  (interactive
   (list (ido-completing-read "Find a src file: "
                              (-map 'zk-project-get-relative-path
                               (process-lines "bash" "-c" (concat "cat '" zk-project-root "/SRCFILES'; echo -n"))))))
  (find-file (zk-project-restore-absolute-path f)))

(defun zk-project-get-relative-path(absolute-path)
  "If the absolute path starts with zk-project-root, remove it and make it a relative path"
  (if (string-prefix-p zk-project-root absolute-path)
      (let ((trimmed (substring absolute-path (length zk-project-root))))
        (if (string-prefix-p "/" trimmed)
            (substring trimmed 1)
          trimmed))
    absolute-path))

(defun zk-project-restore-absolute-path(relative-path)
  "If the path is a relative path, add zk-project-root as its prefix"
  (if (string-prefix-p "/" relative-path)
      relative-path
    (concat zk-project-root "/" relative-path)))

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
            (string< p1 p2) t)
      (if (string-prefix-p "import static" line2)
          nil (string< p1 p2)))))

(defun zk-extract-package-name-from-import (line)
  "Extracts a java package name out of the import line,
which must start with 'import '. It doesn't remove the trailing semicolon."
  (replace-regexp-in-string "^import \\(static \\)?" "" line))

(defun zk-java-identifier-at-point ()
;; sexp includes other non-identifier characters like @ in @Test
;; They must be filtered.
  (let ((id (thing-at-point 'sexp)))
    (if (string-match "[A-Za-z0-9$_]+" id)
        (match-string 0 id)
      id)))

(defun zk-insert-java-import(class-name)
  "Insert an import statement for a Java class."
  (interactive (list
		(let ((default-input (zk-java-identifier-at-point)))
		  (read-string (format "Insert import for (%s): " default-input)
			     nil nil default-input))))
  (let ((result
	 (ido-completing-read "Insert import: "
			  (process-lines "bash" "-c"
					 (concat "zk-find-java-import '" class-name "' '" zk-project-root "/SRCFILES'")))))
    (progn
      ;; Find the insertion point
      (push-mark)
      (goto-char (point-min))
      (let ((in-import-zone-p nil) (at-insert-point-p nil) current-line (continue-p t))
	(while continue-p
	  (setq current-line (zk-trim-string (thing-at-point 'line)))
	  (cond ((string= current-line result) ; import already there
                 (progn (setq continue-p nil)
                        (message "Import already exists")))
                ((string-prefix-p "package " current-line)
                 (setq in-import-zone-p t))
                ((string-prefix-p "import " current-line)
                 (if (zk-java-import-line< result current-line) (setq at-insert-point-p t)))
                ((not (string= "" current-line))
                 (if in-import-zone-p (setq at-insert-point-p t))))
	  (if at-insert-point-p
	      (progn
		(insert result)
		(insert "\n")
                (forward-line -1)  ; Place the cursor on the inserted line
		(message "Import inserted: %s" result)
		(setq continue-p nil))
	    (if (= 1 (forward-line 1))
		(setq continue-p nil))))))))

(defun zk-java-at-end-of-thing-p ()
  "Define the end of a java thing, which is a statement ending with ';',
or code block or class/function definitions that end with '}'"
  (or
   (eq ?} (char-before))
   (looking-at-p ";")))

(defun zk-goto-next-non-empty-line ()
  (let ((continue-loop-p t) (last-point -1))
    (while continue-loop-p
      (progn
        (if (string-match-p "[^\s\t\n]+" (thing-at-point 'line t))
            (setq continue-loop-p nil)
          (next-line))
        (if (eq (point) last-point)
            (setq continue-loop-p nil)
          (setq last-point (point)))
        )
      )
    )
  )

(defun zk-java-align-to-beginning-of-thing ()
  ;; If moving to a new line won't accidentally enter a thing, do it.
  (if (not (looking-at-p ".*{.*$"))
      (move-beginning-of-line 2))
  (zk-goto-next-non-empty-line))

(defun zk-java-next-thing ()
  "Move to the next statement, code block or class/function definition"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing  'forward-sexp))

(defun zk-java-prev-thing()
  "Move to the previous statement, code block or class/function definition"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing
   (lambda ()
     ;; Moving backward twice and forwarding once makes up always
     ;; stop at the same locations that zk-java-next-thing would
     ;; stop at, which allows us to use the same method to identify
     ;; the end of thing.
     (backward-sexp)
     (backward-sexp)
     (forward-sexp)
     )))

(defun zk-java-move-to-thing (move-fun)
  (let ((continue-loop-p t) (last-point -1))
    (while continue-loop-p
      (progn
        (funcall move-fun)
        (if (or (eq (point) last-point)
                (zk-java-at-end-of-thing-p))
            (setq continue-loop-p nil))
        (setq last-point (point))
        )))
  (zk-java-align-to-beginning-of-thing))

(defun zk-escape-string ()
  "Escape the current string if the point is currently in one"
  (let ((parse-state (syntax-ppss)))
    ;; The 4th element in the parse-state indicate the start of
    ;; the current string, or nil if not in.
    (if (nth 3 parse-state)
        (backward-up-list nil t t))))

(defun zk-escape-parens (left-parens-list)
  "Escape the parenthesis listed in left-parens-list"
  (while (let ((left-paren-pos (nth 1 (syntax-ppss))))
           (and left-paren-pos
                (member (char-after left-paren-pos) left-parens-list)
                (progn (backward-up-list nil t t) t)))))

(defun zk-escape-to-braces ()
  "Jump outward until it's not in a string, () or []."
  (zk-escape-string)
  (zk-escape-parens '(?\( ?\[)))

(defun zk-java-beginning-braces-block ()
  "Move to the beginning of current braces block"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing 'zk-java-prev-thing))

(defun zk-java-end-braces-block ()
  "Move to the end of current braces block"
  (interactive)
  (zk-escape-to-braces)
  (zk-java-move-to-thing 'zk-java-next-thing))

(defun zk-java-mark-thing ()
  "Mark the current thing"
  (interactive)
  (zk-escape-to-braces)
  (set-mark (point))
  (zk-java-next-thing)
  (set-mark-command nil)
  (zk-java-prev-thing))

(defun zk-java-enter-braces-block ()
  "Enter the next curly braces block"
  (interactive)
  (zk-escape-to-braces)
  (let ((continue-loop-p t)
        (last-point -1)
        (original-point (point)))
    (while continue-loop-p
      ;; If we have just walked past "}", we will always stop right
      ;; after it.  This is much easier than checking whether a "{" is
      ;; ahead.
      (if (eq ?} (char-before))
          (progn
            (backward-sexp)
            (down-list)
            ;; If the rest of the line is empty or only has line
            ;; comments, move to the next line.
            (if (looking-at-p "[\s\t]*\\(//.*\\)?\n")
                (move-beginning-of-line 2))
            (setq continue-loop-p nil))
        (progn
          (ignore-errors
            (forward-sexp))
          (if (eq (point) last-point)
              ;; Failed
              (progn
                (setq continue-loop-p nil)
                (goto-char original-point))
            (setq last-point (point))))))))


;; The default isearch stops the point at the end of search-term, which is not
;; useful at all.  Here we change it to stop at the beginning of search-term,
;; making isearch a powerful tool to move around.
(defun zk-isearch-forward-to-beginning ()
  "Do a forward search and jump to the beginning of the search-term."
  (interactive)
  (isearch-forward)
  (if isearch-other-end
      (goto-char isearch-other-end)))

(defun zk-isearch-repeat-forward-to-beginning ()
  "Do a forward repeat search and jump to the beginning of the search-term."
  (interactive)
  (isearch-repeat 'forward)
  (goto-char isearch-other-end))

(defun zk-shell-command-current-file (command)
  "Run a shell command for the current file"
  (interactive (list (read-string (format "Shell command with \"%s\": " (buffer-name)))))
  ;;(interactive "sShell command for: ")
  (let ((output-buffer-name "*zk-shell-command-current-file*")
        (command-args (split-string command)))
    (let ((program (car command-args))
          (program-args (cdr command-args))
          (filename (buffer-file-name)))
      (with-output-to-temp-buffer output-buffer-name
        (with-current-buffer output-buffer-name
          (let ((buffer-read-only nil))
            (insert "Executing: " (format "%s" command-args) " \"" filename "\"\n\n")))
        ;; Assemble the call-process call, then evaluate it
        (let ((call-process-form
               (append
                (list 'call-process program nil output-buffer-name t)
                program-args
                (list filename))))
          (eval call-process-form))))))

(global-set-key (kbd "C-s") 'zk-isearch-forward-to-beginning)
(define-key isearch-mode-map (kbd "C-s") 'zk-isearch-repeat-forward-to-beginning)

(provide 'zk)
