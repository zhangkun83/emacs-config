;;; zk-go-to-char.el --- Go to next CHAR which is similar to "f" and "t" in vim
;;; Copyright (C) 2017 Kun Zhang
;;; Insupred by iy-go-to-char.el by Ian Yang
;;; The goal is to provide a consistent and predictable behavior.
;;;
;;; zk-go-to-char-forward and zk-go-to-char-backward start the jump mode, which
;;; continuously accepts the following key events:
;;;   Printable character:  Move forward (backward) to the closest given character
;;;   TAB:                  (Only has effect after typing a character) Move to the next
;;;                         occurance of the last typed characer.
;;;   BACKSPACE:            Same as TAB, but in the reverse direction
;;;   RET:                  Exit the jump mode.
;;;   C-g:                  Jump to the point where jump mode was entered, and exit
;;;                         jump mode.
;;;   Other command-bound key sequence:
;;;                         Exit jump mode and excute the command.

(defvar zk-go-to-char-start-pos nil
  "Position where go to char command is started")

(defvar zk-go-to-char-original-dir 1
  "The jump direction when the go to command started. 1 or -1")

(defvar zk-go-to-char-current-char nil
  "Last char used for jump.")

(defun zk-go-to-char-forward ()
  "Jump forward to chars that are typed subsequently."
  (interactive)
  (zk-go-to-char--init 1)
  (zk-go-to-char--prompt))

(defun zk-go-to-char-backward ()
  "Jump backward to chars that are typed subsequently."
  (interactive)
  (zk-go-to-char--init -1)
  (zk-go-to-char--prompt))

(defun zk-go-to-char--init (dir)
  (setq zk-go-to-char-original-dir dir)
  (setq zk-go-to-char-start-pos (point))
  (setq zk-go-to-char-current-char nil))

(defun zk-go-to-char--prompt ()
  (while
      (let* ((inhibit-quit t)
             (event (read-event (format "Jump %s to a char (RET: Done; C-g: Cancel%s):"
                                        (if (eq zk-go-to-char-original-dir 1) "forward" "backward")
                                        (if zk-go-to-char-current-char
                                            (format "; TAB: next '%c'; Backspace: prev '%c'"
                                                    zk-go-to-char-current-char zk-go-to-char-current-char)
                                          "")
                                        ))))
        (cond
         ((and (characterp event)
               (string-match-p "[[:graph:]]" (string event)))
          (progn
            (setq zk-go-to-char-current-char event)
            (zk-go-to-char--move zk-go-to-char-original-dir)
            t))
         ((eq event 'return)
          (progn
            (message "Done!")
            nil))
         ((eq event ?\^G)
          (progn
            (goto-char zk-go-to-char-start-pos)
            (message "Jump cancelled. Returned to origin.")
            nil))
         ((eq event 'tab)
          (progn
            (zk-go-to-char--move zk-go-to-char-original-dir)
            t))
         ((eq event 'backspace)
          (progn
            (zk-go-to-char--move (- zk-go-to-char-original-dir))
            t))
         (t (progn
              ;; Pass-through
              (push event unread-command-events)
              (let* ((keys (read-key-sequence-vector nil))
                     (command (and keys (key-binding keys))))
                (when (commandp command)
                  ;; Avoid recursion on itself
                  (cond
                   ((eq command 'zk-go-to-char-forward)
                    (progn
                      (setq zk-go-to-char-original-dir 1)
                      t))
                   ((eq command 'zk-go-to-char-backward)
                    (progn
                      (setq zk-go-to-char-original-dir -1)
                      t))
                   (t
                    (progn
                      (setq this-command command
                            this-original-command command)
                      (call-interactively command)
                      nil)))))))))))

(defun zk-go-to-char--move (dir)
  (when zk-go-to-char-current-char
    (if (> dir 0) (forward-char))
    (search-forward (char-to-string zk-go-to-char-current-char) nil nil dir)
    (if (> dir 0) (backward-char))))

(provide 'zk-go-to-char)