(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/neotree"))


;;;; text font under linux
(if (eq system-type 'gnu/linux)
  (setq default-frame-alist '(
    (cursor-color . "#ffffff")
    (font . "-xos4-terminus-medium-r-normal-*-17-*-*-*-*-*-iso8859-1")))
  (setq default-frame-alist '(
    (cursor-color . "#ffffff"))))

;;; Always blink the cursor
(blink-cursor-mode t)

;;;; magit (git integration)
(require 'magit)


;;; neotree (tree directory view)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Do not load evil-mode because I do not like inconsistent editing behavior between buffers.
;; Also there is a issue that if you try to use :e to open a file under a non-existing directory,
;; magit will mess up and prevent you from entering any command. Maybe
;; re-enable evil-mode after I resolved this issue.
;(load-file (expand-file-name "~/.emacs.d/lisp/enable-evil-mode.el"))

;;;; etags-select (better ctags search)
(load "etags-select.el")
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;;;; Always do case-sensitive search
(setq-default tags-case-fold-search nil)
(setq-default case-fold-search nil)


;;;; Load TAGS file under current directory
(add-to-list 'tags-table-list (concat default-directory "TAGS"))


;;;; Don't enable semantic (semantic doesn't work if Java file contains generics)
;;(semantic-mode 1)


;;;; The ido mode for more interactive searching
(require 'ido)
(ido-mode t)


;;;; Load lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;; Load google-specific bits
(load "init-google.el")

;;; magit would complain about git-commit-mode being loaded. It is loaded from google-specific
;;; the system config files, so I have to filter it out after it's added.
(require 'dash)  ; provides --filter
(setq load-path (--filter (not (string-match "/emacs-google-config/.*/git_modes" it)) load-path))

;;;; Turn on outline for these languages
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'java-mode-hook
  (lambda ()
    "Enable Java outline."
    (setq outline-regexp "\\(?:\\([ \t]*.*\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)\\|[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{\\)")))
(add-hook 'java-mode-hook 'outline-minor-mode)
(add-hook 'java-mode-hook 'show-paren-mode)


;;; Quickly switch between the startup directory and current file's
(defun zk-cd-initial()
  "Change to the initial directory from which emacs was started"
  (interactive)
  (cd command-line-default-directory)
  (message default-directory))

(defun zk-cd-current-buffer()
  "Change to the directory of the current file"
  (interactive)
  (if buffer-file-name
      (progn
        (cd (file-name-directory buffer-file-name))
        (message default-directory))
    (message "Current buffer does not have a file")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (misterioso)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(let ((env_server_name (getenv "ZK_EMACS_SERVER_NAME")))
  (if env_server_name
      (progn
	(setq server-name (getenv "ZK_EMACS_SERVER_NAME"))
	(server-start))
    (warn "Server name was not specified. Won't start a server.")))
