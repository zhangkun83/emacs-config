(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/neotree"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/groovy-modes"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/helm"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-themes"))


;;; Load the solarized color theme
(load-theme 'solarized t)
;; Make new frames use the light variant
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'light)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))
;; Make the first frame use the dark variant
(set-frame-parameter nil 'background-mode 'light)
(set-terminal-parameter nil 'background-mode 'light)
(enable-theme 'solarized)

;;; Org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;; Set font
(if (eq system-type 'gnu/linux)
    ; For linux
    (set-face-attribute 'default nil
			:family "DejaVu Sans Mono" :height 130)
    ; For Mac OS X
    (set-face-attribute 'default nil
			:family "Roboto Mono" :height 145 :weight 'regular))

;;; Use this to set cursor color if desired
;(setq default-frame-alist '((cursor-color . "#ffffff")))


;;;; magit (git integration)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;; neotree (tree directory view)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Do not load evil-mode because I do not like inconsistent editing behavior between buffers.
;(load-file (expand-file-name "~/.emacs.d/lisp/enable-evil-mode.el"))

;;;; etags-select (better ctags search)
(load "etags-select.el")
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;;; Always do case-sensitive search for tags
(setq-default tags-case-fold-search nil)

;;;; Load TAGS file under current directory
(add-to-list 'tags-table-list (concat default-directory "TAGS"))


;;;; Don't enable semantic (semantic doesn't work if Java file contains generics)
;;(semantic-mode 1)


;;; Helm provides incremental completion and selection
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)


;; ace-jump-mode for faster cursor movement
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(define-key global-map (kbd "C-x C-SPC") 'ace-jump-mode-pop-mark)


;;; Load markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;;; Load lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;; Load groovy-mode
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

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

(defun zk-helm-set-project-root(f)
  "Set project root for helm"
  (interactive "DProject root: ")
  (setq helm-locate-project-list (list f)))


; Setup auto-saving desktop, which is unfortunately necessary because emacs
; occasionally freezes when idle.
(require 'desktop)

(defun zk-save-everything()
  "Save all files and the desktop"
  (interactive)
  (progn
    (save-some-buffers)
    (desktop-save-in-desktop-dir)))
(global-set-key [f6] 'zk-save-everything)

(defun zk-restore-desktop(bool)
  "Restore the desktop previously saved for the server with the same name"
  (interactive (list (y-or-n-p (concat "Load the desktop from " desktop-dirname "? "))))
  (if bool (desktop-read desktop-dirname)))


;;; Make desktop-save able to save shell buffers
(defun zk-shell-save-desktop-buffer (desktop-dirname)
  default-directory)

(defun zk-shell-restore-desktop-buffer (file-name buffer-name directory)
  (setq default-directory directory)
  (shell buffer-name))

(add-hook 'shell-mode-hook
	  (lambda ()
	    (make-local-variable 'desktop-save-buffer)
	    (setq desktop-save-buffer 'zk-shell-save-desktop-buffer)))

(add-to-list 'desktop-buffer-mode-handlers '(shell-mode . zk-shell-restore-desktop-buffer))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-startup-indented t)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Load site-specific bits
(load "init-site.el")


;;; Start a server
(let ((env_server_name (getenv "ZK_EMACS_SERVER_NAME")))
  (if env_server_name
      (progn
	(setq server-name (getenv "ZK_EMACS_SERVER_NAME"))
	(server-start)
	(setenv "EDITOR" (concat "open-in-emacs-server " server-name))
	(setenv "P4EDITOR" (concat "open-in-emacs-server " server-name))
	(setq frame-title-format '("%b - " server-name "@emacs"))
	(setq desktop-dirname (concat "~/.emacs.d/saved-desktops/" server-name))
	(make-directory desktop-dirname t))
    (progn
      	(setq frame-title-format '("%b - emacs"))
	(warn "Server name was not specified. Won't start a server. Use \"ems\" command to start emacs with a server."))))
