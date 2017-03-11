(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/groovy-modes"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/color-themes"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-themes"))

;; Load site-specific bits
(load "init-site.el")

;; Disable tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set default browser to chrome
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;; Org-mode
(load-library "find-lisp")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files (find-lisp-find-files "~/org" "\.org$"))


;; Slime
(require 'slime)
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

;;; Set font
(if (eq system-type 'gnu/linux)
    ; For linux
    (set-face-attribute 'default nil
			:family "Liberation Mono" :height 115 :weight 'regular)
    ; For Mac OS X
    (set-face-attribute 'default nil
			:family "Liberation Mono" :height 130 :weight 'regular))


;;;; magit (git integration)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Do not load evil-mode because I do not like inconsistent editing behavior between buffers.
;(load-file (expand-file-name "~/.emacs.d/lisp/enable-evil-mode.el"))

;;;; etags-select (better ctags search)

(load "etags-select.el")
(defun zk-load-tags-if-not-loaded ()
  (interactive)
  (unless (get-buffer "TAGS")
    (visit-tags-table zk-project-root)))

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; From some point etags-select stopped loading TAGS.
;; Work around it.
(advice-add 'etags-select-find-tag :before #'zk-load-tags-if-not-loaded)

;;; Always do case-sensitive search for tags
(setq-default tags-case-fold-search nil)


;;;; Don't enable semantic (semantic doesn't work if Java file contains generics)
;;(semantic-mode 1)


;;; Ido
(require 'ido)
(ido-mode t)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))


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


;; Display line and column numbers on status bar
(setq column-number-mode t)


;; Disable tabs
(setq-default indent-tabs-mode nil)


;;; Turn on outline and showing matching parentheses for these languages.
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'java-mode-hook
  (lambda ()
    "Enable Java outline."
    (setq outline-regexp "\\(?:\\([ \t]*.*\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)\\|[ \t]*.*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{\\)")))
(add-hook 'java-mode-hook 'outline-minor-mode)
(add-hook 'java-mode-hook 'show-paren-mode)
(require 'fill-column-indicator)
(add-hook 'java-mode-hook
          (lambda ()
            "A few code-style parameters for Java"
            (set-fill-column 100)
            (fci-mode)
            (setq c-basic-offset 2
                  tab-width 2)
            ;; For newlines in argument list, replace the default indentation that aligns with
            ;; the parentheses, with the Google style that use double indentations (++)
            (c-set-offset 'arglist-cont-nonempty '++)
            (c-set-offset 'arglist-intro '++)
            ;; This was single indentation, should be double.
            (c-set-offset 'statement-cont '++)
            (c-set-offset 'annotation-var-cont 0)
            ;; case: line wasn't indenting. It should be.
            (c-set-offset 'case-label '+)
            ;; Treat camelCase as multiple words instead of one
            (subword-mode)
            ))


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
    (message "Current buffer does not have a file"))
)

;; For jumping quickly back to project-root in shells.
;; Also used by gradlez script
(require 'zk)
(setenv "ZK_PROJECT_ROOT" zk-project-root)
(add-hook 'java-mode-hook
	  (lambda()
	    "Register my own shortcuts for Java mode"
	    (local-set-key (kbd "C-c i") 'zk-insert-java-import)
            (local-set-key (kbd "M-;") 'recenter-top-bottom)
            (local-set-key (kbd "M-h") 'zk-java-mark-thing)
            (local-set-key (kbd "M-i") 'zk-java-enter-braces-block)
            (local-set-key (kbd "M-o") 'backward-up-list)
            (local-set-key (kbd "M-n") 'zk-java-next-thing)
            (local-set-key (kbd "M-p") 'zk-java-prev-thing)
            (local-set-key (kbd "M-a") 'zk-java-beginning-braces-block)
            (local-set-key (kbd "M-e") 'zk-java-end-braces-block)))
(global-set-key (kbd "C-x M-f") 'zk-find-src-file-in-project)

; Java stacktrace detection in compilation-mode
(require 'zk-java-stacktrace)
(global-set-key (kbd "C-x \\") 'compile)
(global-set-key (kbd "C-x |") 'compilation-minor-mode)

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

;;; Allow shell buffers' contents to be saved
(require 'zk-desktop-save-shell)

(add-hook 'shell-mode-hook
          (lambda()
            "Make dots part of the word so full paths can be expanded by M+/"
            (make-local-variable 'dabbrev-abbrev-char-regexp)
            (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|\\.")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(c-echo-syntactic-information-p t)
 '(custom-enabled-themes (quote (tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "be73fbde027b9df15a98a044bcfff4d46906b653cb6eef0d98ebccb7f8425dc9" "109c37722f5b922ab2c023d364bbe3bd1e2a49e6c3267ff7bca2ccdddbf9f9c2" "b6c88a4c9c7a8ace5c1d1c7fc61b9a76142a079ba398e61c8a59161427538c50" "684117b150429c5082829f7fdf1eaa003969f74cfc835d6807fda10d642e7049" "791364f64b4ab3526f8b885e0945d1208637ea4bd13ca269a9c52750fb2d9d1e" "6d77a9905ec4344df3646e0550cb28720bf11bc808f462b3c206fcb12d07cfd6" "2bfe2084cf94c9c4c1e9e3f9a2d43b0096dbf0373bbde7a7ae95996e87d44b08" "6f1b7c39c2b868da0d58ebb4a6ac278654a7c34b9ba22c9ca5a53e7396268729" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(dabbrev-case-replace nil)
 '(fci-rule-color "#c7c7c7")
 '(font-use-system-font t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(ido-enable-flex-matching nil)
 '(inhibit-startup-screen t)
 '(nrepl-message-colors
   (quote
    ("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c")))
 '(ns-command-modifier nil)
 '(org-startup-indented t)
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tags-revert-without-query t)
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   (quote
    ((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Make git use "cat" instead of "less"
(setenv "PAGER" "cat")

;;; Start a server
(setq server-socket-dir "~/.emacs.d/sockets")
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

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'zk-go-to-char)
(global-set-key (kbd "C-f") 'zk-go-to-char-forward)
(global-set-key (kbd "C-b") 'zk-go-to-char-backward)
