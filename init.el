(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/evil/lib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/neotree"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/color-themes"))


;;; Load the solarized color theme
(load-theme 'solarized t)
;; Make new frames use the dark variant
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'dark 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))
;; Make the first frame use the dark variant
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(enable-theme 'solarized)

;;;; text font under linux
(if (eq system-type 'gnu/linux)
  (set-default-font "-xos4-terminus-medium-r-normal-*-17-*-*-*-*-*-iso8859-1"))

;;;; magit (git integration)
(require 'magit)


;;; neotree (tree directory view)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; These key bindings conflict with evil-mode. Override them.
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


;;;; undo-tree.el is required by evil-mode
(require 'undo-tree)
(global-undo-tree-mode)

;;;; evil-mode (vim emulation)
(require 'evil)
(evil-mode 1)


;;;; etags-select (better ctags search)
(load "etags-select.el")
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(define-key evil-normal-state-map "\C-]" 'etags-select-find-tag)

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

;;; Use this to set cursor color if desired
;; (set-cursor-color "#ffffff")
