(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(setq-default tags-case-fold-search nil)
(setq-default case-fold-search nil)

(add-to-list 'tags-table-list (concat default-directory "TAGS"))

; Enable Semantic
(semantic-mode 1)

(require 'ido)
(ido-mode t)

;;;; Load lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;; Load google-specific bits
(load-file (expand-file-name "~/.emacs.d/init-google.el"))
