(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;;; Load lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;;; Load google-specific bits
(load-file (expand-file-name "~/.emacs.d/init-google.el"))
