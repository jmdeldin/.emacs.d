;; graphviz settings

(autoload 'graphviz-dot-mode "graphviz-dot-mode" "Major mode for graphviz files" t)
(add-to-list 'auto-mode-alist '("\\.gv$" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))

