;;; 01-whitespace.el --- whitespace settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

;; wrap lines at column 78
(setq-default fill-column 78)

;; highlight right-margin when whitespace-mode is on
(setq whitespace-line-column fill-column)

;; highlight empty lines
(setq indicate-empty-lines t)

;; hard-wrap lines all the time
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; use spaces, not tabs (C-q C-i to insert a hard-tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; insert a newline at the EOF
(setq-default require-final-newline t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide '01-whitespace)
;;; 01-whitespace.el ends here
