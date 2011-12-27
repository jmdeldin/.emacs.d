;;; 01-ui.el --- UI settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: frames

;;; Code:

;; hide the {menu,tool,scroll}bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; hide the startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; show line & column number in the mode line
(column-number-mode t)

;; show file size
(size-indication-mode t)

;; highlight parens
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; highlight current line
(global-hl-line-mode 1)

;; display time
(display-time)

(provide '01-ui)
;;; 01-ui.el ends here
