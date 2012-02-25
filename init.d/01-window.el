;;; 01-window.el --- window settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: frames

;;; Code:

;; restore window configuration with C-c LEFT
(winner-mode)

;; windmove -- default binding is shift
(windmove-default-keybindings)
(setq windmove-wrap-around t)


(provide '01-window)
;;; 01-window.el ends here
