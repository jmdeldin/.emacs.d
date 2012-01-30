;;; 02-scss.el --- SCSS settings

;; Copyright (C) 2012  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

(setq scss-compile-at-save nil)
(add-hook 'css-mode-hook 'rainbow-mode)
(provide '02-scss)
;;; 02-scss.el ends here
