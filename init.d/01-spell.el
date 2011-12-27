;;; 01-spell.el --- flyspell settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--dont-suggest"))

(provide '01-spell)
;;; 01-spell.el ends here
