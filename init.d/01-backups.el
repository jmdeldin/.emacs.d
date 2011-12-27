;;; 01-backups.el --- backup and auto-save settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local

;;; Code:

;; place backups in ~/.emacs.d/local/backups
(setq backup-by-copying t)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backups" my/local))))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; dump auto-save files in ~/.emacs.d/local/saves
(setq auto-save-list-file-prefix
      (concat my/local "/saves/"))
;(setq auto-save-file-name-transforms
 ;     (list (cons "." (expand-file-name "saves" my/local))))

(provide '01-backups)
;;; 01-backups.el ends here
