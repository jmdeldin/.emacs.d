;;; 01-eshell.el --- eshell settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: terminals

;;; Code:

;; eshell
(setq eshell-directory-name (concat user-emacs-directory "eshell"))

(defun eshell/d (&optional dir)
  "Launches a dired instance in the current working directory or DIR."
  (if dir
      (dired dir)
    (dired ".")))

(defun eshell/clear ()
  "Clear the eshell buffer from URL `http://www.khngai.com/emacs/eshell.php'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide '01-eshell)
;;; 01-eshell.el ends here
