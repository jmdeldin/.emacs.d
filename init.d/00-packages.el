;;; 00-packages.el --- ELPA and required packages

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: lisp

;;; Code:

(setq required-packages
      (list 'magit 'textmate-mode 'scss-mode))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("ess" . "http://kieranhealy.org/packages")))

(package-initialize)

;; (dolist (package required-packages)
;;   (when (not (package-installed-p package))
;;     (package-refresh-contents)
;;     (package-install package)))

(provide '00-packages)
;;; 00-packages.el ends here
