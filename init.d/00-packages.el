;;; 00-packages.el --- ELPA and required packages

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: lisp

;;; Code:

(setq required-packages
      '(
        auctex
        graphviz-dot-mode
        magit
        markdown-mode
        org
        ruby-compilation
        ruby-electric
        ruby-mode
        scss-mode
        textmate-mode
        yaml-mode
        ))

(setq url-http-attempt-keepalives nil)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun setup-packages ()
  "Install required packages."
  (interactive)
  (package-refresh-contents)
  (dolist (p required-packages)
      (message "Installing %s" p)
      (package-install p)))

(provide '00-packages)
;;; 00-packages.el ends here
