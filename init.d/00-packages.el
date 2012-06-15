;;; 00-packages.el --- ELPA and required packages

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: lisp

;;; Code:

(defvar my/required-pkgs (concat user-emacs-directory "pkgs.el")
  "File containing cached packages.")

(defun get-installed-packages ()
  "Caches packages installed with package.el to ~/.emacs.d/pkgs.el.
This could definitely be improved, but for now, it's basically a macro."
  (interactive)
  (package-list-packages-no-fetch)
  (toggle-read-only)
  (flush-lines "  available  ")
  (flush-lines "  built-in  ")
  (lisp-mode)
  (mark-whole-buffer)
  (replace-regexp "[0-9.]+ +installed .*" "")
  (beginning-of-buffer)
  (insert "(setq required-packages '(\n")
  (end-of-buffer)
  (insert "))")
  (write-file my/required-pkgs))

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
