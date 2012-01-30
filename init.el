;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2012 Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local
;; Created:  2010-10-01
;; Compatibility: 24

;;; Commentary:
;;
;; I started using Emacs in fall of 2010 after a few years of Vim. Details of
;; my setup are listed below:
;;
;; Uses:        LaTeX, Org-Mode, Ruby, C
;; Environment: Mac OS X
;; Emacsen:     GNU Emacs 24
;;
;;; Code:

(defvar my/site-lisp (concat user-emacs-directory "site-lisp")
  "Local elisp directory (e.g., ~/.emacs.d/site-lisp).")

(defvar my/vendor (concat my/site-lisp "/vendor")
  "External elisp (e.g., ~/.emacs.d/site-lisp/vendor).")

(defvar my/local (concat user-emacs-directory "local")
  "Directory with site-local customizations. This shouldn't be checked-in.")

(defun localize (dir)
  "Concatenates a DIR with my/local."
  (expand-file-name dir my/local))

(defun load-directory (directory)
  "Load an entire DIRECTORY of elisp files."
  (dolist (f (directory-files directory t ".el"))
    (load-library (file-name-sans-extension f))))

(load-directory my/site-lisp)
(load-directory (concat user-emacs-directory "init.d"))

;; use ~/.emacs.d/local/emacs-custom.el for customizations
(setq custom-file (localize "emacs-custom.el"))
(load custom-file 'noerror)

;; use ~/.emacs.d/local/.emacs.bmk for bookmarks
(setq bookmark-file (localize ".emacs.bmk"))

(textmate-mode)

;; load local config to override any of the above settings
(load (concat my/local "/local") 'noerror)

;;; init.el ends here
