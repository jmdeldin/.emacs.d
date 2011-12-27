;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2011  Jon-Michael Deldin

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
  (dolist (f (directory-files directory t ".el$"))
    (load-file f)))

;; add subdirectories of site-lisp to the *front* of load-path
(let* ((default-directory my/site-lisp)
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(load-directory (concat user-emacs-directory "init.d"))
(load-directory my/site-lisp)

;; use ~/.emacs.d/local/emacs-custom.el for customizations
(setq custom-file (localize "emacs-custom.el"))
(load custom-file 'noerror)

;; ;; use ~/.emacs.d/local/.emacs.bmk for bookmarks
(setq bookmark-file (localize ".emacs.bmk"))

(require 'textmate)
(textmate-mode)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; ;; highlight FIXME & TODO
;; (font-lock-add-keywords nil
;;                         '(("\\<\\(FIXME\\|TODO\\):"
;;                            1 font-lock-warning-face t)))

;; load local config to override any of the above settings
(load (concat my/local "/local") 'noerror)

;;; init.el ends here
