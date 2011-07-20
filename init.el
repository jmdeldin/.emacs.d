;;; init.el --- Jon-Michael Deldin's .emacs
;;
;; Author:      Jon-Michael Deldin <dev@jmdeldin.com>
;; Copyright:   2010-11 Jon-Michael Deldin
;; Created:     2010-10-01
;; Keywords:    local
;;
;;; Commentary:
;;
;; I started using Emacs in fall of 2010 after a few years of
;; Vim. Details of my setup are listed below:
;;
;; Uses:        LaTeX, Org-Mode, Ruby, C
;; Environment: Mac OS X
;; Emacsen:     GNU Emacs 23.2.1+ and Aquamacs 2.2+
;;
;;; Code:

;;
;;; UI settings
;;

;; hide the {menu,tool,scroll}bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; hide the startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; get rid of the audible bell
(setq visible-bell t)

;; turn down visual bells
(setq ring-bell-function 'my/bells)

;; show line & column number in the mode line
(column-number-mode t)
(line-number-mode t)

;; show file size
(size-indication-mode t)

;; highlight parens
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; highlight current line
(global-hl-line-mode 1)

;; syntax highlighting
(global-font-lock-mode t)

;; enable the backtrace
; (setq debug-on-error t)

;;
;;; Whitespace
;;

;; wrap lines at column 78
(setq fill-column 78)

;; highlight right-margin when whitespace-mode is on
(setq whitespace-line-column fill-column)

;; highlight empty lines
(setq indicate-empty-lines t)

;; hard-wrap lines all the time
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; use spaces, not tabs (C-q C-i to insert a hard-tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; 4-spaces for C-modes (e.g., C & Perl)
(setq c-default-style "k&r")
(setq-default c-basic-offset 4)

;; insert a newline at the EOF
(setq-default require-final-newline t)

;;; Variables
(defvar my/site-lisp
  (concat user-emacs-directory "site-lisp")
    "Local elisp directory. Typically ~/.emacs.d/site-lisp.")

(defvar my/vendor
  (concat my/site-lisp "/vendor")
    "External elisp. Typically ~/.emacs.d/site-lisp/vendor")

(defvar my/local (concat user-emacs-directory "local")
  "Directory containing site-local modifications.
   Typically ~/.emacs.d/local. This should not be placed
   under version control.")

;;
;;; Paths
;;

;; add subdirectories of site-lisp to the *front* of load-path
(let* ((default-directory my/site-lisp)
       (orig-load-path load-path))
  (setq load-path (cons default-directory nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(load-library (concat my/site-lisp "/defuns"))
(load-library (concat my/site-lisp "/bindings"))
(load-library (concat my/site-lisp "/aquamacs"))

;; load package/filetype-specific confs (prefix is used to avoid name clashes)
(load-library (concat my/site-lisp "/_gv"))
(load-library (concat my/site-lisp "/_org"))
(load-library (concat my/site-lisp "/_ruby"))
(load-library (concat my/site-lisp "/_yaml"))

;;
;;; Misc. settings
;;

;; Connect with `emacsclient'
(if (not (boundp 'server-process))
    (server-start))

;; Unicode
(prefer-coding-system 'utf-8)

;; use ~/.emacs.d/local/emacs-custom.el for customizations
(setq custom-file (concat my/local "/emacs-custom.el"))
(load custom-file 'noerror)

;; use ~/.emacs.d/local/.emacs.bmk for bookmarks
(setq bookmark-file (concat my/local "/.emacs.bmk"))

;; put backup & autosave files in /tmp
(setq backup-by-copying t)
(setq backup-directory-alist
      `((".*" . ,(concat my/local "/backups/"))))
(setq delete-old-versions t)
(setq kept-new-version 6)
(setq kept-old-versions 2)
(setq version-control t)

;; dump auto-save files in ~/.emacs.d/local/saves
(setq auto-save-list-file-prefix
      (concat my/local "/saves/"))
(setq auto-save-file-name-transforms
      `((".*" ,(concat my/local "/saves") t)))

;; use aspell
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; fill-paragraph (M-q): use single spaces between sentences
(setq sentence-end-double-space nil)

;;
;;; Packages
;;

;; ido is much more useful than C-f or C-b
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-save-directory-list-file (concat my/local "/.ido.last"))
(ido-mode 1)

;; don't use file<n> to distinguish between identically-named files -- use
;; part of the directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; recent files
(require 'recentf)
(setq recentf-save-file (concat my/local "/.recentf"))
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;; remember last edit position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat my/local "/.emacs-places"))

;; save minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (concat my/local "/.savehist"))

;; M-x gdb
(setq gdb-many-windows t)

;; eshell
(setq eshell-directory-name (concat user-emacs-directory "eshell"))

;; autopair -- textmate-like quotation
(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)

;; magit -- load on magit-status
(load-library (concat my/vendor "/magit/50magit"))

;;
;;; Color themes
;;

(if window-system
    (progn
      (require 'color-theme)
      (color-theme-initialize)

      ;; load external themes
      (let ((themes (concat my/vendor "/themes")))
        (dolist (f (directory-files themes))
          (let ((name (concat themes "/" f)))
            (when (and (file-regular-p name))
              (if (string= "el" (file-name-extension name))
                  (load-library name))))))))

;; highlight FIXME & TODO
(font-lock-add-keywords nil
                        '(("\\<\\(FIXME\\|TODO\\):"
                           1 font-lock-warning-face t)))

;;
;;; load local config to override any of the above settings
;;
(load-library (concat my/local "/local"))

;;; end of init.el.
