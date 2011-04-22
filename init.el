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
;; Emacsen:     GNU Emacs 23.2.1
;;
;; Tip: C-u 0 M-x byte-recompile-directory to compile the .el files.
;;
;;; Code:

;;
;;; UI settings
;;

;; hide the {menu,tool,scroll}bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; don't accelerate scrolling (default is terrible with inertial mouse wheels)
(setq mouse-wheel-progressive-speed nil)

;; hide the startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; get rid of the audible bell
(setq visible-bell t)

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

;; load package/filetype-specific confs (prefix is used to avoid name clashes)
(load-library (concat my/site-lisp "/_ruby"))
(load-library (concat my/site-lisp "/_org"))

;;
;;; Misc. settings
;;

;; Connect with `emacsclient'
(server-start)

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

;;
;;; Packages
;;

;; ido is much more useful than C-f or C-b
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-show-dot-for-dired t)

;; don't use file<n> to distinguish between identically-named files -- use
;; part of the directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; recent files
(require 'recentf)
(recentf-mode 1)

(defun rido ()
  "Find a recent file using Ido.
   From URL `http://www.emacswiki.org/emacs-es/RecentFiles#toc5'."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; remember last edit position
(require 'saveplace)
(setq-default save-place t)

;; M-x gdb
(setq gdb-many-windows t)

;;
;;; Color themes
;;

(defun themes ()
  "Wrapper for loading themes"
  (require 'color-theme)
  (color-theme-initialize)

  ;; load external themes
  (let ((themes (concat my/vendor "/themes")))
    (dolist (f (directory-files themes))
      (let ((name (concat themes "/" f)))
        (when (and (file-regular-p name))
          (load-library name))))))

(if window-system
  (themes))

;; highlight FIXME & TODO
(font-lock-add-keywords nil
                        '(("\\<\\(FIXME\\|TODO\\):"
                           1 font-lock-warning-face t)))

;;
;;; Keybindings
;;

;; Swap C-j and RET
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;; C-c w -- toggle whitespace mode
(global-set-key (kbd "C-c w") 'global-whitespace-mode)

;; C-w -- delete the previous word (like most shells)
(global-set-key (kbd "C-w") 'backward-delete-word)

;; C-x C-k -- kill region (since we just unbound it with C-w)
(global-set-key (kbd "C-x C-k") 'kill-region)

;;
;;; Macros
;;

;; Align key-value strings, like
;;   foo: bar
;;   baz:1
;; to
;;   foo: bar
;;   baz: 1
(fset 'align-key-value
   [?\C-u ?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g ?e ?x ?p ?\C-m ?\C-a ?\C-k ?\\ ?\( ?: ?\\ ?\) ?\\ ?\( ?\\ ?s ?- ?* ?\\ ?\) ?\C-m ?\C-? ?2 ?\C-m ?\C-m ?n])

;;
;;; Functions
;;
(defun timestamp (format)
  "Inserts the timestamp given by FORMAT, or selects a default if nil.

The format is any format accepted by `format-time-string'. The default
is ISO 8601, which is ``%Y-%m-%dT%T%z''."
  (interactive "Mformat: ")
  (insert (format-time-string
     (if (string= "" format) "%Y-%m-%dT%T%z" format))))

(defun my/bells ()
  "Don't ring the bell on navigation and cancellation commands.

From URL `http://stackoverflow.com/q/324457#731660'."
  (unless (memq this-command
    '(down up previous-line next-line mwheel-scroll
      backward-char forward-char keyboard-quit))
    (ding)))
(setq ring-bell-function 'my/bells)

;;
;;; load local config to override any of the above settings
;;
(load-library (concat my/local "/local"))

;;; end of init.el.

