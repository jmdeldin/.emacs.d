;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2015 Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local
;; Created:  2010-10-01
;; Compatibility: 24.4

;;; Code:

(package-initialize)

(load-file (concat user-emacs-directory "site-lisp/defuns.el"))

(makunbound 'jm/packages)
(defvar jm/packages '(ag
                      auto-complete
                      cider
                      clojure-mode
                      coffee-mode
                      evil
                      highlight-indentation
                      jabber
                      magit
                      markdown-mode
                      mmm-mode
                      projectile
                      rainbow-mode
                      rspec-mode
                      scss-mode
                      slim-mode
                      smex
                      yaml-mode))

;; On clean installs, some packages will try to append custom vars to init.el,
;; breaking installation.
(setq custom-file (jm/emacs-path "local/emacs-custom.el"))
(load custom-file 'noerror)

;; Prevent =package.el= from timing out on large installs:
(setq url-http-attempt-keepalives nil)

;; Use MELPA in addition to the GNU repository:
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install any missing packages:
(unless (= (length jm/packages) (length (jm/filter 'package-installed-p jm/packages)))
  (package-refresh-contents)
  (mapcar (lambda (pkg)
            (unless (package-installed-p pkg) (package-install pkg)))
          jm/packages))

;; Tangle the rest of the config:
(require 'org)
(org-babel-load-file (expand-file-name "jmdeldin.org" user-emacs-directory))
