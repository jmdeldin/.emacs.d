;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2015 Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local
;; Created:  2010-10-01
;; Compatibility: 24.4

;;; Code:

(package-initialize)

(require 'org)

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
