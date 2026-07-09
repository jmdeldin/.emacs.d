;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2015 Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local
;; Created:  2010-10-01
;; Compatibility: 24.4

;;; Code:

(package-initialize)

;; Only load org (expensive) when README.org is newer than the tangled README.el
(let ((org-file (expand-file-name "README.org" user-emacs-directory))
      (el-file  (expand-file-name "README.el"  user-emacs-directory)))
  (when (or (not (file-exists-p el-file))
            (file-newer-than-file-p org-file el-file))
    (require 'org)
    (org-babel-tangle-file org-file el-file "emacs-lisp"))
  (load-file el-file))
