;;; init.el --- Jon-Michael Deldin's .emacs

;; Copyright (C) 2010-2012 Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: local
;; Created:  2010-10-01
;; Compatibility: 24.1

;;; Code:

(defun get-last-modifed (file)
  "Returns the last modified date of a FILE."
  (interactive)
  (format-time-string "%Y-%m-%d %T"
                      (nth 5 (file-attributes file))))

(defun most-recent-p (file1 file2)
  "Returns t if FILE1 was modified more recently than FILE2."
  (string< (get-last-modifed file1) (get-last-modifed file2)))

;; (if (most-recent-p "~/.emacs.d/jmdeldin.org" "~/.emacs.d/jmdeldin.el")
(require 'org)
(org-babel-load-file (expand-file-name "jmdeldin.org" user-emacs-directory))
  ;; (load-file "~/.emacs.d/jmdeldin.el"))

;;; init.el ends here
