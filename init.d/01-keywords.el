;;; 01-keywords.el --- keywords to highlight

;; Copyright (C) 2012  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

;; highlight FIXME & TODO
(defun my/highlight ()
  "Highlight FIXME and TODO keywords."
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\):"
                             1 font-lock-warning-face t))))

(add-hook 'text-mode-hook 'my/highlight)

(provide '01-keywords)
;;; 01-keywords.el ends here
