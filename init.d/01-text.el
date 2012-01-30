;;; 01-text.el --- text-editing settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

;; camelCase navigation
(global-subword-mode t)

;; match parens and quotes
(electric-pair-mode t)

;; on-the-fly reindentation
(electric-indent-mode t)

;; insert a newline around special characters
(electric-layout-mode t)

;; fill-paragraph (M-q): use single spaces between sentences
(setq sentence-end-double-space nil)

;; Unicode
(prefer-coding-system 'utf-8)

;; changing a region's case is useful
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; remember last edit position
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (localize "/.emacs-places"))

(provide '01-text)
;;; 01-text.el ends here
