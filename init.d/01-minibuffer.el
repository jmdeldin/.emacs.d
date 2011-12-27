;;; 01-minibuffer.el --- minibuffer settings

;; Copyright (C) 2011  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: convenience

;;; Code:

;; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-show-dot-for-dired t)
(setq ido-save-directory-list-file (localize ".ido.last"))
(ido-mode 1)

;; use part of the directory to distinguish between identically-named files
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; save minibuffer history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (localize ".savehist"))

;; recent files
(require 'recentf)
(setq recentf-save-file (localize ".recentf"))
(setq recentf-max-saved-items 100)
(recentf-mode 1)


(provide '01-minibuffer)
;;; 01-minibuffer.el ends here
