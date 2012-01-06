;;
;;; Keybindings
;;

;; C-c C-r -- Revert buffer
(global-set-key (kbd "C-c C-r") 'revert-buffer)

;; Swap C-j and RET
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-j") 'newline)

;; C-c w -- toggle whitespace mode
(global-set-key (kbd "C-c w") 'global-whitespace-mode)

;; C-c C-d -- Remove trailing whitespace
(global-set-key (kbd "C-c C-d") 'delete-trailing-whitespace)

;; C-w -- delete the previous word (like most shells)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; C-x C-k -- kill region (since we just unbound it with C-w)
(global-set-key (kbd "C-x C-k") 'kill-region)

;; C-x C-j -- join line
(global-set-key (kbd "C-x C-j") 'join-line)

;; window movement from <http://nex-3.com/posts/45-efficient-window-switching-in-emacs>
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

;; better commenting (replaces the original comment-dwim)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; C-x m -- recompile
(global-set-key (kbd "C-x m") 'recompile)

;; Cmd-F -- full-screen mode
(global-set-key (kbd "s-F") 'ns-toggle-fullscreen)

(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
