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

;; window movement
(global-set-key (kbd "M-<left>")  'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down)

;; better commenting (replaces the original comment-dwim)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; C-x m -- recompile
(global-set-key (kbd "C-x m") 'recompile)

