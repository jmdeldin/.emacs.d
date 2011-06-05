;;; Make Aquamacs more like traditional Emacs

(when (featurep 'aquamacs)
  (aquamacs-autoface-mode -1)                      ;; no mode-specific faces, everything in Monaco
  (setq aquamacs-scratch-file nil)                 ;; do not save scratch file across sessions
  (setq initial-major-mode 'lisp-interaction-mode) ;; scratch should be a lisp env
  (setq special-display-regexps nil)               ;; do not open certain buffers in special windows/frames
  )

