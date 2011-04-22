(defun color-theme-thor ()
  (interactive)
  (color-theme-install
   '(color-theme-thor
     ((background-color . "#090A1B")
      (background-mode . dark)
      (border-color . "#454545")
      (cursor-color . "#888888")
      (foreground-color . "#F6F3E8")
      (mouse-color . "#660000"))
     (default ((t (:background "#090A1B" :foreground "#F6F3E8"))))
     (vertical-border ((t (:background "#666666"))))
     ;(blue ((t (:foreground "blue"))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
     (font-lock-comment-face ((t (:foreground "#7C7C7C"))))
     (font-lock-constant-face ((t (:foreground "#0A9BFE"))))
     (font-lock-doc-string-face ((t (:foreground "#FFFF69"))))
     (font-lock-function-name-face ((t (:foreground "#6FD3FF"))))
     (font-lock-builtin-face ((t (:foreground "#5789F9"))))
     (font-lock-keyword-face ((t (:foreground "#FE3853"))))
     (font-lock-preprocessor-face ((t (:foreground "#96CBFE"))))
     (font-lock-reference-face ((t (:foreground "#C6C5FE"))))
     
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     
     (linum ((t (:background "#090A1B" :foreground "#666666"))))

     (minibuffer-prompt ((t (:foreground "#888888"))))
     (ido-subdir ((t (:background "#0080409" :foreground "#FFFFFF"))))
     (ido-first-match ((t (:background "#008040" :foreground "#FFFFFF"))))
     (ido-only-match ((t (:background "#008040" :foreground "#FFFFFF"))))
     (mumamo-background-chunk-submode ((t (:background "#008040"))))

	 ;; eshell
	 (eshell-prompt-face ((t (:foreground "#6FD3FF"))))
     (eshell-prompt ((t (:foreground "#6FD3FF"))))

	 ;; minibuffer
	 (minibuffer-prompt ((t (:foreground "#5789F9"))))
     
     (font-lock-string-face ((t (:foreground "#A8FF60"))))
     (font-lock-type-face ((t (:foreground "#FFFFB6"))))
     (font-lock-variable-name-face ((t (:foreground "#C6C5FE"))))
     (font-lock-warning-face ((t (:background "#CC1503" :foreground "#FFFFFF"))))
     (gui-element ((t (:background "#090A1B" :foreground "black"))))
     (region ((t (:background "#660000"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (highlight ((t (:background "#111111"))))
     (highline-face ((t (:background "#090A1B"))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (show-paren-mismatch ((t (:background "#FF1100"))))
     (underline ((nil (:underline nil))))

     ;; mumamo
     (mumamo-background-chunk-major ((t (:background "#090A1B"))))
     (mumamo-background-chunk-submode1 ((t (:background "#090A1B"))))
     (mumamo-background-chunk-submode2 ((t (:background "#090A1B"))))
     (mumamo-background-chunk-submode3 ((t (:background "#090A1B"))))
     (mumamo-background-chunk-submode4 ((t (:background "#090A1B"))))

     ;; diff-mode
     (diff-added ((t (:background "#253B22" :foreground "#F8F8F8"))))
     (diff-removed ((t (:background "#420E09" :foreground "#F8F8F8"))))
     (diff-content ((t nil)))
     (diff-header ((t (:background "#0E2231" :foreground "#F8F8F8"))))
     
     
     ;; nxml
     (nxml-delimiter ((t (:foreground "#FE3853"))))
     (nxml-name ((t (:foreground "#6FD3FF"))))
     (nxml-element-local-name ((t (:foreground "#6FD3FF"))))
     (nxml-attribute-local-name ((t (:foreground "#6FD3FF"))))

     )))
