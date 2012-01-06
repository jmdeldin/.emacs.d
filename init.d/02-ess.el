;; autoloads borrowed from http://braeburn.aquamacs.org/code/master/aquamacs/src/site-lisp/aquamacs-mode-defaults.el
(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-site" "Emacs Speaks Statistics" t)
(autoload 'Rnw-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-transcript-mode "ess-site" "Emacs Speaks Statistics" t)

(setq auto-mode-alist
      (append '(
                ("\\.[rR]$" . R-mode)
                ("\\.[rR]profile$" . R-mode)
                ("\\.[Rr]t$" . R-transcript-mode)
                ("\\.[Rr]out$" . R-transcript-mode))
              auto-mode-alist))
