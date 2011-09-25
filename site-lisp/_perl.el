;;; _perl.el -- Perl settings

;; use cperl-mode
(defalias 'perl-mode 'cperl-mode)

;; turn on for tests
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

;; 4 SPC
(setq cperl-indent-level 4)

;; indent only 4-spaces in broken-up calls like
;;   someCall(
;;       $var,
;;       $var2
;;   )
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)

;; Indentation for lines not starting statements (e.g., hash members)
(setq cperl-continued-statement-offset 0)
