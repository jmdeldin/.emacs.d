;;; 02-org.el --- Org-Mode customizations

;; Copyright (C) 2012  Jon-Michael Deldin

;; Author: Jon-Michael Deldin <dev@jmdeldin.com>
;; Keywords: tools

;;; Commentary:

;; This configuration assumes org files live in the ~/org directory. You can
;; customize it by setting these variables in `../local/local.el':
;;
;;   (setq org-default-notes-file PATH_TO_CAPTURE.ORG)
;;   (setq org-journal-file PATH_TO_JOURNAL.ORG)
;;   (setq org-log-file PATH_TO_LOG.ORG)
;;   (setq org-archive-location PATH_TO_ARCHIVE.ORG)
;;   (setq org-agenda-files LIST)
;;
;;; Code:

(require 'org-install)

(setq org-modules (quote (org-habit)))

;; paths
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/capture.org")
(setq org-journal-file "~/org/journal.org")
(setq org-log-file "~/org/log.org")
(setq org-archive-location "archive/%s_archive::")
(setq org-agenda-files (filter (lambda (fn)
                                 (not (string-match (rx "#") fn)))
                               (file-expand-wildcards org-directory)))
;; capture templates (C-c c)
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Task} %^g \n%U \n%?")
        ("j" "Journal" entry (file+datetree org-journal-file)
         "* %^{Title}\n%U \n%?\n")
        ("l" "Log" entry (file+datetree+prompt org-log-file)
         "* %^{Task} %^g\n%?" :clock-in t :clock-resume t)
        ))

;; timer/clock-in
(setq org-timer-default-timer 25)

;; templates
(define-skeleton orgmode-skeleton
  "Inserts orgmode defaults into the current buffer."
  "Title: "
  "#+TITLE:       " str | (file-name-nondirectory buffer-file-name) \n
  "#+DESCRIPTION: " (skeleton-read "Description: ") \n
  "#+STARTUP:     align hidestars indent lognotedone" \n
  \n _)

;; bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; babel
(setq org-babel-load-languages (mapcar (lambda (l) (cons l t))
        '(C calc emacs-lisp gnuplot latex perl python R ruby screen sh)))

;; show the agenda from the current day
(setq org-agenda-start-on-weekday nil)

;; remove "Valid XHTML" link
(setq org-export-html-validation-link nil)

;; minted latex export
(setq org-export-latex-minted-options
      '(("fontsize" "\\scriptsize")))

;; show all habits
(setq org-habit-show-habits-only-for-today nil)

(provide '02-org)
;;; 02-org.el ends here
