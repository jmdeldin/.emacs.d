(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; file paths
(setq org-default-notes-file "~/Dropbox/notes/capture.org")
(setq org-archive-location "~/Dropbox/notes/archive.org::")

;; capture templates (C-c c)
(setq org-capture-templates
      '(("t" "TODO" entry (file org-default-notes-file "Tasks")
         "* TODO %?\n%U\n")))

;; agenda
;; (setq org-agenda-to-appt t)
;; FIXME: Might remove

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
