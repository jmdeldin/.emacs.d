;;
;;; Functions
;;

;; Align key-value strings, like
;;   foo: bar
;;   baz:1
;; to
;;   foo: bar
;;   baz: 1
;; TODO: Convert to a function
(fset 'align-key-value
      [?\C-u ?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g ?e ?x ?p ?\C-m ?\C-a ?\C-k ?\\ ?\( ?: ?\\ ?\) ?\\ ?\( ?\\ ?s ?- ?* ?\\ ?\) ?\C-m ?\C-? ?2 ?\C-m ?\C-m ?n])

(defun timestamp (format)
  "Inserts the timestamp given by FORMAT, or selects a default if nil.

The format is any format accepted by `format-time-string'. The default
is ISO 8601, which is ``%Y-%m-%dT%T%z''."
  (interactive "Mformat: ")
  (insert (format-time-string
     (if (string= "" format) "%Y-%m-%dT%T%z" format))))

(defun pry ()
  "Inserts a pry debugging line."
  (interactive)
  (insert "require 'pry'; binding.pry"))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.

From URL `http://stackoverflow.com/q/43765'."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defadvice comment-or-uncomment-region (before slick-comment activate compile)
  "When called interactively with no active region, (un)comment the whole line.

From URL `http://kill-0.com/duplo/2010/03/04/emacs-ruby-mode-comment-keybinding/'."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun swap-buffers-in-windows ()
  "Swap buffers between two windows.

By Chris Webber from URL `http://www.emacswiki.org/emacs/TransposeWindows'."
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))

(setq swapping-buffer nil)
(setq swapping-window nil)

(defun filter (cond-p lst)
  "Delete elements from a LST not matching condition COND-P."
  (delq nil
        (mapcar (lambda (x) (and (funcall cond-p x) x)) lst)))

(defun jm/shell (name)
  "Creates a unique shell buffer labeled NAME."
  (interactive (list (read-string "Shell: ")))
  (shell (concat "> " name)))

(defvar jm/sql-regexp
  (mapconcat 'downcase
             (mapcar 'symbol-name '(select from inner outer left join where
                                           and in on group by into min max
                                           sum limit create alter table))
             "\\|")
  "Regexp of SQL keywords")

(defun jm/fix-sql-case (start end)
  "Uppercase SQL keywords"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp jm/sql-regexp nil t)
        (replace-match (upcase (match-string 0)) t nil)))))
