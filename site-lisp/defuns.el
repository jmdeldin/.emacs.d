;;
;;; Functions
;;

(defun jm/filter (pred lst)
  "Filter elements from a LST matching PRED."
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(defun jm/local-path (file)
  "Path to ~/.emacs.d/local/FILE."
  (concat user-emacs-directory "local/" file))

(defun jm/packages-installed-p (pkg-list)
  "Determine whether all packages declared in PKG-LIST are installed."
  (= (length pkg-list)
     (length (jm/filter 'package-installed-p pkg-list))))

(defun jm/install-packages (pkg-list)
  "Install all packages declared in PKG-LIST."
  (package-refresh-contents)
  (mapcar (lambda (pkg)
	    (unless (package-installed-p pkg)
	      (package-install pkg)))
	  pkg-list))

(defun jm/reload-init ()
  "Reload `~/.emacs.d/init.el'"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun associate-file-type (file-list mode)
  "Associates a FILE-LIST with a MODE."
  (let* ((regexp (concat (regexp-opt file-list t) "\\'")))
    (add-to-list 'auto-mode-alist (cons regexp mode))))

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
           (if (string= "" format)
               "%Y-%m-%dT%T%z"
             format))))

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

;;;
;;; Hacky code for toggling between minitest spec and implementation
;;
(defvar mt-lib-dir
  "/home/jmdeldin/src/takky/lib/takky")

(defvar mt-test-dir
  "/home/jmdeldin/src/takky/spec")

(defun mt-format-impl-name (fname)
  (format "%s.rb" (replace-regexp-in-string "_spec" "" (file-name-sans-extension fname))))

(defun mt-format-test-name (fname)
  (format "%s_spec.rb" (replace-regexp-in-string "_spec" "" (file-name-sans-extension fname))))

(defun mt-swap-directory (from-dir to-dir fname)
  (replace-regexp-in-string from-dir to-dir fname))

(defun mt-lib-to-test ()
  (interactive)
  (message (mt-format-test-name (mt-swap-directory mt-lib-dir mt-test-dir buffer-file-name))))

(defun mt-test-to-lib ()
  (interactive)
  (message (mt-format-impl-name (mt-swap-directory mt-test-dir mt-lib-dir buffer-file-name))))

(defun mt-toggle ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name))))
    (cond
     ((string-match mt-lib-dir dir) (find-file (mt-lib-to-test)))
     ((string-match mt-test-dir dir) (find-file (mt-test-to-lib))))))

(defun pivotal-ticket (id)
  "Inserts a Markdown link to a Pivotal Tracker ticket."
  (interactive "nticket id: ")
  (insert (format ":o: [Finishes #%s](https://www.pivotaltracker.com/story/show/%s)" id id)))

(defun jira-ticket (id)
  "Inserts a Markdown link to a Jira ticket."
  (interactive "sticket id: ")
  (insert (format ":o: [Finishes %s](https://jira.copiousinc.com/browse/%s)" id id)))

(defun make-password ()
  "Generates an XKCD-style password, prints it, and saves it to
the kill ring. Shell-fu by @ckuttruff."
  (interactive)
  (let* ((cmd "egrep '{6,}' /usr/share/dict/words | fgrep -v \"'\" | shuf -n 4 | tr '[:upper:]' '[:lower:]' | tr \"\\n\" ' '")
         (pass (replace-regexp-in-string " $" "" (shell-command-to-string cmd))))
    (message pass)
    (kill-new pass)))

(defun jm/clean-and-copy (str)
  "Clean up a string and throw it on the clipboard"
  (kill-new (s-trim (substring-no-properties str))))

(defun jm/org-table-field (N)
  "Wrapper around `org-table-get-field'."
  (require 'org-table) ;; prevent undefined org-table-get-field errors
  (org-table-get-field N))

(defun copy-password ()
  "Copy a password to the clipboard.

(Passwords are stored in the last column of an org table.)"
  (interactive)
  (jm/clean-and-copy (jm/org-table-field 3)))

(defun copy-user ()
  "Copy the username to the clipboard.

(Usernames are stored in the second column of an org table.)"
  (interactive)
  (jm/clean-and-copy (jm/org-table-field 2)))

(defun jm/yeller ()
  (interactive)
  (set-background-color "#FFFFDD"))

(defun jm/minty ()
  (interactive)
  (set-background-color "#F0FFF0"))
