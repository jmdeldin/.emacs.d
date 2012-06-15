(defun ruby-run-buffer ()
  "Run the current Ruby script and switch focus back to the script."
  (interactive)
  (ruby-compilation-this-buffer)
  (other-window -1))

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (flymake-ruby-load)
            (autoload 'ri "ri")
            (local-set-key (kbd "C-h r") 'yari)
            (local-set-key (kbd "C-c C-c") 'ruby-run-buffer)))

(autoload 'rdoc-mode "rdoc-mode" "Major mode for rdoc files" t)
(add-to-list 'auto-mode-alist '("\\.rdoc$" . rdoc-mode))
