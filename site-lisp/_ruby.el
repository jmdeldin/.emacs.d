(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'ruby-compilation)
            (require 'ruby-electric)
            (require 'inf-ruby)
            (autoload 'run-ruby "inf-ruby")
            (autoload 'inf-ruby-keys "inf-ruby")
            (inf-ruby-keys)))

