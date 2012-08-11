(require 'ruby-block)

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(eval-after-load 'ruby-mode
  '(progn
     (defun eric-ruby-mode-defaults ()
       (inf-ruby-setup-keybindings)
       ;; turn off the annoying input echo in irb
       (setq comint-process-echoes t)
       (ruby-block-mode t)
       (ruby-end-mode +1)
       (ruby-tools-mode +1)
       ;; CamelCase aware editing operations
       (subword-mode +1)
       ;; bind yari in the local keymap
       (local-set-key (kbd "C-h r") 'yari))

     (add-hook 'ruby-mode-hook 'eric-ruby-mode-hook)))

(provide 'eric-ruby)
