(add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp")
(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'scala-mode-hook (lambda ()
                             (define-key scala-mode-map (kbd "C-c C-l") 'ensime-inf-load-file)))

(provide 'my-scala)
