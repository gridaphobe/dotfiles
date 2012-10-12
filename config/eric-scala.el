(add-to-list 'load-path "~/.emacs.d/vendor/ensime/elisp")
(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
