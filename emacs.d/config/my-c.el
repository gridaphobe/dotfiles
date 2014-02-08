(defun my-c-mode-common-defaults ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook 'my-c-mode-common-defaults)

(defun my-makefile-mode-defaults ()
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'my-makefile-mode-defaults)

(provide 'my-c)
