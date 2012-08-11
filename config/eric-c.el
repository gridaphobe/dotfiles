(defun eric-c-mode-common-defaults ()
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook 'eric-c-mode-common-hook)

(defun eric-makefile-mode-defaults ()
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'eric-makefile-mode-hook)

(provide 'eric-c)
