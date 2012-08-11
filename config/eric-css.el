(defun eric-css-mode-defaults ()
  (setq css-indent-offset 2)
  (rainbow-mode +1))

(add-hook 'css-mode-hook 'eric-css-mode-defaults)

(provide 'eric-css)
