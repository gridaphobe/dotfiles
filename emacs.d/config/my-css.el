(defun my-css-mode-defaults ()
  (setq css-indent-offset 2)
  (rainbow-mode +1))

(add-hook 'css-mode-hook 'my-css-mode-defaults)

(provide 'my-css)
