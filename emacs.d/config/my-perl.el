;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

(defun my-cperl-mode-defaults ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 8)
  ;; cperl-hairy affects all those variables, but I prefer
  ;; a more fine-grained approach as far as they are concerned
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)

  ;; if you want all the bells and whistles
  ;; (setq cperl-hairy)

  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)
  (setq cperl-invalid-face nil))

(add-hook 'cperl-mode-hook 'my-cperl-mode-hook t)

(provide 'my-perl)
