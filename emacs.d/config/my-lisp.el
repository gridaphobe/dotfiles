;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun my-lisp-coding-defaults ()
  ;; (paredit-mode +1)
  )

;; interactive modes don't need whitespace checks
(defun my-interactive-lisp-coding-defaults ()
  ;; (paredit-mode +1)
  (whitespace-mode -1))


;; clojure
(add-hook 'clojure-mode-hook 'my-lisp-coding-defaults)
(add-hook 'clojure-mode-hook 'nrepl-interaction-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'my-interactive-lisp-coding-defaults)
(add-hook 'nrepl-mode-hook 'subword-mode)
(setq nrepl-popup-stacktraces nil)


;; common-lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))
(add-hook 'lisp-mode-hook 'my-lisp-coding-defaults)
(add-hook 'slime-repl-mode-hook 'my-interactive-lisp-coding-defaults)


;; emacs lisp
(defun my-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun my-emacs-lisp-mode-defaults ()
  (my-lisp-coding-defaults)
  (turn-on-eldoc-mode)
  (my-remove-elc-on-save)
  (rainbow-mode +1))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-defaults)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)


;; scheme
(add-hook 'scheme-mode-hook 'my-lisp-coding-defaults)


(provide 'my-lisp)
