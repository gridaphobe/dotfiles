;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun eric-lisp-coding-defaults ()
  (paredit-mode +1))

;; interactive modes don't need whitespace checks
(defun eric-interactive-lisp-coding-defaults ()
  (paredit-mode +1)
  (whitespace-mode -1))


;; clojure
(add-hook 'clojure-mode-hook 'eric-lisp-coding-defaults)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'eric-interactive-lisp-coding-defaults)
(setq nrepl-popup-stacktraces nil)

;; common-lisp
;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

(add-hook 'lisp-mode-hook 'eric-lisp-coding-defaults)
(add-hook 'slime-repl-mode-hook 'eric-interactive-lisp-coding-defaults)


;; emacs lisp
(defun eric-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun eric-emacs-lisp-mode-defaults ()
  (eric-lisp-coding-defaults)
  (turn-on-eldoc-mode)
  (eric-remove-elc-on-save)
  (rainbow-mode +1))

(add-hook 'emacs-lisp-mode-hook 'eric-emacs-lisp-mode-defaults)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)


;; scheme
(add-hook 'scheme-mode-hook 'eric-lisp-coding-defaults)


(provide 'eric-lisp)
