;;; my-keybindings.el --- useful keybindings.

;; better window-switching
(require 'switch-window)

;; smex keybindings
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; smarter kill-line
(global-set-key (kbd "C-k") 'sp-kill-sexp)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

;; Start proced in a similar manner to dired
(global-set-key (kbd "C-x p") 'proced)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c n") 'my-cleanup-buffer)
(global-set-key (kbd "C-x M-f") 'my-sudo-edit)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c w") (make-repeatable-command 'er/expand-region))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; C-x C-c is too easy to hit
(global-unset-key (kbd "C-x C-c"))

(provide 'my-keybindings)
