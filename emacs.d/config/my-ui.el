;;; my-ui.el --- UI optimizations and tweaks.

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; i don't like the scrollbars..
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq ring-bell-function 'ignore)

;; For daemon mode, with-selected-frame seems to be required.  Normal
;; mode seems to require with-selected-frame to be absent.
;; (require 'server) ;;for server-running-p
;; (defun my-frame-config (frame)
;;   "Custom behaviours for new frames."
;;   (if (eq system-type 'darwin)
;;       (if (server-running-p)
;;           (with-selected-frame frame
;;             (if (display-graphic-p)
;;                 (modify-frame-parameters frame '((menu-bar-lines . 1)))
;;               (modify-frame-parameters frame '((menu-bar-lines . 0)))))
;;         (if (display-graphic-p)
;;             (modify-frame-parameters frame '((menu-bar-lines . 1)))
;;           (modify-frame-parameters frame '((menu-bar-lines . 0)))))
;;     (menu-bar-mode -1)))
;; ;; run now
;; (my-frame-config (selected-frame))
;; ;; and later
;; (add-hook 'after-make-frame-functions 'my-frame-config)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name "" (:eval (if (buffer-file-name)
                                         (abbreviate-file-name (buffer-file-name))
                                       "%b"))))

;; use zenburn as the default theme
(load-theme 'zenburn t)
;; (setq solarized-distinct-fringe-background t
;;       solarized-high-contrast-mode-line t)
;; (load-theme 'solarized-dark t)

;; stupid italics
(make-face-unitalic 'font-lock-comment-face)
(make-face-unitalic 'font-lock-comment-delimiter-face)

;; make whitespace-mode less intrusive
(setq whitespace-line-count 80
      whitespace-style '(face trailing tabs lines-tail indentation
                              space-after-tab space-before-tab))


(provide 'my-ui)
