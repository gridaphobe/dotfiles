;;; eric-ui.el --- UI optimizations and tweaks.

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; i don't like the scrollbars..
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-frame-font "Ubuntu Mono-14")

(setq org-pretty-entities t)

;; For daemon mode, with-selected-frame seems to be required.  Normal
;; mode seems to require with-selected-frame to be absent.
(require 'server) ;;for server-running-p
(defun eric-frame-config (frame)
  "Custom behaviours for new frames."
  (if (eq system-type 'darwin)
      (if (server-running-p)
          (with-selected-frame frame
            (if (display-graphic-p)
                (modify-frame-parameters frame '((menu-bar-lines . 1)))
              (modify-frame-parameters frame '((menu-bar-lines . 0)))))
        (if (display-graphic-p)
            (modify-frame-parameters frame '((menu-bar-lines . 1)))
          (modify-frame-parameters frame '((menu-bar-lines . 0)))))
    (menu-bar-mode -1)))
;; run now
(eric-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'eric-frame-config)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

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

;; make whitespace-mode less intrusive
(setq whitespace-line-count 80
      whitespace-style '(face trailing tabs lines-tail indentation
                              space-after-tab space-before-tab))

(provide 'eric-ui)
