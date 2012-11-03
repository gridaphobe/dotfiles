;;; my-ui.el --- UI optimizations and tweaks.

;; experimental
;; (require 'fwb-cmds)
;; (setq pop-up-frames 'graphic-only)
;; ;; this shouldn't be necessary as it's the default...
;; (setq ido-default-file-method 'raise-frame)
;; (defun quit-window (&optional kill window)
;;     "Quit the current buffer.  Bury it, and maybe delete the selected frame.
;; \(The frame is deleted if it contains a dedicated window for the buffer.)
;; With a prefix argument, kill the buffer instead.

;; Noninteractively, if KILL is non-nil, then kill the current buffer,
;; otherwise bury it.

;; If WINDOW is non-nil, it specifies a window; we delete that window,
;; and the buffer that is killed or buried is the one in that window."
;;     (interactive "P")
;;     (let ((buffer (window-buffer window))
;;           (frame (window-frame (or window (selected-window))))
;;           (window-solitary
;;            (save-selected-window
;;              (if window
;;                  (select-window window))
;;              (one-window-p t)))
;;           window-handled)

;;       (save-selected-window
;;         (if window
;;             (select-window window))
;;         (or (window-minibuffer-p)
;;             (window-dedicated-p (selected-window))
;;             (switch-to-buffer (other-buffer))))

;;       ;; Get rid of the frame, if it has just one dedicated window
;;       ;; and other visible frames exist.
;;       (and (or (window-minibuffer-p) (window-dedicated-p window))
;;            (delq frame (visible-frame-list))
;;            window-solitary
;;            (if (and (eq default-minibuffer-frame frame)
;;                     (= 1 (length (minibuffer-frame-list))))
;;                (setq window nil)
;;              (delete-frame frame)
;;              (setq window-handled t)))

;;       ;; Deal with the buffer.
;;       (if kill
;;           (kill-buffer buffer)
;;         (bury-buffer buffer))

;;       ;; Maybe get rid of the window.
;;       (unless window-handled
;;         (condition-case nil
;;             (delete-window window)
;;           (error nil)))))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; i don't like the scrollbars..
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(add-to-list 'default-frame-alist '(font . "Consolas-10"))

(setq org-pretty-entities t)

;; For daemon mode, with-selected-frame seems to be required.  Normal
;; mode seems to require with-selected-frame to be absent.
(require 'server) ;;for server-running-p
(defun my-frame-config (frame)
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
(my-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'my-frame-config)

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

(provide 'my-ui)
