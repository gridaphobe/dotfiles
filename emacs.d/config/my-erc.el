;; Join the a couple of interesting channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#ruby" "#lisp" "#clojure"
                                     "#fluidinfo" "#haskell")))
;; set your nickname
(setq erc-nick "gridaphobe")

;; share your username (optional)
(setq erc-user-full-name "Eric Seidel")

(setq erc-autojoin-timing 'ident)

;; ignore join/part/quit spam
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; clean up the erc modeline
(setq erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face))

;; always display a timestamp on the left, and only on the left
(setq erc-timestamp-format "[%H:%M] ")
(defun my-erc-timestamp-left (string)
  (let* ((ct (current-time))
         (ts (erc-format-timestamp ct erc-timestamp-format)))
    (erc-insert-timestamp-left ts)))
(setq erc-insert-timestamp-function 'my-erc-timestamp-left)

;;; trying rcirc

;; get notifications
(eval-after-load 'rcirc '(require 'rcirc-notify))

;; Turn on spell checking.
(add-hook 'rcirc-mode-hook (lambda ()
                             (flyspell-mode 1)))

;; Keep input line at bottom.
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively)
                 8192)))

;; Include date in time stamp.
(setq rcirc-time-format "[%H:%M] ")

;; Change user info
(setq rcirc-default-nick "gridaphobe")
(setq rcirc-default-user-name "gridaphobe")
(setq rcirc-default-full-name "Eric Seidel")

;; Join these channels at startup.
(setq rcirc-server-alist
      '(("irc.freenode.net" :channels
         ("#emacs" "#fluidinfo" "#haskell" "#ucsdpl"))))

;; dynamically set fill-column at redisplay time
;;
(defvar dim:dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

;; dynamically set fill-column at redisplay time
(defun dim:dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
            (- (window-width window)
               (or margin dim:dynamic-fill-column-margin))))))

(defun dim:dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'dim:dynamic-fill-column-window 'no-minibuf frame))

(eval-after-load 'rcirc
  '(add-to-list 'window-size-change-functions 'dim:dynamic-fill-column))

(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels))))
(provide 'my-erc)
