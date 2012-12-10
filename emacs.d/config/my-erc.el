;; Join the a couple of interesting channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#ruby" "#lisp" "#clojure"
                                     "#fluidinfo" "#haskell" "#ucsdpl"
                                     "#racket" "#stumpwm")))
(defvar my-erc-frame-buffers
  '("#fluidinfo" "#ucsdpl" "#emacs" "#haskell" "#racket"))

(add-hook 'erc-join-hook
          (lambda ()
            (let ((b (buffer-name (current-buffer))))
              (when (member b my-erc-frame-buffers)
                  (switch-to-buffer-other-frame b)
                  (shell-command "stumpish place-existing-windows")))))

(defun start-erc ()
  "Start erc and open a few frames."
  (interactive)
  (load "~/.ercpass")
  (erc :password erc-pass))

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
(setq erc-timestamp-format "[%H:%M] "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 15
      erc-fill-variable-maximum-indentation nil
      erc-fill-column 80)
(setq erc-header-line-format "%t: %o")

(require 'erc-notifications)
(add-to-list 'erc-modules 'notifications)

(provide 'my-erc)
