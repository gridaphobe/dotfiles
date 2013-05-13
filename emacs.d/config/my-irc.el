;; Join the a couple of interesting channels whenever connecting to Freenode.
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#ruby" "#lisp" "#clojure"
                                     "#fluidinfo" "#haskell" "#ucsdpl"
                                     "#racket" "#stumpwm")))
(defvar my-erc-frame-buffers
  '("#fluidinfo" "#ucsdpl" "#emacs" "#haskell" "#racket"))

;; only open a new buffer when I join interactively
(setq erc-join-buffer 'buffer)
;; (defun my-erc-join-channel (channel &optional key)
;;   (interactive)
;;   (let ((erc-join-buffer 'buffer))
;;     (erc-join-channel channel key)))


;; (add-hook 'erc-join-hook
;;           (lambda ()
;;             (let ((b (buffer-name (current-buffer))))
;;               (when (member b my-erc-frame-buffers)
;;                   (switch-to-buffer-other-frame b)
;; ;                  (shell-command "stumpish place-existing-windows")
;;                   ))))
(load "~/.ercpass")

(defun start-erc ()
  "Start erc and open a few frames."
  (interactive)
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
(make-variable-buffer-local
 (defvar erc-last-datestamp nil))
(defun my-insert-timestamp (string)
  (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
    (unless (string= datestamp erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setq erc-last-datestamp datestamp)))
  (erc-insert-timestamp-left string))
(setq erc-timestamp-format "[%H:%M] "
      erc-datestamp-format " === [%Y-%m-%d %a] ===\n"
      erc-insert-timestamp-function 'my-insert-timestamp)
;; (setq erc-fill-function 'erc-fill-static
;;       erc-fill-static-center 15
;;       erc-fill-variable-maximum-indentation nil
;;       erc-fill-column 80)
(setq erc-header-line-format "%t: %o")

;; (require 'erc-notifications)
;; (add-to-list 'erc-modules 'notifications)

;; (make-variable-buffer-local 'erc-fill-column)
;; (add-hook 'window-configuration-change-hook
;;           '(lambda ()
;;              (save-excursion
;;                (walk-windows
;;                 (lambda (w)
;;                   (let ((buffer (window-buffer w)))
;;                     (set-buffer buffer)
;;                     (when (eq major-mode 'erc-mode)
;;                       (setq erc-fill-column (- (window-width w) 2))
;; ;                      (erc-fill)
;;                       )))))))

;; circe
(setq circe-default-nick "gridaphobe"
      circe-default-realname "Eric Seidel"
      circe-network-options `(("Freenode"
                               :nickserv-password ,erc-pass
                               :channels ("#ucsdpl" "#fluidinfo"
                                          "#fluidinfo-private toaster" "#haskell"
                                          "#emacs" "#racket"
                                          )))
      circe-nickserv-ghost-style 'after-auth
      circe-server-auto-join-default-type :after-nick
      circe-reduce-lurker-spam t
      circe-active-users-timeout 300
      circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
      circe-default-part-message "part"
      circe-default-quit-message "quit"
      circe-color-nicks-everywhere t
      circe-highlight-nick-type 'occurrence
      circe-new-buffer-behavior 'switch
      circe-new-buffer-behavior-ignore-auto-joins t
      circe-format-self-say "<{nick}> {body}"
      lui-flyspell-p t
      lui-time-stamp-position 'right
      lui-time-stamp-format "[%H:%M]"
      lui-time-stamp-only-when-changed-p t
      lui-fill-type "    "
      lui-max-buffer-size 30000
      tracking-ignored-buffers '(("#haskell" circe-highlight-nick-face)
                                 ("#emacs" circe-highlight-nick-face)
                                 ("#racket" circe-highlight-nick-face)))

(require 'circe-highlight-all-nicks)
(set-face-attribute 'circe-highlight-all-nicks-face nil
                    :foreground "wheat")

;; (defun my-lui-setup ()
;;   (setq ;fringes-outside-margins t
;;         ;left-margin-width 11
;;         word-wrap nil
;;         wrap-prefix "          |     "))
;; (add-hook 'lui-mode-hook 'my-lui-setup)

(require 'lui-logging)
(setq lui-logging-directory "~/.emacs.d/var/log/circe")
(require 'lui-autopaste)

(add-hook 'circe-channel-mode-hook 'enable-circe-highlight-all-nicks)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-logging)
(add-hook 'circe-channel-mode-hook 'enable-lui-irc-colors)

(add-hook 'circe-receive-message-functions 'circe-notify-on-mention)
(defun circe-notify-on-mention (nick user host command args)
  (when (and (string= command "PRIVMSG")
             (string-match circe-server-user (cadr args)))
    (call-process "terminal-notifier" nil 0 nil
                  "-title" (car args)
                  "-subtitle" nick
                  "-message" (cadr args))))
;; (let ((circe-server-user "gridaphobe"))
;;   (circe-notify-on-mention "bob" nil nil "PRIVMSG" '("#emacs" "gridaphobe: yo'")))

(add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")
(add-to-list 'clean-buffer-list-kill-never-regexps "^irc.*")

(provide 'my-irc)
