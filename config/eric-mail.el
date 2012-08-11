(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ma-gnus"))
(require 'gnus-load)
(setq gnus-select-method
      '(nntp "news.gmane.org"))
(setq gnus-secondary-select-methods
      '((nnimap "imap.gmail.com"
                (nnimap-address "imap.gmail.com")
                (nnimap-inbox "INBOX")
                (nnimap-server-port 993)
                (nnimap-expunge t)
                (nnimap-stream ssl))))
(setq mm-text-html-renderer 'shr)
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Accessing the [Gmail] folders http://www.emacswiki.org/emacs/GnusGmail
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq gnus-ignored-newsgroups "")
;; . save sent items
(setq gnus-message-archive-method '(nnimap "imap.gmail.com"))
(setq gnus-message-archive-group "nnimap+imap.gmail.com:[Gmail]/Sent Mail")
(setq gnus-gcc-mark-as-read t)

;; General speedups.
(setq gnus-read-active-file nil)
(setq gnus-check-new-newsgroups nil)
(setq gnus-nov-is-evil nil)
(setq gnus-save-newsrc-file t)
(setq gnus-asynchronous t)
;; .
;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-number
;;         gnus-thread-sort-by-date
;;         gnus-thread-sort-by-total-score))
(setq gnus-summary-line-format
      "%U%R%z %~(pad-right 30)&user-date; %I%(%[%-25,25n%]%) %s\n")

(setq message-alternative-emails
      (regexp-opt '("gridaphobe@gmail.com" "eric@eseidel.org"
                    "eric9@mac.com" "eric9@me.com" "eric@fluidinfo.com"
                    "eseidel@eng.ucsd.edu")))
(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%A %d")
        ((gnus-seconds-year) . "%B %d")
        (t . "%B %d '%y"))) ;;this one is used when no other does match

(setq user-mail-address "gridaphobe@gmail.com"
      user-full-name "Eric Seidel")

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gridaphobe@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      mu4e-sent-messages-behavior 'delete)

(setq message-kill-buffer-on-exit t)

(provide 'eric-mail)
