(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ma-gnus"))
(require 'gnus-load)
(require 'google-contacts)
(require 'google-contacts-gnus)
(require 'google-contacts-message)
(require 'nnir)

(setq message-directory "~/.mail")
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap")
               ))
;; save sent items
(setq gnus-message-archive-group "nnimap+gmail:Sent Mail")
(setq gnus-gcc-mark-as-read t)

(add-hook 'gnus-summary-exit-hook 'gnus-group-get-new-news)
(setq gnus-fetch-old-headers 'some)

;; bbdb
;; (require 'bbdb-autoloads)
;; (setq bbdb/mail-auto-create-p t)
;; (setq bbdb/news-auto-create-p (quote bbdb-ignore-some-messages-hook))
;; (bbdb-initialize 'gnus 'message)
;; (bbdb-insinuate-message)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; (setq gnus-select-method
;;       '(nntp "news.gmane.org"))
;; (setq gnus-secondary-select-methods
;;       '((nnimap "imap.gmail.com"
;;                 (nnimap-address "imap.gmail.com")
;;                 (nnimap-inbox "INBOX")
;;                 (nnimap-server-port 993)
;;                 (nnimap-stream ssl))))
(setq mm-text-html-renderer 'w3m) ;; need emacs-w3m-cvs for this to work!
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
;; Accessing the [Gmail] folders http://www.emacswiki.org/emacs/GnusGmail
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
(setq gnus-ignored-newsgroups "")

;; General speedups.
(setq gnus-read-active-file 'some)
(setq gnus-check-new-newsgroups nil)
(setq gnus-nov-is-evil t)
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil)
(setq gnus-asynchronous t)
(setq gnus-fetch-old-headers t)

;; .
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))
(setq gnus-summary-line-format
      "%U%R%z %~(pad-right 30)&user-date; %(%-25,25n%) %B%S\n")

(setq message-alternative-emails
      (regexp-opt '("gridaphobe@gmail.com" "eric@eseidel.org"
                    "eric9@mac.com" "eric9@me.com" "eric@fluidinfo.com"
                    "eseidel@eng.ucsd.edu" "eseidel@cs.ucsd.edu"
                    "eseidel@ucsd.edu")))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%A %d")
        ((gnus-seconds-year) . "%B %d")
        (t . "%B %d '%y"))) ;;this one is used when no other does match

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-summary-make-false-root 'adopt
      ;; things get really strange if only some threads have false roots..
      gnus-summary-make-false-root-always t
      gnus-sum-thread-tree-false-root " ┌ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├> "
      gnus-sum-thread-tree-root " ┬ "
      gnus-sum-thread-tree-single-indent " ─ "
      gnus-sum-thread-tree-single-leaf "└> "
      gnus-sum-thread-tree-vertical "│")

(setq user-mail-address "gridaphobe@gmail.com"
      user-full-name "Eric Seidel")

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gridaphobe@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq message-kill-buffer-on-exit t)

(provide 'my-mail)
