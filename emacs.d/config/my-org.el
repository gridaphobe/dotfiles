(require 'org-special-blocks)

;; org-mode configuration stuff
(setq org-directory "~/Dropbox/org"
      org-agenda-files '("~/Dropbox/org/todo.org")
      org-agenda-ndays 7
      org-agenda-repeating-timestamp-show-all nil
      org-agenda-restore-windows-after-quit t
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-sorting-strategy '((agenda time-up priority-down tag-up)
                                    (todo tag-up))
      org-agenda-start-on-weekday nil
      org-agenda-todo-ignore-deadlines t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-with-date t
      org-agenda-window-setup 'other-window
      org-capture-templates '(("t"
                               "Todo"
                               entry
                               (file+headline "todo.org" "Tasks")
                               "* TODO %^{Brief Description} %^g\n%?\nAdded: %U")
                              ("n"
                               "Note"
                               entry
                               (file+headline "notes.org" "Notes")
                               "* %^{Brief Description} %^g\n%?\nAdded: %U"))
      org-completion-use-ido t
      org-deadline-warning-days 7
      org-default-notes-file "~/Dropbox/org/notes.org"
      org-fast-tag-selection-single-key nil
      org-log-done 'done
      org-refile-targets '(("todo.org" :maxlevel . 1)
                           ("someday.org" :level . 2))
      org-reverse-note-order nil
      org-tags-match-list-sublevels nil
      org-use-fast-todo-selection t
      org-use-tag-inheritance nil)

(setq org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull "~/Dropbox/org/mobile_inbox.org")

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map [f8] 'remember)
(define-key global-map [f9] 'remember-region)

(provide 'my-org)
