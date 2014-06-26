;;; init.el --- Emacs' configuration entry point.

(defconst on-mac (eq system-type 'darwin)
  "Are we on a Mac?")
(defconst is-netmacs (string= invocation-name "Netmacs")
  "Are we just running mail/irc?")
(defconst ns-bundle-id (concat "org.gnu." invocation-name))

(defvar emacs-dir (file-name-directory load-file-name)
  "The root folder of the configuration.")
(defvar emacs-vendor-dir (concat emacs-dir "vendor/")
  "This folder houses Emacs Lisp packages that are not yet available in
ELPA (or MELPA).")
(defvar emacs-var-dir (concat emacs-dir "var/"))
(defvar emacs-savefile-dir (concat emacs-var-dir "savefile/")
  "This folder stores all the automatically generated save/history-files.")

;; config changes made through the customize UI will be store here
(setq custom-file (concat emacs-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(unless (file-exists-p emacs-var-dir)
  (make-directory emacs-var-dir))
(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir))

(add-to-list 'load-path emacs-vendor-dir)

;; Turn off mouse interface early in startup to avoid momentary display
(when (and (fboundp 'menu-bar-mode) (not on-mac)) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir (concat emacs-dir "elpa/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;;;; macros
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(with-eval-after-load ,mode
     ,@body))


;;;; bind-key
(require-package 'bind-key)
(require 'bind-key)


;;;; diminish
(require-package 'diminish)


;;;; misc
(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; stupid italics
(make-face-unitalic 'font-lock-comment-face)
(make-face-unitalic 'font-lock-comment-delimiter-face)

;; make whitespace-mode less intrusive
(setq whitespace-line-count 80
      whitespace-style '(face trailing tabs lines-tail indentation
                              space-after-tab space-before-tab))

;; utf-8 stuff
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(setq require-final-newline t)

;; (setq debug-on-error t)
(setq gc-cons-threshold (* 20 (expt 2 20))) ; gc after 20MB

(setq-default fill-column 80)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; delete the selection with a keypress
(delete-selection-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      temporary-file-directory)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)

;; auto-save when switching buffers
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; try to complete at point if already indented
(setq tab-always-indent 'complete)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; kill old buffers
(require 'midnight)
(setq midnight-mode t)
(add-to-list 'clean-buffer-list-kill-never-regexps
             "^#\w+")
(add-to-list 'clean-buffer-list-kill-never-buffer-names
             "192.241.212.224:5000")

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)

;; enabled change region case commands
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; remember things between sessions
(recentf-mode 1)
(setq recentf-save-file (concat emacs-var-dir "recentf"))
(savehist-mode 1)
(setq savehist-file (concat emacs-var-dir "savehist"))
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file (concat emacs-var-dir "saveplace"))



;;;; subword-mode
;; (global-subword-mode 1)
;; (diminish 'subword-mode)


;;;; rainbow-mode
(require-package 'rainbow-mode)
(after 'rainbow-mode
  (diminish 'rainbow-mode))


;;;; ace-jump-mode
(require-package 'ace-jump-mode)
(after "ace-jump-mode-autoloads"
  (bind-key "C-;" 'ace-jump-mode)
  (bind-key "C-:" 'ace-jump-word-mode))


;;;; ack-and-a-half
(require-package 'ack-and-a-half)
(after "ack-and-a-half-autoloads"
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))


;;;; auto complete
(require-package 'company)
(after "company-autoloads"
  (global-company-mode 1)
  (diminish 'company-mode))


;;;; compile
(setq compilation-scroll-output 'first-error
      compilation-window-height 10)
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-window (get-buffer-window buf)))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)


;;;; css-mode
(add-hook 'css-mode-hook 'rainbow-mode)


;;;; edit-server
(require-package 'edit-server)


;;;; emacs lisp
(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '(nil "^;;;; \\(.+\\)$" 1) t))

(defun my/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'my/remove-elc-on-save)

(after "eldoc"
  (diminish 'eldoc-mode))

(bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
(bind-key "TAB" 'completion-at-point read-expression-map)


;;;; email
;; (when is-netmacs
;;   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  ;; (require 'mu4e)
  (require 'gnus)
  (require 'nnir)
  (require 'smtpmail)
  (require 'org-gnus)
  (require-package 'notmuch)
  (require 'notmuch)


  (defvar my/emails '("gridaphobe@gmail.com"
                      "eseidel@ucsd.edu"
                      "eseidel@cs.ucsd.edu"
                      "eseidel@eng.ucsd.edu"
                      "eric@eseidel.org"
                      "eseidel01@ccny.cuny.edu"
                      "eric9@mac.com"
                      "eric9@me.com"
                      "eric9@icloud.com"
                      "eric@fluidinfo.com"))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
;;   (setq mu4e-user-mail-address-list my/emails
;;         mu4e-view-show-images t
;;         mu4e-mu-binary "/usr/local/bin/mu"
;;         mu4e-maildir "~/.gmail"
;;         mu4e-sent-folder "/[Gmail].Sent Mail"
;;         mu4e-refile-folder "/[Gmail].All Mail"
;;         mu4e-drafts-folder "/[Gmail].Drafts"
;;         mu4e-trash-folder "/[Gmail].Trash"
;;         mu4e-headers-skip-duplicates t
;;         mu4e-headers-include-related nil
;;         mu4e-compose-cite-function 'message-cite-original
;;         mu4e-compose-signature-auto-include nil
;;         mu4e-compose-dont-reply-to-self t
;;         mu4e-sent-messages-behavior 'delete
;;         mu4e-html2text-command "w3m -dump -T text/html" ;"pandoc -f html -t org"
;;         mu4e-view-actions '(("bview in browser" . mu4e-action-view-in-browser)
;;                             ("pview as pdf" . mu4e-action-view-as-pdf)
;;                             ("cmodify tags" . mu4e-action-retag-message))
;;         mu4e-bookmarks '(("flag:unread AND NOT (maildir:/[Gmail].Trash OR maildir:/[Gmail].Spam)"
;;                           "Unread messages" ?n)
;;                          ("tag:\\\\Inbox" "Inbox" ?i)
;;                          ("flag:flagged" "Important" ?!)
;;                          ("maildir:[Gmail].Drafts" "Drafts" ?d)
;;                          ("tag:UCSD" "UCSD" ?u)
;;                          ("maildir:\"/[Gmail].Sent Mail\"" "Sent" ?s)
;;                          ("date:today..now"
;;                           "Today's messages" ?t)
;;                          ("maildir:/[Gmail].Spam" "Spam" ?j)))

;;   (setq mu4e-debug t)
;;   (setq mu4e-hide-index-messages t)
  (defun my/terminal-notifier (msg &optional title subtitle action)
    (call-process "terminal-notifier"
                  nil ;"*terminal-notifier*"
                  nil ;"terminal-notifier"
                  nil
                  "-message" (my/shell-escape msg)
                  "-title" (my/shell-escape (or title invocation-name))
                  "-subtitle" (my/shell-escape (or subtitle "Alert"))
                  "-sender" ns-bundle-id))
(defun my/shell-escape (s)
  (s-replace-all
   '(("[" . "\\\\[")
     ("(" . "\\\\(")
     ("{" . "\\\\{"))
   s))
(defun my/notify-new-mail ()
  (let ((mails (read (shell-command-to-string
                      "notmuch search --format=sexp tag:new"))))
    (--each mails
      (let ((from (plist-get it :authors))
            (subject (plist-get it :subject)))
        (my/terminal-notifier subject "New Mail" from)))
  ))
;;   ;; (my/terminal-notifier "hola" "hello" "hi" nil)
;;   (defun my/mu-notify (msg &optional point)
;;     (my/terminal-notifier (mu4e-message-field msg :subject)
;;                           "New Mail"
;;                           (mu4e~headers-contact-str
;;                            (mu4e-message-field msg :from)))
;;     (mu4e~proc-move (mu4e-message-field msg :docid) nil "-N"))
;;   (defun my/mu4e~headers-header-handler (msg &optional point)
;;     (if (eq major-mode 'mu4e-headers-mode)
;;         (mu4e~headers-header-handler msg point)
;;       (my/mu-notify msg point)))
;;   (setq mu4e-header-func 'my/mu4e~headers-header-handler)
;;   (defun my/update-mu-and-notify ()
;;     (mu4e~start
;;      (lambda ()
;;        (mu4e-update-index)
;;        (mu4e~proc-find "flag:new AND tag:\\\\INBOX" nil :date 'descending nil nil nil))))

;;   (defadvice mu4e (around mu4e-fullscreen activate)
;;     (window-configuration-to-register :mu4e-fullscreen)
;;     ad-do-it
;;     (delete-other-windows))

;;   (defadvice mu4e-quit (after mu4e-restore-screen activate)
;;     (jump-to-register :mu4e-fullscreen))


;;   (setq gnus-select-method
;;         '(nnimap "gmail"
;;                  (nnimap-stream shell)
;;                  (nnimap-shell-program "/usr/local/libexec/dovecot/imap")
;;                  (nnir-search-method imap))
;;         gnus-message-archive-group nil)

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)))

  (require-package 'bbdb)
  (require 'bbdb)
  (bbdb-initialize 'gnus 'message)

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls

        user-full-name "Eric Seidel"
        user-mail-address "gridaphobe@gmail.com"
        message-kill-buffer-on-exit t
        message-alternative-emails (regexp-opt my/emails)

        ;; gnus-parameters '(("nnimap.*"
        ;;                    (gnus-use-scoring nil)
        ;;                    (display . all))
        ;;                   )
        gnus-read-active-file 'some
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
        mm-discouraged-alternatives '("text/html" "text/richtext")
        gnus-buttonized-mime-types '("multipart/alternative")
        gnus-suppress-duplicates t
        gnus-use-cache t
        gnus-use-adaptive-scoring t
        gnus-completing-read-function 'gnus-ido-completing-read
        ;; gnus-ignored-newsgroups nil ;;"^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        gnus-spam-newsgroup-contents '((".*[Ss][Pp][Aa][Mm].*"
                                        gnus-group-spam-classification-spam)
                                       (".*" neither)))

  (setq message-dont-reply-to-names message-alternative-emails)

  (gnus-demon-add-handler 'gnus-demon-scan-news 5 t)

;;   (defadvice gnus (around gnus-fullscreen activate)
;;     (window-configuration-to-register :gnus-fullscreen)
;;     ad-do-it
;;     (delete-other-windows))

;;   (defadvice gnus-group-exit (after gnus-restore-screen activate)
;;     (jump-to-register :gnus-fullscreen)))


;;;; eshell
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-cmpl-cycle-completions nil
      eshell-cmpl-ignore-case t
      eshell-directory-name (concat emacs-var-dir "eshell/"))

;; per-project Eshell
(defun projectile-eshell ()
  (interactive)
  (let ((eshell-buffer-name
         (concat "*eshell"
                 (if (projectile-project-name)
                     (concat "-" (projectile-project-name))
                   "")
                 "*")))
    (eshell)))
(bind-key "C-x m" 'projectile-eshell)

;; Eshell Hooks
(defun eshell-settings ()
  (setq show-trailing-whitespace nil)
  (eshell-smart-initialize))

(add-hook 'eshell-mode-hook 'eshell-settings)
(add-hook 'eshell-mode-hook 'exec-path-from-shell-initialize)

;; Eshell Commands
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;;; expand-region
(require-package 'expand-region)
(require 'expand-region)
(bind-key "C-=" 'er/expand-region)


;;;; evil
(require-package 'evil)
(require 'evil)
(evil-mode 1)
(require-package 'surround)
(global-surround-mode 1)

(setq evil-search-module 'evil-search
      evil-cross-lines t
      evil-move-cursor-back nil)

;; (defadvice switch-to-buffer (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))

;; (defadvice select-window (around evil-back-to-initial-state activate)
;;   (when (ad-get-arg 1)
;;     (evil-change-state
;;      (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;   ad-do-it)
;; (defadvice other-window (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;; (defadvice other-frame (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;; (defadvice windmove-up (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;; (defadvice windmove-down (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;; (defadvice windmove-left (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;; (defadvice windmove-right (before evil-back-to-initial-state activate)
;;   (evil-change-state
;;    (evil-initial-state-for-buffer (current-buffer) evil-default-state)))


;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")     'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")     'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(defun evil-undefine ()
 (interactive)
 (let (evil-mode-map-alist)
   (call-interactively (key-binding (this-command-keys)))))

(bind-key "C-e" 'evil-end-of-line    evil-normal-state-map)
(bind-key "C-e" 'end-of-line         evil-insert-state-map)
(bind-key "C-e" 'evil-end-of-line    evil-visual-state-map)
(bind-key "C-e" 'evil-end-of-line    evil-motion-state-map)
(bind-key "C-f" 'evil-forward-char   evil-normal-state-map)
(bind-key "C-f" 'evil-forward-char   evil-insert-state-map)
(bind-key "C-f" 'evil-forward-char   evil-insert-state-map)
(bind-key "C-b" 'evil-backward-char  evil-normal-state-map)
(bind-key "C-b" 'evil-backward-char  evil-insert-state-map)
(bind-key "C-b" 'evil-backward-char  evil-visual-state-map)
(bind-key "C-d" 'evil-delete-char    evil-normal-state-map)
(bind-key "C-d" 'evil-delete-char    evil-insert-state-map)
(bind-key "C-d" 'evil-delete-char    evil-visual-state-map)
(bind-key "C-n" 'evil-next-line      evil-normal-state-map)
(bind-key "C-n" 'evil-next-line      evil-insert-state-map)
(bind-key "C-n" 'evil-next-line      evil-visual-state-map)
(bind-key "C-p" 'evil-previous-line  evil-normal-state-map)
(bind-key "C-p" 'evil-previous-line  evil-insert-state-map)
(bind-key "C-p" 'evil-previous-line  evil-visual-state-map)
(bind-key "C-w" 'evil-delete         evil-normal-state-map)
(bind-key "C-w" 'evil-delete         evil-insert-state-map)
(bind-key "C-w" 'evil-delete         evil-visual-state-map)
(bind-key "C-y" 'yank                evil-normal-state-map)
(bind-key "C-y" 'yank                evil-insert-state-map)
(bind-key "C-y" 'yank                evil-visual-state-map)
(bind-key "C-k" 'kill-line           evil-normal-state-map)
(bind-key "C-k" 'kill-line           evil-insert-state-map)
(bind-key "C-k" 'kill-line           evil-visual-state-map)
(bind-key "C-r" 'isearch-backward    evil-normal-state-map)
(bind-key "C-r" 'isearch-backward    evil-insert-state-map)
(bind-key "C-r" 'isearch-backward    evil-visual-state-map)
(bind-key "Q"   'call-last-kbd-macro evil-normal-state-map)
(bind-key "Q"   'call-last-kbd-macro evil-visual-state-map)
(bind-key "TAB" 'evil-undefine       evil-normal-state-map)
(bind-key "RET" 'evil-undefine       evil-insert-state-map)


;;;; flycheck
(require-package 'flycheck)
(require-package 'flycheck-haskell)
(after "flycheck-autoloads"
  (global-flycheck-mode 1))
(after 'flycheck
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-highlighting-mode nil))


;;;; flyspell
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra")
      flyspell-issue-message-flag nil ; issuing a message for each word is slow
      )
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)


;;;; god-mode
(require-package 'god-mode)

(require-package 'evil-god-state)
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))


;;;; haskell
(require-package 'haskell-mode)
(after 'haskell-mode
  (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
  (bind-key "C-c C-t" 'haskell-process-do-type   haskell-mode-map)
  (bind-key "C-c C-i" 'haskell-process-do-info   haskell-mode-map)
  (bind-key "SPC" 'haskell-mode-contextual-space haskell-mode-map)
  (setq haskell-process-type 'cabal-repl
        haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans"
                                          ;;"--with-ghc=ghci-ng"
                                          )
        ;; haskell-process-type 'ghci
        ;; haskell-process-path-ghci "ghci"
        haskell-process-log t)

  ;; (defun my/haskell-sp-forward-slurp-sexp (&optional ARG)
  ;;   "For some reason `sp-forward-slurp-sexp' in `haskell-mode'
  ;; inserts an extra space at the beginning of the line..."
  ;;   (interactive)
  ;;   (sp-forward-slurp-sexp ARG)
  ;;   ;; (save-excursion
  ;;   ;;   (beginning-of-line)
  ;;   ;;   (delete-forward-char 1))
  ;;   )
  ;; (bind-key "C-)" 'my/haskell-sp-forward-slurp-sexp haskell-mode-map)

  ;; (defun my/haskell-sp-forward-barf-sexp (&optional ARG)
  ;;   "For some reason `sp-forward-barf-sexp' in `haskell-mode'
  ;; inserts an extra space at the beginning of the line..."
  ;;   (interactive)
  ;;   (sp-forward-barf-sexp ARG)
  ;;   ;; (save-excursion
  ;;   ;;   (beginning-of-line)
  ;;   ;;   (delete-forward-char 1))
  ;;   )
  ;; (bind-key "C-}" 'my/haskell-sp-forward-barf-sexp haskell-mode-map)
  )

;; (require-package 'hi2)
;; (after "hi2-autoloads"
;;   (add-hook 'haskell-mode-hook 'turn-on-hi2)
;;   (setq hi2-show-indentations nil))
;; (after 'hi2
;;   (diminish 'hi2-mode))

(require-package 'shm)
(after "shm-autoloads"
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  ;; (add-hook 'haskell-mode-hook 'turn-off-smartparens-mode)
  (add-hook 'haskell-interactive-mode 'structured-haskell-repl-mode)
  ;; (add-hook 'haskell-interactive-mode 'turn-off-smartparens-mode)
  )

(after 'flycheck-haskell
  (flycheck-define-checker haskell-hdevtools
  "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
  :command
  ("hdevtools" "check" "-g" "-Wall"
   (eval (when flycheck-ghc-no-user-package-database
           (list "-g" "-no-user-package-db")))
   (eval (apply #'append (mapcar (lambda (db) (list "-g" "-package-db" "-g" db))
                                 flycheck-ghc-package-databases)))
   (eval (list
          "-g" "-i" "-g"
          (flycheck-module-root-directory
           (flycheck-find-in-buffer flycheck-haskell-module-re))))
   (eval (apply #'append (mapcar (lambda (db) (list "-g" "-i" "-g" db))
                                 flycheck-ghc-search-path)))
   source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n    ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))

  (defun killall-hdevtools ()
    (interactive)
    (shell-command "killall hdevtools")
    (flycheck-buffer))
  (bind-key "C-c C" 'killall-hdevtools haskell-mode-map)
  )

(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".hdevtools.sock")

;; haskell-mode doesn't derive from prog-mode
(add-hook 'haskell-mode-hook 'my/prog-mode-defaults)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook 'flycheck-mode)


;;;; helm
(require-package 'helm)
;; (helm-mode 1)
(setq helm-buffers-fuzzy-matching t)
;; (bind-key "M-x" 'helm-M-x)
;; (require-package 'popwin)
;; (require 'popwin)
;; (popwin-mode 1)
;; (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
;; (push '("*helm M-x*" :height 20) popwin:special-display-config)
;; (require 'helm-rdio)


;;;; irc
;; (when is-netmacs)
(defvar znc-server "")
(defvar znc-port "")
(defvar znc-tls nil)
(defvar znc-user "")
(defvar znc-pass "")
(load (concat emacs-dir "private.el"))
(require 'erc)
(require 'tls)
(require 'znc)
(setq erc-server   znc-server
      erc-port     znc-port
      erc-nick     znc-user
      erc-password znc-pass
      erc-server-auto-reconnect nil
      )
(require 'erc-terminal-notifier)

(require-package 'circe)
(after "circe-autoloads"
  (setq circe-network-options `(("Freenode"
                                 :host ,znc-server
                                 :port ,znc-port
                                 :tls  ,znc-tls
                                 :user ,znc-user
                                 :pass ,znc-pass
                                 :nick ,znc-user))
        circe-reduce-lurker-spam t
        circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
        lui-flyspell-p t
        lui-flyspell-alist '((".*" "american"))
        lui-max-buffer-size 10000
        lui-time-stamp-position 'right-margin
        lui-time-stamp-format "%H:%M"
        lui-fill-type nil
        tracking-ignored-buffers '(("#emacs" circe-highlight-nick-face)
                                   ("#haskell" circe-highlight-nick-face))
        circe-format-self-say "<{nick}> {body}")
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (add-hook 'circe-chat-mode-hook 'my/circe-prompt)
  (defun my/circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) ">")
                         'face 'circe-prompt-face)
             " ")))
  (require 'lui-logging)
  (add-hook 'circe-channel-mode-hook 'enable-lui-logging)
  (add-hook 'lui-mode-hook 'my/lui-setup)
  (defun my/lui-setup ()
    (setq wrap-prefix "    "
          fringes-outside-margins t
          right-margin-width 5)
    (turn-on-visual-line-mode))
  (require 'circe-color-nicks)
  (enable-circe-color-nicks)
  (setq circe-color-nicks-everywhere t)

  ;; make channel-join messages display in the right buffer..
  (defun my/circe-message-option-chanserv (nick user host command args)
    (when (and (string= "ChanServ" nick)
               (string-match "^\\[#.+?\\]" (cadr args)))
      '((dont-display . t))))
  (add-hook 'circe-message-option-functions 'my/circe-message-option-chanserv)

  (defun my/circe-chanserv-message-handler (nick user host command args)
    (when (and (string= "ChanServ" nick)
               (string-match "^\\[\\(#.+?\\)\\]" (cadr args)))
      (let* ((channel (match-string 1 (cadr args)))
             (buffer (circe-server-get-chat-buffer channel t)))
        (let ((circe-server-last-active-buffer buffer))
          (circe-display-NOTICE nick user host command args)))))
  (circe-add-message-handler "NOTICE" 'my/circe-chanserv-message-handler))



;;;; ido-mode
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-max-prospects 10
      ido-save-directory-list-file (concat emacs-savefile-dir "ido.hist")
      ido-default-file-method 'selected-window
      ido-use-faces nil)

(require-package 'flx-ido)
(after "flx-ido-autoloads"
  (flx-ido-mode 1))

(require-package 'ido-ubiquitous)
(after "ido-ubiquitous-autoloads"
  (ido-ubiquitous-mode 1))

(require-package 'ido-vertical-mode)
(after "ido-vertical-mode-autoloads"
  (ido-vertical-mode 1))


;;;; javascript
(require-package 'js3-mode)
;; (after "js2-mode-autoloads"
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;   (setq js2-auto-indent-p t
;;         js2-cleanup-whitespace t
;;         js2-enter-indents-newline t
;;         js2-indent-on-enter-key t
;;         js2-pretty-multiline-declarations t))


;;;; latex
(require-package 'auctex)
(require-package 'auctex-latexmk)
(after "auctex-autoloads"
  (setq-default TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (setq reftex-plug-into-AUCTeX t
        TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil))


;;;; magit
(require-package 'magit)
(after "magit-autoloads"
  (bind-key "C-x g" 'magit-status)
  ;; (setq magit-restore-window-configuration t)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defadvice magit-mode-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  )


;;;; markdown
(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))


;;;; org-mode
(require-package 'org-plus-contrib)
(require 'org)
(setq org-src-fontify-natively t)


;;;; paredit
;; (require-package 'paredit)
;; (after "paredit-autoloads"
;;   ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
;;   (defun conditionally-enable-paredit-mode ()
;;     (if (eq this-command 'eval-expression)
;;         (paredit-mode 1)))

;;   (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
;;   (add-hook 'prog-mode-hook 'enable-paredit-mode)
;;   (add-hook 'haskell-mode-hook 'enable-paredit-mode))

;; (electric-indent-mode 1)
;; (electric-layout-mode 1)
;; (electric-pair-mode t)

;;(show-paren-mode t)

;;;; prog-mode
(defun my/local-comment-auto-fill ()
  (auto-fill-mode 1)
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

(require-package 'ws-butler)
(ws-butler-global-mode 1)
(diminish 'ws-butler-mode)
(diminish 'highlight-changes-mode)

(defun my/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (my/local-comment-auto-fill)
  ;; (whitespace-mode 1)
  (abbrev-mode 1)
  (my/add-watchwords))

(after "abbrev"
  (setq abbrev-file-name (concat emacs-var-dir "abbrev"))
  (diminish 'abbrev-mode))

(add-hook 'prog-mode-hook 'my/prog-mode-defaults)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))
      (recenter)))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names (substring-no-properties name))
          (add-to-list 'name-and-pos (cons (substring-no-properties name) position))))))))

(bind-key "C-c i" 'ido-goto-symbol)


;;;; pretty symbols
(global-prettify-symbols-mode 1)


;;;; projectile
(require-package 'projectile)
(after "projectile-autoloads"
  (projectile-global-mode)
  (diminish 'projectile-mode)
  (setq projectile-remember-window-configs t
        ;; projectile-enable-caching t
        projectile-cache-file (concat emacs-var-dir "projectile.cache")
        projectile-known-projects-file (concat emacs-var-dir "projectile-bookmarks.eld")))


;;;; smart-mode-line
(require-package 'smart-mode-line)
;; (after "smart-mode-line-autoloads"
;;   (sml/setup))


;;;; smartparens
(require-package 'smartparens)
(require 'smartparens-config)
(setq-default sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(show-smartparens-global-mode 1)
(smartparens-global-mode 1)

(after 'diminish
  (diminish 'smartparens-mode))
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;; (setq sp-ignore-modes-list
;;       (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

;; (defun conditionally-enable-smartparens-mode ()
;;   (when (eq this-command 'eval-expression)
;;     (smartparens-strict-mode 1)
;;     (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

;; (add-hook 'minibuffer-exit-hook
;;           (lambda () (sp-local-pair 'minibuffer-inactive-mode "'" "'"
;;                                :unless '(sp-point-after-word-p))))


;; only using these keys for now since smartparens seems to override
;; any buffer-local bindings
(bind-key "C-k" 'sp-kill-hybrid-sexp)
(bind-key "C-)" 'sp-forward-slurp-sexp)
(bind-key "C-}" 'sp-forward-barf-sexp)


;;;; smex
(require-package 'smex)
(after "smex-autoloads"
  (smex-initialize)
  (setq smex-save-file (concat emacs-var-dir "smex"))
  (bind-key "M-x" 'smex)
  (bind-key "M-X" 'smex-major-mode-commands))


;;;; solarized
(require-package 'solarized-theme)


;;;; switch-window
(require-package 'switch-window)
(bind-key "C-x o"   'switch-window)
(bind-key "C-x C-o" 'switch-window)

;;;; tramp
(require 'tramp)
(setq tramp-default-method "ssh"
      tramp-persistency-file-name (concat emacs-var-dir "tramp"))

;;;; undo-tree
(require-package 'undo-tree)
(after "undo-tree-autoloads"
  (global-undo-tree-mode 1)
  (diminish 'undo-tree-mode)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))


;;;; volatile-highlights
(require-package 'volatile-highlights)
;; HACK: there's nothing in "volatile-highlights-autoloads.el" so
;; can't use the standard `after' macro
(require 'volatile-highlights)
(volatile-highlights-mode 1)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-kill activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;;;; whitespace
(after "whitespace"
  (diminish 'whitespace-mode))


;;;; ws-butler
;; (require-package 'ws-butler)
;; (after "ws-butler-autoloads"
;;   (ws-butler-global-mode t))


;;;; xml
(push '("<\\?xml" . nxml-mode) magic-mode-alist)

(setq nxml-child-indent 4)
(setq nxml-attribute-indent 4)
(setq nxml-auto-insert-xml-declaration-flag nil)
(setq nxml-bind-meta-tab-to-complete-flag t)
(setq nxml-slash-auto-complete-flag t)


;;;; theme
(defadvice load-theme (around disable-other-themes activate)
  (mapc #'disable-theme custom-enabled-themes)
  ad-do-it
  ;;(sml/apply-theme 'respectful)
  )

(require-package 'zenburn-theme)
;; (after "zenburn-theme-autoloads"
;;   (load-theme 'zenburn)
;;   (sml/apply-theme 'dark))
(require-package 'leuven-theme)
(load-theme 'leuven)
(set-background-color "WhiteSmoke")
(require-package 'powerline)
(powerline-center-evil-theme)

;;;; generic keybindings
(bind-key "C-x \\" 'align-regexp)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-h A" 'apropos)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-o" 'open-next-line)
(bind-key "M-o" 'open-previous-line)
(windmove-default-keybindings)

;; Activate occur easily inside isearch
(bind-key "C-o"
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string)))))
  isearch-mode-map)


;;;; mac stuff
(when on-mac
  ;; Emacs users obviously have little need for Command and Option keys,
  ;; but they do need Meta and Super
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  (bind-key "<s-return>" 'toggle-frame-fullscreen)

  (set-fontset-font "fontset-default"
                    'unicode
                    '("Menlo" . "iso10646-1"))
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :slant  'normal
                      :weight 'normal
                      :width  'normal
                      :height 140)

  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  (setq browse-url-browser-function 'browse-url-default-macosx-browser))


;; finally, start the server for emacsclient
(require 'server)
(when is-netmacs
  (setq server-name "netmacs"))
(unless (server-running-p)
  (server-start))

;;;; mode line settings
;; FIXME: something in this file is toggling `line-number-mode' so I have to put
;; this at the end......
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(xterm-mouse-mode 1)

(message "Emacs is ready to do thy bidding, Master %s!" (getenv "USER"))
