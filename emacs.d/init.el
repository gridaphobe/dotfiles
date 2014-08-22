;;; init.el --- Emacs' configuration entry point.

;;; Commentary:
;;; Emacs' configuration entry point.

;;; Code:

(defconst on-mac (eq system-type 'darwin)
  "Are we on a Mac?")
(defconst is-netmacs (string= invocation-name "Netmacs")
  "Are we just running mail/irc?")
(defconst ns-bundle-id (concat "org.gnu." invocation-name))

;; config changes made through the customize UI will be store here
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off mouse interface early in startup to avoid momentary display
(when (and (fboundp 'menu-bar-mode) (not on-mac)) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;;;; package.el
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; make sure we have req-package installed
(when (not (package-installed-p 'req-package))
  (package-refresh-contents)
  (package-install 'req-package))
(require 'req-package)

;;;; misc
(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

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
(savehist-mode 1)
(setq-default save-place t)
(require 'saveplace)


;;;; subword-mode
;; (global-subword-mode 1)
;; (diminish 'subword-mode)


;;;; rainbow-mode
(req-package rainbow-mode
             :diminish "")

;;;; ace-jump-mode
(req-package ace-jump-mode)


;;;; auto complete
;; (req-package company
;;   :diminish ""
;;   )


;;;; compile
(req-package compile
  :init
  (progn
    (setq compilation-scroll-output 'first-error
          compilation-window-height 10)
    (defun bury-compile-buffer-if-successful (buffer string)
      "Bury a compilation buffer if succeeded without warnings."
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
    (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)))


;;;; css-mode
(add-hook 'css-mode-hook 'rainbow-mode)


;;;; discover-my-major
(req-package discover-my-major
  :init (define-key 'help-command (kbd "C-m") 'discover-my-major))


;;;; edit-server
;; (require-package 'edit-server)


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
;;(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'my/remove-elc-on-save)

(req-package eldoc
  :diminish "")

(bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
(bind-key "TAB" 'completion-at-point read-expression-map)


;;;; expand-region
(req-package expand-region
  :bind (("M-m" . er/expand-region)))


;;;; evil
(req-package evil
  :require (ace-jump-mode helm-config undo-tree)
  :init 
  (progn
    (evil-mode 1)
    ;; prevent esc-key from translating to meta-key in terminal mode
    (setq evil-esc-delay 0)
    (setq ;; evil-motion-state-modes (append evil-emacs-state-modes
          ;;                                 evil-motion-state-modes)
          ;; evil-emacs-state-modes '(magit-mode dired-mode)
          evil-search-module 'evil-search
          evil-cross-lines t
          evil-move-cursor-back nil))
  :config
  (progn
    (evil-ex-define-cmd "e[dit]" 'helm-find-files)
    (evil-ex-define-cmd "b[uffer]" 'helm-buffers-list)
    (bind-key "[escape]" 'keyboard-escape-quit evil-normal-state-map)
    (bind-key "[escape]" 'keyboard-escape-quit evil-visual-state-map)
    (bind-key "<escape>" 'keyboard-escape-quit)
    (bind-key "\"" 'ace-jump-mode evil-normal-state-map)

    ;; Make movement keys work like they should
    
    (bind-key "<remap> <evil-next-line>"     
              'evil-next-visual-line     
              evil-normal-state-map)
    (bind-key "<remap> <evil-previous-line>" 
              'evil-previous-visual-line 
              evil-normal-state-map)
    (bind-key "<remap> <evil-next-line>"     
              'evil-next-visual-line     
              evil-motion-state-map)
    (bind-key "<remap> <evil-previous-line>" 
              'evil-previous-visual-line 
              evil-motion-state-map)
    
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
    ))

(req-package evil-surround
  :require (evil)
  :init (global-evil-surround-mode 1))

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




;;;; flycheck
(req-package flycheck
  :init (global-flycheck-mode 1)
  :config (setq flycheck-check-syntax-automatically '(mode-enabled save)))
(req-package flycheck-pos-tip
  :require (flycheck)
  :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;;;; flyspell
(req-package flyspell
  :init
  (progn
    (setq ispell-program-name "aspell" ; use aspell instead of ispell
          ispell-extra-args '("--sug-mode=ultra")
          flyspell-issue-message-flag nil ; issuing a message for each word is slow
          )
    (add-hook 'message-mode-hook 'flyspell-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)
    ))


;;;; god-mode
(req-package god-mode)

(req-package evil-god-state
  :require (diminish evil god-mode)
  :init
  (progn
    (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
    (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
    (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))))


;;;; haskell
(req-package haskell-mode
  :require (flycheck)
  :init
  (progn
    (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
    (bind-key "C-c C-t" 'haskell-process-do-type   haskell-mode-map)
    (bind-key "C-c C-i" 'haskell-process-do-info   haskell-mode-map)
    (bind-key "SPC" 'haskell-mode-contextual-space haskell-mode-map)
    (flycheck-define-checker
     haskell-hdevtools
     "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
     :command
     ("hdevtools" "check" "-g" "-Wall"
      source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line ":" column ":"
               (or " " "\n    ") "Warning:" (optional "\n")
               (message
                (one-or-more " ") (one-or-more not-newline)
                (zero-or-more "\n"
                              (one-or-more " ")
                              (one-or-more not-newline)))
               line-end)
      (error line-start (file-name) ":" line ":" column ":"
             (or (message (one-or-more not-newline))
                 (and "\n"
                      (message
                       (one-or-more " ") (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (one-or-more " ")
                                     (one-or-more not-newline)))))
             line-end))
     :error-filter
     (lambda (errors)
       (-> errors
         flycheck-dedent-error-messages
         flycheck-sanitize-errors))
     :modes (haskell-mode literate-haskell-mode)
     :next-checkers ((warnings-only . haskell-hlint)))
    
    (defun killall-hdevtools ()
      (interactive)
      (shell-command "killall hdevtools")
      (flycheck-buffer))
    (bind-key "C-c C" 'killall-hdevtools haskell-mode-map)

    (setq haskell-process-type 'ghci
          haskell-process-log t
          haskell-align-imports-pad-after-name t
          ;; haskell-font-lock-symbols 'unicode
          haskell-stylish-on-save nil
          haskell-process-suggest-hoogle-imports t
          haskell-process-suggest-remove-import-lines t
          haskell-process-use-presentation-mode nil)

    ;; haskell-mode doesn't derive from prog-mode
    (add-hook 'haskell-mode-hook 'my/prog-mode-defaults)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    ;; (add-hook 'haskell-mode-hook 
    ;;           '(lambda () (flycheck-select-checker 'haskell-hdevtools)))
    (add-hook 'haskell-mode-hook '(lambda () (flycheck-mode -1)))
    ))

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

;; (require-package 'hi2)
;; (after "hi2-autoloads"
;;   (add-hook 'haskell-mode-hook 'turn-on-hi2)
;;   (setq hi2-show-indentations nil))
;; (after 'hi2
;;   (diminish 'hi2-mode))

;; (req-package shm
;;   :require (haskell-mode)
;;   :init
;;   (progn
;;     (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;;     ;; (add-hook 'haskell-mode-hook 'turn-off-smartparens-mode)
;;     (add-hook 'haskell-interactive-mode 'structured-haskell-repl-mode)
;;     ;; (add-hook 'haskell-interactive-mode 'turn-off-smartparens-mode)
;;     (setq shm-colon-enabled t
;;           shm-indent-point-after-adding-where-clause t
;;           shm-lambda-indent-style 'leftmost-parent
;;           shm-use-hdevtools t
;;           shm-use-presentation-mode t)))

(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".hdevtools.sock")



;;;; helm
(req-package helm-config
  :ensure helm
  :diminish ((helm-mode . ""))
  :init
  (progn
    (helm-mode 1)
    (helm-adaptive-mode 1)
    (setq helm-buffers-fuzzy-matching t
          ido-use-virtual-buffers t
          helm-ff-auto-update-initial-value t
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files t
          helm-quick-update t                   ; do not display invisible candidates
          helm-split-window-default-side 'other ; open helm buffer in another window
          helm-split-window-in-side-p t ; open helm buffer inside current window,
                                        ; don't occupy whole other window
          
          helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                               '("\\.$" "\\.\\.$"))
          )

    (bind-key "C-x b" 'helm-mini)
    (bind-key "C-c i" 'helm-semantic-or-imenu))
  :config
  (progn
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map) ; rebind tab to do persistent action
    (bind-key "C-i"   'helm-execute-persistent-action helm-map) ; make TAB work in terminal
    (bind-key "C-z"   'helm-select-action             helm-map) ; list actions using C-z
    
    (bind-key "<return>" 'helm-grep-mode-jump-other-window          helm-grep-mode-map)
    (bind-key "n"        'helm-grep-mode-jump-other-window-forward  helm-grep-mode-map)
    (bind-key "p"        'helm-grep-mode-jump-other-window-backward helm-grep-mode-map)))
    
(req-package helm-spotify
  :require (helm))


;;;; javascript
(req-package js3-mode)
;; (after "js2-mode-autoloads"
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;   (setq js2-auto-indent-p t
;;         js2-cleanup-whitespace t
;;         js2-enter-indents-newline t
;;         js2-indent-on-enter-key t
;;         js2-pretty-multiline-declarations t))


;;;; latex
(req-package tex-site
  :ensure auctex
  :init
  (progn
    (setq-default TeX-PDF-mode t)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)
    (setq reftex-plug-into-AUCTeX t
          reftex-default-bibliography '("main.bib")
          TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil))
  )
(req-package auctex-latexmk
  :require (tex-site))


;;;; magit
(req-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :init
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    
    (defadvice magit-mode-quit-window (after magit-restore-screen activate)
      (jump-to-register :magit-fullscreen))))

  
;;;; markdown
(req-package markdown-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
    ))


;;;; nix-mode
(req-package nix-mode)


;;;; org-mode
(req-package org
  :ensure org-plus-contrib
  :init (setq orc-src-fontify-natively t))


;;;; prog-mode
(defun my/local-comment-auto-fill ()
  (auto-fill-mode 1)
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\)"
          1 font-lock-warning-face t))))

(electric-indent-mode -1)
(electric-layout-mode -1)
(electric-pair-mode -1)

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; (require-package 'ws-butler)
;; (ws-butler-global-mode 1)
;; (diminish 'ws-butler-mode)
;; (diminish 'highlight-changes-mode)

(defun my/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (my/local-comment-auto-fill)
  ;; (whitespace-mode 1)
  (my/add-watchwords))

(add-hook 'prog-mode-hook 'my/prog-mode-defaults)


;;;; projectile
(req-package projectile
  :diminish ""
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-remember-window-configs t
          projectile-completion-system 'helm)))


;;;; shells
(req-package eshell
  :require (projectile)
  :init
  (progn
    (require 'em-smart)
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t
          eshell-cmpl-cycle-completions nil
          eshell-cmpl-ignore-case t
          eshell-output-filter-functions '(eshell-handle-ansi-color
                                           eshell-handle-control-codes
                                           eshell-watch-for-password-prompt
                                           eshell-truncate-buffer))
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
        (erase-buffer)))))

(req-package shell
  :init (setq shell-file-name "zsh"))


;;;; smart-mode-line
(req-package smart-mode-line
  :init (sml/setup))


;;;; smartparens
(req-package smartparens-config
  :ensure smartparens
  :bind (("C-k"   . sp-kill-hybrid-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-p" . sp-forward-slurp-sexp)
         ("C-M-o" . sp-forward-barf-sexp))
  :init
  (progn
    (setq-default sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (show-smartparens-global-mode 1)
    (smartparens-global-strict-mode 1))
  
  :config
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))

;;;; switch-window
(req-package switch-window
  :bind (("C-x o"   . switch-window)
         ("C-x C-o" . switch-window)))

;;;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;;;; undo-tree
(req-package undo-tree
  :diminish ""
  :init
  (progn
    (setq undo-tree-visualizer-relative-timestamps t
          undo-tree-visualizer-timestamps t)
    (global-undo-tree-mode)))


;;;; uniqify
(req-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (require 'uniquify)
    ))


;;;; volatile-highlights
(req-package volatile-highlights
  :diminish ""
  :init (volatile-highlights-mode 1))

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


;;;; xml
(req-package nxml-mode
  :init
  (progn
    (push '("<\\?xml" . nxml-mode) magic-mode-alist)
    (setq nxml-child-indent 4
          nxml-attribute-indent 4
          nxml-auto-insert-xml-declaration-flag nil
          nxml-bind-meta-tab-to-complete-flag t
          nxml-slash-auto-complete-flag t)))

(req-package yaml-mode)

;; (req-package leuven-theme
;;   :init
;;   (progn
;;     (load-theme 'leuven)
;;     (set-background-color "WhiteSmoke")))

;; (req-package color-theme-sanityinc-tomorrow)
;; (req-package leuven-theme)
;; (req-package zenburn-theme)
(req-package solarized-theme
  :init 
  (setq solarized-distinct-fringe-background t ; make the fringe stand out from the background
        solarized-high-contrast-mode-line t    ; make the modeline high contrast
        ))

;; (req-package powerline
;;   :init
;;   (progn
;;     (setq powerline-default-separator nil)
;;     (powerline-center-evil-theme)))

;;;; generic keybindings
(bind-key "C-x \\" 'align-regexp)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-h A" 'apropos)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-x C-b" 'ibuffer)
(windmove-default-keybindings)
(winner-mode 1)

;; Activate occur easily inside isearch
(bind-key "C-o"
          (lambda () (interactive)
            (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string)))))
  isearch-mode-map)


;;;; email
(setq user-full-name "Eric Seidel"
      user-mail-address "gridaphobe@gmail.com")

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq mu4e-maildir "~/.mail/gmail"
      mu4e-drafts-folder "/drafts"
      mu4e-refile-folder "/archive"
      mu4e-sent-folder "/sent"
      mu4e-trash-folder "/trash"
      mu4e-attachment-dir "~/Downloads"
      mu4e-user-mail-address-list '("gridaphobe@gmail.com"
                                    "eric@eseidel.org"
                                    "eric9@mac.com"
                                    "eric9@me.com"
                                    "eric9@icloud.com"
                                    "eseidel@cs.ucsd.edu"
                                    "eseidel@ucsd.edu"
                                    "eseidel@eng.ucsd.edu"
                                    "eseidel01@ccny.cuny.edu"
                                    "eric@fluidinfo.com"
                                    "seidel@apple.com")
      mu4e-bookmarks '(("flag:flagged AND NOT (maildir:/spam OR maildir:/trash)"
                        "Starred Messages"
                        ?s)
                       ("flag:unread AND NOT (maildir:/spam OR maildir:/trash)"
                        "Unread Messages"
                        ?u)
                       ("to:*.ucsd.edu AND NOT (maildir:/spam OR maildir:/trash)"
                        "UCSD"
                        ?w))
      mu4e-sent-messages-behavior 'delete
      mu4e-auto-retrieve-keys t
      mu4e-headers-actions '(("capture message" . mu4e-action-capture-message)
                             ("tag message" . mu4e-action-retag-message))
      mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                          ("view as pdf" . mu4e-action-view-as-pdf)
                          ("tag message" . mu4e-action-retag-message))
      mu4e-completing-read-function 'completing-read
      mu4e-change-filenames-when-moving t
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-signature-auto-include nil
      mu4e-headers-skip-duplicates t
      mu4e-headers-include-related nil
      mu4e-headers-results-limit 100
      mu4e-hide-index-messages nil
      mu4e-use-fancy-chars t
      mu4e-debug nil
      mu4e-get-mail-command "mbsync gmail"
      mu4e-update-interval nil ;(* 5 60)
      )

(setq mu4e-html2text-command
      #'(lambda () 
          (shr-render-region (point-min) (point-max))))

(add-hook 'mu4e-compose-pre-hook
  (defun my/set-from-address ()
    (let ((msg mu4e-compose-parent-message))
      (when msg
        (setq user-mail-address
              (cond
               ((mu4e-message-contact-field-matches msg :to "ucsd.edu")
                "eseidel@cs.ucsd.edu")
               (t "gridaphobe@gmail.com")))))))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(defun my/terminal-notifier (title subtitle message)
  (call-process "terminal-notifier" nil nil nil
                "-sender" "org.gnu.Emacs"
                "-title" title
                "-subtitle" subtitle
                "-message" message))
;;(my/terminal-notifier "Hello" "from Emacs" "Hello World")

(defvar my/mu4e-tmp-erase-func nil)
(defvar my/mu4e-tmp-found-func nil)
(defvar my/mu4e-tmp-header-func nil)
(defvar my/msgids-to-move nil)
(add-hook 'mu4e-index-updated-hook
          (defun my/notify-new-mail ()
            (setq my/msgids-to-move nil
                  my/mu4e-tmp-erase-func mu4e-erase-func
                  my/mu4e-tmp-found-func mu4e-found-func
                  my/mu4e-tmp-header-func mu4e-header-func
                  mu4e-erase-func (lambda () nil)
                  mu4e-found-func (lambda (n)
                                    (setq mu4e-erase-func my/mu4e-tmp-erase-func
                                          mu4e-found-func my/mu4e-tmp-found-func
                                          mu4e-header-func my/mu4e-tmp-header-func)
                                    (dolist (msgid my/msgids-to-move)
                                      (mu4e~proc-move msgid nil "-N"))
                                    (setq my/msgids-to-move nil))
                  mu4e-header-func (lambda (msg) 
                                     (my/terminal-notifier
                                      "New Mail"
                                      (caar (mu4e-message-field msg :from))
                                      (mu4e-message-field msg :subject))
                                     (add-to-list 'my/msgids-to-move (mu4e-message-field msg :message-id))))
            (mu4e~proc-find "tag:\\\\Inbox and flag:new" nil :date 'descending nil t nil)))

(req-package mu4e-maildirs-extension
  :require (mu4e)
  :init (mu4e-maildirs-extension))

;;;; irc
(req-package weechat
  :init
  (progn 
    (setq weechat-color-list '(unspecified "black" "dim gray" "dark red" "red"
                                           "dark green" "green" "brown"
                                           "orange" "dark blue" "blue"
                                           "dark magenta" "magenta" "dark cyan"
                                           "royal blue" "dark gray" "gray")
          weechat-host-default "seidel.io"
          weechat-port-default 40900
          weechat-mode-default nil
          )
    (add-to-list 'weechat-notification-handler-functions
                 (defun my/weechat-notify (type sender text date buffer-ptr)
                   (when (eq type :highlight)
                     (my/terminal-notifier "IRC Mention"
                                           sender
                                           text))))))

;;;; mac stuff
(when (and on-mac window-system)
  ;; Emacs users obviously have little need for Command and Option keys,
  ;; but they do need Meta and Super
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  (bind-key "<s-return>" 'toggle-frame-fullscreen)

  (set-fontset-font "fontset-default"
                    'unicode
                    '("Source Code Pro" . "iso10646-1"))
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 140)

  (req-package exec-path-from-shell
    :init 
    (progn
      (setq exec-path-from-shell-variables
            (append '("NIX_GHC" "NIX_GHCPKG" "NIX_GHC_DOCDIR" "NIX_GHC_LIBDIR")
                    exec-path-from-shell-variables))
      (exec-path-from-shell-initialize)))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser))

;;; IMPORTANT
(req-package-finish)


;;;; pretty symbols
(when (fboundp 'global-prettify-symbols-mode)
  (setq-default prettify-symbols-alist
                '(("<-" . ?\u2190)
                  ("->" . ?\u2192)))
  (global-prettify-symbols-mode +1))


;;;; mode line settings
;; FIXME: something in this file is toggling `line-number-mode' so I have to put
;; this at the end......
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(setq display-time-format "%a %m-%d %R")
(display-time-mode 1)
;; (global-hl-line-mode 1)

(xterm-mouse-mode 1)

;;;; theme
(defadvice load-theme (around disable-other-themes activate)
  (mapc #'disable-theme custom-enabled-themes)
  ad-do-it)
(load-theme 'solarized-light)

;; FIXME: why is this being set to nil?!
(setq mu4e-mu-binary (executable-find "mu"))

(message "Emacs is ready to do thy bidding, Master %s!" (getenv "USER"))

(provide 'init)
;;; init.el ends here
