;;; init.el --- Emacs' configuration entry point.

;;; Commentary:
;;; Emacs' configuration entry point.

;;; Code:

(defmacro csetq-1 (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(defmacro csetq (&rest binds)
  (pcase binds
    (`(,var ,val)
     `(csetq-1 ,var ,val))
    (`(,var ,val . ,rest)
     `(progn
        (csetq-1 ,var ,val)
        (csetq ,@rest)))))


(defconst on-mac (eq system-type 'darwin)
  "Are we on a Mac?")
(defconst is-netmacs (string= invocation-name "Netmacs")
  "Are we just running mail/irc?")
(defconst ns-bundle-id (concat "org.gnu." invocation-name))

(require 'server)
(unless (server-running-p)
  (server-start))

;; config changes made through the customize UI will be store here
(csetq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off mouse interface early in startup to avoid momentary display
(when (and (fboundp 'menu-bar-mode) (not (and on-mac window-system))) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please... jeez
(csetq inhibit-startup-screen t)

(require 'package)
(if nil ;(or (file-exists-p "~/.nix-profile/share/emacs/site-lisp")
                                        ;    (file-exists-p "~/.nix-profile/share/emacs-with-packages/site-lisp"))
    (progn
      (csetq package-archives nil)
      (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/")
      (add-to-list 'package-directory-list "~/.nix-profile/share/emacs/site-lisp/elpa")
      (add-to-list 'load-path "~/.nix-profile/share/emacs-with-packages/site-lisp/")
      (add-to-list 'package-directory-list "~/.nix-profile/share/emacs-with-packages/site-lisp/elpa")
      (load-file "~/.nix-profile/share/emacs-with-packages/site-lisp/site-start.el"))
  (progn
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
    ;; (add-to-list 'package-archives
    ;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
    (csetq package-archive-priorities
          '(("gnu" . 2)
            ;; ("melpa-stable" . 1)
            ("melpa" . 0)))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'bind-key)

;;;; mac stuff
(when (and on-mac window-system)
  ;; extra config for emacs-mac
  (when (eq window-system 'mac)
    ;; Emacs users obviously have little need for Command and Option keys,
    ;; but they do need Meta and Super
    (csetq mac-pass-command-to-system nil
           mac-pass-control-to-system nil)
    (csetq mac-command-modifier 'super)
    (csetq mac-option-modifier 'meta)
    (csetq mac-mouse-wheel-smooth-scroll t)
    ;; (mac-auto-operator-composition-mode)
    (csetq mac-frame-tabbing nil)
    (defun my/reset-frame ()
      (make-frame)
      (select-frame (get-other-frame))
      (delete-frame (get-other-frame)))
    (add-hook 'after-init-hook 'my/reset-frame))
  (bind-key "<s-return>" 'toggle-frame-fullscreen)
  (bind-key "s-`" 'other-frame)
  (bind-key "s-c" 'kill-ring-save)
  (bind-key "s-n" 'make-frame)
  (bind-key "s-w" 'delete-frame)
  (bind-key "s-v" 'yank)
  (bind-key "s-s" 'save-buffer)
  (bind-key "s-q" 'save-buffers-kill-emacs)

  (use-package exec-path-from-shell
    :ensure t
    :config
    (csetq exec-path-from-shell-variables
          '("PATH" "MANPATH" "DYLD_LIBRARY_PATH" "NIX_PATH"
            "NIX_GHC" "NIX_GHCPKG" "NIX_GHC_DOCDIR" "NIX_GHC_LIBDIR"
            "https_proxy" "http_proxy" "no_proxy"))
    (exec-path-from-shell-initialize))

  (csetq browse-url-browser-function 'browse-url-default-macosx-browser))

(let ((font
       ;;"Input Mono Compressed"
       "Monoid"
       ))
  (set-fontset-font "fontset-default"
                    'unicode
                    `(,font . "iso10646-1"))
  (set-face-attribute 'default nil
                      :family font
                      :height 140
                      ;; :weight 'light
                      ))

(defun pre-process-kill-ring-element (element)
  (replace-regexp-in-string "^[[:space:]]+" ""
                            (replace-regexp-in-string "[[:space:]]+$" "" (substring-no-properties element))))
(defun preprocess-kill-ring ()
  (let ((result nil)
        (element nil))
    (dolist (element kill-ring)
      (progn
        (csetq element (pre-process-kill-ring-element element))
        (when (not (or
                    (eq 0 (length element))
                    (string-match-p "[\r\n]+" element)))
          (csetq result (cons element result)))))
    (reverse result)))
(defun browse-kill-ring ()
  (interactive)
  (let ((str (completing-read "Pick an element: "
                              (preprocess-kill-ring))))
    (insert str)))
(bind-key "C-M-y" 'browse-kill-ring)

;;;; theme
;; (defadvice load-theme (around disable-other-themes activate)
;;   (mapc #'disable-theme custom-enabled-themes)
;;   ad-do-it)

(use-package solarized-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'solarized-light))

(use-package leuven-theme
  :disabled t
  :ensure t
  :config
  (load-theme 'leuven)
  (custom-theme-set-faces
   'leuven
   '(default ((default :background "WhiteSmoke")) t)))

;;;; smart-mode-line
(use-package smart-mode-line
  :disabled t
  :ensure t
  :config
  (progn
    (csetq sml/name-width '(20 . 30))
    (sml/setup)
    (sml/apply-theme 'light)))

(use-package powerline
  :disabled t
  :ensure t
  :config
  (powerline-center-theme))

(use-package nlinum
  :if (< emacs-major-version 26)
  :ensure t
  :config
  (csetq nlinum-highlight-current-line t)
  (add-hook 'prog-mode-hook #'nlinum-mode))
(when (>= emacs-major-version 26)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq display-line-numbers t))))

(use-package doom-themes
  :ensure t
  :diminish 'doom-buffer-mode
  :config
  (csetq doom-one-brighter-comments nil
        doom-one-brighter-modeline nil
        doom-enable-bold t
        doom-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; ;; DEPRECATED
  ;; (doom-themes-nlinum-config)
  ;; ;; brighter source buffers (that represent files)
  ;; (add-hook 'find-file-hook #'doom-buffer-mode-maybe)
  ;; ;; ...if you use auto-revert-mode
  ;; (add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
  ;; ;; And you can brighten other buffers (unconditionally) with:
  ;; (add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)
  ;; ;; brighter minibuffer when active
  ;; (add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)
  )

(use-package solaire-mode
  :disabled t
  :ensure t
  :after doom-themes
  :config
  ;; brighten buffers (that represent real files)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

  ;; ...if you use auto-revert-mode:
  (add-hook 'after-revert-hook #'turn-on-solaire-mode)

  ;; highlight the minibuffer when it is activated:
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  ;; if the bright and dark background colors are the wrong way around, use this
  ;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
  ;; This should be used *after* you load the active theme!
  ;;
  ;; NOTE: This is necessary for themes in the doom-themes package!
  (solaire-mode-swap-bg))

(use-package all-the-icons
  :ensure t)

(load "~/.emacs.d/doom-modeline.el")

;;;; misc
(blink-cursor-mode -1)

(csetq tags-case-fold-search nil)
(use-package dired-x
  :disabled t
  :ensure dired+
  :config
  (progn
    (csetq dired-omit-files-p t)
    (csetq dired-omit-files
          (concat "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$\\|"
                  dired-omit-files))))

(csetq ring-bell-function 'ignore)
(csetq enable-recursive-minibuffers t)

(electric-indent-mode -1)
(electric-layout-mode -1)
(electric-pair-mode -1)
(show-paren-mode -1)

(csetq major-mode 'text-mode)

(setq-default bidi-display-reordering nil)

;; When popping the mark, continue popping until the cursor
;; actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(csetq set-mark-command-repeat-pop t)

(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;; nice scrolling
(csetq scroll-margin 0
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

(csetq require-final-newline t)

;; (csetq debug-on-error t)
                                        ; see https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; (csetq gc-cons-threshold (* 20 (expt 2 20))) ; gc after 20MB

(csetq fill-column 72)

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
(csetq indent-tabs-mode nil)   ;; don't use tabs to indent
(csetq tab-width 8)            ;; but maintain correct appearance

;; delete the selection with a keypress
(delete-selection-mode 1)

;; store all backup and autosave files in the tmp dir
(csetq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(csetq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(csetq auto-save-list-file-prefix
      temporary-file-directory)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode 1)
;; (csetq auto-revert-verbose t)

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
(csetq hippie-expand-try-functions-list
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
(csetq tab-always-indent 'complete)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; kill old buffers
(use-package midnight
  :config
  (progn
    (csetq midnight-mode t)
    (add-to-list 'clean-buffer-list-kill-never-regexps
                 "^#\w+")
    (add-to-list 'clean-buffer-list-kill-never-buffer-names
                 "192.241.212.224:5000")
    (add-to-list 'clean-buffer-list-kill-never-buffer-names
                 "seidel.io:5000")))

;; saner regex syntax
(use-package re-builder
  :config
  (csetq reb-re-syntax 'string))

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
(use-package saveplace
  :config
  (csetq save-place t))

;; ediff defaults
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")


;;;; ProofGeneral
;; (load "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")


;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (csetq inferior-lisp-program "sbcl")


;;;; Rectangle-aware commands
(eval-when-compile (require 'rect))
(defun my/kill-region (beg end &optional region)
  "Kill text between BEG and END.

Use `kill-rectangle' if `rectangle-mark-mode' is set."
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (kill-rectangle beg end)
    (kill-region beg end region)))
(bind-key "C-w" 'my/kill-region)

(defun my/copy-region (beg end &optional region)
  "Copy text between BEG and END.

Use `copy-rectangle-as-kill' if `rectangle-mark-mode' is set."
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (copy-rectangle-as-kill beg end)
    (kill-ring-save beg end region)))
(bind-key "M-w" 'my/copy-region)

;;;; subword-mode
(global-subword-mode 1)
(diminish 'subword-mode)

;;;; smartparens
(use-package smartparens-config
  :ensure smartparens
  :diminish 'smartparens-mode
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (bind-key "C-k"   'sp-kill-hybrid-sexp)
  (bind-key "C-M-f" 'sp-forward-sexp)
  (bind-key "C-M-b" 'sp-backward-sexp)
  (bind-key "C-M-k" 'sp-kill-sexp)
  (bind-key "C-M-p" 'sp-forward-slurp-sexp)
  (bind-key "C-M-o" 'sp-forward-barf-sexp))

;; (csetq sp-autoskip-closing-pair 'always)
;; (csetq sp-hybrid-kill-entire-symbol nil)
;; (electric-pair-mode)

;; (show-smartparens-global-mode 1)
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
;; (add-hook 'prog-mode-hook 'turn-on-show-smartparens-mode)
;; (smartparens-global-strict-mode 1)
;; (add-to-list 'rm-blacklist " SP/s")

;;;; rainbow-mode
;; (use-package rainbow-mode
;;   :diminish "")

;;;; ace-jump-mode
(use-package ace-jump-mode
  :disabled t
  :bind ("M-j" . ace-jump-char-mode))

;;;; anzu
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (csetq anzu-cons-mode-line-p nil))

;;;; helm

;; (require 'smex)
;; (smex-initialize)
;; (bind-key "M-x" 'smex)
;; (bind-key "M-X" 'smex-major-mode-commands)

(use-package ivy
  :ensure t
  :pin "gnu"
  :diminish 'ivy-mode
  :demand t
  :bind
  (("C-c C-r" . ivy-resume))
  :config
  (csetq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (use-package swiper
    :demand t
    :bind
    (("C-s" . counsel-grep-or-swiper)
     ("C-r" . counsel-grep-or-swiper)))
  (use-package counsel
    :demand t
    :config
    (csetq counsel-find-file-ignore-regexp "\(?:\`[#.]\)\|\(?:[#~]\'\)"
           counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    (counsel-mode 1)
    (use-package smex
      :ensure t
      :config (smex-initialize))
    :bind
    (("C-x C-f" . counsel-find-file)
     ("M-x" . counsel-M-x)
     ("C-c i" . counsel-imenu))))

;; (require 'helm-config)
;; (require 'helm)
;; (require 'helm-files)
;; (require 'ido)
;; (csetq helm-buffers-fuzzy-matching t
;;       ido-use-virtual-buffers t
;;       helm-ff-auto-update-initial-value t
;;       helm-ff-file-name-history-use-recentf t
;;       helm-ff-skip-boring-files t
;;       helm-quick-update t                   ; do not display invisible candidates
;;       helm-split-window-default-side 'below
;;       helm-split-window-in-side-p t
;;       ;; helm-always-two-windows nil ; t
;;       helm-autoresize-max-height 30
;;       helm-autoresize-min-height 30
;;       )
;; (helm-mode 1)
;; (diminish 'helm-mode)
;; ;; (add-to-list 'rm-blacklist " Helm")
;; (helm-adaptive-mode 1)
;; (helm-autoresize-mode 1)

;; (bind-key "C-x b" 'helm-mini)
;; (bind-key "C-c i" 'helm-semantic-or-imenu)
;; (bind-key "<tab>" 'helm-execute-persistent-action helm-map) ; rebind tab to do persistent action
;; (bind-key "C-i"   'helm-execute-persistent-action helm-map) ; make TAB work in terminal
;; (bind-key "C-z"   'helm-select-action             helm-map) ; list actions using C-z

;; (bind-key "<return>" 'helm-grep-mode-jump-other-window          helm-grep-mode-map)
;; (bind-key "n"        'helm-grep-mode-jump-other-window-forward  helm-grep-mode-map)
;; (bind-key "p"        'helm-grep-mode-jump-other-window-backward helm-grep-mode-map)

;; (use-package helm-swoop
;;   :config (csetq helm-multi-swoop-edit-save t))
;; (bind-key "M-i" 'helm-swoop-from-isearch isearch-mode-map)
;; (bind-key "M-i" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)

;;;; ace-isearch
;; (use-package ace-isearch
;;   :diminish ""
;;   :config (progn (global-ace-isearch-mode +1)
;;                (csetq ace-isearch-input-idle-delay 0.2)))

;;;; projectile
;; (require 'projectile)
;; (require 'perspective)
;; (persp-mode)
;; (require 'persp-projectile)
(use-package projectile
  :ensure t
  :diminish 'projectile-mode
  :config
  (csetq projectile-completion-system 'ivy
        projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".liquid")
	projectile-switch-project-action 'projectile-vc
        projectile-globally-ignored-file-suffixes '(".o" ".hi"))
  (projectile-global-mode)
  ;; (use-package perspective
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (persp-mode)
  ;;     (use-package persp-projectile :ensure t)))
  )

;; (projectile-global-mode)
;; (diminish 'projectile-mode)
;; (add-to-list 'rm-blacklist " Projectile\\*")

;; (require 'helm-projectile)
;; (helm-projectile-on)
;; (csetq projectile-switch-project-action 'helm-projectile)


;;;; auto complete
;; (require 'auto-complete)
;; (ac-config-default)
;; (csetq ac-auto-start nil)
;; (diminish 'auto-complete-mode)
;; (ac-set-trigger-key "TAB")
(use-package company
  :ensure t
  :diminish 'company-mode
  :config
  (csetq
   company-idle-delay nil
   company-dabbrev-downcase nil
   company-dabbrev-ignore-case t
   company-dabbrev-code-ignore-case t)
  (add-hook #'prog-mode-hook #'company-mode))


;;;; c/c++
(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook   #'irony-mode)
  (add-hook 'c-mode-hook     #'irony-mode)
  (add-hook 'objc-mode-hook  #'irony-mode)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options))
(use-package company-irony
  :ensure t
  :after (irony company)
  :config
  (add-to-list 'company-backends #'company-irony))
(use-package flycheck-irony
  :ensure t
  :after (irony flycheck)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(use-package irony-eldoc
  :ensure t
  :after irony
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc))

;;;; compile
(use-package compile
  :config
  (csetq compilation-scroll-output 'first-error
        compilation-window-height 10
        compilation-context-lines 0)
  (defun bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation BUFFER if STRING has no warnings."
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
  (add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful))


;;;; css-mode
(add-hook 'css-mode-hook 'rainbow-mode)


;;;; dash-at-point
(use-package dash-at-point
  :ensure t)


;;;; discover-my-major
;; (use-package discover-my-major
;;   :config (bind-key "M-m" 'discover-my-major help-map))


;;;; docker
(use-package dockerfile-mode
  :ensure t)

;;;; edit-server
;; (require-package 'edit-server)


;;;; emacs lisp
(defun imenu-elisp-sections ()
  (csetq imenu-prev-index-position-function nil)
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


(add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))
;; (add-to-list 'rm-blacklist " ElDoc")

(bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
(bind-key "TAB" 'completion-at-point read-expression-map)


;;;; expand-region
(use-package expand-region
  :ensure t
  :bind (("M-m" . er/expand-region)
         ("M-S-m" . er/contract-region)))

;;;; change-inner
(use-package change-inner
  :disabled t
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;;;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish 'undo-tree-mode
  :config
  (csetq undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))
;; (add-to-list 'rm-blacklist " Undo-Tree")


;;;; evil
(use-package evil
  :disabled t
  :ensure t
  :init
  (csetq evil-esc-delay 0
        evil-cross-lines t
        evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-want-C-u-scroll t
        )
  :config
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map
    [escape] 'evil-normal-state)
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
  ;; (bind-key "C-d" 'evil-delete-char    evil-normal-state-map)
  ;; (bind-key "C-d" 'evil-delete-char    evil-insert-state-map)
  ;; (bind-key "C-d" 'evil-delete-char    evil-visual-state-map)
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
  )
(use-package evil-god-state
  :disabled t
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (evil-define-key 'visual global-map "," 'evil-execute-in-god-state))
(use-package evil-surround
  :disabled t
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode +1))

;; (require 'evil)
;; (evil-mode 1)
;; ;; prevent esc-key from translating to meta-key in terminal mode
;; (csetq evil-esc-delay 0)
;; (csetq evil-search-module 'evil-search
;;       evil-cross-lines t
;;       evil-move-cursor-back nil)
;; (add-to-list 'evil-emacs-state-modes 'special-mode)
;; ;; (evil-ex-define-cmd "e[dit]" 'helm-find-files)
;; ;; (evil-ex-define-cmd "b[uffer]" 'helm-buffers-list)
;; (bind-key "[escape]" 'keyboard-escape-quit evil-normal-state-map)
;; (bind-key "[escape]" 'keyboard-escape-quit evil-visual-state-map)
;; (bind-key "<escape>" 'keyboard-escape-quit)
;; (bind-key "\"" 'ace-jump-mode evil-normal-state-map)

;; (setcdr evil-insert-state-map nil)
;; (define-key evil-insert-state-map
;;   (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
;; (define-key evil-insert-state-map
;;   [escape] 'evil-normal-state)


;; (use-package evil-surround
;;   :config (global-evil-surround-mode 1))

;;(defadvice switch-to-buffer (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;
;;(defadvice select-window (around evil-back-to-initial-state activate)
;;  (when (ad-get-arg 1)
;;    (evil-change-state
;;     (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;  ad-do-it)
;;(defadvice other-window (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;(defadvice other-frame (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;(defadvice windmove-up (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;(defadvice windmove-down (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;(defadvice windmove-left (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))
;;(defadvice windmove-right (before evil-back-to-initial-state activate)
;;  (evil-change-state
;;   (evil-initial-state-for-buffer (current-buffer) evil-default-state)))




;;;; flycheck
(use-package flycheck
  :ensure t
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :config
  (csetq flycheck-disabled-checkers '(emacs-lisp-checkdoc json-jsonlint json-python-json haskell-hlint))
  (csetq flycheck-check-syntax-automatically '(mode-enabled save))
  (csetq flycheck-mode-line-prefix "✓")
  (global-flycheck-mode 1)
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (flycheck-pos-tip-mode)
    (csetq flycheck-pos-tip-timeout -1)))


;;;; flyspell
(use-package flyspell
  :disabled t
  :diminish 'flyspell-mode
  :config
  (csetq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra")
        flyspell-issue-message-flag nil ; issuing a message for each word is slow
        )
  (progn
    (add-hook 'message-mode-hook #'flyspell-mode)
    (add-hook 'text-mode-hook #'flyspell-mode)))


;;;; god-mode
(use-package god-mode
  :disabled t
  :ensure t
  :config
  (god-mode)
  (csetq cursor-type 'bar)
  (defun my/god-toggle-cursor-type ()
    (csetq cursor-type (if god-local-mode
                          'box
                        'bar)))
  (add-hook 'god-local-mode-hook #'my/god-toggle-cursor-type)
  (bind-key "<escape>" 'god-local-mode)
  (bind-key "C-z" 'god-local-mode)
  (bind-key "i" 'god-local-mode god-local-mode-map)
  (bind-key "." 'repeat god-local-mode-map)
  (add-to-list 'god-exempt-major-modes 'custom-mode)
  )

;; (defun god-toggle-on-overwrite ()
;;  "Toggle god-mode on command `overwrite-mode'."
;;  (if (bound-and-true-p overwrite-mode)
;;      (god-local-mode-pause)
;;    (god-local-mode-resume)))
;; (add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)

;;(defun my-god-mode-indicator ()
;;  "Display a custom indicator for `god-mode' in the mode-line."
;;  (when god-local-mode (propertize " <G> " 'font-lock-face '(:background "#FF9999" :weight bold))))
;;(add-to-list 'mode-line-position
;;             '((:eval (my-god-mode-indicator))))
;;(diminish 'god-local-mode)

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)


;; (require 'evil-god-state)
;; ;; (load "~/Source/evil-god-state/evil-god-state.el")
;; (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
;; (evil-define-key 'visual global-map "," 'evil-execute-in-god-state)
;; (add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
;; (add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))

;;;; haskell
(use-package haskell-mode
  :ensure t
  :config
  ;; (require 'haskell-interactive-mode)
  ;; (require 'haskell-process)
  ;; (sp-local-pair '(haskell-mode literate-haskell-mode)
  ;;                "{- " " -}"
  ;;                :trigger "-{")
  ;; (sp-local-pair '(haskell-mode literate-haskell-mode)
  ;;                "{-@ " " @-}"
  ;;                :trigger "@{")
  ;; (sp-local-pair '(haskell-mode literate-haskell-mode)
  ;;                "{-# " " #-}"
  ;;                :trigger "#{")
  ;; (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
  ;; (bind-key "C-c C-t" 'haskell-mode-show-type-at haskell-mode-map)
  ;; (bind-key "C-c C-i" 'haskell-process-do-info   haskell-mode-map)
  ;; (bind-key "C-c C-." 'haskell-mode-goto-loc haskell-mode-map)
  ;; (bind-key "C-c C-?" 'haskell-mode-find-uses haskell-mode-map)
  ;; (bind-key "M-n" 'next-error haskell-mode-map)
  ;; (bind-key "M-p" 'previous-error haskell-mode-map)

  ;; (add-to-list 'evil-emacs-state-modes 'haskell-presentation-mode)

  (csetq haskell-process-type 'auto
        haskell-process-path-ghci "ghci"
        haskell-process-args-ghci '("-ferror-spans" "-idist/build:dist/build/autogen")
        haskell-process-args-cabal-repl '(;"--with-ghc=ghci-ng"
                                          "--ghc-option=-ferror-spans"
                                          )
        haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans")
        haskell-process-log t
        haskell-align-imports-pad-after-name t
        haskell-ask-also-kill-buffers nil
        haskell-completing-read-function 'completing-read
        haskell-interactive-popup-errors nil
        haskell-interactive-types-for-show-ambiguous t
        haskell-process-auto-import-loaded-modules t
        ;;haskell-process-reload-with-fbytecode t
        haskell-process-suggest-add-package t
        haskell-process-suggest-hoogle-imports t
        haskell-process-suggest-language-pragmas t
        haskell-process-suggest-remove-import-lines t
        haskell-process-suggest-overloaded-strings t
        haskell-process-suggest-restart t
        haskell-process-use-presentation-mode nil
        haskell-stylish-on-save nil
        haskell-tags-on-save nil
        )
  (add-to-list 'completion-ignored-extensions ".hi")
  (add-to-list 'completion-ignored-extensions ".hdevtools.sock")
  (add-to-list 'completion-ignored-extensions ".liquid/")
  (add-to-list 'completion-ignored-extensions ".hpc/")

  ;; (add-hook 'haskell-mode-hook 'eldoc-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)

  (csetq flycheck-ghc-args '("-package" "ghc"))

  ;; (bind-key "M-n" 'next-error     haskell-interactive-mode-map)
  ;; (bind-key "M-p" 'previous-error haskell-interactive-mode-map)

  (use-package hindent
    :ensure t
    :diminish 'hindent-mode
    :config
    (csetq hindent-process-path "~/.local/bin/hindent")
    (add-hook 'haskell-mode-hook #'hindent-mode)
    )

  (use-package intero
    ;; :disabled t
    :ensure t
    :diminish 'intero-mode
    :pin melpa
    :config
    (add-hook 'haskell-mode-hook 'intero-mode)
    ;; (csetq intero-blacklist '("/Users/gridaphobe/Source/ghc"))
    ;; (add-hook 'haskell-mode-hook 'intero-mode-blacklist)
    )
  (use-package dante
    :disabled t
    :ensure t
    ;; :pin melpa
    :load-path "~/Source/dante"
    :commands 'dante-mode
    :init
    (add-hook 'haskell-mode-hook 'dante-mode)
    (add-hook 'haskell-mode-hook 'flycheck-mode)
    :config
    )

  ;;(load "~/.emacs.d/haskell-flycheck.el")
  (add-hook 'haskell-mode-hook
            (lambda ()
              ;; (turn-on-haskell-indentation)
              (diminish 'haskell-indentation-mode)
              (diminish 'interactive-haskell-mode)
              ;; (flycheck-haskell-setup)
              ;; (bind-key "M-n" 'next-error interactive-haskell-mode-map)
              ;; (bind-key "M-p" 'previous-error interactive-haskell-mode-map)
              ;;(flycheck-select-checker 'haskell-process)
              ;;(flycheck-select-checker 'haskell-ghc-modi-check)
              )))



;; (require 'shm)
;; (require 'shm-case-split)
;; (require 'shm-reformat)
;; (add-hook 'haskell-mode-hook 'turn-off-smartparens-mode)
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
;; (add-hook 'haskell-interactive-mode 'turn-off-smartparens-mode)
;; (add-hook 'haskell-interactive-mode 'structured-haskell-repl-mode)
;; (remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (csetq shm-colon-enabled t
;;       shm-indent-point-after-adding-where-clause t
;;       shm-lambda-indent-style 'leftmost-parent
;;       shm-use-hdevtools nil
;;       shm-use-presentation-mode nil
;;       shm-auto-insert-skeletons t
;;       shm-indent-point-after-adding-where-clause t
;;       shm-type-info-fallback-to-ghci t
;;       )
;; (custom-set-faces
;;  '(shm-quarantine-face ((t (:inherit font-lock-error))))
;;  '(shm-current-face ((t (:background "#efefef")))))
;; (bind-key "C-c C-p" 'shm/expand-pattern shm-map)
;; (bind-key "C-c C-s" 'shm/case-split shm-map)

;; (add-to-list 'load-path "~/.nix-profile/share/x86_64-osx-ghc-7.10.2/hindent-4.5.4/elisp")
;; (require 'hindent)
;; (add-hook 'haskell-mode-hook #'hindent-mode)

;;;; javascript
;; (use-package js3-mode)
;; (after "js2-mode-autoloads"
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;   (csetq js2-auto-indent-p t
;;         js2-cleanup-whitespace t
;;         js2-enter-indents-newline t
;;         js2-indent-on-enter-key t
;;         js2-pretty-multiline-declarations t))


;;;; latex
(use-package tex-site
  :ensure auctex
  :config
  (progn
    (csetq TeX-PDF-mode t)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)
    (csetq reftex-plug-into-AUCTeX t
          reftex-default-bibliography '("main.bib")
          TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil))
  )
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (global-magit-file-mode)
  (defalias 'magit 'magit-status)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (csetq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  )
(use-package magit-gh-pulls
  :disabled t
  :after magit
  :pin melpa
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
(use-package ghub
  :ensure t
  :pin melpa)
(use-package magithub
  :disabled t
  :after (magit ghub)
  :ensure t
  :config (magithub-feature-autoinject t))
;; (defadvice magit-status (around magit-fullscreen activate)
;;   (window-configuration-to-register :magit-fullscreen)
;;   ad-do-it
;;   (delete-other-windows))
;; (defadvice magit-mode-quit-window (after magit-restore-screen activate)
;;   (jump-to-register :magit-fullscreen))


;;;; markdown
(use-package markdown-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))))


;;;; nix-mode
(use-package nix-mode
  :disabled t
  )

;;;; ocaml
(use-package tuareg
  :ensure t)

;;;; org-mode
(use-package org
  :bind
  (("C-c a"   . org-agenda)
   ("C-c b"   . org-iswitchb)
   ("C-c c"   . org-capture)
   ("C-c s l" . org-store-link))

  :config
  (use-package org-bullets
    :ensure t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (progn
    (csetq org-directory "~/Work/org")
    (csetq org-agenda-files `(,(concat org-directory "/notes.org")
                                        ; "~/Dropbox/org/galois.org"
                             ))
    (csetq org-default-notes-file (concat org-directory "/notes.org"))

    ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (csetq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))

    ;; Use full outline paths for refile targets - we file directly with IDO
    (csetq org-refile-use-outline-path t)

    ;; Targets complete directly with IDO
    (csetq org-outline-path-complete-in-steps nil)

    ;; Allow refile to create parent tasks with confirmation
    (csetq org-refile-allow-creating-parent-nodes (quote confirm))

    (csetq org-cycle-separator-lines 0)

    (csetq org-blank-before-new-entry (quote ((heading . nil)
                                             (plain-list-item . auto))))

    (csetq orc-src-fontify-natively t)
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (csetq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (csetq org-clock-in-resume t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (csetq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (csetq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (csetq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (csetq org-clock-report-include-clocking-task t)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (csetq org-clock-out-remove-zero-time-clocks t)

    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (csetq org-clock-into-drawer t)

    ;; Don't show SCHEDULED times if the item is DONE
    (csetq org-agenda-skip-scheduled-if-done t)

    (csetq org-log-into-drawer t)
    (csetq org-log-done 'time)
    (csetq org-log-refile 'time)

    ;; Org todo keywords
    (csetq org-todo-keywords
          (quote
           ((sequence "SOMEDAY(s)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

    ;;(add-to-list 'org-export-backends 'md)
    (use-package ox-gfm
      :ensure t)

    (use-package ox-latex
      :config
      (progn
        (csetq org-latex-pdf-process '("latexmk -pdf %f"))
        (add-to-list 'org-latex-classes
                     '("sigplanconf"
                       "\\documentclass{sigplanconf}\n[PACKAGES]\n[EXTRA]"
                       ("\\section{%s}" . "\\section*{%s}")
                       ("\\subsection{%s}" . "\\subsection*{%s}")
                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       ("\\paragraph{%s}" . "\\paragraph*{%s}")
                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
        ;; (require 'ox-bibtex)
        ;; (org-add-link-type
        ;;  "cite" 'ebib
        ;;  (lambda (path desc format)
        ;;    (cond
        ;;     ((eq format 'html)
        ;;      (format "(<cite>%s</cite>)" path))
        ;;     ((eq format 'latex)
        ;;      (if (or (not desc) (equal 0 (search "cite:" desc)))
        ;;          (format "\\cite{%s}" path)
        ;;        (format "\\cite[%s][%s]{%s}"
        ;;                (cadr (split-string desc ";"))
        ;;                (car (split-string desc ";"))  path))))))
        (csetq org-latex-listings nil)
        ;; (add-to-list 'org-latex-packages-alist '("" "listings"))
        ;; (add-to-list 'org-latex-packages-alist '("" "color"))
        ;; (add-to-list 'org-latex-packages-alist '("" "minted"))

        ;; Nicolas Goaziou, http://article.gmane.org/gmane.emacs.orgmode/67692
        (defun org-latex-ignore-heading-filter-headline (headline backend info)
          "Strip headline from HEADLINE. Ignore BACKEND and INFO."
          (when (and (org-export-derived-backend-p backend 'latex)
                     (string-match "\\`.*ignoreheading.*\n" headline))
            (replace-match "" nil nil headline)))
        (add-to-list 'org-export-filter-headline-functions
                     'org-latex-ignore-heading-filter-headline)))))

;;;; prog-mode
;; (defun my/local-comment-auto-fill ()
;;   (auto-fill-mode 1)
;;   (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(XXX\\|FIXME\\|TODO\\|NOTE\\|FIX\\|HACK\\|REFACTOR\\)"
          1 font-lock-warning-face t))))


;; show the name of the current function definition in the modeline
(use-package which-func
  :disabled t
  :config
  (which-function-mode 1))

;; (require-package 'ws-butler)
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode 1)
  (diminish 'ws-butler-mode)
  (diminish 'highlight-changes-mode))

(defun my/prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  ;; (my/local-comment-auto-fill)
  ;; (whitespace-mode 1)
  (my/add-watchwords))

(add-hook 'prog-mode-hook 'my/prog-mode-defaults)


(use-package rust-mode
  :ensure t
  :config
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook #'cargo-minor-mode))
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package racer
    :ensure t
    :config
    (csetq racer-rust-src-path "/usr/local/share/rust/rust_src/")
    (csetq racer-cmd "racer")
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode))
  ;; (use-package rustfmt
  ;;   :ensure t
  ;;   :config
  ;;   (csetq rustfmt-bin "~/.cargo/bin/rustfmt"))
  )


;;;; shells
(use-package eshell
  :bind
  (("C-x m" . projectile-eshell))
  :config
  (progn
    (use-package em-smart
      :config
      (csetq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t
            eshell-cmpl-cycle-completions nil
            eshell-cmpl-ignore-case t
            eshell-buffer-maximum-lines 30000
            eshell-output-filter-functions '(eshell-handle-ansi-color
                                             eshell-handle-control-codes
                                             eshell-watch-for-password-prompt
                                             eshell-truncate-buffer)))

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

    ;; Eshell Hooks
    (defun eshell-settings ()
      (csetq show-trailing-whitespace nil)
      (eshell-smart-initialize))

    (add-hook 'eshell-mode-hook 'eshell-settings)
    (add-hook 'eshell-mode-hook 'exec-path-from-shell-initialize)

    ;; Eshell Commands
    (defun eshell/clear ()
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(use-package shell
  :config
  (csetq shell-file-name "bash"))


;;;; switch-window
(use-package switch-window
  :ensure t
  :bind (("C-x o"   . switch-window)
         ("C-x C-o" . switch-window)))

;;;; tramp
(use-package tramp
  :config
  (csetq tramp-default-method "ssh"))


;;;; uniqify
(use-package uniquify
  :config
  (csetq uniquify-buffer-name-style 'post-forward-angle-brackets))


;;;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :diminish ""
  :config (volatile-highlights-mode 1))

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice my/copy-region (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice my/kill-region (before slick-kill activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Killed line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;;;; yaml
(use-package yaml-mode
  :ensure t)

;; (use-package color-theme-sanityinc-tomorrow)
;; (use-package leuven-theme)
;; (use-package zenburn-theme)
;; (use-package solarized-theme
;;   :config
;;   (csetq solarized-distinct-fringe-background t ; make the fringe stand out from the background
;;         solarized-high-contrast-mode-line t    ; make the modeline high contrast
;;         ))

;; (use-package powerline
;;   :config
;;   (progn
;;     (csetq powerline-default-separator nil)
;;     (powerline-center-evil-theme)))

;;;; generic keybindings
(bind-key "C-x \\" 'align-regexp)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-h A" 'apropos)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-x C-b" 'ibuffer)
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
(csetq user-full-name "Eric Seidel"
      user-mail-address "eric@seidel.io")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; (add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/mu4e")
;; (add-to-list 'Info-directory-list "~/.nix-profile/share/info")
(use-package mu4e
  :disabled t
  :config
  (use-package org-mu4e)
  (use-package mu4e-maildirs-extension
    :ensure t
    :config (mu4e-maildirs-extension))
  (csetq mu4e-maildir "~/.mail"
        mu4e-drafts-folder "/drafts"
        mu4e-refile-folder "/archive"
        mu4e-sent-folder "/sent"
        mu4e-trash-folder "/trash"
        mu4e-attachment-dir "~/Downloads"
        mu4e-user-mail-address-list '("gridaphobe@gmail.com"
                                      "eseidel@galois.com"
                                      "eric@eseidel.org"
                                      "eric@seidel.io"
                                      "eric9@mac.com"
                                      "eric9@me.com"
                                      "eric9@icloud.com"
                                      "eseidel@cs.ucsd.edu"
                                      "eseidel@ucsd.edu"
                                      "eseidel@eng.ucsd.edu"
                                      "eseidel01@ccny.cuny.edu"
                                      "eric@fluidinfo.com"
                                      "seidel@apple.com")
        ;; mu4e-bookmarks '(("flag:flagged AND NOT (maildir:/gmail/spam OR maildir:/gmail/trash)"
        ;;                   "Starred Messages"
        ;;                   ?s)
        ;;                  ("flag:unread AND NOT (maildir:/gmail/spam OR maildir:/gmail/trash)"
        ;;                   "Unread Messages"
        ;;                   ?u)
        ;;                  ("to:*.ucsd.edu AND NOT (maildir:/gmail/spam OR maildir:/gmail/trash)"
        ;;                   "UCSD"
        ;;                   ?w))
        mu4e-sent-messages-behavior 'delete
        mu4e-auto-retrieve-keys t
        mu4e-headers-actions '(("capture message" . mu4e-action-capture-message)
                               ("tag message" . mu4e-action-retag-message))
        mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                            ("view as pdf" . mu4e-action-view-as-pdf)
                            ("tag message" . mu4e-action-retag-message))
        mu4e-completing-read-function 'ivy-completing-read
        mu4e-change-filenames-when-moving t
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-signature-auto-include nil
        mu4e-headers-skip-duplicates t
        mu4e-headers-include-related t
        mu4e-headers-results-limit 100
        mu4e-hide-index-messages nil
        mu4e-use-fancy-chars t
        mu4e-debug nil
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval (* 5 60)
        ))

;; (csetq mu4e-html2text-command
;;      #'(lambda ()
;;          (shr-render-region (point-min) (point-max))))

;; (load "~/.emacs.d/vendor/window-margin.el")
;; (add-hook 'mu4e-view-mode-hook 'turn-on-window-margin-mode)

;; (add-hook 'mu4e-compose-pre-hook
;;  (defun my/set-from-address ()
;;    (let ((msg mu4e-compose-parent-message))
;;      (when msg
;;        (csetq user-mail-address
;;              (cond
;;               ((mu4e-message-contact-field-matches msg :to "ucsd.edu")
;;                "eseidel@cs.ucsd.edu")
;;               ((mu4e-message-contact-field-matches msg :to "galois.com")
;;                "eseidel@galois.com")
;;               (t "gridaphobe@gmail.com")))))))

;; (csetq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)
;; ;; don't keep message buffers around
;; (csetq message-kill-buffer-on-exit t)

(defun my/terminal-notifier (title subtitle message)
 (call-process "terminal-notifier" nil nil nil
               "-sender" "org.gnu.Emacs"
               "-title" title
               "-subtitle" subtitle
               "-message" message))
;; (my/terminal-notifier "Hello" "from Emacs" "Hello World")

;; (defvar my/mu4e-tmp-erase-func nil)
;; (defvar my/mu4e-tmp-found-func nil)
;; (defvar my/mu4e-tmp-header-func nil)
;; (defvar my/msgids-to-move nil)
;; (add-hook 'mu4e-index-updated-hook
;;          (defun my/notify-new-mail ()
;;            (csetq my/msgids-to-move nil
;;                  my/mu4e-tmp-erase-func mu4e-erase-func
;;                  my/mu4e-tmp-found-func mu4e-found-func
;;                  my/mu4e-tmp-header-func mu4e-header-func
;;                  mu4e-erase-func (lambda () nil)
;;                  mu4e-found-func (lambda (n)
;;                                    (csetq mu4e-erase-func my/mu4e-tmp-erase-func
;;                                          mu4e-found-func my/mu4e-tmp-found-func
;;                                          mu4e-header-func my/mu4e-tmp-header-func)
;;                                    (dolist (msgid my/msgids-to-move)
;;                                      (mu4e~proc-move msgid nil "-N"))
;;                                    (csetq my/msgids-to-move nil))
;;                  mu4e-header-func (lambda (msg)
;;                                     (my/terminal-notifier
;;                                      "New Mail"
;;                                      (caar (mu4e-message-field msg :from))
;;                                      (mu4e-message-field msg :subject))
;;                                     (add-to-list 'my/msgids-to-move (mu4e-message-field msg :message-id))))
;;            (mu4e~proc-find "(maildir:/gmail/inbox or maildir:/galois/inbox) and flag:new" nil :date 'descending nil t nil)))

;; (require 'mu4e-maildirs-extension)
;; (mu4e-maildirs-extension)

(use-package gnus
  :disabled t
  :config
  ;; accounts
  (csetq gnus-select-method '(nntp "news.gmane.org"))
  (csetq gnus-secondary-select-methods
        '((nnimap "seidel.io"
                  (nnimap-address "mail.messagingengine.com")
                  ;;(nnimap-server-port 8143)
                  (nnimap-user "eric@seidel.io")
                  ;;(nnimap-authenticator login)
                  (nnimap-stream ssl))))

  ;; display
  (csetq gnus-sum-thread-tree-indent " ")
  (csetq gnus-sum-thread-tree-root "")
  (csetq gnus-sum-thread-tree-false-root "")
  (csetq gnus-sum-thread-tree-single-indent "")
  (csetq gnus-sum-thread-tree-vertical        "│")
  (csetq gnus-sum-thread-tree-leaf-with-other "├> ")
  (csetq gnus-sum-thread-tree-single-leaf     "└> ")
  (csetq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}"
         "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
         "  "
         "%4{%-20,20f%}"               ;; name
         "  "
         "%3{│%}"
         " "
         "%{%B%}"
         "%s\n"))
  (csetq gnus-summary-display-arrow t))

;; (require 'gnus)
;; (csetq gnus-select-method '(nnimap "local"
;;                                   (nnimap-address "localhost")
;;                                   (nnimap-server-port 8143)
;;                                   (nnimap-user "eric@seidel.io")
;;                                   (nnimap-authenticator login)
;;                                   (nnimap-stream network))
;;       ;;gnus-select-method '(nntp "news.gmane.org")
;;       ;; gnus-select-method '(nnimap "seidel"
;;       ;;                             (nnimap-address "mail.messagingengine.com")
;;       ;;                             ;;(nnimap-server-port 8143)
;;       ;;                             (nnimap-user "eric@seidel.io")
;;       ;;                             ;;(nnimap-authenticator login)
;;       ;;                             (nnimap-stream ssl))
;;       ;gnus-secondary-select-methods '((nnimap "seidel"
;;       ;                                        (nnimap-address "localhost")
;;       ;                                        (nnimap-server-port 8143)
;;       ;                                        (nnimap-user "eric@seidel.io")
;;       ;                                        (nnimap-authenticator login)
;;       ;                                        (nnimap-stream network)
;;       ;                                        )
;;       ;                                (nnimap "galois"
;;       ;                                        (nnimap-address "localhost")
;;       ;                                        (nnimap-server-port 8143)
;;       ;                                        (nnimap-user "eseidel@galois.com")
;;       ;                                        (nnimap-authenticator login)
;;       ;                                        (nnimap-stream network)
;;       ;                                        )
;;       ;                                )
;;       gnus-asynchronous t

;;       gnus-message-archive-group nil
;;       ;; gnus-sum-thread-tree-false-root      ""
;;       ;; gnus-sum-thread-tree-single-indent   ""
;;       ;; gnus-sum-thread-tree-root            ""
;;       ;; gnus-sum-thread-tree-vertical        "|"
;;       ;; gnus-sum-thread-tree-leaf-with-other "+-> "
;;       ;; gnus-sum-thread-tree-single-leaf     "\\-> "
;;       ;; gnus-sum-thread-tree-indent          " "

;;       nndraft-directory "~/.cache/gnus/drafts/"
;;       gnus-agent t
;;       gnus-agent-directory "~/.cache/gnus/agent/"

;;       ;; gnus-use-cache nil
;;       ;; gnus-cache-directory "~/.cache/gnus/"
;;       ;; gnus-cache-enter-articles '(read unread ticked dormant)
;;       ;; gnus-cache-remove-articles nil

;;       gnus-read-newsrc-file nil
;;       gnus-save-newsrc-file nil

;;       gnus-read-active-file 'some

;;       gnus-article-sort-functions '(gnus-article-sort-by-most-recent-date)
;;       gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
;;       gnus-thread-hide-subtree t
;;       gnus-thread-ignore-subject nil
;; )

;; (csetq gnus-simplify-subject-functions '(rrix/gnus-simplify-phab-headers
;;                                         gnus-simplify-subject-re
;;                                         gnus-simplify-subject-fuzzy
;;                                         gnus-simplify-whitespace
;;                                         gnus-simplify-all-whitespace)
;;       rrix/gnus-simplify-phab-headers-list '("\\[Differential\\]"
;;                                              "\\[Maniphest\\]"
;;                                              "\\[Updated.*\\]"
;;                                              "\\[Request.*\\]"
;;                                              "\\[Commented On.*\\]"
;;                                              "\\[Raised Concern.*\\]"
;;                                              "\\[Commandeered.*\\]"
;;                                              "\\[Accepted.*\\]"
;;                                              "\\[Planned.*\\]"
;;                                              "\\[Closed.*\\]"
;;                                              "\\[Resigned.*\\]"
;;                                              "^\ [0-9] "))

;; (defsubst rrix/gnus-simplify-phab-headers (subject)
;;   "Remove Phabricator headers from subject lines."
;;   (let ((transformed-subject subject))
;;     (dolist (regex rrix/gnus-simplify-phab-headers-list)
;;               (csetq transformed-subject (replace-regexp-in-string regex "" transformed-subject)))
;;     transformed-subject))

;; ;; ;; (add-to-list 'evil-emacs-state-modes 'gnus-category-mode)
;; ;; ;; (add-to-list 'evil-emacs-state-modes 'gnus-custom-mode)
;; ;; ;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f

;; (gnus-demon-add-handler 'gnus-demon-scan-news 5 nil) ; this does a call to gnus-group-get-new-news

;; (require 'gnus-notifications)
;; (defun gnus-notifications-notify (from subject photo-file)
;;   "Send a notification about a new mail.
;; Return a notification id if any, or t on success."
;;   (my/terminal-notifier "Gnus - New Message" from subject)
;;   t
;;   )
;; (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)
;; ;; ;; (require 'gnus-alias)
;; ;; (csetq gnus-posting-styles
;; ;;       '(;
;; ;;         (".*"
;; ;;          (address "eric@seidel.io")
;; ;;          ("X-Message-SMTP-Method" "smtp mail.messagingengine.com 587")
;; ;;          )
;; ;;         ((header "to" "gridaphobe@gmail\\.com")
;; ;;          (address "gridaphobe@gmail.com")
;; ;;          ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
;; ;;         ((header "to" "@.*\\.ucsd\\.edu")
;; ;;          (address "eseidel@cs.ucsd.edu")
;; ;;          ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
;; ;;         ;((header "to" "eseidel@galois\\.com")
;; ;;         ; (address "eseidel@galois.com")
;; ;;         ; ("X-Message-SMTP-Method" "smtp relay.galois.com 587"))
;; ;;         ))
;; ;; ;; (defadvice smtpmail-via-smtp (around set-smtp-server-from-header activate)
;; ;; ;;   (let ((smtpmail-smtp-server (or
;; ;; ;;                                (save-restriction
;; ;; ;;                                  (message-narrow-to-headers)
;; ;; ;;                                  (mail-fetch-field "X-SMTP-Server"))
;; ;; ;;                                smtpmail-smtp-server)))
;; ;; ;;     (message-remove-header "X-SMTP-Server")
;; ;; ;;     ad-do-it
;; ;; ;;     ))


;; ;; ;; (csetq gnus-select-method
;; ;; ;;       '(nnimap (st)))
;; ;; ;; (csetq gnus-secondary-select-methods
;; ;; ;;       '((nnmaildir )))

;;;; irc
;; (require 'circe)



(use-package circe
  :disabled t
  :ensure t
  :config
  (csetq lui-max-buffer-size 30000
        circe-reduce-lurker-spam t
        circe-highlight-nick-type 'occurrence
        tracking-ignored-buffers '(("#emacs" circe-highlight-nick-face)
                                   ("#fluidinfo" circe-highlight-nick-face)
                                   ("#fluidinfo-private" circe-highlight-nick-face)
                                   ("#haskell" circe-highlight-nick-face)
                                   ("#haskell-emacs" circe-highlight-nick-face)
                                   ("#haskell-ide-engine" circe-highlight-nick-face)
                                   ("#haskell-infrastructure" circe-highlight-nick-face)
                                   ("#idris" circe-highlight-nick-face)
                                   ("#nixos" circe-highlight-nick-face)))
  (when (load-file "~/.emacs.d/private.el") ;;(require 'private nil t)
    (csetq circe-network-options
          `(("seidel.io"
             :user "gridaphobe/freenode"
             :pass ,irc-pass
             :port 5000
             :nick "gridaphobe"
             :nickserv-password ,irc-nick-pass
             )))))


;;;; wgrep
(use-package wgrep
  :ensure t
  :config
  (csetq wgrep-auto-save-buffer t
        wgrep-enable-key "r"))


;;;; pretty symbols
;; (when (fboundp 'global-prettify-symbols-mode)
;;   (csetq prettify-symbols-alist nil
;;               ;  '(("<-" . ?\u2190)
;;                ;   ("->" . ?\u2192)))
;;                 )
;;   (global-prettify-symbols-mode +1))


;;;; mode line settings
;; FIXME: something in this file is toggling `line-number-mode' so I have to put
;; this at the end......
(line-number-mode 1)
(column-number-mode 1)
;; (size-indication-mode 1)
;; (csetq display-time-format "%R"
;;       display-time-default-load-average nil)
;; (display-time-mode 1)
(global-hl-line-mode 1)

(xterm-mouse-mode 1)


;; FIXME: why is this being set to nil?!
;; (csetq mu4e-mu-binary (executable-find "mu"))

;; (add-to-list 'load-path "~/Downloads/idris-mode-master")
;; (require 'idris-mode)

(message "Emacs is ready to do thy bidding, Master %s!" (getenv "USER"))

;;; init.el ends here

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
