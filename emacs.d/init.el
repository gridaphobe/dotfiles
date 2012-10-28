;;; init.el --- Emacs' configuration entry point.

(message "Emacs is powering up... Be patient, Master %s!" (getenv "USER"))

(defvar emacs-dir (file-name-directory load-file-name)
  "The root folder of the configuration.")
(defvar emacs-config-dir (concat emacs-dir "config/")
  "This folder houses all configuration files.")
(defvar emacs-vendor-dir (concat emacs-dir "vendor/")
  "This folder houses Emacs Lisp packages that are not yet available in
ELPA (or MELPA).")
(defvar emacs-savefile-dir (concat emacs-dir "savefile/")
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p emacs-savefile-dir)
  (make-directory emacs-savefile-dir))

;; add configuration directories to Emacs' `load-path'
(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-vendor-dir)

;; the core stuff
(require 'my-packages)
(require 'my-ui)
(require 'my-misc)
(require 'my-editing)
(require 'my-keybindings)

;; mode-specific
(require 'my-c)
(require 'my-css)
(require 'my-erc)
(require 'my-haskell)
(require 'my-js)
(require 'my-latex)
(require 'my-lisp)
(require 'my-mail)
(require 'my-org)
(require 'my-perl)
(require 'my-programming)
(require 'my-python)
(require 'my-ruby)
(require 'my-scala)
(require 'my-xml)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'my-osx))

;; config changes made through the customize UI will be store here
(setq custom-file (concat emacs-config-dir "custom.el"))

(add-to-list
 'command-switch-alist
 '("gnus" . (lambda (&rest ignore)
              ;; Start Gnus when Emacs starts
              (add-hook 'emacs-startup-hook 'gnus t)
              ;; Exit Emacs after quitting Gnus
              (add-hook 'gnus-after-exiting-gnus-hook
                        'save-buffers-kill-emacs))))

(add-to-list
 'command-switch-alist
 '("erc" . (lambda (&rest ignore)
             ;; Start Erc when Emacs starts
             (load "~/.ercpass")
             (add-hook 'emacs-startup-hook
                       (lambda ()
                         (erc :password erc-pass))
                       t))))

;; run a server for emacsclient
(when (not (server-running-p))
  (server-start))

(message "Emacs is ready to do thy bidding, Master %s!" (getenv "USER"))

