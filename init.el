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
(require 'eric-packages)
(require 'eric-ui)
(require 'eric-misc)
(require 'eric-editing)
(require 'eric-keybindings)

;; mode-specific
(require 'eric-c)
(require 'eric-css)
(require 'eric-erc)
(require 'eric-haskell)
(require 'eric-js)
(require 'eric-latex)
(require 'eric-lisp)
(require 'eric-mail)
(require 'eric-org)
(require 'eric-perl)
(require 'eric-programming)
(require 'eric-python)
(require 'eric-ruby)
(require 'eric-scala)
(require 'eric-xml)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'eric-osx))

;; config changes made through the customize UI will be store here
(setq custom-file (concat emacs-config-dir "custom.el"))

;; run a server for emacsclient
(when (not (server-running-p))
  (server-start))

(message "Emacs is ready to do thy bidding, Master %s!" (getenv "USER"))

