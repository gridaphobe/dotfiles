;;; eric-packages.el --- default package selection.

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar eric-packages
  '(ace-jump-mode
    ack-and-a-half
    auctex
    bbdb
    browse-kill-ring
    clojure-mode
    cperl-mode
    deft
    edit-server
    expand-region
    flymake-cursor
    gist
    go-mode
    haskell-mode
    js2-mode
    less-css-mode
    magit
    magithub
    markdown-mode
    melpa
    nginx-mode
    org
    org-magit
    paredit
    pkgbuild-mode
    puppet-mode
    python
    rainbow-mode
    restclient
    ruby-block
    scala-mode
    smex
    switch-window
    tuareg
    undo-tree
    virtualenv
    volatile-highlights
    writegood-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun eric-packages-installed-p ()
  (loop for p in eric-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun eric-install-packages ()
  (unless (eric-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p eric-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(eric-install-packages)


;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide 'eric-packages)
