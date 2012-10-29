;;; my-packages.el --- default package selection.

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(ac-dabbrev
    ac-nrepl
    ace-jump-mode
    ack-and-a-half
    auctex
    bbdb
    browse-kill-ring
    caml
    clojure-mode
    clojure-test-mode
    cperl-mode
    deft
    edit-server
    expand-region
    flymake-cursor
    fwb-cmds
    gist
    go-mode
    haskell-mode
    js2-mode
    less-css-mode
    logito
    lua-mode
    magit
    magithub
    markdown-mode
    melpa
    nginx-mode
    nrepl
    org
    org-magit
    paredit
    pcache
    pkgbuild-mode
    puppet-mode
    rainbow-mode
    restclient
    scala-mode
    smex
    sr-speedbar
    switch-window
    tuareg
    undo-tree
    virtualenv
    volatile-highlights
    writegood-mode
    yaml-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun my-install-packages ()
  (unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p my-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(my-install-packages)


;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide 'my-packages)