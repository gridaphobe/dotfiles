;;; my-packages.el --- default package selection.

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; required because of a package.el bug
(setq url-http-attempt-keepalives nil)

(defvar my-packages
  '(
    ace-jump-mode
    ack-and-a-half
    auctex
    auctex-latexmk
    bbdb
    browse-kill-ring
    cider
    circe
    cperl-mode
    dash
    diminish
    ebib
    edit-server
    evil
    evil-leader
    surround
    expand-region
    f
    flx-ido
    flycheck
    flycheck-haskell
    fwb-cmds
    geiser
    gist
    go-mode
    haskell-mode
    helm
    helm-projectile
    hi2
    ido-ubiquitous
    ido-vertical-mode
    idris-mode
    js2-mode
    less-css-mode
    logito
    lua-mode
    magit
    markdown-mode
    nginx-mode
    org-plus-contrib
    paredit
    pcache
    pkgbuild-mode
    projectile
    puppet-mode
    quack
    rainbow-mode
    restclient
    s
    scala-mode
    shm
    smartparens
    smex
    solarized-theme
    switch-window
    tuareg
    undo-tree
    virtualenv
    volatile-highlights
    wrap-region
    writegood-mode
    ws-trim
    yaml-mode
    zenburn-theme
    )
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
