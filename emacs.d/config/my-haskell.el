;; agda!
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "~/.cabal/bin/agda-mode locate")))
;; (setq agda2-include-dirs '("." "/Users/gridaphobe/Source/agda/lib-0.7/src"))

;; idris!
(add-to-list 'load-path "~/.emacs.d/vendor/idris-mode")
(require 'idris-mode)

(require 'hsenv)

;; don't show me .hi files!!
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".hdevtools.sock")

;; courtesy of johan tibell
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        ;; haskell-indentation-layout-offset 4
        ;; haskell-indentation-left-offset 4
        ;; haskell-indentation-ifte-offset 4
        ))

(setq haskell-process-type 'cabal-repl
      haskell-process-log t
      haskell-stylish-on-save nil
      haskell-tags-on-save nil)

(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))

(add-to-list 'exec-path "~/.cabal/bin")

(add-to-list 'auto-mode-alist '("\\.xmobarrc$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.spec$" . haskell-mode))
;; yesod
(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))


(require 'hi2)
(setq hi2-show-indentations nil)
;;(add-to-list 'load-path "~/Source/structured-haskell-mode/elisp")
(require 'shm)

(defun my-haskell-mode-defaults ()
  ;; run manually since haskell-mode is not derived from prog-mode
  (run-hooks 'my-prog-mode-hook)
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  ;; (turn-on-haskell-indent)
  ;; (turn-on-haskell-indentation)
  (turn-on-hi2)
  ;; (structured-haskell-mode +1)
  (turn-on-haskell-decl-scan)
  (when (and
         ;; haskell-process creates temp buffers to parse ghci
         ;; responses, no buffer-file-name...
         (stringp (buffer-file-name))
         (not (string-equal ".spec" (file-name-extension (buffer-file-name) t))))
    (flycheck-haskell-setup)
    (setq flycheck-checker 'haskell-hdevtools)
    (flycheck-mode +1))
;  (haskell-style)
  )

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-mode-hook 'my-haskell-mode-defaults)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(defun my/find-cabal-sandbox-pkg-db ()
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-root (when cabal-file (file-name-directory cabal-file)))
         (pkg-db (file-expand-wildcards
                  (concat cabal-root ".cabal-sandbox/*.conf.d"))))
    (if pkg-db (car pkg-db))))

;; (defvar flycheck-haskell-options
;;   '("-Wall" "-isrc" "-fno-warn-missing-signatures"))

(require 'flycheck)
(require 'flycheck-haskell)

(flycheck-define-checker haskell-hdevtools
  "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
  :command
  ("hdevtools" "check" "-g" "-Wall"
   (eval (when flycheck-ghc-no-user-package-database
           (list "-g" "-no-user-package-db")))
   (eval (apply #'append (mapcar (lambda (db) (list "-g" "-package-db" "-g" db))
                                 flycheck-ghc-package-databases)))
   (eval (concat
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
(define-key haskell-mode-map (kbd "C-c C") 'killall-hdevtools)

;; testing these..
;; Load the current file (and make a session if not already made).
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;; (define-key haskell-mode-map [f5] 'haskell-process-load-file)

;; Switch to the REPL.
(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; “Bring” the REPL, hiding all other windows apart from the source
;; and the REPL.
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; Build the Cabal project.
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; Interactively choose the Cabal command to run.
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; Get the type and info of the symbol at point, print it in the
;; message buffer.
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

;; Contextually do clever things on the space key, in particular:
;;   1. Complete imports, letting you choose the module name.
;;   2. Show the type of the symbol after the space.
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; Jump to the imports. Keep tapping to jump between import
;; groups. C-u f8 to jump back again.
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;; Jump to the definition of the current symbol.
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; Indent the below lines on columns after the current column.
(define-key haskell-mode-map (kbd "C-<right>")
  (lambda ()
    (interactive)
    (haskell-move-nested 1)))
;; Same as above but backwards.
(define-key haskell-mode-map (kbd "C-<left>")
  (lambda ()
    (interactive)
    (haskell-move-nested -1)))


(provide 'my-haskell)
