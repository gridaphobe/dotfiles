;; agda!
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))

;; don't show me .hi files!!
(add-to-list 'completion-ignored-extensions ".hi")

;; courtesy of johan tibell
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4))

(setq haskell-process-type 'ghci
      haskell-stylish-on-save nil
      haskell-tags-on-save t)

(add-to-list 'exec-path "~/.cabal/bin")

(add-to-list 'auto-mode-alist '("\\.xmobarrc$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.spec$" . haskell-mode))
;; yesod
(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(require 'virthualenv)

(defun my-haskell-mode-defaults ()
  ;; run manually since haskell-mode is not derived from prog-mode
  (run-hooks 'my-prog-mode-hook)
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (haskell-style)

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

  ;; ;; Jump to the imports. Keep tapping to jump between import
  ;; ;; groups. C-u f8 to jump back again.
  ;; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

  ;; ;; Indent the below lines on columns after the current column.
  ;; (define-key haskell-mode-map (kbd "C-<right>")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (haskell-move-nested 1)))
  ;; ;; Same as above but backwards.
  ;; (define-key haskell-mode-map (kbd "C-<left>")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (haskell-move-nested -1)))
  )

;; Useful to have these keybindings for .cabal files, too.
(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

(add-hook 'haskell-mode-hook 'my-haskell-mode-defaults)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

(provide 'my-haskell)
