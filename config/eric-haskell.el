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
      haskell-stylish-on-save t
      haskell-tags-on-save t)

;;(setq haskell-font-lock-symbols 'unicode)

(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'exec-path "~/Library/Haskell/bin")

(add-to-list 'auto-mode-alist '("\\.xmobarrc$" . haskell-mode))
;; yesod
(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(require 'virthualenv)

(defun eric-haskell-mode-defaults ()
  ;; run manually since haskell-mode is not derived from prog-mode
  (run-hooks 'eric-prog-mode-hook)
  (subword-mode +1)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (haskell-style))

(add-hook 'haskell-mode-hook 'eric-haskell-mode-defaults)

(provide 'eric-haskell)
