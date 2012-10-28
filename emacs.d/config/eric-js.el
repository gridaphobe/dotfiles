(require 'flymake)
(setq js2-auto-indent-p t
      js2-cleanup-whitespace t
      js2-enter-indents-newline t
      js2-indent-on-enter-key t
      js2-pretty-multiline-declarations t)

(defun flymake-jshint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name)))
         (arglist (list local-file)))
    (list "jshint" arglist)))

(setq flymake-err-line-patterns
      (cons '(".*: line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
              nil 1 2 3)
            flymake-err-line-patterns))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" flymake-jshint-init)
             '("\\.json\\'" flymake-jshint-init))

(add-hook 'js2-mode-hook (lambda () (flymake-mode t)))

(provide 'eric-js)
