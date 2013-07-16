;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Super
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(set-fontset-font "fontset-default"
                  'unicode
                  '("DejaVu Sans Mono" . "iso10646-1"))
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :slant  'normal
                    :weight 'normal
                    :width  'normal
                    :height 140)


(exec-path-from-shell-initialize)

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; (let (osx-paths)
;;   (dolist (path exec-path (setenv "PATH" osx-paths))
;;     (setq osx-paths (concat osx-paths (concat path ":")))))

;; (setq eshell-path-env (getenv "PATH"))

(provide 'my-osx)
