;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Super
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(let (osx-paths)
  (dolist (path exec-path (setenv "PATH" osx-paths))
    (setq osx-paths (concat osx-paths (concat path ":")))))

(provide 'eric-osx)
