(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (bib-cite-minor-mode 1)))
(setq reftex-plug-into-AUCTeX t)

(require 'ebib)
(add-to-list 'ebib-preload-bib-files "~/Dropbox/gridaphobe.bib")

(require 'markdown-mode)
(setq markdown-enable-math t)

(provide 'my-latex)
