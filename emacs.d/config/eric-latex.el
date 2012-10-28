(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook (lambda () (bib-cite-minor-mode 1)))
(setq reftex-plug-into-AUCTeX t)

(provide 'eric-latex)
