(defun my-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun my-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

(defun my-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  ;;(flyspell-prog-mode)
  (my-local-comment-auto-fill)
  (whitespace-mode +1)
  (abbrev-mode +1)
  (my-add-watchwords)
  ;; keep the whitespace decent all the time (in this buffer)
  ;;(add-hook 'before-save-hook 'whitespace-cleanup nil t)
  )

(require 'ws-trim)
(global-ws-trim-mode 1)

(add-hook 'prog-mode-hook 'my-prog-mode-defaults)

(require 'thingatpt)
(require 'imenu)

(defun ido-goto-symbol-at-point ()
  "Will navigate to the symbol at the current point of the cursor"
  (interactive)
  (ido-goto-symbol (thing-at-point 'symbol)))

(defun ido-goto-symbol (&optional a-symbol)
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol
            (if (null a-symbol)
                (ido-completing-read "Symbol? " symbol-names)
              a-symbol))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position))))))
(global-set-key (kbd "C-c i")     'ido-goto-symbol)

(provide 'my-programming)
