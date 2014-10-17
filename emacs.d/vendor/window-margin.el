;;; window-margin.el --- automatic margins for visual-line-mode wrapping

;; Copyright (C) 2012 by Aaron Culich

;; Maintainer: Aaron Culich <aculich@gmail.com>
;; Version: 0.1
;; Keywords: margins, text, visual-line, word wrap
;; URL: http://github.com/aculich/window-margin.el
;; Description: automatic margins for visual-line-mode wrapping

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode will automatically resize windows to the width of
;; the fill-column, or optionally to some fixed size set with the
;; window-margin-width variable.
;;
;; To enable it with text-mode use:
;;
;;   (add-hook 'text-mode-hook 'turn-on-window-margin-mode)
;;
;; This minor mode was inspired by reading an entry on StackOverflow
;;
;;   http://stackoverflow.com/q/14009223/462302
;;
;; when I discovered that (the quirky, but useful) longlines-mode was
;; being discontinued. I can't claim that this mode will be any less
;; quirky (probably moreso at this early 0.1 release), but it makes
;; use of margins and visual-line mode which is better way to
;; accomplish the effect than implemented by longlines-mode


;;; Code:

(defgroup window-margin nil
  "Restrict visual width of window using margins for `visual-line-mode'."
  :group 'visual-line
  :version "24.1.1")

(defvar window-margin-width nil
  "Restrict visual width of window using margins for `visual-line-mode'.
If non-nil use value of `fill-column', otherwise if an integer
will be a fixed value, or if a floating point number then a
percentage of the frame width.")
(make-local-variable 'window-margin-width)
(setq-default window-margin-width nil)

(defcustom window-margin-mode-line-string " Margin"
  "String displayed on the modeline when window-margin is active.
Set this to nil if you don't want a modeline indicator."
  :group 'window-margin
  :type '(choice string (const :tag "None" nil)))

;;;###autoload
(define-minor-mode window-margin-mode
  "Restrict visual width of window using margins for `visual-line-mode'.
With a prefix argument ARG, enable Window Margin mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Window Margin mode is enabled, the visual width of windows
will be restricted using margins in `visual-line-mode' (which
will also be turned on for the selected buffer).
"
  :lighter window-margin-mode-line-string
  :group 'window-margin
  (if window-margin-mode
      (progn
        (unless window-margin-width
          (setq window-margin-width t))
        (add-hook 'window-configuration-change-hook 'window-margin-update t t)
        (turn-on-visual-line-mode)
        (window-margin-update))
    (progn
      (setq window-margin-width nil)
      (remove-hook 'window-configuration-change-hook 'window-margin-update t)
      (set-window-margins (get-buffer-window) nil nil))))

;;;###autoload
(defun turn-on-window-margin-mode ()
  (window-margin-mode 1))
(custom-add-option 'text-mode-hook 'turn-on-window-margin-mode)

;;;###autoload
(defun turn-off-window-margin-mode ()
  (window-margin-mode -1))

(defun window-margin-update ()
  (let* ((window-configuration-change-hook
          (remove 'window-margin-update window-configuration-change-hook))
         (window (selected-window))
         (buffer-width
          (cond ((integerp window-margin-width)
                 window-margin-width)
                ((floatp window-margin-width)
                 window-margin-width)
                ((and window-margin-width)
                 (or fill-column nil))
                (t 80))))
    (unless (active-minibuffer-window)
      (set-window-margins window nil nil)
      (if (window-at-side-p window 'right)
          (let ((rm (+ (- (window-total-width) buffer-width)
                       (- (window-body-width) (window-total-width) 1))))
            (when (> rm 0)
              (set-window-margins window nil rm)))
        (let ((window-delta (+ (- buffer-width (window-total-width))
                               (- (window-total-width) (window-body-width))
                               1)))
          (window-resize window window-delta t))))))

(defadvice set-fill-column (after window-margin-update activate)
  (window-margin-update))

(provide 'window-margin)

;;; window-margin.el ends here
