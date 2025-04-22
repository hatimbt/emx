;;; emx-base.el ---                     -*- lexical-binding: t -*-
;;;
;;; Some sane defaults for a base Emacs installation.
;;;
;;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/

(require 'use-package)

;;; Editing

;; Delete the selected/highlighted text as soon as the user types something.
(use-package delsel
  :ensure nil				; built-in
  :hook (after-init . delete-selection-mode))

;; Show the dict definition of word at point.
(use-package dictionary
  :ensure nil 				; built-in
  :bind ("M-#" . dictionary-lookup-definition)
  :config
  ;; Show dictionary definition on the left
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*"
		 (display-buffer-in-side-window)
		 (side . left)
		 (window-width . 70))))

;; Enable automatic spell check for text and source code.
(use-package ispell
  :ensure nil				; built-in
  :config
  (setopt ispell-dictionary "en_US") ;; Set the default dictionary for spell checking.
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;; vundo - Visual undo tree
;;
;; 'https://github.com/casouri/vundo'
;;
;; This is an alternative to the much older 'undo-tree' package. Vundo displays
;; a horizontal tree whereas undo-tree displays a vertical tree.
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;; Helper functions

(defun emx/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'emx/keyboard-quit-dwim)


(provide 'emx-base)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; emx-base.el ends here
