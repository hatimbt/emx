;;; emx-prose.el --- EmX Prose, Writing, Reading     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@thayyil.net>
;; Keywords:

;;; Commentary:

;;
;; This set of packages provides quality of life improvements for writing or
;; reading prose.

;;; Code:

;; titlecase
;;
;; https://github.com/duckwork/titlecase.el
;;
;; A tool to help capitalise titles.
(use-package titlecase)

;; flycheck
;;
;; https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html
;;
;; Flycheck is presented as featureful alternative to Emacs
;;
;; Source: https://emacstil.com/til/2022/03/05/setting-up-vale-prose-linter-on-emacs/
(use-package flycheck
  :config
  (flycheck-define-checker vale
   "A checker for prose"
   :command ("vale" "--output" "line"
             source)
   :standard-input nil
   :error-patterns
   ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
   :modes (markdown-mode org-mode text-mode))
  (add-to-list 'flycheck-checkers 'vale 'append))
 
(provide 'emx-prose)
;;; emx-prose.el ends here
