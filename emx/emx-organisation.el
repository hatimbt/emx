;;; emx-organisation.el --- Org-mode and tools to stay organised  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@thayyil.net>
;; Keywords:

;; Calendar
(use-package calendar
  :ensure nil
  :commands (calendar)
  :config
  (setq calendar-time-display-form
        '( 24-hours ":" minutes
           (when time-zone (format "(%s)" time-zone))))
  (setq calendar-time-zone-style 'numeric)

  (require 'cal-dst)
  (setq calendar-standard-time-zone-name "+0000")
  (setq calendar-daylight-time-zone-name "+0100"))

;; org-mode
(use-package org-mode
  :init
  (setq org-directory (expand-file-name "~/Documents/org/"))
  (setq org-imenu-depth 7)
  :bind
  ( :map global-map
    ("C-c l" . org-store-link)
    ("C-c o" . org-open-at-point-global))
  :config
  (setq org-ellipsis "⮧"))
