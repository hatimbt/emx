;;; elpaca.el --- ELisp sources to be fetched using Elpaca  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@thayyil.net>
;; Keywords:

;;; Commentary:

;; Here, we specify Emacs Lisp packages to fetch and their source origins.

;;; Code:

;; emx-appearance
(elpaca (rainbow-csv :host github :repo "emacs-vs/rainbow-csv"))

;; emx-completion
(elpaca vertico)

;; emx-research
(elpaca org-node)

;; emx-ai
(elpaca gptel)

;; emx-footer
(elpaca envrc)

(elpaca-process-queues)
