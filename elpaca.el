;;; elpaca.el --- ELisp sources to be fetched using Elpaca  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@thayyil.net>
;; Keywords:

;;; Commentary:

;; Here, we specify Emacs Lisp packages to fetch and their source origins. It
;; would be nice to specify packages that should be provided by Nix

;;; Code:

;; emx-appearance
(elpaca modus-themes)
(elpaca ef-themes)
(elpaca doom-themes)
(elpaca poet-theme)
(elpaca lin)
(elpaca spacious-padding)
(elpaca fontaine)
(elpaca show-font)
(elpaca show-font-mode)
(elpaca (rainbow-csv :host github :repo "emacs-vs/rainbow-csv"))

;; emx-completion
(elpaca vertico)
(elpaca orderless)
(elpaca marginalia)
(elpaca embark)
(elpaca consult)
(elpaca embark-consult)
(elpaca tabspaces)

;; emx-version-control
(elpaca diff-hl)
(elpaca git-gutter)
(elpaca magit)
(elpaca magit-todos)
(elpaca magit-stgit)

;; emx-research
(elpaca consult-notes)
(elpaca denote)
(elpaca howm)
(elpaca zk)
(elpaca (org-node :host github :repo "meedstrom/org-node"))
(elpaca (org-mem :host github :repo "meedstrom/org-mem"))
(elpaca citar)
(elpaca citar-denote)
(elpaca auctex)
(elpaca cdlatex)
(elpaca ebib)
(elpaca biblio)
(elpaca (persid :host github :repo "rougier/persid"))

;; emx-prose
(elpaca writeroom-mode)
(elpaca titlecase)
(elpaca jinx)

;; emx-communications
(elpaca notmuch)
(elpaca consult-notmuch)
(elpaca elfeed)
(elpaca org-msg)

;; emx-finance
(elpaca beancount)

;; emx-ai
(elpaca gptel)

;; emx-footer
(elpaca envrc)
