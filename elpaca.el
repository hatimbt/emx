;;; elpaca.el --- ELisp sources to be fetched using Elpaca  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@thayyil.net>
;; Keywords:

;;; Commentary:

;; Here, we specify Emacs Lisp packages to fetch and their source origins. It
;; would be nice to specify packages that should be provided by Nix

;;; Code:

(elpaca compat)
(elpaca no-littering)
(elpaca vundo)
(elpaca ws-butler)

;; emx-navigation
(elpaca beginend)
(elpaca mwim)

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
(elpaca transient)
(elpaca magit)
(elpaca magit-todos)
(elpaca magit-stgit)
(elpaca magit-imerge)
(elpaca forge)
(elpaca git-modes)
(elpaca with-editor)

;; emx-programming
(elpaca parinfer-rust-mode) ;; requires pkgs.parinfer-rust-emacs
(elpaca paredit)
(elpaca nix-mode)
(elpaca markdown-mode)
(elpaca docker)
(elpaca dockerfile-mode)
(elpaca docker-compose-mode)
(elpaca tempel)
(elpaca tempel-collection)
(elpaca yasnippet)
(elpaca yasnippet-snippets)
(elpaca sly)
(elpaca (sly-stepper :host github :repo "joaotavora/sly-stepper"))
(elpaca sly-asdf)
(elpaca sly-quicklisp)
(elpaca sly-macrostep)
(elpaca cider)
(elpaca geiser)
(elpaca geiser-guile)
;;(elpaca geiser-racket)                  ;broken
(elpaca geiser-overlay)
(elpaca macrostep-geiser)
(elpaca package-lint)
(elpaca flycheck)
(elpaca erk)
(elpaca guix)

;; emx-research
(elpaca consult-notes)
(elpaca denote)
(elpaca howm)
(elpaca zk)
(elpaca (org-node :host github :repo "meedstrom/org-node"))
(elpaca (org-mem :host github :repo "meedstrom/org-mem"))
(elpaca citar)
(elpaca citar-denote)
(elpaca (auctex :host github :repo "emacsmirror/auctex" :branch "master"))
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

;; emx-language-machine
(elpaca gptel)
(elpaca ellama)

;; emx-footer
(elpaca envrc)
