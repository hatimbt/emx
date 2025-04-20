;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Networking

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;(global-hl-line-mode 1)		; Enable highlight of the current line
