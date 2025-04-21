;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-

;; We use Guix for our package management
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Set emx directory in different contexts
(defvar emx-directory
  (file-name-directory
   (or load-file-name                   ; when loaded from init.el shim
       buffer-file-name                 ; when buffer is evaluated
       (expand-file-name "init.el" user-emacs-directory))) ; fallback
  "The root directory of EMX configuration.")

(load (expand-file-name "emx/emx-early-init.el" emx-directory))

;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
