;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

;; Early birds
(progn					;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time))))

;; Support direct evaluation with fallbacks if not run through shim. This
;; ensures that the paths are in a clean Emacs env (eg: emacs -Q).
(unless (boundp 'emx-source-dir)
  (defvar emx-source-dir
    (file-name-directory
     (or load-file-name                   ; when loaded from shim
         buffer-file-name                 ; when buffer is evaluated
         (expand-file-name "init.el" user-emacs-directory))) ; fallback
    "Directory for EMX source files."))

(unless (boundp 'emx-config-dir)
  (defvar emx-config-dir
    (expand-file-name "~/.config/emx/")
    "Directory for EMX configuration files."))

(unless (boundp 'emx-data-dir)
  (defvar emx-data-dir
    (expand-file-name "~/.local/share/emx/")
    "Directory for EMX data files."))

(unless (boundp 'emx-cache-dir)
  (defvar emx-cache-dir
    (expand-file-name "~/.cache/emx/")
    "Directory for EMX cache files."))

;; Set user-emacs-directory if not already set by shim
(unless (equal user-emacs-directory emx-config-dir)
  (setq user-emacs-directory emx-config-dir))

;; Load EmX package management
(load (expand-file-name "lisp/emx-package.el" emx-source-dir))

;; Load Emacs Lisp from Elpaca
(load (expand-file-name "elpaca.el" emx-source-dir))

;; FIXME Does not work, need a way to have local overrides.
;; Load local overrides if it exists.
;; (let ((local (expand-file-name "elpaca.local.el" emx-source-dir)))
;;   (when (file-readable-p local)
;;     (message "Elpaca: loading local overrides from %s" local)
;;     (load local nil 'nomessage)))

(elpaca-process-queues)
(elpaca-wait)

;; Benchmark
(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; Load EmX init shim
(load (expand-file-name "lisp/emx.el" emx-source-dir))

;; Load module system
(load (expand-file-name "lisp/emx-modules.el" emx-source-dir))

(progn					;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;; Load modules from modules/ directory
(emx-modules! :emacs
              undo
              dired
              :completion
              vertico
              :terminal
              command-line-interface
              :emx
              base
              appearance
              multimedia
              programming
              version-control
              organisation
              research
              window
              navigation
              finance
              language-machine
              footer
              )

(load "~/.assistant/gptel-config.el")

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
