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
(load (expand-file-name "emx/emx-package.el" emx-source-dir))

;; Load EmX init shim
(load (expand-file-name "emx/emx-init.el" emx-source-dir))

(progn					;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

;;; Load modules.
(let ((posimacs-files '("emx-base.el"
                        "emx-defaults.el"
                        "emx-appearance.el"
                        "emx-completion.el"
                        "emx-multimedia.el"
                        "emx-programming.el"
                        "emx-version-control.el"
                        "emx-research.el"
                        ;"emx-filesystem.el"
                        ;"emx-window.el"
                        ;"emx-navigation.el"
                        ;"emx-finance.el"
                        ;"emx-ai.el"
                        )))

  (dolist (file-name posimacs-files)
    (load (expand-file-name
           (concat "emx/" file-name)
           emx-source-dir))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
