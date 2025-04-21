;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn					;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time))))

;; Load EmX package management
(load (expand-file-name "emx/emx-package.el" emx-directory))

;; Load EmX init shim
(load (expand-file-name "emx/emx-init.el" emx-directory))

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
           emx-directory))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
