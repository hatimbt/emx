;;; emx-modules.el --- Module system for EMX -*- lexical-binding: t -*-

;;; Commentary:
;;
;; A simple module system for selective loading of configuration modules.
;; Modules are organized by category and can be selectively enabled.
;;
;; Usage:
;;   (emx-modules! :emacs undo
;;                 :tools magit)

;;; Code:

(defvar emx-modules-enabled '()
  "List of enabled modules in the form (category . module).")

(defvar emx-modules-dir (expand-file-name "modules/" emx-source-dir)
  "Directory containing EMX modules.")

(defun emx-module-path (category module)
  "Return the path to MODULE in CATEGORY."
  (expand-file-name (format "%s.%s.el" category module) emx-modules-dir))

(defun emx-module-exists-p (category module)
  "Check if MODULE exists in CATEGORY."
  (file-exists-p (emx-module-path category module)))

(defun emx-load-module (category module)
  "Load MODULE from CATEGORY if it exists."
  (let ((module-file (emx-module-path category module)))
    (when (file-exists-p module-file)
      (load module-file nil t)
      (push (cons category module) emx-modules-enabled)
      (message "EMX: Loaded module %s.%s" category module))))

(defmacro emx-modules! (&rest modules)
  "Enable MODULES.
MODULES is a list in the form (:category module1 module2 :category2 module3)."
  (let ((category nil)
        (result '()))
    (dolist (item modules)
      (cond
       ((keywordp item)
        (setq category (substring (symbol-name item) 1)))
       (t
        (when category
          (push `(emx-load-module ,category ',(symbol-name item)) result)))))
    `(progn ,@(nreverse result))))

(defun emx-module-enabled-p (category module)
  "Check if MODULE in CATEGORY is enabled."
  (member (cons category module) emx-modules-enabled))

(defun emx-list-enabled-modules ()
  "Return a list of enabled modules."
  emx-modules-enabled)

(provide 'emx-modules)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; emx-modules.el ends here