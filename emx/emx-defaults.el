;;; emx-defaults.el --- EmX Defaults                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Hatim Thayyil

;; Author: Hatim Thayyil <hatim@owl>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Essential packages that improve the default funcionality of Emacs.

;;; Code:

;;; ws-butler - Cleanup whitespace at end of lines
(use-package ws-butler
  :ensure t
  :delight
  :config (ws-butler-global-mode t))

(use-package transient
  :ensure t)

(provide 'emx-defaults)
;;; emx-defaults.el ends here
