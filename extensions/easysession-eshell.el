;;; easysession-eshell.el --- Persist and restore Eshell buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: systemfreund <github@o9z.de>
;; Version: 1.3.0
;; URL: https://github.com/jamescherti/easysession.el
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This extension makes EasySession persist and restore Eshell buffers.
;;
;; To enable `easysession-eshell-mode', add the following to your
;; configuration:
;;   (with-eval-after-load 'easysession
;;     (require 'easysession-eshell)
;;     (easysession-eshell-mode 1))
;;
;; Only the buffer name and `default-directory' are persisted; each Eshell
;; buffer is recreated with an empty prompt in the directory it was in when
;; the session was saved.

;;; Code:

(require 'easysession)

(defgroup easysession-eshell nil
  "Customization options for persisting Eshell buffers."
  :group 'easysession
  :prefix "easysession-eshell-")

(declare-function eshell-mode "esh-mode")

(defun easysession-eshell--directory-exists-p (state)
  "Return non-nil if STATE's `default-directory' still exists."
  (let ((directory (alist-get 'default-directory state)))
    (and directory (file-directory-p directory))))

(defun easysession-eshell--restore (state)
  "Restore an Eshell buffer from STATE."
  (require 'esh-mode)
  (let* ((buffer-name (alist-get 'buffer-name state))
         (directory (alist-get 'default-directory state))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (when (and directory (file-directory-p directory))
        (setq default-directory directory))
      (unless (derived-mode-p 'eshell-mode)
        (eshell-mode)))
    buffer))

;;;###autoload
(define-minor-mode easysession-eshell-mode
  "Persist and restore Eshell buffers."
  :global t
  :group 'easysession-eshell
  (if easysession-eshell-mode
      (easysession-add-managed-major-mode
       'eshell-mode
       :restore #'easysession-eshell--restore
       :validate #'easysession-eshell--directory-exists-p)
    (easysession-remove-managed-major-mode 'eshell-mode)))

(provide 'easysession-eshell)
;;; easysession-eshell.el ends here
