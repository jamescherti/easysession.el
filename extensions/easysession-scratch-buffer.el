;;; easysession-scratch-buffer.el --- Persist and restore the scratch buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.4
;; URL: https://github.com/jamescherti/easysession.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
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
;; Make EasySession Persist and restore the scratch buffer.

;;; Code:

(require 'easysession)

(defgroup easysession-scratch-buffer nil
  "Non-nil if `easysession-scratch-buffer-mode' is enabled."
  :group 'easysession-scratch-buffer
  :prefix "easysession-scratch-buffer-")

(defun easysession-scratch-buffer--get-scratch-buffer-create ()
  "Return the *scratch* buffer, creating a new one if needed."
  (or (get-buffer "*scratch*")
      (let ((scratch (get-buffer-create "*scratch*")))
        (with-current-buffer scratch
          (when initial-scratch-message
            (insert (substitute-command-keys initial-scratch-message))
            (set-buffer-modified-p nil))
          (funcall initial-major-mode)
          (when (eq initial-major-mode 'lisp-interaction-mode)
            (setq-local trusted-content :all)))
        scratch)))

;;;###autoload
(define-minor-mode easysession-scratch-buffer-mode
  "Persist and restore the scratch buffer."
  :global t
  :group 'easysession-scratch-buffer
  (if easysession-scratch-buffer-mode
      (easysession-define-handler
       "scratch"

       ;; Load
       #'(lambda (session-data)
           "Load SESSION-DATA."
           (dolist (item session-data)
             (let ((buffer-name (car item)))
               (when (string= buffer-name "*scratch*")
                 (let* ((buffer
                         (easysession-scratch-buffer--get-scratch-buffer-create))
                        (buffer-data (cdr item))
                        (buffer-string
                         (when buffer-data
                           (assoc-default 'buffer-string buffer-data))))
                   (when (and buffer buffer-string)
                     (with-current-buffer buffer
                       (erase-buffer)
                       (insert buffer-string))))))))

       ;; Save
       #'(lambda(buffers)
           "Save the BUFFERS buffer."
           (easysession-save-handler-dolist-buffers
            buffers
            (let ((buffer-name (buffer-name)))
              (when (string= buffer-name "*scratch*")
                (cons buffer-name
                      (list
                       (cons 'buffer-string
                             (buffer-substring-no-properties (point-min)
                                                             (point-max))))))))))
    (easysession-undefine-handler "scratch")))

(provide 'easysession-scratch-buffer)
;;; easysession-scratch-buffer.el ends here
