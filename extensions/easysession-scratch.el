;;; easysession-scratch.el --- Persist and restore the scratch buffer -*- lexical-binding: t; -*-

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

(defgroup easysession-scratch nil
  "Non-nil if `easysession-scratch-mode' is enabled."
  :group 'easysession-scratch
  :prefix "easysession-scratch-")

(defun easysession-scratch--get-scratch-create ()
  "Return the *scratch* buffer, creating a new one if needed."
  (or (get-buffer "*scratch*")
      (and (fboundp 'get-scratch-buffer-create)
           (get-scratch-buffer-create))
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
(define-minor-mode easysession-scratch-mode
  "Persist and restore the scratch buffer."
  :global t
  :group 'easysession-scratch
  (if easysession-scratch-mode
      (easysession-define-handler
       "scratch"

       ;; Load
       #'(lambda (session-data)
           "Load SESSION-DATA."
           ;; Load the scratch buffer
           (let (buffer-string)
             (catch 'done
               (dolist (item session-data)
                 (let ((buffer-name (car item)))
                   (when (string= buffer-name "*scratch*")
                     (when-let* ((buffer-data (cdr item)))
                       (setq buffer-string (assoc-default
                                            'buffer-string buffer-data))
                       (throw 'done t))))))

             (if buffer-string
                 ;; Modify the scratch buffer
                 (let ((buffer (easysession-scratch--get-scratch-create)))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (erase-buffer)
                       (insert buffer-string))))
               ;; Erase the scratch buffer if it exists
               (let ((buffer (get-buffer "*scratch*")))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (erase-buffer)))))))

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

(provide 'easysession-scratch)
;;; easysession-scratch.el ends here
