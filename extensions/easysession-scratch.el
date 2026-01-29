;;; easysession-scratch.el --- Persist and restore the scratch buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.7
;; URL: https://github.com/jamescherti/easysession.el
;; Keywords: convenience
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
;; This extension makes EasySession Persist and restore the scratch buffer.
;;
;; To enable `easysession-scratch-mode', add the following to your
;; configuration:
;;   (with-eval-after-load 'easysession
;;     (require 'easysession-scratch)
;;     (easysession-scratch-mode 1))

;;; Code:

(require 'easysession)

(defgroup easysession-scratch nil
  "Customization options for persisting the Emacs scratch buffer."
  :group 'easysession-scratch
  :prefix "easysession-scratch-")

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
            (unless (member "*scratch*"
                            easysession-visible-buffer-list-include-names)
              (add-to-list 'easysession-visible-buffer-list-include-names
                           "*scratch*"))

            ;; Load the scratch buffer
            (let (buffer-string)
              (catch 'done
                (dolist (item session-data)
                  (when (and item (string= (car item) "*scratch*"))
                    (let ((buffer-data (cdr item)))
                      (when buffer-data
                        (setq buffer-string (assoc-default
                                             'buffer-string buffer-data))
                        (throw 'done t))))))

              (if buffer-string
                  ;; Modify the scratch buffer
                  (let ((buffer (easysession--get-scratch-buffer-create)))
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (save-excursion
                          (erase-buffer)
                          (insert buffer-string)))))
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
