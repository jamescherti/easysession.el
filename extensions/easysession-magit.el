;;; easysession-magit.el --- Persist and restore Magit buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Emre Yolcu

;; Author: Emre Yolcu
;; Version: 1.1.7
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
;; Make EasySession persist and restore Magit buffers.
;;
;; To enable `easysession-magit-mode', add the following to your
;; configuration:
;;   (require 'easysession-magit)
;;   (with-eval-after-load 'easysession
;;     (easysession-magit-mode 1))

;;; Code:

(require 'easysession)

(defgroup easysession-magit nil
  "Customization options for persisting Magit buffers."
  :group 'easysession-magit
  :prefix "easysession-magit-")

(defvar magit-display-buffer-noselect)

(defvar easysession-magit--mode-configs
  '((magit-status-mode . ( :restore-fn magit-status-setup-buffer
                           :restore-args ()))
    (magit-log-mode . ( :save-vars ((revisions . magit-buffer-revisions)
                                    (args . magit-buffer-log-args)
                                    (files . magit-buffer-files))
                        :restore-fn magit-log-setup-buffer
                        :restore-args (revisions args files)))
    (magit-diff-mode . ( :save-vars ((range . magit-buffer-range)
                                     (typearg . magit-buffer-typearg)
                                     (args . magit-buffer-diff-args)
                                     (files . magit-buffer-diff-files))
                         :restore-fn magit-diff-setup-buffer
                         :restore-args (range typearg args files)))
    (magit-revision-mode . ( :save-vars ((revision . magit-buffer-revision))
                             :restore-fn magit-revision-setup-buffer
                             :restore-args (revision nil nil)))
    (magit-refs-mode . ( :save-vars ((upstream . magit-buffer-upstream)
                                     (args . magit-buffer-arguments))
                         :restore-fn magit-refs-setup-buffer
                         :restore-args (upstream args)))
    (magit-stashes-mode . ( :restore-fn magit-stashes-setup-buffer
                            :restore-args ())))
  "Configuration for saving/restoring each Magit mode.")

(defun easysession-magit--git-repo-p (state)
  "Return non-nil if STATE's directory is a Git repository."
  (let ((directory (alist-get 'default-directory state)))
    (when directory
      (let ((git-path (expand-file-name ".git" directory)))
        (or (file-directory-p git-path) (file-regular-p git-path))))))

;;;###autoload
(define-minor-mode easysession-magit-mode
  "Persist and restore Magit buffers."
  :global t
  :group 'easysession-magit
  (if easysession-magit-mode
      (dolist (config easysession-magit--mode-configs)
        (let* ((props (cdr config))
               (save-vars (plist-get props :save-vars))
               (restore-args (plist-get props :restore-args)))
          (easysession-register-mode (car config)
            :save (when save-vars
                    (lambda ()
                      (mapcar (lambda (entry)
                                (let ((key (car entry))
                                      (var (cdr entry)))
                                  (cons key (and (boundp var) (symbol-value var)))))
                              save-vars)))
            :restore (lambda (state)
                       (require 'magit nil t)
                       (let ((magit-display-buffer-noselect t)
                             (data (alist-get 'data state)))
                         (apply (plist-get props :restore-fn)
                                (mapcar (lambda (arg)
                                          (when arg (alist-get arg data)))
                                        restore-args))))
            :validate #'easysession-magit--git-repo-p)))
    (dolist (config easysession-magit--mode-configs)
      (easysession-unregister-mode (car config)))))

(provide 'easysession-magit)
;;; easysession-magit.el ends here
