;;; easysession.el --- Easily persist and restore your editing sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.1
;; URL: https://github.com/jamescherti/easysession.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1") (f "0.18.2"))
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

;; Easily persist and restore your editing sessions effortlessly.

;;; Code:

(require 'dired)
(require 'frameset)
(require 'f)

(defgroup easysession nil
  "Non-nil if easysession mode mode is enabled."
  :group 'easysession
  :prefix "easysession-")

(defcustom easysession-directory (expand-file-name "easysession"
                                                   user-emacs-directory)
  "Directory where the session files are stored."
  :type 'directory
  :group 'easysession)

(defcustom easysession-before-load-hook nil
  "Hooks to run before the session is loaded.
Each element should be a function to be called with no arguments."
  :type '(repeat function)
  :group 'easysession)

(defcustom easysession-after-load-hook nil
  "Hooks to run after the session is loaded.
Each element should be a function to be called with no arguments."
  :type '(repeat function)
  :group 'easysession)

(defcustom easysession-after-new-session-created-hook nil
  "Hooks to run after a new session is created.
Each element should be a function to be called with no arguments.

This can be used to customize behavior, such as emptying a session
after a new one is created."
  :type '(repeat function)
  :group 'easysession)

(defvar easysession-overwrite-frameset-filter-alist
  '((GUI:bottom . :never)
    (GUI:font . :never)
    (GUI:fullscreen . :never)
    (GUI:height . :never)
    (GUI:left . :never)
    (GUI:right . :never)
    (GUI:top . :never)
    (GUI:width . :never)
    (alpha . :never)
    (alpha-background . :never)
    (auto-lower . :never)
    (auto-raise . :never)
    (background-color . :never)
    (background-mode . :never)
    (border-color . :never)
    (border-width . :never)
    (bottom . :never)
    (bottom-divider-width . :never)
    (buffer-list . :never)
    (buffer-predicate . :never)
    (buried-buffer-list . :never)
    (child-frame-border-width . :never)
    (client . :never)
    (cursor-color . :never)
    (cursor-type . :never)
    (delete-before . :never)
    (display-type . :never)
    (environment . :never)
    (font . :never)
    (font-backend . :never)
    (font-parameter . :never)
    (foreground-color . :never)
    (frameset--text-pixel-height . :never)
    (frameset--text-pixel-width . :never)
    (fullscreen . :never)
    (height . :never)
    (horizontal-scroll-bars . :never)
    (icon-type . :never)
    (inhibit-double-buffering . :never)
    (internal-border-width . :never)
    (left . :never)
    (left-fringe . :never)
    (line-spacing . :never)
    (menu-bar-lines . :never)
    (minibuffer . :never)
    (mouse-color . :never)
    (mouse-wheel-frame . :never)
    (name . :never)
    (no-accept-focus . :never)
    (no-focus-on-map . :never)
    (no-special-glyphs . :never)
    (ns-appearance . :never)
    (outer-window-id . :never)
    (override-redirect . :never)
    (parent-frame . :never)
    (parent-id . :never)
    (right . :never)
    (right-divider-width . :never)
    (right-fringe . :never)
    (screen-gamma . :never)
    (scroll-bar-background . :never)
    (scroll-bar-foreground . :never)
    (scroll-bar-height . :never)
    (scroll-bar-width . :never)
    (shaded . :never)
    (skip-taskbar . :never)
    (sticky . :never)
    (tab-bar-lines . :never)
    (title . :never)
    (tool-bar-lines . :never)
    (tool-bar-position . :never)
    (top . :never)
    (tty . :never)
    (tty-type . :never)
    (undecorated . :never)
    (use-frame-synchronization . :never)
    (vertical-scroll-bars . :never)
    (visibility . :never)
    (wait-for-wm . :never)
    (width . :never)
    (window-id . :never)
    (window-system . :never)
    (z-group . :never))
  "Alist of frame parameters and filtering functions.")

(defvar easysession-frameset-filter-alist-geometry
  '(GUI:bottom
    GUI:fullscreen
    GUI:height
    GUI:left
    GUI:right
    GUI:top
    GUI:width
    height
    left
    right
    top
    width)
  "List of frame parameters related to geometry.")

(defvar easysession-file-version "2"
  "Version number of easysession file format.")

(defvar easysession--modified-filter-alist nil
  "Each time a session is saved, this list is overwritten.
It is overwritten with the values from 'frameset-filter-alist'. Afterwards, the
values of specific entries are replaced with ':never' for each frame parameter
listed in 'easysession-overwrite-frameset-filter-alist'. This process ensures
that certain settings are not persisted across sessions, focusing the restored
environment on essential user configurations and omitting system or
GUI-specific settings that are not relevant or desirable to persist when
switching between sessions multiple times while using Emacs.")

(defvar easysession--current-session-name "main"
  "Current session.")

(defvar easysession--current-session-loaded nil
  "Was the current session loaded at least once?")

(defun easysession-set-current-session (&optional session-name)
  "Set the current session to SESSION-NAME.
Return t if the session name is successfully set."
  (when (or (not session-name) (string= session-name ""))
    (error "The provided session name is invalid: '%s'" session-name))
  (setq easysession--current-session-name session-name)
  t)

(defun easysession--init-frame-parameters-filters ()
  "Initialize frame parameter filters for EasySession.

This function sets up `easysession--modified-filter-alist` by copying the
default `frameset-filter-alist` and then overwriting specific entries with
those provided in `easysession-overwrite-frameset-filter-alist`."
  (setq easysession--modified-filter-alist (copy-tree frameset-filter-alist))
  (dolist (pair easysession-overwrite-frameset-filter-alist)
    (setf (alist-get (car pair) easysession--modified-filter-alist) (cdr pair))))

(defun easysession--get-all-names ()
  "Return a list of all session names."
  (if (file-directory-p easysession-directory)
      (remove "." (remove ".." (directory-files easysession-directory nil nil t)))
    '()))

(defun easysession--prompt-session-name (prompt &optional session-name)
  "Prompt for a session name with PROMPT. Use SESSION-NAME as the default value."
  (completing-read prompt (easysession--get-all-names) nil nil nil nil session-name))

(defun easysession--get-base-buffer-path (buf)
  "Get the name and path of the buffer BUF.
Return nil When the buffer is not a base buffer.
Return a cons cell (buffer-name . path)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((buffer (current-buffer))
             (path (cond ((derived-mode-p 'dired-mode)
                          (dired-current-directory))
                         (buffer
                          (buffer-file-name buffer))
                         (t nil)))
             (buffer-name (buffer-name)))
        (if path
            ;; File visiting buffer and base buffers (not carbon copies)
            (cons buffer-name path)
          ;; This buffer is not visiting a file or it is a carbon copy
          nil)))))

(defun easysession--get-indirect-buffer-info (buf)
  "Get information about the indirect buffer BUF.

This function retrieves details about the indirect buffer BUF and its base
buffer. It returns a list of cons cells containing the names of both buffers,
the point position, window start position, and horizontal scroll position of
the base buffer.

- BUF: The buffer to get information from.

Return a list of cons cells:
'((indirect-buffer-name . name-of-indirect-buffer)
  (base-buffer-name . name-of-base-buffer)
  (base-buffer-point . point-position)
  (base-buffer-window-start . window-start-position)
  (base-buffer-hscroll . horizontal-scroll-position))

Return nil if BUF is not an indirect buffer or if the base buffer cannot be
determined."
  (when (and buf (buffer-live-p buf))
    (let* ((base-buffer (buffer-base-buffer buf)))
      (when (and base-buffer
                 (buffer-live-p base-buffer)
                 (not (eq base-buffer buf))
                 ;; The base has to be a file visiting buffer
                 (buffer-file-name base-buffer))
        (with-current-buffer buf  ; indirect buffer
          (let ((base-buffer-name (buffer-name base-buffer))
                (indirect-buffer-name (buffer-name)))
            (when (and base-buffer-name indirect-buffer-name)
              `((indirect-buffer-name . ,indirect-buffer-name)
                (indirect-buffer-point . ,(point))
                (indirect-buffer-window-start . ,(window-start))
                (indirect-buffer-hscroll . ,(window-hscroll))
                (base-buffer-name . ,base-buffer-name)))))))))

(defun easysession--check-session-name (session-name)
  "Validate the provided SESSION-NAME.

If the SESSION-NAME is invalid, an error is raised with a message indicating
the invalid name.

Return the SESSION-NAME if it is valid.

Errors:
Raise an error if the session name is invalid."
  (when (or (not session-name)
            (string= session-name "")
            (string-match-p "/" session-name)
            (string= session-name "..")
            (string= session-name "."))
    (error "Invalid session name: %s" session-name))
  session-name)

(defun easysession--get-session-file-name (session-name)
  "Return the fully qualified session file name for SESSION-NAME."
  (when session-name
    (easysession--check-session-name session-name)
    (expand-file-name session-name easysession-directory)))

(defun easysession--handler-save-frameset (session-name)
  "Return a frameset for FRAME-LIST, a list of frames.
SESSION-NAME is the session name."
  (easysession--init-frame-parameters-filters)
  (frameset-save nil
                 :app `(easysession . ,easysession-file-version)
                 :name session-name
                 :predicate #'easysession--check-dont-save
                 :filters easysession--modified-filter-alist))

(defun easysession--handler-load-frameset (session-info)
  "Load the frameset from the SESSION-INFO argument."
  (frameset-restore (assoc-default "frameset" session-info)
                    :reuse-frames t
                    :force-display t
                    :force-onscreen nil
                    :cleanup-frames t))

(defun easysession--handler-save-base-buffers ()
  "Return data about the base buffers and Dired buffers."
  (cl-remove nil (mapcar #'easysession--get-base-buffer-path (buffer-list))))

(defun easysession--handler-load-base-buffers (session-info)
  "Load base buffers from the SESSION-INFO variable."
  (let ((buffer-file-names (assoc-default "buffers" session-info)))
    (when buffer-file-names
      (dolist (buffer-name-and-path buffer-file-names)
        (let ((buffer-name (car buffer-name-and-path))
              (buffer-path (cdr buffer-name-and-path)))
          (when (and buffer-name buffer-path)
            (unless (or (get-buffer buffer-name)
                        (find-buffer-visiting buffer-path))
              (with-current-buffer (find-file-noselect buffer-path)
                (rename-buffer buffer-name t)))))))))

(defun easysession--handler-save-indirect-buffers ()
  "Return data about the indirect buffers."
  (cl-remove nil (mapcar #'easysession--get-indirect-buffer-info
                         (buffer-list))))

(defun easysession--handler-load-indirect-buffers (session-info)
  "Load indirect buffers from the SESSION-INFO variable."
  (let ((indirect-buffers (assoc-default "indirect-buffers"
                                         session-info)))
    (dolist (item indirect-buffers)
      (let* ((indirect-buffer-name (alist-get 'indirect-buffer-name item))
             (base-buffer-name (alist-get 'base-buffer-name
                                          (cdr item))))
        (when (and base-buffer-name indirect-buffer-name)
          (let ((base-buffer (get-buffer base-buffer-name))
                (indirect-buffer (get-buffer indirect-buffer-name)))
            (when (and (or (not indirect-buffer)
                           (not (buffer-live-p indirect-buffer)))
                       base-buffer
                       (buffer-live-p base-buffer))
              (with-current-buffer base-buffer
                (clone-indirect-buffer indirect-buffer-name nil)))))))))

(defun easysession--check-dont-save (frame)
  "Check if FRAME is a real frame and should be saved.
Also checks if 'easysession-dont-save is set to t."
  ;; One common use of the parent-frame parameter is in the context of child
  ;; frames, often used by packages like posframe, which create transient,
  ;; overlay, or tooltip-like frames. These child frames are associated with a
  ;; parent frame to maintain a logical and spatial relationship.
  (and (not (frame-parameter frame 'parent-frame)) ; Exclude frames with a parent frame
       (not (frame-parameter frame 'easysession-dont-save))))

(defun easysession-exists (session-name)
  "Check if a session with the given SESSION-NAME exists.

Returns t if the session file exists, nil otherwise."
  (when (file-exists-p (easysession--get-session-file-name session-name))
    t))

(defun easysession-delete (&optional session-name)
  "Delete a session. Prompt for SESSION-NAME if not provided."
  (interactive)
  (let* ((session-name (easysession--prompt-session-name
                        "Delete session: " session-name))
         (session-file (easysession--get-session-file-name session-name)))
    (if-let ((session-buffer (find-buffer-visiting session-file)))
        (kill-buffer session-buffer))
    (when (file-exists-p session-file)
      (delete-file session-file nil))
    (message "Session deleted: %s" session-name)))

(defun easysession-get-current-session-name ()
  "Return the name of the current session."
  easysession--current-session-name)

;; (defun easysession-rename (&optional session-name new-session-name)
;;   (interactive)
;;   ;; TODO
;;   (message "Not implemented yet."))

(defun easysession-save (&optional session-name)
  "Save the current session.
SESSION-NAME is the name of the session."
  (interactive)
  ;; Close the minibuffer to avoid prevent the mini buffer from being part of
  ;; the session
  (when (called-interactively-p 'any)
    (when (> (minibuffer-depth) 0)
      (let ((inhibit-message t))
        (abort-recursive-edit))))

  (let* ((session-name (if session-name session-name (easysession-get-current-session-name)))
         (session-file (easysession--get-session-file-name session-name))
         (data-frameset (easysession--handler-save-frameset session-name))
         (data-buffer (easysession--handler-save-base-buffers))
         (indirect-buffers (easysession--handler-save-indirect-buffers))
         (session-data nil)
         (session-dir (file-name-directory session-file)))

    ;; Handlers
    (push (cons "frameset" data-frameset) session-data)
    (push (cons "buffers" data-buffer) session-data)
    (push (cons "indirect-buffers" indirect-buffers) session-data)

    (unless (file-directory-p session-dir)
      (make-directory session-dir t))
    (f-write (prin1-to-string session-data) 'utf-8 session-file)
    (when (called-interactively-p 'any)
      (message "Session saved: %s" session-name))
    t))

(defun easysession-load (&optional session-name)
  "Load the current session. SESSION-NAME is the session name."
  (interactive)
  (let* ((session-name (if session-name
                           session-name
                         easysession--current-session-name))
         (session-info nil)
         (session-file (easysession--get-session-file-name session-name)))
    (when (and session-file (file-exists-p session-file))
      ;; Load
      (with-temp-buffer
        (insert-file-contents session-file)
        (setq session-info (read (current-buffer))))

      (unless session-info
        (error "Could not read '%s' session information" session-name))

      ;; Handlers
      (run-hooks 'easysession-before-load-hook)
      (easysession--handler-load-base-buffers session-info)
      (easysession--handler-load-indirect-buffers session-info)
      (easysession--handler-load-frameset session-info)
      (run-hooks 'easysession-after-load-hook)

      (setq easysession--current-session-loaded t)
      (when (called-interactively-p 'any)
        (message "Session loaded: %s" session-name))
      t)))

(defun easysession-save-as (&optional session-name)
  "Save the state of all frames into a session with the given name.
If SESSION-NAME is provided, use it; otherwise, use current session.
If the function is called interactively, ask the user."
  (interactive)
  (let* ((session-name (if (called-interactively-p 'any)
                           (easysession--prompt-session-name
                            "Save session as: " (easysession-get-current-session-name))
                         (easysession-get-current-session-name)))
         (previous-session-name easysession--current-session-name))
    (easysession--check-session-name session-name)
    (easysession-save session-name)
    (easysession-set-current-session session-name)
    (if (string= previous-session-name easysession--current-session-name)
        (message "Saved the session: %s" session-name)
      (message "Saved and switched to session: %s" session-name))
    t))

(defun easysession-switch-to (&optional session-name)
  "Save the current session and load a new one.

This function handles saving the current session and loading a new session
specified by SESSION-NAME. If SESSION-NAME is not provided, it will prompt the
user for a session name if called interactively. If the session already exists,
it will be loaded; otherwise, a new session will be created.

SESSION-NAME is the name of the session to switch to. If nil, the function
prompts the user for a session name if called interactively.

Behavior:
- If the current session is loaded and not being reloaded, the current session
  is saved.
- Loads the specified session.
- Sets the specified session as the current session.
- If the session does not exist, it is saved and an empty session is
  initialized."
  (interactive)
  (let* ((session-name (cond (session-name session-name)
                             ((called-interactively-p 'any)
                              (easysession--prompt-session-name
                               "Load and switch to session: "
                               (easysession-get-current-session-name)))
                             (t (easysession-get-current-session-name))))
         (session-file (easysession--get-session-file-name session-name))
         (session-reloaded (string= session-name easysession--current-session-name))
         (saved nil)
         (new-session nil))
    (when (and easysession--current-session-loaded (not session-reloaded))
      (easysession-save easysession--current-session-name)
      (setq saved t))
    (easysession-load session-name)

    (unless session-reloaded
      (easysession-set-current-session session-name)
      (unless (file-exists-p session-file)
        (run-hooks 'easysession-after-new-session-created-hook)
        (easysession-save)
        (setq new-session t)))

    (cond (session-reloaded (message "Reloaded session: %s" session-name))
          (saved (message "Saved and switched to %ssession: %s"
                          (if new-session "new " "") session-name))
          (t (message "Switched to %ssession: %s"
                      (if new-session "new " "") session-name)))))

;;;###autoload
(define-minor-mode easysession-save-mode
  "Toggle `easysession-save-mode'."
  :global t
  :lighter " easysession"
  :group 'easysession
  (if easysession-save-mode
      (progn
        (add-hook 'kill-emacs-hook #'easysession-save))
    (remove-hook 'kill-emacs-hook #'easysession-save)))

(provide 'easysession)
;;; easysession.el ends here
