;;; easysession.el --- Easily persist and restore your editing sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.4
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
;; The easysession Emacs package is a lightweight session manager for Emacs that
;; can persist and restore file editing buffers, indirect buffers (clones),
;; Dired buffers, the tab-bar, and Emacs frames (including or excluding the
;; geometry: frame size, width, and height). It offers a convenient and
;; effortless way to manage Emacs editing sessions and utilizes built-in Emacs
;; functions to persist and restore frames.
;;
;; With Easysession.el, you can effortlessly switch between sessions,
;; ensuring a consistent and uninterrupted editing experience.
;;
;; Key features include:
;; - Minimalist design focused on performance and simplicity, avoiding
;;   unnecessary complexity.
;; - Quickly switch between sessions while editing without disrupting the frame
;;   geometry, enabling you to resume work immediately.
;; - Save and load file editing buffers, indirect buffers/clones, Dired buffers,
;;   windows/splits, the Emacs frame (including support for the tab-bar).
;; - Helper functions: Switch to a session (i.e., load and change the current
;;   session) with `easysession-switch-to', load the Emacs editing session with
;;   `easysession-load', save the Emacs editing session with `easysession-save'
;;   and `easysession-save-as', delete the current Emacs session with
;;   `easysession-delete', and rename the current Emacs session with
;;   `easysession-rename'.

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

(defcustom easysession-before-save-hook nil
  "Hooks to run before the session is saved.
Each element should be a function to be called with no arguments."
  :type '(repeat function)
  :group 'easysession)

(defcustom easysession-after-save-hook nil
  "Hooks to run after the session is saved.
Each element should be a function to be called with no arguments."
  :type '(repeat function)
  :group 'easysession)

(defcustom easysession-new-session-hook nil
  "Hooks to run after a new session is created.
Each element should be a function to be called with no arguments.

This can be used to customize behavior, such as emptying a session
after a new one is created."
  :type '(repeat function)
  :group 'easysession)

(defcustom easysession-quiet nil
  "If non-nil, suppress all messages and only show errors and warnings.
This includes messages such as 'Session deleted:', 'Session loaded:', 'Session
saved:', etc. "
  :type 'boolean
  :group 'easysession)

(defcustom easysession-save-interval nil
  "The interval between automatic session saves.
If set to nil, it disables timer-based autosaving. Automatic session saves are
activated when `easysession-save-mode' is enabled."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'easysession)

(defvar easysession--timer nil)

(defvar easysession--overwrite-frameset-filter-alist
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

(defvar easysession--overwrite-frameset-filter-include-geometry-alist
  '(;; (GUI:bottom . :never)
    (GUI:font . :never)
    ;; (GUI:fullscreen . :never)
    ;; (GUI:height . :never)
    ;; (GUI:left . :never)
    ;; (GUI:right . :never)
    ;; (GUI:top . :never)
    ;; (GUI:width . :never)
    (alpha . :never)
    (alpha-background . :never)
    (auto-lower . :never)
    (auto-raise . :never)
    (background-color . :never)
    (background-mode . :never)
    (border-color . :never)
    (border-width . :never)
    ;; (bottom . :never)
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
    ;; (frameset--text-pixel-height . :never)
    ;; (frameset--text-pixel-width . :never)
    ;; (fullscreen . :never)
    ;; (height . :never)
    (horizontal-scroll-bars . :never)
    (icon-type . :never)
    (inhibit-double-buffering . :never)
    (internal-border-width . :never)
    ;; (left . :never)
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
    ;; (right . :never)
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
    ;; (top . :never)
    (tty . :never)
    (tty-type . :never)
    (undecorated . :never)
    (use-frame-synchronization . :never)
    (vertical-scroll-bars . :never)
    (visibility . :never)
    (wait-for-wm . :never)
    ;; (width . :never)
    (window-id . :never)
    (window-system . :never)
    (z-group . :never))
  "Alist of frame parameters and filtering functions.")

(defvar easysession-file-version 3
  "Version number of easysession file format.")

(defvar easysession--load-geometry nil
  "Non-nil to make `easysession-load' load the geometry.
Do not modify this variable, use the `easysession-load-including-geometry'
function instead.")

(defvar easysession--current-session-name "main"
  "Current session.")

(defvar easysession--current-session-loaded nil
  "Was the current session loaded at least once?")

(defvar easysession--is-loading nil
  "Non-nil if a session is currently being loaded.
This variable is used to indicate whether a session loading process is in
progress.")

(defun easysession--message (&rest args)
  "Display a message with '[easysession]' prepended.
The message is formatted with the provided arguments."
  (unless easysession-quiet
    (apply 'message (concat "[easysession] " (car args)) (cdr args))))

(defun easysession--warning (&rest args)
  "Display a warning message with '[easysession] Warning: ' prepended.
The message is formatted with the provided arguments."
  (apply 'message (concat "[easysession] Warning: " (car args)) (cdr args)))

(defun easysession--ensure-session-name-valid (session-name)
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
    (error "[easysession] Invalid session name: %s" session-name))
  session-name)

(defun easysession-set-current-session (&optional session-name)
  "Set the current session to SESSION-NAME.
Return t if the session name is successfully set."
  (easysession--ensure-session-name-valid session-name)
  (setq easysession--current-session-name session-name)
  t)

(defun easysession--init-frame-parameters-filters (overwrite-alist)
  "Return the EasySession version of `frameset-filter-alist'.
OVERWRITE-ALIST is an alist similar to
`easysession--overwrite-frameset-filter-alist'."
  (unless overwrite-alist
    (setq overwrite-alist easysession--overwrite-frameset-filter-alist))
  (let ((result (copy-tree frameset-filter-alist)))
    (dolist (pair overwrite-alist)
      (setf (alist-get (car pair) result)
            (cdr pair)))
    result))

(defun easysession--get-all-names ()
  "Return a list of all session names."
  (if (file-directory-p easysession-directory)
      (remove "." (remove ".." (directory-files easysession-directory nil
                                                nil t)))
    '()))

(defun easysession--prompt-session-name (prompt &optional session-name)
  "Prompt for a session name with PROMPT. Use SESSION-NAME as the default value."
  (completing-read (concat "[easysession] " prompt)
                   (easysession--get-all-names) nil nil nil nil session-name))

(defun easysession--get-base-buffer-path (buffer)
  "Get the name and path of the buffer BUFFER.
Return nil When the buffer is not a base buffer.
Return a cons cell (buffer-name . path)."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((path (cond ((derived-mode-p 'dired-mode)
                          (dired-current-directory))
                         (t (buffer-file-name)))))
        (if path
            ;; File visiting buffer and base buffers (not carbon copies)
            (cons (buffer-name) path)
          ;; This buffer is not visiting a file or it is a carbon copy
          nil)))))

(defun easysession--get-indirect-buffer-info (indirect-buffer)
  "Get information about the indirect buffer INDIRECT-BUFFER.

This function retrieves details about the indirect buffer BUF and its base
buffer. It returns a list of cons cells containing the names of both buffers,
the point position, window start position, and horizontal scroll position of
the base buffer.

- BUF: The buffer to get information from.

Return:
A list of cons cells: ((indirect-buffer-name . name-of-indirect-buffer)
                       (base-buffer-name . name-of-base-buffer)
                       (base-buffer-point . point-position)
                       (base-buffer-window-start . window-start-position)
                       (base-buffer-hscroll . horizontal-scroll-position))

Return nil if BUF is not an indirect buffer or if the base buffer cannot be
determined."
  (when (and indirect-buffer (buffer-live-p indirect-buffer))
    (let* ((base-buffer (buffer-base-buffer indirect-buffer)))
      (when (and base-buffer
                 (buffer-live-p base-buffer)
                 (not (eq base-buffer indirect-buffer))
                 ;; The base has to be a file visiting buffer
                 (or (buffer-file-name base-buffer)
                     (derived-mode-p 'dired-mode)))
        (with-current-buffer indirect-buffer  ; indirect buffer
          (let ((base-buffer-name (buffer-name base-buffer))
                (indirect-buffer-name (buffer-name)))
            (when (and base-buffer-name indirect-buffer-name)
              `((indirect-buffer-name . ,indirect-buffer-name)
                (base-buffer-name . ,base-buffer-name)))))))))
(defun easysession--get-session-file-name (session-name)
  "Return the fully qualified session file name for SESSION-NAME."
  (when session-name
    (easysession--ensure-session-name-valid session-name)
    (expand-file-name session-name easysession-directory)))

(defun easysession--save-frameset (session-name
                                   &optional save-geometry)
  "Return a frameset for FRAME-LIST, a list of frames.
SESSION-NAME is the session name.
When SAVE-GEOMETRY is non-nil, include the frame geometry."
  (let ((modified-filter-alist
         (if save-geometry
             ;; Include geometry
             (easysession--init-frame-parameters-filters
              easysession--overwrite-frameset-filter-include-geometry-alist)
           ;; Exclude geometry
           (easysession--init-frame-parameters-filters
            easysession--overwrite-frameset-filter-alist))))
    (frameset-save nil
                   :app `(easysession . ,easysession-file-version)
                   :name session-name
                   :predicate #'easysession--check-dont-save
                   :filters modified-filter-alist)))

(defun easysession--load-frameset (session-info session-name &optional load-geometry)
  "Load the frameset from the SESSION-INFO argument.
SESSION-NAME is the session name.
When LOAD-GEOMETRY is non-nil, load the frame geometry."
  (let* ((key (if load-geometry
                  "frameset-geo"
                "frameset"))
         (data (when (assoc key session-info)
                 (assoc-default key session-info))))
    (when (and (not data) load-geometry)
      (setq data (when (assoc "frameset" session-info)
                   (assoc-default "frameset" session-info))))
    (when data
      (unless (ignore-errors
                (frameset-restore data
                                  :reuse-frames t
                                  :force-display t
                                  :force-onscreen nil
                                  :cleanup-frames t)
                t)
        (easysession--warning "%s: Failed to restore the frameset:"
                              session-name)))))

(defun easysession--ensure-buffer-name (buffer name)
  "Ensure that BUFFER name is NAME."
  (interactive)
  (when (not (string= (buffer-name buffer) name))
    (with-current-buffer buffer
      (rename-buffer name t))))

(defun easysession--handler-load-base-buffers (session-info)
  "Load base buffers from the SESSION-INFO variable."
  (let ((buffer-file-names (assoc-default "buffers" session-info)))
    (when buffer-file-names
      (dolist (buffer-name-and-path buffer-file-names)
        (let ((buffer-name (car buffer-name-and-path))
              (buffer-path (cdr buffer-name-and-path)))
          (let* ((buffer (ignore-errors (find-file-noselect buffer-path t))))
            (if buffer
                ;; We are going to be using buffer-base-buffer to make sure that
                ;; the buffer that was returned by find-file-noselect is a base
                ;; buffer and not a clone
                (let* ((base-buffer (buffer-base-buffer buffer))
                       (buffer (if base-buffer base-buffer buffer)))
                  (when (and buffer (buffer-live-p buffer))
                    (easysession--ensure-buffer-name buffer buffer-name)))
              (easysession--warning "Failed to restore the buffer '%s': %s"
                                    buffer-name buffer-path))))))))

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
                (let ((indirect-buffer
                       (ignore-errors (clone-indirect-buffer
                                       indirect-buffer-name nil))))
                  (if indirect-buffer
                      (easysession--ensure-buffer-name indirect-buffer
                                                       indirect-buffer-name)
                    (easysession--warning
                     (concat "Failed to restore the indirect "
                             "buffer (clone): %s")
                     indirect-buffer-name)))))))))))

(defun easysession--check-dont-save (frame)
  "Check if FRAME is a real frame and should be saved.
Also checks if `easysession-dont-save' is set to t."
  ;; Exclude frames with a parent frame. One common use of the parent-frame
  ;; parameter is in the context of child frames, often used by packages like
  ;; posframe, which create transient, overlay, or tooltip-like frames. These
  ;; child frames are associated with a parent frame to maintain a logical and
  ;; spatial relationship.
  (and (not (frame-parameter frame 'parent-frame))
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
    (easysession--message "Session deleted: %s" session-name)))

(defun easysession-get-current-session-name ()
  "Return the name of the current session."
  easysession--current-session-name)

(defun easysession-get-session-name ()
  "Return the name of the current session."
  easysession--current-session-name)

(defun easysession-rename (&optional new-session-name)
  "Rename the current session to NEW-SESSION-NAME."
  (interactive)
  (unless new-session-name
    (setq new-session-name (easysession--prompt-session-name
                            (format "Rename session '%s' to: "
                                    (easysession-get-session-name)))))
  (let* ((old-path (easysession--get-session-file-name
                    easysession--current-session-name))
         (new-path (easysession--get-session-file-name new-session-name)))
    (unless (file-regular-p old-path)
      (error "[easysession] No such file or directory: %s" old-path))
    (rename-file old-path new-path)
    (setq easysession--current-session-name new-session-name)))

(defun easysession-save (&optional session-name)
  "Save the current session.
SESSION-NAME is the name of the session."
  (interactive)
  (run-hooks 'easysession-before-save-hook)
  (let* ((session-name (if session-name
                           session-name
                         (easysession-get-session-name)))
         (session-file (easysession--get-session-file-name session-name))
         (data-frameset (easysession--save-frameset session-name))
         (data-frameset-geometry (easysession--save-frameset
                                  session-name t))
         (session-data nil)
         (session-dir (file-name-directory session-file)))

    ;; Handlers
    (push (cons "frameset" data-frameset) session-data)
    (push (cons "frameset-geo" data-frameset-geometry) session-data)

    ;; Buffers and file buffers
    (let ((file-editing-buffers)
          (indirect-buffers))
      (dolist (buf (buffer-list))
        (let ((indirect-buf-info (easysession--get-indirect-buffer-info buf)))
          (if indirect-buf-info
              ;; Indirect buffers
              (push indirect-buf-info indirect-buffers)
            ;; File editing buffers and dired buffers
            (let ((path (easysession--get-base-buffer-path buf)))
              (when path
                (push path file-editing-buffers))))))

      (push (cons "buffers" file-editing-buffers) session-data)
      (push (cons "indirect-buffers" indirect-buffers) session-data))

    (unless (file-directory-p session-dir)
      (make-directory session-dir t))

    (let ((fwrite-success (progn (f-write (prin1-to-string session-data)
                                          'utf-8 session-file)
                                 t)))
      (if fwrite-success
          (when (called-interactively-p 'any)
            (easysession--message "Session saved: %s" session-name)
            (run-hooks 'easysession-after-save-hook))
        (error "[easysession] %s: failed to save the session to %s"
               session-name session-file)))
    t))

(defun easysession-load (&optional session-name)
  "Load the current session. SESSION-NAME is the session name."
  (interactive)
  (setq easysession--is-loading nil)
  (let* ((easysession--is-loading t)
         (session-name (if session-name
                           session-name
                         easysession--current-session-name))
         (session-info nil)
         (session-file (easysession--get-session-file-name session-name)))
    (when (and session-file (file-exists-p session-file))
      ;; Load file
      (setq file-contents (ignore-errors (f-read session-file)))
      (when file-contents
        (setq file-contents (string-trim file-contents)))
      (when (or (not file-contents) (string= file-contents ""))
        (error "[easysession] %s: Failed to read session information from %s"
               session-name session-file))

      ;; Evaluate file
      (setq session-info (ignore-errors (read file-contents)))
      (when (not session-info)
        (error
         "[easysession] %s: Failed to evaluate session information from %s"
         session-name session-file))

      (run-hooks 'easysession-before-load-hook)
      ;; Load buffers first because the cursor might be changed by packages such
      ;; as saveplace. This will allow frameset to change the cursor later
      ;; on.
      (easysession--handler-load-base-buffers session-info)
      (easysession--handler-load-indirect-buffers session-info)

      (easysession--load-frameset session-info
                                  session-name
                                  easysession--load-geometry)
      (setq easysession--current-session-loaded t)
      (when (called-interactively-p 'any)
        (easysession--message "Session loaded: %s" session-name))
      (run-hooks 'easysession-after-load-hook)
      t)))

(defun easysession-load-including-geometry (&optional session-name)
  "Load the session and restore the position and size of the Emacs frames.
SESSION-NAME is the session name.

This function is typically used when Emacs is initially loaded. It ensures that
session settings, including the positions and sizes (geometry) of all frames,
are restored.

For subsequent session switching, consider using `easysession-load' or
`easysession-switch-to', which load the session without resizing or moving the
Emacs frames."
  (let ((easysession--load-geometry t))
    (easysession-load session-name)))

(defun easysession-save-as (&optional session-name)
  "Save the state of all frames into a session with the given name.
If SESSION-NAME is provided, use it; otherwise, use current session.
If the function is called interactively, ask the user."
  (interactive)
  (unless session-name
    (setq session-name
          (easysession-get-current-session-name)))
  (let* ((new-session-name (if (called-interactively-p 'any)
                               (easysession--prompt-session-name
                                "Save session as: " session-name)
                             session-name))
         (previous-session-name easysession--current-session-name))
    (easysession--ensure-session-name-valid new-session-name)
    (easysession-save new-session-name)
    (easysession-set-current-session new-session-name)
    (if (string= previous-session-name easysession--current-session-name)
        (easysession--message "Saved the session: %s" new-session-name)
      (easysession--message "Saved and switched to session: %s"
                            new-session-name))
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
         (session-reloaded (string= session-name
                                    easysession--current-session-name))
         (saved nil)
         (new-session nil))
    (when (and easysession--current-session-loaded (not session-reloaded))
      (easysession-save easysession--current-session-name)
      (setq saved t))
    (easysession-load session-name)

    (unless session-reloaded
      (easysession-set-current-session session-name)
      (unless (file-exists-p session-file)
        (run-hooks 'easysession-new-session-hook)
        (easysession-save)
        (setq new-session t)))

    (cond (session-reloaded
           (easysession--message "Reloaded session: %s" session-name))
          (saved
           (easysession--message "Saved and switched to %ssession: %s"
                                 (if new-session "new " "") session-name))
          (t (easysession--message "Switched to %ssession: %s"
                                   (if new-session "new " "") session-name)))))

;;;###autoload
(define-minor-mode easysession-save-mode
  "Toggle `easysession-save-mode'."
  :global t
  :lighter " EasySess"
  :group 'easysession
  (if easysession-save-mode
      (progn
        (when (and easysession-save-interval
	                 (null easysession--timer))
          (setq easysession--timer
	              (run-with-timer easysession-save-interval
			                          easysession-save-interval
                                #'easysession-save)))
        (add-hook 'kill-emacs-hook #'easysession-save))
    (when easysession--timer
      (cancel-timer easysession--timer)
      (setq easysession--timer nil))
    (remove-hook 'kill-emacs-hook #'easysession-save)))

(provide 'easysession)
;;; easysession.el ends here
