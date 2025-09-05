;;; easysession.el --- Persist and restore your sessions (desktop.el alternative) -*- lexical-binding: t; -*-

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
;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers (clones), Dired buffers,
;; the tab-bar, and Emacs frames (including or excluding the geometry: frame
;; size, width, and height). It offers a convenient and effortless way to manage
;; Emacs editing sessions and utilizes built-in Emacs functions to persist and
;; restore frames.
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
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package easysession
;;   :ensure t
;;   :custom
;;   ;; Interval between automatic session saves
;;   (easysession-save-interval (* 10 60))
;;   ;; Make the current session name appear in the mode-line
;;   (easysession-mode-line-misc-info t)
;;   :init
;;   (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
;;   (add-hook 'emacs-startup-hook #'easysession-save-mode 103))
;;
;; Usage:
;; ------
;; It is recommended to use the following functions:
;; - (easysession-switch-to) to switch to another session or (easysession-load)
;;   to reload the current one,
;; - (easysession-save-as) to save the current session as the current name or
;;   another name.
;;
;; To facilitate session management, consider using the following key mappings:
;; C-c l for switching sessions with easysession-switch-to, and C-c s for
;; saving the current session with easysession-save-as:
;;   (global-set-key (kbd "C-c l") 'easysession-switch-to)
;;   (global-set-key (kbd "C-c s") 'easysession-save-as)
;;
;; Links:
;; ------
;; - More information about easysession (Frequently asked questions, usage...):
;;   https://github.com/jamescherti/easysession.el

;;; Code:

(require 'frameset)

;;; Variables

(defgroup easysession nil
  "Customization options for EasySession."
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
  :type 'hook
  :group 'easysession)

(defcustom easysession-after-load-hook nil
  "Hooks to run after the session is loaded.
Each element should be a function to be called with no arguments."
  :type 'hook
  :group 'easysession)

(defcustom easysession-before-save-hook nil
  "Hooks to run before the session is saved.
Each element should be a function to be called with no arguments."
  :type 'hook
  :group 'easysession)

(defcustom easysession-after-save-hook nil
  "Hooks to run after the session is saved.
Each element should be a function to be called with no arguments."
  :type 'hook
  :group 'easysession)

(defcustom easysession-new-session-hook nil
  "Hooks to run after a new session is created.
Each element should be a function to be called with no arguments.

This can be used to customize behavior, such as emptying a session
after a new one is created."
  :type 'hook
  :group 'easysession)

(defcustom easysession-before-reset-hook nil
  "Hooks to run before the `easysession-reset' function.
Each element should be a function to be called with no arguments."
  :type 'hook
  :group 'easysession)

(defcustom easysession-after-reset-hook nil
  "Hooks to run after the `easysession-reset' function.
Each element should be a function to be called with no arguments."
  :type 'hook
  :group 'easysession)

(defcustom easysession-quiet nil
  "If non-nil, suppress all messages and only show errors and warnings.
This includes messages such as `Session deleted:`, `Session loaded:`, `Session
saved:`, etc."
  :type 'boolean
  :group 'easysession)

(defcustom easysession-save-interval nil
  "The interval between automatic session saves.
If set to nil, it disables timer-based autosaving. Automatic session saves are
activated when `easysession-save-mode' is enabled."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'easysession)

;; Mode line
(defface easysession-mode-line-session-name-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used in the mode-line to indicate the current session.")

(defcustom easysession-mode-line-misc-info-format
  '(:eval (easysession--mode-line-session-name-format))
  "Mode-line format used to display the session name."
  :type 'sexp
  :group 'easysession
  :set (lambda (symbol value)
         (set symbol value)
         (setq mode-line-misc-info
               (assq-delete-all 'easysession-mode-line-misc-info
                                mode-line-misc-info))
         (add-to-list 'mode-line-misc-info `(easysession-mode-line-misc-info
                                             ,value))))
(put 'easysession-mode-line-misc-info-format 'risky-local-variable t)

(defcustom easysession-mode-line-misc-info nil
  "If non-nil, add `easysession` to `mode-line-misc-info'. If nil, remove it."
  :type 'boolean
  :group 'easysession
  :set (lambda (symbol value)
         (set symbol value)
         (setq mode-line-misc-info
               (assq-delete-all
                'easysession-mode-line-misc-info mode-line-misc-info))
         (add-to-list 'mode-line-misc-info
                      `(easysession-mode-line-misc-info
                        easysession-mode-line-misc-info-format))))

(defvar easysession-mode-line-misc-info-prefix " [EasySession:"
  "Prefix string displayed before the session name in the mode line.")

(defvar easysession-mode-line-misc-info-suffix "] "
  "Suffix string displayed after the session name in the mode line.")

(defcustom easysession-switch-to-save-session t
  "Non-nil means save the current session when using `easysession-switch-to'."
  :type 'boolean
  :group 'easysession)

(defcustom easysession-switch-to-exclude-current nil
  "Non-nil to exclude the current session when switching sessions.

This can be useful to prevent accidental re-selection of the session already in
use, especially when cycling through or interactively selecting among available
sessions."
  :type 'boolean
  :group 'easysession)

;; Lighter
(defvar easysession-save-mode-lighter " EasySessionSv"
  "Default lighter string for `easysession-save-mode'.")

(defvar easysession-save-mode-lighter-show-session-name nil
  "If non-nil, display the session name in the lighter.")

(defvar easysession-save-mode-lighter-session-name-spec
  '((:eval (if easysession--current-session-name
               (list (format "%s[" easysession-save-mode-lighter)
                     (propertize easysession--current-session-name
                                 'face 'easysession-mode-line-session-name-face)
                     "]")
             easysession-save-mode-lighter)))
  "Mode line lighter specification for displaying the current session name.
This is only displayed when a session is active.")
(put 'easysession-save-mode-lighter-session-name-spec 'risky-local-variable t)

;; Other
(defcustom easysession-buffer-list-function #'buffer-list
  "Function used to retrieve the buffers for persistence and restoration.
This holds a function that returns a list of buffers to be saved and restored
during session management. By default, it is set to `buffer-list', which
includes all buffers. You can customize this variable to use a different
function, such as one that filters buffers based on visibility or other
criteria."
  :type 'function
  :group 'easysession)

(defun easysession--default-auto-save-predicate ()
  "Default predicate function for `easysession-save-predicate`.
This function always returns non-nil, ensuring the session is saved."
  t)

(defcustom easysession-save-mode-predicate
  #'easysession--default-auto-save-predicate
  "Predicate that determines if the session is saved automatically.
This function is called with no arguments and should return non-nil if
`easysession-save-mode' should save the session automatically. The default
predicate always returns non-nil, ensuring all sessions are saved
automatically."
  :type 'function
  :group 'easysession)

(define-obsolete-variable-alias
  'easysession-restore-frames
  'easysession-enable-frameset-restore
  "1.1.2"
  "Use `easysession-enable-frameset-restore' instead.")

(defcustom easysession-enable-frameset-restore t
  "Non-nil to restore frames.

When non-nil, frames will be restored alongside buffers when a session is
loaded.

If set to nil, only the buffers will be restored, and frame restoration will be
skipped.

See related options:
- `easysession-frameset-restore-reuse-frames'
- `easysession-frameset-restore-force-display'
- `easysession-frameset-restore-force-onscreen'
- `easysession-frameset-restore-cleanup-frames'"
  :type 'boolean
  :group 'easysession)

(defcustom easysession-frameset-restore-reuse-frames t
  "Specifies the policy for reusing frames when restoring:
t        All existing frames can be reused.
nil      No existing frames can be reused.
match    Only frames with matching frame IDs can be reused.
PRED     A predicate function that receives a live frame as an argument
         and returns non-nil to allow reusing it, or nil otherwise.

For more details, see the `frameset-restore' docstring."
  :type '(choice (const :tag "Reuse all frames" t)
                 (const :tag "Reuse no frames" nil)
                 (const :tag "Reuse frames with matching IDs" match)
                 (function :tag "Predicate function"))
  :group 'easysession)

(defcustom easysession-frameset-restore-force-display t
  "Specifies how frames are restored with respect to display:
t        Frames are restored on the current display.
nil      Frames are restored, if possible, on their original displays.
delete   Frames in other displays are deleted instead of being restored.
PRED     A function called with two arguments: the parameter alist and
         the window state (in that order). It must return t, nil, or
         delete, as described above, but affecting only the frame
         created from that parameter alist.

For more details, see the `frameset-restore' docstring."
  :type '(choice (const :tag "Restore on current display" t)
                 (const :tag "Restore on original displays" nil)
                 (const :tag "Delete frames in other displays" delete)
                 (function :tag "Function to determine frame restoration"))
  :group 'easysession)

(defcustom easysession-frameset-restore-force-onscreen (display-graphic-p)
  "Specifies how frames are handled when they are offscreen:
t        Only frames that are completely offscreen are forced onscreen.
nil      No frames are forced back onscreen.
all      Any frame that is fully or partially offscreen is forced onscreen.
PRED     A function called with three arguments:
         - The live frame just restored.
         - A list (LEFT TOP WIDTH HEIGHT) describing the frame.
         - A list (LEFT TOP WIDTH HEIGHT) describing the work area.
         It must return non-nil to force the frame onscreen, or nil otherwise.

For more details, see the `frameset-restore' docstring."
  :type '(choice
          (const :tag "Force onscreen only fully offscreen frames" t)
          (const :tag "Do not force any frames onscreen" nil)
          (const
           :tag "Force onscreen any frame fully or partially offscreen" all)
          (function :tag "Function to determine onscreen status"))
  :group 'easysession)

(defcustom easysession-frameset-restore-cleanup-frames t
  "Specifies the policy for cleaning up the frame list after restoring.
t        Delete all frames that were not created or restored.
nil      Retain all frames.
FUNC     A function called with two arguments:
         - FRAME, a live frame.
         - ACTION, which can be one of:
           :rejected  Frame existed but was not a candidate for reuse.
           :ignored   Frame existed, was a candidate, but was not reused.
           :reused    Frame existed, was a candidate, and was reused.
           :created   Frame did not exist, was created and restored upon.
         The return value is ignored.

For more details, see the `frameset-restore' docstring."
  :type '(choice (const :tag "Delete all unneeded frames" t)
                 (const :tag "Retain all frames" nil)
                 (function :tag "Function to determine cleanup actions"))
  :group 'easysession)

(defvar easysession-frameset-restore-geometry nil
  "If non-nil, `easysession-load' restores frame position and size.

Do not modify this variable directly; use `easysession-load-including-geometry'
instead.

Set this variable to t only if you want `easysession-load' or
`easysession-switch-to' to always restore frame geometry.

By default, this variable is nil, meaning `easysession-load' does not restore
geometry.")

(defcustom easysession-exclude-from-find-file-hook '(recentf-track-opened-file)
  "List of hooks to be excluded from `find-file-hook'.

When EasySession restores a file editing buffer using `find-file-noselect', the
functions in this list are skipped and not executed by `find-file-hook'. This
provides control over which hooks should be bypassed during the file restoration
process, ensuring that certain actions (e.g., tracking opened files) are not
triggered in this context."
  :type '(repeat symbol)
  :group 'easysession)

;;; Internal variables

(defvar easysession--debug nil)

(defvar easysession--timer nil)

;; Overrides `frameset-filter-alist' while preserving its keys,
;; but replaces their values with the ones specified in the following alist:
(defvar easysession--overwrite-frameset-filter-alist
  '(; Same as `frameset-persistent-filter-alist'
    (background-color . :never)
    (bottom . :never)
    (buffer-list . :never)
    (buffer-predicate . :never)
    (buried-buffer-list . :never)
    (client . :never)
    (delete-before . :never)
    (font . :never)
    (font-backend . :never)
    (foreground-color . :never)
    (frameset--text-pixel-height . :never)
    (frameset--text-pixel-width . :never)
    (fullscreen . :never)
    (GUI:bottom . :never)
    (GUI:font . :never)
    (GUI:fullscreen . :never)
    (GUI:height . :never)
    (GUI:left . :never)
    (GUI:right . :never)
    (GUI:top . :never)
    (GUI:width . :never)
    (height . :never)
    (left . :never)
    (parent-frame . :never)
    (mouse-wheel-frame . :never)
    (right . :never)
    (top . :never)
    ;; (tty . :never)
    ;; (tty-type . :never)
    (width . :never)
    (window-system . :never)

    ;; Affect the geometry, but let the user configure them
    (internal-border-width . :never)
    (child-frame-border-width . :never)
    (left-fringe . :never)
    (right-fringe . :never)
    (bottom-divider-width . :never)
    (right-divider-width . :never)

    ;; Let the user configure the scroll bars, tool-bar, and menu-bar
    (vertical-scroll-bars . :never)
    (horizontal-scroll-bars . :never)
    (scroll-bar-width . :never)
    (scroll-bar-height . :never)
    (menu-bar-lines . :never)
    (tool-bar-lines . :never)
    (tool-bar-position . :never)
    (line-spacing . :never)
    (no-special-glyphs . :never)

    (scroll-bar-background . :never)
    (scroll-bar-foreground . :never)

    ;; Can be changed by the window manager
    (skip-taskbar . :never)
    (sticky . :never)
    (shaded . :never)
    (undecorated . :never)
    (border-width . :never)

    ;; Allow restoring alpha and tab-bar-lines
    ;; (alpha . :never)  ; transparency
    ;; (alpha-background . :never)  ; transparency
    ;; Never exclude: tab-bar-lines

    ;; Other exclusions
    (override-redirect . :never)
    (auto-lower . :never)  ; control focus behavior
    (auto-raise . :never)  ; control focus behavior
    (background-mode . :never)  ; Affects theme/light/dark mode
    (border-color . :never)
    (child-frame-border-width . :never)
    (cursor-color . :never)
    (cursor-type . :never)
    (display-type . :never)
    (environment . :never)
    (font-parameter . :never)
    (icon-type . :never)
    (inhibit-double-buffering . :never)
    (minibuffer . :never)
    (mouse-color . :never)
    (name . :never)
    (no-accept-focus . :never)
    (no-focus-on-map . :never)
    (ns-appearance . :never)
    (outer-window-id . :never)
    (parent-id . :never)
    (screen-gamma . :never)
    (use-frame-synchronization . :never)
    (window-id . :never)

    ;; Ensures frames are positioned only after the window manager maps them,
    ;; helping avoid small shifts in geometry.
    ;; Let the user configure this with:
    ;;   (add-to-list 'default-frame-alist '(wait-for-wm . t))

    ;; Do not restore wait-for-wm. Let the user configure it.
    (wait-for-wm . :never)

    ;; Restore the frame title
    ;; (title . :never)

    ;; Restore z-group parameter, which controls stacking or z-order
    ;; grouping of frames in Emacs, which affects how frames are layered
    ;; relative to each other on the screen.
    ;; (z-group . :never)

    ;; Fixes #24: Restoring a saved session does not restore all frames It was
    ;;            caused by: (visibility . :never).
    ;; (visibility . :never)

    ;; Third party package (Fixes #22)
    (lsp-ui-doc-buffer . :never)

    ;; Third party package (Fixes #32)
    (dv-preview-last . :never))
  "Alist of frame parameters to keep.")

(defun easysession--filter-out-frameset-filters (list-keys)
  "Remove geometry.
LIST-KEYS is the list of keys (e.g., GUI:left, bottom, height...)
from `easysession--overwrite-frameset-filter-alist`."
  (seq-remove (lambda (entry)
                (memq (car entry) list-keys))
              easysession--overwrite-frameset-filter-alist))

(defvar easysession--overwrite-frameset-filter-include-geometry-alist
  (easysession--filter-out-frameset-filters
   '(; Same as `frameset-persistent-filter-alist'
     bottom
     fullscreen
     GUI:bottom
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
     width

     ;; TODO experimental
     ;; vertical-scroll-bars
     ;; horizontal-scroll-bars
     ;; scroll-bar-width
     ;; scroll-bar-height
     ;; menu-bar-lines
     ;; tool-bar-lines
     ;; tool-bar-position
     ;; line-spacing
     ;; no-special-glyphs

     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Layout-Parameters.html

     ;; The frame is hidden from the taskbar or panel.
     skip-taskbar

     ;; The frame is pinned to all virtual desktops/workspaces.
     sticky

     ;; The frame is rolled up, usually showing only the title bar.
     shaded

     ;; Decoration effect the total frame height
     undecorated

     ;; border-width controls the thickness of the frame’s outer border (on all
     ;; sides) in pixels.
     border-width

     ;; Internal border width is the padding between the text area and the outer
     ;; frame edge.
     internal-border-width

     ;; The width in pixels of the frame’s internal border (see Frame Geometry)
     ;; if the given frame is a child frame (see Child Frames). If this is nil,
     ;; the value specified by the internal-border-width parameter is used
     ;; instead.
     child-frame-border-width

     ;; Define the width (in pixels) of the left and right fringes, the narrow
     ;; areas next to the text area where indicators like git-gutter or
     ;; display-line-numbers appear. Restoring them ensures the text area width
     ;; and overall frame width remain consistent. Changes to these values can
     ;; shift text horizontally and slightly alter total frame width.
     left-fringe
     right-fringe

     ;; Specify the thickness of dividers separating the text area from the
     ;; bottom or right edges (or between window splits). Restoring them
     ;; preserves the exact pixel size of the text area and prevents small
     ;; offsets in frame height or width. Ignoring these can cause frames to
     ;; appear slightly larger or smaller than when saved.
     bottom-divider-width
     right-divider-width

     ;; Pixel perfect width and height
     frameset--text-pixel-height
     frameset--text-pixel-width))
  "Alist of frame parameters to keep.")

(defvar easysession-file-version 3
  "Version number of easysession file format.")

(defvar easysession--current-session-name nil
  "Current session.")

(defvar easysession--load-error nil
  "Non-nil indicates whether loading the current session has failed.

This variable is non-nil if an error occurred while attempting to load
the current session, otherwise it remains nil.")

(defvar easysession--load-handlers '()
  "A list of functions used to load session data.
Each function in this list is responsible for loading a specific type of
buffer (e.g., file editing buffers, indirect buffers) from the session
information. These functions are applied sequentially to restore the session
state based on the saved session data.")

(defvar easysession--save-handlers '()
  "A list of functions used to save session data.
Each function in this list is responsible for saving a specific type of
buffer (e.g., file editing buffers, indirect buffers) from the current
session. These functions are applied sequentially to capture the state of
the session, which can later be restored by the corresponding load handlers.")

(defvar easysession--builtin-load-handlers
  '(easysession--handler-load-file-editing-buffers
    easysession--handler-load-indirect-buffers)
  "Internal variable.")

(defvar easysession--builtin-save-handlers
  '(easysession--handler-save-file-editing-buffers
    easysession--handler-save-indirect-buffers)
  "Internal variable.")

(defvar easysession-load-in-progress nil
  "Session name (string) if a session is currently being loaded.
This is an internal variable that is meant to be read-only. Do not modify it.
This variable is used to indicate whether a session loading process is in
progress.")

;;; Internal functions

(defun easysession--message (&rest args)
  "Display a message with '[easysession]' prepended.
The message is formatted with the provided arguments ARGS."
  (unless easysession-quiet
    (apply #'message (concat "[easysession] " (car args)) (cdr args))))

(defun easysession--warning (&rest args)
  "Display a warning message with '[easysession] Warning: ' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[easysession] Warning: " (car args)) (cdr args)))

(defun easysession--ensure-session-name-valid (session-name)
  "Validate the provided SESSION-NAME.

If the SESSION-NAME is invalid, an error is raised with a message indicating
the invalid name.

Return the SESSION-NAME if it is valid.

Errors:
Raise an error if the session name is invalid."
  (when (or (not session-name)
            (string= session-name "")
            (let ((case-fold-search nil))
              (or (string-match-p "\\\\" session-name)
                  (string-match-p "/" session-name)))
            (string= session-name "..")
            (string= session-name "."))
    (user-error "[easysession] Invalid session name: %S" session-name))
  session-name)

(defun easysession-set-current-session-name (&optional session-name)
  "Set the current session name to SESSION-NAME.
Return t if the session name is successfully set."
  (easysession--ensure-session-name-valid session-name)
  (setq easysession--current-session-name session-name)
  t)

(defun easysession--set-current-session (&optional session-name)
  "Backward compatibility. SESSION-NAME is the session name."
  (easysession-set-current-session-name session-name))

(defun easysession--init-frame-parameters-filters (overwrite-alist)
  "Return the EasySession version of `frameset-filter-alist'.
OVERWRITE-ALIST is an alist similar to
`easysession--overwrite-frameset-filter-alist'."
  (let ((result (copy-tree frameset-filter-alist)))
    (dolist (pair overwrite-alist)
      (setq result (assq-delete-all (car pair) result))
      (push pair result))
    result))

(defun easysession--get-all-names (&optional exclude-current)
  "Return a list of all session names.
If EXCLUDE-CURRENT is non-nil, exclude the current session name from the list."
  (if (file-directory-p easysession-directory)
      (seq-filter (lambda (session-name)
                    (not (or (string-equal session-name ".")
                             (string-equal session-name "..")
                             (and exclude-current
                                  easysession--current-session-name
                                  (string= session-name
                                           easysession--current-session-name)))))
                  (directory-files easysession-directory nil nil t))
    '()))

(defun easysession--prompt-session-name (prompt &optional session-name
                                                exclude-current initial-input)
  "Prompt for a session name with PROMPT.
Use SESSION-NAME as the default value.
If EXCLUDE-CURRENT is non-nil, exclude the current session from
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
completion candidates."
  (completing-read (concat "[easysession] " prompt)
                   (easysession--get-all-names exclude-current)
                   nil nil initial-input nil session-name))

(defun easysession--get-base-buffer-info (buffer)
  "Get the name and path of the buffer BUFFER.
Return nil When the buffer is not a base buffer.
Return a cons cell (buffer-name . path)."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((path (cond ((derived-mode-p 'dired-mode)
                          default-directory)
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

(defun easysession-get-session-name ()
  "Return the name of the current session.
Return nil when no session has been loaded before."
  easysession--current-session-name)

(defalias 'easysession-get-current-session-name
  'easysession-get-session-name
  "Renamed to `easysession-get-session-name'.")

(make-obsolete 'easysession-get-current-session-name
               'easysession-get-session-name
               "1.1.3")

(defun easysession-get-session-file-path (&optional session-name)
  "Return the absolute path to the session file for SESSION-NAME.
If SESSION-NAME is nil, use the currently loaded session.
Return nil if no session is loaded."
  (unless session-name
    (setq session-name easysession--current-session-name))
  (unless session-name
    (user-error "%s%s"
                "[easysession] No session is active. "
                "Load a session with `easysession-switch-to'"))
  (when session-name
    (easysession--ensure-session-name-valid session-name)
    (expand-file-name session-name easysession-directory)))

(defun easysession--save-frameset (session-name
                                   &optional save-geometry)
  "Return a frameset for FRAME-LIST, a list of frames.
SESSION-NAME is the session name.
When SAVE-GEOMETRY is non-nil, include the frame geometry.
Return nil if there is no frame to save."
  (let ((modified-filter-alist
         (if save-geometry
             ;; Include geometry
             (easysession--init-frame-parameters-filters
              easysession--overwrite-frameset-filter-include-geometry-alist)
           ;; Exclude geometry
           (easysession--init-frame-parameters-filters
            easysession--overwrite-frameset-filter-alist))))
    ;; Auto save when there is at least one frame
    (when (frame-list)
      (frameset-save nil
                     :app `(easysession . ,easysession-file-version)
                     :name session-name
                     :predicate #'easysession--check-dont-save
                     :filters modified-filter-alist))))

(defun easysession--can-restore-frameset-p ()
  "True if calling `easysession--load-frameset' will actually restore it."
  (and easysession-enable-frameset-restore
       ;; Skip restoring frames when the current frame is the daemon's initial
       ;; frame.
       (not (and (daemonp)
                 (not (frame-parameter nil 'client))))
       t))

(defun easysession--load-frameset (session-data &optional load-geometry)
  "Load the frameset from the SESSION-DATA argument.
When LOAD-GEOMETRY is non-nil, load the frame geometry."
  (when (and easysession-enable-frameset-restore
             (easysession--can-restore-frameset-p))
    (let* ((key (if load-geometry
                    "frameset-geo"
                  "frameset"))
           (data (when (assoc key session-data)
                   (assoc-default key session-data))))
      (when (and (not data) load-geometry)
        (setq data (when (assoc "frameset" session-data)
                     (assoc-default "frameset" session-data))))
      (when data
        (unless (ignore-errors
                  (frameset-restore
                   data
                   :reuse-frames easysession-frameset-restore-reuse-frames
                   :cleanup-frames easysession-frameset-restore-cleanup-frames
                   :force-display easysession-frameset-restore-force-display
                   :force-onscreen easysession-frameset-restore-force-onscreen)

                  (when (fboundp 'tab-bar-mode)
                    (when (seq-some
                           (lambda (frame)
                             (menu-bar-positive-p
                              (frame-parameter frame 'tab-bar-lines)))
                           (frame-list))
                      (tab-bar-mode 1)))

                  ;; Return t
                  t)
          (easysession--warning "%s: Failed to restore the frameset"))))))

(defun easysession--ensure-buffer-name (buffer name)
  "Ensure that BUFFER name is NAME."
  (when (not (string= (buffer-name buffer) name))
    (with-current-buffer buffer
      (rename-buffer name t))))

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

(defun easysession--exists (session-name)
  "Check if a session with the given SESSION-NAME exists.
Returns the session file if the session file exists, nil otherwise."
  (let ((file-name-handler-alist nil))
    (let ((file-name (easysession-get-session-file-path session-name)))
      (when (file-exists-p file-name)
        file-name))))

(defun easysession--auto-save ()
  "Save the session automatically based on the auto-save predicate.
This function is usually called by `easysession-save-mode'. It evaluates the
`easysession-save-mode-predicate' function."
  ;; Auto save when there is at least one frame and a session has been loaded
  (unwind-protect
      (progn
        (when (and (frame-list)
                   easysession--current-session-name)
          (if (funcall easysession-save-mode-predicate)
              (easysession-save)
            (when easysession--debug
              (easysession--message
               (concat
                "[DEBUG] Auto-save ignored: `easysession-save-mode-predicate' "
                "returned nil.")))))
        ;; Always return t, since this `easysession--auto-save' is part of
        ;; `kill-emacs-query-functions'. Returning nil would prevent Emacs from
        ;; exiting.
        t)
    ;; Always return t, since this `easysession--auto-save' is part of
    ;; `kill-emacs-query-functions'. Returning nil would prevent Emacs from
    ;; exiting.
    t))

(defun easysession--mode-line-session-name-format ()
  "Return the mode-line representation of the current EasySession.
The current session is displayed only when a session is actively loaded."
  (if (bound-and-true-p easysession--current-session-name)
      (let* ((session-name (eval easysession--current-session-name)))
        (list
         easysession-mode-line-misc-info-prefix
         (propertize session-name
                     'face 'easysession-mode-line-session-name-face
                     'help-echo (format "Current session: %s" session-name)
                     'mouse-face 'mode-line-highlight)
         easysession-mode-line-misc-info-suffix))))

(defun easysession--get-scratch-buffer-create ()
  "Return the *scratch* buffer, creating a new one if needed."
  (if (fboundp 'get-scratch-buffer-create)
      (funcall 'get-scratch-buffer-create)
    (or (get-buffer "*scratch*")
        (let ((scratch (get-buffer-create "*scratch*")))
          (with-current-buffer scratch
            (when initial-scratch-message
              (insert (substitute-command-keys initial-scratch-message))
              (set-buffer-modified-p nil))
            (funcall initial-major-mode)
            (when (eq initial-major-mode 'lisp-interaction-mode)
              (with-no-warnings
                (setq-local trusted-content :all))))
          scratch))))

;;; Internal functions: handlers

(defun easysession--handler-load-file-editing-buffers (session-data)
  "Load base buffers from the SESSION-DATA variable."
  (let ((buffer-file-names (assoc-default "buffers" session-data)))
    (when buffer-file-names
      (dolist (buffer-name-and-path buffer-file-names)
        (let ((buffer-name (car buffer-name-and-path))
              (buffer-path (cdr buffer-name-and-path)))
          (let* ((buffer (get-file-buffer buffer-path)))
            (unless buffer
              (setq buffer
                    (ignore-errors
                      (let ((find-file-hook
                             (seq-difference find-file-hook
                                             easysession-exclude-from-find-file-hook)))
                        (find-file-noselect buffer-path :nowarn)))))
            (if (and buffer (buffer-live-p buffer))
                (progn
                  ;; We are going to be using buffer-base-buffer to make sure
                  ;; that the buffer that was returned by find-file-noselect is
                  ;; a base buffer and not a clone
                  (let* ((base-buffer (buffer-base-buffer buffer))
                         (buffer (if base-buffer
                                     base-buffer
                                   buffer)))
                    ;; Fixes the issue preventing font-lock-mode from fontifying
                    ;; restored buffers, causing the text to remain unfontified
                    ;; until the user presses a key.
                    (when (and
                           (bound-and-true-p font-lock-mode)
                           (bound-and-true-p
                            redisplay-skip-fontification-on-input)
                           (fboundp 'jit-lock-fontify-now))
                      (with-current-buffer buffer
                        (ignore-errors (jit-lock-fontify-now))))

                    (when buffer
                      (easysession--ensure-buffer-name buffer buffer-name))))
              (easysession--warning "Failed to restore the buffer '%s': %s"
                                    buffer-name buffer-path))))))))

(defun easysession--handler-save-file-editing-buffers (buffers)
  "Collect and categorize file editing buffers from the provided list.
BUFFERS is the list of buffers to process. This function identifies buffers
that are associated with files (file editing buffers) and those that are
not. It returns an alist with the following structure."
  (let ((file-editing-buffers '())
        (remaining-buffers '()))
    (dolist (buf buffers)
      (let ((base-buffer-info (easysession--get-base-buffer-info buf)))
        (if base-buffer-info
            (push base-buffer-info file-editing-buffers)
          (push buf remaining-buffers))))
    `((key . "buffers")
      (value . ,file-editing-buffers)
      (remaining-buffers . ,remaining-buffers))))

(defun easysession--handler-load-indirect-buffers (session-data)
  "Load indirect buffers from the SESSION-DATA variable."
  (let ((indirect-buffers (assoc-default "indirect-buffers"
                                         session-data)))
    (when indirect-buffers
      (dolist (item indirect-buffers)
        (let* ((indirect-buffer-name (alist-get 'indirect-buffer-name item))
               (base-buffer-name (alist-get 'base-buffer-name (cdr item))))
          (when (and base-buffer-name indirect-buffer-name)
            (let ((base-buffer (get-buffer base-buffer-name))
                  (indirect-buffer (get-buffer indirect-buffer-name)))
              (when (and (not (buffer-live-p indirect-buffer))
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
                       indirect-buffer-name))))))))))))

(defun easysession--handler-save-indirect-buffers (buffers)
  "Collect and categorize indirect buffers from the provided list.
BUFFERS is the list of buffers to process. This function identifies indirect
buffers and separates them from other buffers."
  (let ((indirect-buffers '())
        (remaining-buffers '()))
    (dolist (buf buffers)
      (let ((indirect-buffer-info (easysession--get-indirect-buffer-info buf)))
        (if indirect-buffer-info
            (push indirect-buffer-info indirect-buffers)
          (push buf remaining-buffers))))
    `((key . "indirect-buffers")
      (value . ,indirect-buffers)
      (remaining-buffers . ,remaining-buffers))))

(defun easysession-add-load-handler (handler-fn)
  "Add a load handler.
The handler is only added if it's not already present and if HANDLER-FN is a
symbol representing an existing function. HANDLER-FN is the function to load
session data."
  (unless (and (symbolp handler-fn)
               (fboundp handler-fn))
    (error "[easysession] HANDLER-FN must be a symbol representing a function"))
  (unless (memq handler-fn easysession--load-handlers)
    (setq easysession--load-handlers
          (append easysession--load-handlers (list handler-fn)))))

(defun easysession-add-save-handler (handler-fn)
  "Add a save handler.
HANDLER-FN is the function to save session data.
The HANDLER-FN handler is only added if it's not already present."
  (unless (and (symbolp handler-fn)
               (fboundp handler-fn))
    (error "[easysession] HANDLER-FN must be a symbol representing a function"))
  (unless (memq handler-fn easysession--save-handlers)
    ;; (push handler-fn easysession--save-handlers)
    (setq easysession--save-handlers
          (append easysession--save-handlers (list handler-fn)))))

(defun easysession-remove-load-handler (handler-fn)
  "Remove a load handler.
HANDLER-FN is the function to load session data.
The HANDLER-FN handler is only added if it's not already present."
  (unless (and (symbolp handler-fn)
               (fboundp handler-fn))
    (error "[easysession] HANDLER-FN must be a symbol representing a function"))
  (setq easysession--load-handlers
        (delq handler-fn easysession--load-handlers)))

(defun easysession-remove-save-handler (handler-fn)
  "Remove a save handler.
HANDLER-FN is the function to be removed."
  (unless (and (symbolp handler-fn)
               (fboundp handler-fn))
    (error "[easysession] HANDLER-FN must be a symbol representing a function"))
  (setq easysession--save-handlers (delete handler-fn
                                           easysession--save-handlers)))

(defun easysession-get-save-handlers ()
  "Return a list of all built-in and user-defined save handlers."
  (append easysession--builtin-save-handlers
          easysession--save-handlers))

(defun easysession-get-load-handlers ()
  "Return a list of all built-in and user-defined load handlers."
  (append easysession--builtin-load-handlers
          easysession--load-handlers))

(defmacro easysession-define-load-handler (key handler-func)
  "Add a load handler for a specific session KEY.

KEY is the identifier used in the EasySession file. Avoid reserved keys like:
buffers, indirect-buffers, frameset, and frameset-geo. Prefix with an
underscore to be safe.

HANDLER-FUNC is a callable that is invoked with session data when the key is
found."
  (declare (indent 0) (debug t))
  `(progn
     (defun
         ,(intern (concat "easysession--" key "-load-handler")) (session-data)
       ,(format "Load handler for restoring: %s."
                key)
       (let ((handler-data (assoc-default ,key session-data)))
         (when handler-data
           (funcall ,handler-func handler-data))))

     (easysession-add-load-handler
      ',(intern (concat "easysession--" key "-load-handler")))))

(defmacro easysession-define-generic-save-handler (key &rest body)
  "Add a save handler to EasySession.

KEY is the identifier for this session data within EasySession. Prefix with an
underscore to be safe. Avoid using reserved keys such as: buffers,
indirect-buffers, frameset, and frameset-geo.

BODY is executed."
  (declare (indent 0) (debug t))
  `(progn
     (defun ,(intern (concat "easysession--" key "-save-handler")) (buffers)
       ,(format "Save handler for: %s." key)
       (when buffers
         t)  ;; Remove warnings
       ,@body)

     (easysession-add-save-handler
      ',(intern (concat "easysession--" key "-save-handler")))))

(defmacro easysession-define-save-handler (key handler-func)
  "Add a save handler to EasySession.

KEY is the identifier for this session data within EasySession. Prefix with an
underscore to be safe. Avoid using reserved keys such as: buffers,
indirect-buffers, frameset, and frameset-geo.

HANDLER-FUNC is a callable that processes each buffer and returns its session
data."
  (declare (indent 0) (debug t))
  `(easysession-define-generic-save-handler
     ,key
     (let ((result (funcall ,handler-func buffers)))
       (when result
         ;; TODO Check if buffers and remaining buffers exist
         (push (cons 'key ,key) result)))))

(defmacro easysession-define-handler (key load-handler-func save-handler-func)
  "Register both load and save handlers for a given KEY.

KEY is the session identifier. Avoid reserved keys.

LOAD-HANDLER-FUNC and SAVE-HANDLER-FUNC are functions for handling session
data."
  (declare (indent 0) (debug t))
  `(progn
     (easysession-define-load-handler ,key ,load-handler-func)
     (easysession-define-save-handler ,key ,save-handler-func)))

(defmacro easysession-save-handler-dolist-buffers (buffers &rest body)
  "Iterate over BUFFERS, execute BODY inside each buffer's context.
Classify buffers based on BODY's result.

Returns a list:
  ((buffers . SAVED-BUFFERS)
   (remaining-buffers . REMAINING-BUFFERS))"
  (declare (indent 0) (debug t))
  (let ((saved-buffers (make-symbol "saved-buffers"))
        (remaining-buffers (make-symbol "remaining-buffers"))
        (buffer (make-symbol "buffer"))
        (buffer-data (make-symbol "buffer-data")))
    `(let ((,saved-buffers nil)
           (,remaining-buffers nil)
           (,buffer-data nil)
           (,buffer nil))
       (dolist (,buffer ,buffers)
         (with-current-buffer ,buffer
           (let ((,buffer-data (progn ,@body)))
             (if ,buffer-data
                 (push ,buffer-data ,saved-buffers)
               (push ,buffer ,remaining-buffers)))))
       (list
        (cons 'buffers ,saved-buffers)
        (cons 'remaining-buffers ,remaining-buffers)))))



(defmacro easysession-undefine-load-handler (key)
  "Remove the load handler associated with KEY from EasySession."
  (declare (indent 0) (debug t))
  `(let ((fn ',(intern (concat "easysession--" key "-load-handler"))))
     (when (fboundp fn)
       (unintern fn nil))
     (setq easysession--load-handlers
           (delq fn easysession--load-handlers))))

(defmacro easysession-undefine-save-handler (key)
  "Remove the save handler associated with KEY from EasySession."
  (declare (indent 0) (debug t))
  `(let ((fn ',(intern (concat "easysession--" key "-save-handler"))))
     (when (fboundp fn)
       (unintern fn nil))
     (setq easysession--save-handlers
           (delq fn easysession--save-handlers))))

(defmacro easysession-undefine-handler (key)
  "Remove both load and save handlers associated with KEY from EasySession."
  (declare (indent 0) (debug t))
  `(progn
     (easysession-undefine-load-handler ,key)
     (easysession-undefine-save-handler ,key)))

;;; Autoloaded functions

;;;###autoload
(defun easysession-reset ()
  "Kill all buffers and close all frames, tabs, and windows."
  (interactive)
  ;; Hooks
  (run-hooks 'easysession-before-reset-hook)

  ;; Kill all buffers
  (mapc #'kill-buffer (cl-remove-if
                       (lambda (buffer)
                         (or (string= (buffer-name buffer) "*scratch*")
                             (string= (buffer-name buffer) "*Messages*")))
                       (buffer-list)))

  ;; Delete frames
  (delete-other-frames)

  ;; Close tabs
  (when (and (bound-and-true-p tab-bar-mode)
             (fboundp 'tab-bar-close-other-tabs))
    (tab-bar-close-other-tabs))

  ;; Close windows
  (delete-other-windows)

  ;; Switch to the scratch buffer
  (switch-to-buffer (easysession--get-scratch-buffer-create) nil t)

  ;; Hooks
  (run-hooks 'easysession-after-reset-hook))

;;;###autoload
(defun easysession-rename (new-session-name)
  "Rename the current session.
NEW-SESSION-NAME is the session name."
  (interactive
   (list
    (progn
      (unless easysession--current-session-name
        (error
         "No session is active. Load a session with `easysession-switch-to'"))

      (easysession--prompt-session-name
       (format "Rename session '%s' to: "
               easysession--current-session-name)
       nil
       nil
       easysession--current-session-name))))
  (unless easysession--current-session-name
    (user-error "%s%s"
                "[easysession] No session is active. "
                "Load a session with `easysession-switch-to'"))
  (unless new-session-name
    (user-error (concat "[easysession] easysession-rename: You need to specify"
                        "the new session name.")))
  (let* ((old-path (easysession-get-session-file-path
                    easysession--current-session-name))
         (new-path (easysession-get-session-file-path new-session-name)))
    (unless (file-regular-p old-path)
      (error "[easysession] No such file or directory: %s" old-path))
    (rename-file old-path new-path)
    (setq easysession--current-session-name new-session-name)))

;;;###autoload
(defun easysession-delete (session-name)
  "Delete a session.
SESSION-NAME is the session name."
  (interactive (list (easysession--prompt-session-name "Delete session: ")))
  (let* ((session-file (easysession--exists session-name)))
    (if session-file
        (progn
          (let ((session-buffer (find-buffer-visiting session-file)))
            (when session-buffer
              (kill-buffer session-buffer)))

          (delete-file session-file nil)
          (easysession--message "Session deleted: %s" session-name)
          t)
      (easysession--warning
       "The session '%s' cannot be deleted because it does not exist"
       session-name)
      nil)))

;;;###autoload
(defun easysession-load (&optional session-name)
  "Load the current session. SESSION-NAME is the session name."
  (interactive)
  (setq easysession-load-in-progress nil)
  (setq easysession--load-error t)

  ;; The default session loaded when none is specified is 'main'.
  (when (and (not session-name)
             (not easysession--current-session-name))
    (easysession--set-current-session "main"))

  (let* ((original-file-name-handler-alist file-name-handler-alist)
         (file-name-handler-alist nil)
         (session-name (if session-name
                           session-name
                         easysession--current-session-name))
         (easysession-load-in-progress session-name)
         (session-data nil)
         (file-contents nil)
         (session-file (easysession--exists session-name)))
    (when session-file
      ;; Load session
      (setq file-contents (let ((coding-system-for-read 'utf-8-emacs)
                                (file-coding-system-alist nil))
                            (with-temp-buffer
                              (insert-file-contents session-file)
                              (buffer-string))))
      (when (or (not file-contents)
                (and (stringp file-contents)
                     (string= (string-trim file-contents) "")))
        (error "[easysession] %s: Failed to read session information from %s"
               session-name session-file))

      ;; Evaluate file
      (progn
        (setq session-data (ignore-errors (read file-contents)))

        (when (not session-data)
          (error
           "[easysession] %s: Failed to evaluate session information from %s"
           session-name session-file))

        ;; Load buffers first because the cursor, window-start, or hscroll
        ;; might be altered by packages such as saveplace. This will allow
        ;; the frameset to modify the cursor later on.
        (let ((file-name-handler-alist original-file-name-handler-alist))
          (run-hooks 'easysession-before-load-hook)

          (dolist (handler (easysession-get-load-handlers))
            (when handler
              (cond
               ((and (symbolp handler)
                     (fboundp handler))
                (funcall handler session-data))

               (t
                (easysession--warning
                 "The following load handler is not a defined function: %s"
                 handler))))))

        ;; Load the frame set
        (easysession--load-frameset session-data
                                    (bound-and-true-p
                                     easysession-frameset-restore-geometry))

        (setq easysession--load-error nil)
        (when (called-interactively-p 'any)
          (easysession--message "Session loaded: %s" session-name))

        (let ((file-name-handler-alist original-file-name-handler-alist))
          (run-hooks 'easysession-after-load-hook)))))
  ;; Return easysession--load-error
  (not easysession--load-error))

;;;###autoload
(defun easysession-load-including-geometry (&optional session-name)
  "Load the session and restore the position and size of the Emacs frames.
SESSION-NAME is the session name.

This function is typically used when Emacs is initially loaded. It ensures that
session settings, including the positions and sizes (geometry) of all frames,
are restored.

For subsequent session switching, consider using `easysession-switch-to', which
load the session without resizing or moving the Emacs frames."
  (let ((easysession-frameset-restore-geometry t))
    (easysession-load session-name)))

;;;###autoload
(defun easysession-switch-to-and-restore-geometry (session-name)
  "Load a session and restore the position and size of frames.
SESSION-NAME is the session name.
When `easysession-switch-to-save-session' is non nil, it saves the current
session before loading the session that is specified.

This interactive function prompts the user for a session name when called
interactively.

This function will:
- Save the current session if it is loaded and not being reloaded.
  (When `easysession-switch-to-save-session' is non nil.)
- Load the specified session.
- Set the specified session as the current session.

If the session is already loaded, a message is displayed to indicate it has been
reloaded. If the session is newly created or switched to, a message is displayed
accordingly."
  (interactive
   (list (easysession--prompt-session-name
          "Switch to session and restore geometry: "
          (unless easysession-switch-to-exclude-current
            (or easysession--current-session-name
                ""))
          easysession-switch-to-exclude-current)))
  (let ((easysession-frameset-restore-geometry t))
    (easysession-switch-to session-name)))

;;;###autoload
(defun easysession-save (&optional session-name)
  "Save the current session.
SESSION-NAME is the name of the session."
  (interactive)
  (when (and (not session-name)
             (not easysession--current-session-name))
    (user-error "%s%s"
                "[easysession] No session is active. "
                "Load a session with `easysession-switch-to'"))

  (run-hooks 'easysession-before-save-hook)
  (let* ((original-file-name-handler-alist file-name-handler-alist)
         (file-name-handler-alist nil)
         (session-name (if session-name
                           session-name
                         easysession--current-session-name))
         (session-file (easysession-get-session-file-path session-name))
         (data-frameset (easysession--save-frameset session-name))
         (data-frameset-geometry (easysession--save-frameset
                                  session-name t))
         (session-data nil)
         (session-dir (file-name-directory session-file)))
    ;; Frameset
    (push (cons "frameset" data-frameset) session-data)
    (push (cons "frameset-geo" data-frameset-geometry) session-data)

    ;; Buffers and file buffers
    (let* ((file-name-handler-alist original-file-name-handler-alist)
           (buffers (funcall easysession-buffer-list-function)))
      (dolist (handler (easysession-get-save-handlers))
        (if (not (and handler
                      (symbolp handler)
                      (fboundp handler)))
            (easysession--warning
             "The following save handler is not a defined function: %s"
             handler)
          (let ((result (funcall handler buffers)))
            (when result
              (let* ((key (alist-get 'key result))
                     (value (let ((value (alist-get 'buffers result)))
                              (if value
                                  ;; Backward compatibility
                                  value
                                ;; New: 'value
                                (alist-get 'value result))))
                     (remaining-buffers (alist-get 'remaining-buffers result)))
                ;; Push results into session-data
                (push (cons key value) session-data)

                ;; The following optimizes buffer processing by updating the
                ;; list of buffers for the next iteration By setting buffers
                ;; to the remaining-buffers returned by each handler function,
                ;; it ensures that each subsequent handler only processes
                ;; buffers that have not yet been handled. This approach
                ;; avoids redundant processing of buffers that have already
                ;; been classified or processed by previous handlers,
                ;; resulting in more efficient processing. As a result, each
                ;; handler operates on a progressively reduced set of buffers.
                (setq buffers remaining-buffers)))))))

    (unless (file-directory-p session-dir)
      (make-directory session-dir :parents))

    (let* ((file-name-handler-alist nil)
           (serialized-data (prin1-to-string session-data)))
      (with-temp-buffer
        (insert serialized-data)
        (let ((coding-system-for-write 'utf-8-emacs)
              (write-region-annotate-functions nil)
              (write-region-post-annotation-function nil))
          (write-region (point-min) (point-max) session-file nil 'silent)
          nil))

      (when (called-interactively-p 'any)
        (easysession--message "Session saved: %s" session-name)))

    (run-hooks 'easysession-after-save-hook)))

;;;###autoload
(defun easysession-save-as (session-name)
  "Save the session as SESSION-NAME.
SESSION-NAME is the session to save to and switch to.
If the function is called interactively, prompt the user for a session name."
  (interactive
   (list (easysession--prompt-session-name "Save session as: "
                                           (or easysession--current-session-name
                                               ""))))
  (when (or (not session-name)
            (string= session-name ""))
    (user-error "[easysession] Please provide a valid session name"))

  (let* ((new-session-name (or session-name
                               easysession--current-session-name)))
    (easysession--ensure-session-name-valid new-session-name)
    (easysession-save new-session-name)

    (easysession--message
     "Session saved as: '%s'. Use 'M-x easysession-switch-to' to switch to it."
     new-session-name)
    t))

;;;###autoload
(defun easysession-switch-to (session-name)
  "Load a session without altering the frame's size or position.
SESSION-NAME is the session name.

When `easysession-switch-to-save-session' is non nil, it saves the current
session before loading the session that is specified.

This interactive function prompts the user for a session name when called
interactively.

This function will:
- Save the current session if it is loaded and not being reloaded.
  (When `easysession-switch-to-save-session' is non nil.)
- Load the specified session.
- Set the specified session as the current session.

If the session is already loaded, a message is displayed to indicate it has been
reloaded. If the session is newly created or switched to, a message is displayed
accordingly."
  (interactive
   (list (easysession--prompt-session-name
          "Switch to session: "
          (unless easysession-switch-to-exclude-current
            (or easysession--current-session-name
                ""))
          easysession-switch-to-exclude-current)))
  (let ((session-name (or session-name
                          easysession--current-session-name)))
    (unless session-name
      (user-error "%s%s"
                  "[easysession] A session name must be provided "
                  "to `easysession-switch-to'"))

    (let* ((session-file (easysession-get-session-file-path session-name))
           (session-reloaded (and easysession--current-session-name
                                  (string= session-name
                                           easysession--current-session-name)))
           (saved nil)
           (new-session nil))
      ;; TODO: Prompt the user for confirmation before loading a new session
      ;;       if the current session has not been loaded.
      (when (and (not easysession--load-error) (not session-reloaded))
        ;; TODO ask the user to save the current session
        (when (and easysession--current-session-name
                   easysession-switch-to-save-session)
          (easysession-save easysession--current-session-name)
          (setq saved t)))

      (easysession-load session-name)
      (easysession-set-current-session-name session-name)

      (unless session-reloaded
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
                                     (if new-session "new " "") session-name))))))

;;;###autoload
(define-minor-mode easysession-save-mode
  "Toggle `easysession-save-mode'."
  :global t
  :lighter (:eval
            (if easysession-save-mode-lighter-show-session-name
                easysession-save-mode-lighter-session-name-spec
              easysession-save-mode-lighter))
  :group 'easysession
  (if easysession-save-mode
      (progn
        (when (and easysession-save-interval
	                 (null easysession--timer))
          (setq easysession--timer (run-with-timer easysession-save-interval
			                                             easysession-save-interval
                                                   #'easysession--auto-save)))
        ;; `kill-emacs-query-functions' is preferable to `kill-emacs-hook' for
        ;; saving frames, as it is called before frames are closed.
        ;; In contrast, `kill-emacs-hook' is invoked after the frames are
        ;; killed, such as when Emacs is terminated using `xkill` or the
        ;; `kill-emacs' Emacs function.
        (add-hook 'kill-emacs-query-functions #'easysession--auto-save)
        (add-hook 'kill-emacs-hook #'easysession--auto-save))
    (when easysession--timer
      (cancel-timer easysession--timer)
      (setq easysession--timer nil))
    (remove-hook 'kill-emacs-hook #'easysession--auto-save)
    (remove-hook 'kill-emacs-query-functions #'easysession--auto-save)))

(provide 'easysession)
;;; easysession.el ends here
