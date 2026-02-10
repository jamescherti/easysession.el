;;; easysession.el --- Persist and restore your sessions (desktop.el alternative) -*- lexical-binding: t; -*-

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
;; - Quickly switch between sessions while editing with or without disrupting
;;   the frame geometry.
;; - Capture the full Emacs workspace state: file buffers, indirect buffers and
;;   clones, buffer narrowing, Dired buffers, window layouts and splits, the
;;   built-in tab-bar with its tabs and buffers, and Emacs frames with optional
;;   position and size restoration.
;; - Built from the ground up with an emphasis on speed, minimalism, and
;;   predictable behavior, even in large or long-running Emacs setups.
;; - Never lose context with automatic session persistence. (Enable
;;   `easysession-save-mode' to save the active session at regular intervals
;;   defined by `easysession-save-interval' and again on Emacs exit.)
;; - Comprehensive command set for session management: switch sessions instantly
;;   with `easysession-switch-to', save with `easysession-save', delete with
;;   `easysession-delete', and rename with `easysession-rename'.
;; - Highly extensible architecture that allows custom handlers for non-file
;;   buffers, making it possible to restore complex or project-specific buffers
;;   exactly as needed.
;; - Fine-grained control over file restoration by selectively excluding
;;   individual functions from `find-file-hook' during session loading via
;;   `easysession-exclude-from-find-file-hook'.
;; - Clear visibility of the active session through modeline integration or a
;;   lighter.
;; - Built-in predicate to determine whether the current session qualifies for
;;   automatic saving.
;; - Optional scratch buffer persistence via the `easysession-scratch'
;;   extension, preserving notes and experiments across restarts.
;; - Optional Magit state restoration via the `easysession-magit' extension,
;;   keeping version control workflows intact.
;; - Exact restoration of narrowed regions in both base and indirect buffers,
;;   ensuring each buffer reopens with the same visible scope as when it was
;;   saved.
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package easysession
;;   :ensure t
;;   :custom
;;   (easysession-save-interval (* 10 60))  ; Save every 10 minutes
;;
;;   ;; Display the active session name in the mode-line lighter.
;;   (easysession-save-mode-lighter-show-session-name t)
;;
;;   ;; Optionally, the session name can be shown in the modeline info area:
;;   ;; (easysession-mode-line-misc-info t)
;;
;;   :config
;;   ;; Key mappings
;;   (global-set-key (kbd "C-c sl") #'easysession-switch-to) ; Load session
;;   (global-set-key (kbd "C-c ss") #'easysession-save) ; Save session
;;   (global-set-key (kbd "C-c sL")
;;    #'easysession-switch-to-and-restore-geometry)
;;   (global-set-key (kbd "C-c sr") #'easysession-rename)
;;   (global-set-key (kbd "C-c sR") #'easysession-reset)
;;   (global-set-key (kbd "C-c sd") #'easysession-delete)
;;
;;   ;; Non-nil: `easysession-setup' loads the session automatically.
;;   ;; Nil: session is not loaded automatically; the user can load it manually.
;;   (setq easysession-setup-load-session t)
;;
;;   ;; Priority depth used when `easysession-setup' adds `easysession' hooks.
;;   ;; 102 ensures that the session is loaded after all other packages.
;;   (setq easysession-setup-add-hook-depth 102)
;;
;;   ;; The `easysession-setup' function adds hooks:
;;   ;; - To automatically load the session during `emacs-startup-hook'.
;;   ;; - To automatically save the session at regular intervals, and when Emacs
;;   ;; exits.
;;   (easysession-setup))
;;
;; Usage:
;; ------
;; It is recommended to use the following functions:
;; - (easysession-switch-to) to switch to another session or (easysession-load)
;;   to reload the current one,
;; - (easysession-save) to save the current session as the current name or
;;   another name.
;;
;; Links:
;; ------
;; - More information about easysession (Frequently asked questions, usage...):
;;   https://github.com/jamescherti/easysession.el

;;; Code:

(require 'frameset)
(require 'cl-lib)
(require 'seq)

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

This setting affects the interactive prompt used by the following functions:
  - `easysession-load'
  - `easysession-switch-to'
  - `easysession-switch-to-and-restore-geometry'

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
                     (unless easysession--session-loaded
                       " <NOT LOADED>")
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

(defcustom easysession-edit-read-only t
  "Non-nil means session buffers opened with `easysession-edit` are read-only."
  :type 'boolean
  :group 'easysession)

(defcustom easysession-exclude-from-find-file-hook '(recentf-track-opened-file)
  "List of hooks to be excluded from `find-file-hook'.
When EasySession restores a file editing buffer using `find-file-noselect', the
functions in this list are skipped and not executed by `find-file-hook'. This
provides control over which hooks should be bypassed during the file restoration
process, ensuring that certain actions (e.g., tracking opened files) are not
triggered in this context."
  :type '(repeat symbol)
  :group 'easysession)

(defvar easysession-frameset-restore-geometry nil
  "If non-nil, `easysession-load' restores frame position and size.

Do not modify this variable directly; use `easysession-load-including-geometry'
instead.

Set this variable to t only if you want `easysession-load' or
`easysession-switch-to' to always restore frame geometry.

By default, this variable is nil, meaning `easysession-load' does not restore
geometry.")

(defvar easysession-load-in-progress nil
  "Session name (string) if a session is currently being loaded.
This is an internal variable that is meant to be read-only. Do not modify it.
This variable is used to indicate whether a session loading process is in
progress.")

(defvar easysession-save-in-progress nil
  "Session name (string) if a session is currently being saved.
This is an internal variable that is meant to be read-only. Do not modify it.
This variable is used to indicate whether a session saving process is in
progress.")

(defvar easysession-confirm-new-session t
  "Non-nil prompts the user for confirmation when creating a new session.")

(defvar easysession-save-pretty-print nil
  "Non-nil means session data is pretty-printed when written to disk.
When it is nil, session data is saved in a more compact form that is harder for
humans to read but takes less space.
This option only changes how the session file looks, not what information is
stored.")

(defvar easysession-setup-add-hook-depth 102
  "Priority depth used when `easysession-setup' adds `easysession' hooks.
Higher values ensure that `easysession' hooks run after most other startup or
frame hooks.

The default value of 102 ensures that the session loads after all other
packages. Setting the depth to 102 is useful for users of minimal-emacs.d, where
certain optimizations restore `file-name-handler-alist' at depth 101 during
`emacs-startup-hook'.")

(defvar easysession-setup-load-session t
  "Non-nil means `easysession-setup' automatically loads the session.
Nil means the session is not loaded automatically; the user can load it
manually.")

(defvar easysession-setup-load-session-including-geometry t
  "Non-nil means the `easysession-setup' session restores frame geometry.
If nil, the first session is loaded without restoring frame sizes or positions.")

(defvar easysession-setup-load-predicate nil
  "Predicate to determine whether `easysession-setup' loads the session.
When nil, the session loads without additional checks. If assigned a function,
the function is called without arguments; the session restoration proceeds only
if the return value is non-nil.

This variable allows restricting session restoration to specific environments,
such as graphical frames.")

;;; Internal variables

(defvar easysession--debug nil)

(defvar easysession--timer nil)

;; Overrides `frameset-filter-alist' while preserving its keys,
;; but replaces their values with the ones specified in the following alist:
(defvar easysession--overwrite-frameset-filter-alist
  '(
    ;; Already excluded by frameset-persistent-filter-alist
    (background-color . :never)
    (buffer-list . :never)
    (buffer-predicate . :never)
    (buried-buffer-list . :never)
    (delete-before . :never)
    (foreground-color . :never)
    (parent-frame . :never)
    (mouse-wheel-frame . :never)
    (window-system . :never)
    (name . :never)
    (parent-id . :never)
    (window-id . :never)
    (font . :never)
    (font-backend . :never)
    (GUI:font . :never)

    ;; Don't save the 'client' parameter to avoid that a subsequent
    ;; `save-buffers-kill-terminal' in a non-client session barks at
    ;; the user (Emacs Bug#29067).
    (client . :never)

    ;; Geometry
    (GUI:bottom . :never)
    (GUI:fullscreen . :never)
    (GUI:height . :never)
    (GUI:left . :never)
    (GUI:right . :never)
    (GUI:top . :never)
    (GUI:width . :never)
    (bottom . :never)
    (fullscreen . :never)
    (height . :never)
    (left . :never)
    (right . :never)
    (top . :never)
    (width . :never)

    ;; Window-manager mutable flags: leave commented out
    ;; TODO Testing without these parameters
    ;; (skip-taskbar . :never)
    ;; (sticky . :never)
    ;; (shaded . :never)
    ;; (undecorated . :never)
    ;; (border-width . :never)

    ;; Pixel Precision (Affect the geometry, but let the user configure them)
    ;; TODO Testing without these parameters
    (frameset--text-pixel-height . :never)
    (frameset--text-pixel-width . :never)
    ;; (child-frame-border-width . :never)
    ;; (bottom-divider-width . :never)
    ;; (right-divider-width . :never)

    ;; Let the user configure the scroll bars, tool-bar, and menu-bar
    ;; TODO Testing without these parameters
    ;; (vertical-scroll-bars . :never)
    ;; (horizontal-scroll-bars . :never)

    ;; (tool-bar-position . :never)
    ;; (no-special-glyphs . :never)

    ;; Fonts, fringes, toolbars, scrollbars: leave commented out
    ;; (scroll-bar-width . :never)
    ;; (scroll-bar-height . :never)
    ;; (internal-border-width . :never)
    ;; (left-fringe . :never)
    ;; (right-fringe . :never)
    ;; (menu-bar-lines . :never)
    ;; (tool-bar-lines . :never)
    (line-spacing . :never)
    ;; (parent-id . :never)

    ;; Other exclusions
    ;; TODO Commented out. Testing the effect.
    (scroll-bar-background . :never)
    (scroll-bar-foreground . :never)
    ;; (override-redirect . :never)
    ;; (auto-lower . :never)  ; control focus behavior
    ;; (auto-raise . :never)  ; control focus behavior
    (background-mode . :never)  ; Affects theme/light/dark mode
    (border-color . :never)
    ;; (child-frame-border-width . :never)
    (cursor-color . :never)
    ;; (cursor-type . :never)
    ;; (display-type . :never)
    ;; (environment . :never)
    ;; (font-parameter . :never)
    ;; (icon-type . :never)
    ;; (inhibit-double-buffering . :never)
    ;; (minibuffer . :never)
    (mouse-color . :never)
    ;; (no-accept-focus . :never)
    ;; (no-focus-on-map . :never)

    ;; On macOS, this controls whether the window title bar looks dark or light.
    (ns-appearance . :never)

    ;; (outer-window-id . :never)
    ;; (screen-gamma . :never)
    ;; (use-frame-synchronization . :never)

    ;; Ensures frames are positioned only after the window manager maps them,
    ;; helping avoid small shifts in geometry.
    ;; (Do not restore wait-for-wm. Let the user configure it.)
    ;; TODO Commented out. Testing the effect.
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
    )
  "EasySession overrides to prevent restoration of frame geometry.")

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
     GUI:bottom
     GUI:fullscreen
     GUI:height
     GUI:left
     GUI:right
     GUI:top
     GUI:width
     bottom
     fullscreen
     height
     left
     right
     top
     width

     ;; Pixel perfect width and height
     frameset--text-pixel-height
     frameset--text-pixel-width

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
     right-divider-width))
  "EasySession overrides enabling restoration of frame geometry.

This alist is derived from `frameset-persistent-filter-alist' with explicit
inclusion of geometry-related frame parameters, including character-based,
pixel-based, and window-manager-sensitive layout attributes.")

(defvar easysession-file-version 3
  "Version number of easysession file format.")

(defvar easysession--current-session-name nil
  "Current session.")

(defvar easysession--session-loaded nil
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

(defvar easysession-visible-buffer-list-include-names '()
  "List of buffer names that are always included in the session.

Each entry must be a string matching `buffer-name'. Buffers whose
names appear in this list are persisted and restored regardless
of their visibility.")

(defvar easysession--builtin-load-handlers
  '(easysession--handler-load-file-editing-buffers
    easysession--handler-load-managed-major-modes
    easysession--handler-load-indirect-buffers)
  "Internal variable.")

(defvar easysession--builtin-save-handlers
  '(easysession--handler-save-file-editing-buffers
    easysession--handler-save-managed-major-modes
    easysession--handler-save-indirect-buffers)
  "Internal variable.")

(defvar easysession--managed-major-modes nil
  "Alist of (MODE . PROPS) for managed major modes.")

(defvar uniquify-buffer-name-style)

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

Raise an error if the session name is invalid."
  (when (or (not session-name)
            (string= session-name "")
            (let ((case-fold-search nil))
              (or (string-match-p "\\\\" session-name)
                  (string-match-p "/" session-name)))
            (string= session-name "..")
            (string= session-name "."))
    (user-error "[easysession] Invalid session name: %s" session-name))
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
  "Return an adjusted version of `frameset-filter-alist'.

This function produces a frameset filter alist based on the current
`frameset-filter-alist', applying any overrides specified in OVERWRITE-ALIST.

OVERWRITE-ALIST should be an alist where each element has the
form (FRAME-PARAM . VALUE), similar to
`easysession--overwrite-frameset-filter-alist'. Each pair in OVERWRITE-ALIST
replaces or adds the corresponding parameter in the resulting alist.

The returned alist can be used to control which frame parameters are
saved by EasySession."
  (let ((result (copy-alist frameset-filter-alist)))
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
  "Read a session name from the minibuffer using PROMPT.

SESSION-NAME specifies the default selection.
When EXCLUDE-CURRENT is non-nil, the active session name is omitted from the
completion candidates.
When INITIAL-INPUT is non-nil, it is inserted into the minibuffer as the initial
contents.

Return the selected session name as a string."
  (completing-read (concat "[easysession] " prompt)
                   (easysession--get-all-names exclude-current)
                   nil nil initial-input nil session-name))

(defun easysession--prompt-multiple-session-names (prompt
                                                   &optional session-name
                                                   exclude-current
                                                   initial-input)
  "Read a session name from the minibuffer using PROMPT.

SESSION-NAME specifies the default selection.
When EXCLUDE-CURRENT is non-nil, the active session name is omitted from the
completion candidates.
When INITIAL-INPUT is non-nil, it is inserted into the minibuffer as the initial
contents.

Return the selected session name as a string."
  (completing-read-multiple (concat "[easysession] " prompt)
                            (easysession--get-all-names exclude-current)
                            nil t initial-input nil session-name))

(defun easysession--buffer-narrowing-bounds (buffer)
  "Return narrowing boundaries of BUFFER if it is narrowed.
If BUFFER is narrowed, return a cons cell (POINT-MIN . POINT-MAX) representing
the active narrowing region. If BUFFER is not narrowed, return nil."
  (with-current-buffer buffer
    (when (buffer-narrowed-p)
      (cons (point-min) (point-max)))))

(defun easysession--get-indirect-buffer-info (indirect-buffer)
  "Get information about the indirect buffer INDIRECT-BUFFER.

This function retrieves details about the indirect buffer INDIRECT-BUFFER and
its base buffer. It returns a list of cons cells containing the names of both
buffers, and narrowing bounds.

Return a list of cons cells: ((indirect-buffer-name . name-of-indirect-buffer)
                              (base-buffer-name . name-of-base-buffer)
                              (narrowing-bounds . (a, b))

Return nil if BUF is not an indirect buffer or if the base buffer cannot be
determined."
  (when (buffer-live-p indirect-buffer)
    (let ((base-buffer (buffer-base-buffer indirect-buffer)))
      (when (and base-buffer ; Indirect buffer?
                 (buffer-live-p base-buffer))
        (let ((base-buffer-name (buffer-name base-buffer))
              (indirect-buffer-name (buffer-name indirect-buffer)))
          (when (and base-buffer-name
                     indirect-buffer-name)
            `((indirect-buffer-name . ,indirect-buffer-name)
              (base-buffer-name . ,base-buffer-name)
              (narrowing-bounds . ,(easysession--buffer-narrowing-bounds
                                    indirect-buffer)))))))))

(defun easysession--get-managed-major-mode-buffer-info (buffer)
  "Retrieve the persistent state for BUFFER if the major mode is managed.

Returns an alist containing `buffer-name', `major-mode', and `default-directory'
if the buffer's major mode derives from a key in
`easysession--managed-major-modes'.

If the configuration includes a `:save' function, it is invoked safely to obtain
custom state data, which is appended to the result under the `data' key.

Returns nil if BUFFER is not live or if no matching entry exists."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((entry (cl-find-if (lambda (entry)
                                 (derived-mode-p (car entry)))
                               easysession--managed-major-modes)))
        (when entry
          (let* ((state `((buffer-name . ,(buffer-name))
                          (major-mode . ,(car entry))
                          (default-directory . ,default-directory)))
                 (save-fn (plist-get (cdr entry) :save))
                 (data (when (functionp save-fn)
                         (ignore-errors
                           (funcall save-fn)))))
            (when data
              (nconc state (list (cons 'data data))))
            state))))))

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

;; (defvar easysession--frame-parameters-filters nil)
;; (defvar easysession--frame-parameters-filters-including-geo nil)

(defun easysession--save-frameset (session-name
                                   &optional save-geometry)
  "Create and return a frameset for the current Emacs session.

SESSION-NAME identifies the session associated with the saved frameset. When
SAVE-GEOMETRY is non-nil, frame geometry parameters are included; otherwise,
geometry-related parameters are excluded.

The frameset is generated only when at least one live frame exists. Return nil
when no frames are available for persistence."
  ;; TODO implement this
  ;; (setq easysession--frame-parameters-filters
  ;;       (easysession--init-frame-parameters-filters
  ;;        easysession--overwrite-frameset-filter-alist))
  ;; (setq easysession--frame-parameters-filters-including-geo
  ;;       (easysession--init-frame-parameters-filters
  ;;        easysession--overwrite-frameset-filter-include-geometry-alist))
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
  (when (easysession--can-restore-frameset-p)
    (let* ((key (if load-geometry
                    "frameset-geo"
                  "frameset"))
           (data (when (assoc key session-data)
                   (assoc-default key session-data))))
      (when (and (not data) load-geometry)
        (setq data (when (assoc "frameset" session-data)
                     (assoc-default "frameset" session-data))))
      (when data
        (frameset-restore
         data
         :reuse-frames easysession-frameset-restore-reuse-frames
         :cleanup-frames
         (if (eq easysession-frameset-restore-cleanup-frames t)
             (lambda (frame action)
               (when (and (memq action '(:rejected :ignored))
                          ;; Avoid cleaning up the initial daemon frame during
                          ;; frameset restoration by disabling frame cleanup
                          ;; when running under a daemon with a single frame.
                          (not (and (daemonp)
                                    (equal (terminal-name (frame-terminal frame))
                                           "initial_terminal"))))
                 (delete-frame frame)))
           easysession-frameset-restore-cleanup-frames)
         :force-display easysession-frameset-restore-force-display
         :force-onscreen easysession-frameset-restore-force-onscreen)

        (when (fboundp 'tab-bar-mode)
          (when (seq-some
                 (lambda (frame)
                   (menu-bar-positive-p
                    (frame-parameter frame 'tab-bar-lines)))
                 (frame-list))
            (tab-bar-mode 1)))))))

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
       (not (frame-parameter frame 'easysession-dont-save))
       (not (frame-parameter frame 'desktop-dont-save))
       ;; Avoid saving the initial daemon frame
       (not (and (daemonp)
                 (equal (terminal-name (frame-terminal frame))
                        "initial_terminal")))))

(defun easysession--session-file (session-name)
  "Check if a session with the given SESSION-NAME exists.
Returns the session file if the session file exists, nil otherwise."
  (let ((file-name (easysession-get-session-file-path session-name)))
    (when (file-exists-p file-name)
      file-name)))

(defun easysession--auto-save ()
  "Automatically save the current session when permitted.

This function is invoked by `easysession-save-mode'. It evaluates
`easysession-save-mode-predicate' and saves the current session when a session
is loaded, a session name is defined, and at least one frame exists.

The function always returns non-nil so that it does not inhibit Emacs
termination when used from `kill-emacs-query-functions'."
  ;; Auto save when there is at least one frame and a session has been loaded
  (unwind-protect
      (when (and (> (length (frame-list)) 0)
                 easysession--current-session-name
                 easysession--session-loaded)
        (if (and easysession-save-mode-predicate
                 (funcall easysession-save-mode-predicate))
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
  ;; The second one is important.
  ;;
  ;; Always return t, since this `easysession--auto-save' is part of
  ;; `kill-emacs-query-functions'. Returning nil would prevent Emacs from
  ;; exiting.
  t)

(defun easysession--mode-line-session-name-format ()
  "Return a mode-line construct for the currently loaded session.
The session name is displayed only when a session is actively loaded."
  (if (bound-and-true-p easysession--current-session-name)
      (list easysession-mode-line-misc-info-prefix
            (propertize
             easysession--current-session-name
             'face 'easysession-mode-line-session-name-face
             'help-echo (format "Current session: %s"
                                easysession--current-session-name)
             'mouse-face 'mode-line-highlight)
            (unless easysession--session-loaded
              " <NOT LOADED>")
            easysession-mode-line-misc-info-suffix)))

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

(defun easysession--buffer-is-visible (buffer)
  "Return non-nil if BUFFER is currently visible in the Emacs session.

A buffer is considered visible if it is:

- Displayed in any visible window (`get-buffer-window').
- Associated with a visible tab in `tab-bar-mode' (if enabled).

Returns nil if the buffer is not displayed in a window or tab."
  (or
   ;; Windows
   (get-buffer-window buffer 'visible)
   ;; Tab-bar windows
   (and (bound-and-true-p tab-bar-mode)
        (fboundp 'tab-bar-get-buffer-tab)
        (tab-bar-get-buffer-tab buffer t nil))))

(defun easysession--cl-list* (&rest args)
  "Return a list from ARGS using `cl-list*', or nil if ARGS is empty.
This is a safe wrapper around `cl-list*' that avoids errors when called without
any arguments. Each element of ARGS becomes part of the resulting list, with the
last argument as the final element of the list."
  (and args (apply #'cl-list* args)))

;; (defvar easysession--internal-delay-hook nil
;;   "Hooks that run after all buffers have loaded; intended for internal use.")

(defun easysession--serialize-to-quoted-sexp (value)
  "Convert VALUE to a pair (QUOTE . SEXP); (eval SEXP) gives VALUE.
SEXP is an sexp that when evaluated yields VALUE.
QUOTE may be `may' (value may be quoted),
`must' (value must be quoted), or nil (value must not be quoted)."
  (cond
   ((or (numberp value) (null value) (eq t value) (keywordp value))
    (cons 'may value))
   ((stringp value)
    ;; Remove any unreadable text properties
    (if (condition-case nil (read (format "%S" value)) (error nil))
        (cons 'may value)
      (let ((copy (copy-sequence value)))
        (set-text-properties 0 (length copy) nil copy)
        (cons 'may copy))))
   ((symbolp value)
    (cons 'must value))
   ((vectorp value)
    (let* ((pass1 (mapcar #'easysession--serialize-to-quoted-sexp value))
           (special (assq nil pass1)))
      (if special
          (cons nil `(vector
                      ,@(mapcar (lambda (el)
                                  (if (eq (car el) 'must)
                                      `',(cdr el) (cdr el)))
                                pass1)))
        (cons 'may `[,@(mapcar #'cdr pass1)]))))
   ((and (recordp value) (symbolp (aref value 0)))
    (let* ((pass1 (let ((res ()))
                    (dotimes (i (length value))
                      (push (easysession--serialize-to-quoted-sexp
                             (aref value i)) res))
                    (nreverse res)))
           (special (assq nil pass1)))
      (if special
          (cons nil `(record
                      ,@(mapcar (lambda (el)
                                  (if (eq (car el) 'must)
                                      `',(cdr el) (cdr el)))
                                pass1)))
        (cons 'may (apply #'record (mapcar #'cdr pass1))))))
   ((consp value)
    (let ((p value)
          newlist
          use-list*)
      (while (consp p)
        (let ((q.sexp (easysession--serialize-to-quoted-sexp (car p))))
          (push q.sexp newlist))
        (setq p (cdr p)))
      (when p
        (let ((last (easysession--serialize-to-quoted-sexp p)))
          (setq use-list* t)
          (push last newlist)))
      (if (assq nil newlist)
          (cons nil
                `(,(if use-list* 'easysession--cl-list* 'list)
                  ,@(mapcar (lambda (el)
                              (if (eq (car el) 'must)
                                  `',(cdr el) (cdr el)))
                            (nreverse newlist))))
        (cons 'must
              `(,@(mapcar #'cdr
                          (nreverse (if use-list* (cdr newlist) newlist)))
                . ,(if use-list* (cdar newlist)))))))
   ((subrp value)
    (cons nil `(symbol-function
                ',(intern-soft (substring (prin1-to-string value) 7 -1)))))

   ;; NOTE: Causes issues when opening `org-agenda', then
   ;;       `window-toggle-side-windows', then `easysession-save'
   ;; ((markerp value)
   ;;  (let ((pos (marker-position value))
   ;;        (buf (buffer-name (marker-buffer value))))
   ;;    (cons nil
   ;;          `(let ((mk (make-marker)))
   ;;             (add-hook 'easysession--internal-delay-hook
   ;;                       (lambda ()
   ;;                         (set-marker mk ,pos (get-buffer ,buf))))
   ;;             mk))))

   (t
    (cons 'may "Unprintable entity"))))

(defvar easysession--daemon-session-loaded nil
  "Non-nil if an EasySession session has already been loaded in daemon mode.
This variable prevents multiple session loads when Emacs is running as a daemon.
It is set to t after the first successful session load and should not be
manually modified under normal operation.")

(defun easysession--persist-session-on-frame-delete-maybe (frame)
  "Save the current session when a client frame is deleted in daemon mode.
FRAME designates the frame scheduled for deletion.

This ensures the session is saved before the last client frame is closed in
daemon mode, allowing correct restoration when a new frame is created."
  (when (and easysession--current-session-name
             easysession--session-loaded
             (daemonp)
             (frame-live-p frame)
             ;; The number 2 accounts for both the initial daemon frame and the
             ;; client frame
             (= (length (seq-filter
                         (lambda (frame)
                           (not (equal (terminal-name (frame-terminal frame))
                                       "initial_terminal")))
                         (frame-list))) 1))
    (setq easysession--daemon-session-loaded nil)
    (easysession-unload)))

(defun easysession--setup-load-session ()
  "Load an EasySession session.
After loading in daemon mode, `easysession--daemon-session-loaded' is set to t
to prevent multiple loads during the same daemon session."
  (when (or (not easysession-setup-load-predicate)
            (funcall easysession-setup-load-predicate))
    (if (daemonp)
        (unless easysession--daemon-session-loaded
          (if easysession-setup-load-session-including-geometry
              (easysession-load-including-geometry)
            (easysession-load))

          (setq easysession--daemon-session-loaded t))
      (if easysession-setup-load-session-including-geometry
          (easysession-load-including-geometry)
        (easysession-load)))))

;;; Internal functions: handlers

(defun easysession--restore-buffer-state (buffer buffer-info)
  "Restore the state of BUFFER from previously recorded session DATA.

BUFFER is the target buffer to restore. BUFFER-INFO is an alist containing saved
buffer attributes, currently including `narrowing-bounds` which specifies the
start and end of the narrowed region.

This function currently restores the narrowing region in the buffer so that it
reflects the same visible portion as when the session was saved.

In the future, this function may be extended to restore additional buffer
attributes such as point position, mark, local variables, or other editor state
information."
  (let* ((narrowing-bounds (alist-get 'narrowing-bounds buffer-info)))
    (when (and narrowing-bounds (consp narrowing-bounds))
      (let* ((start (car narrowing-bounds))
             (end (cdr narrowing-bounds)))
        (when (and (numberp start)
                   (numberp end))
          (with-current-buffer buffer
            (narrow-to-region start end)))))))

(defun easysession--handler-load-file-editing-buffers (session-data)
  "Load base buffers from SESSION-DATA.

SESSION-DATA may encode buffer entries using either a legacy or a current
representation:
- In the legacy representation, each buffer entry is a cons cell of the
  form (BUFFER-NAME . BUFFER-PATH). In this case, the buffer name is taken from
  the car and the file path from the cdr, and no additional buffer state is
  available.
- In the current representation, each buffer entry is an alist. This format is
  identified when the car of the entry is itself a cons. The alist provides
  structured fields such as `buffer-name' and `buffer-path', and may optionally
  include `narrowing-bounds', allowing additional buffer state to be restored.

The loader detects the representation dynamically and restores buffers
accordingly, ensuring backward compatibility with legacy session files."
  (dolist (buffer-info (or (assoc-default "path-buffers" session-data)
                           (assoc-default "buffers" session-data)))
    (let* ((new-format-p (and (consp buffer-info)
                              (consp (car buffer-info))))
           (buffer-name (if new-format-p
                            (alist-get 'buffer-name buffer-info)
                          (car buffer-info)))
           (buffer-path (if new-format-p
                            (alist-get 'buffer-path buffer-info)
                          (cdr buffer-info))))
      (when buffer-path
        (let ((original-buffer (get-file-buffer buffer-path))
              buffer)
          (if (buffer-live-p original-buffer)
              (setq buffer (or (buffer-base-buffer original-buffer)
                               original-buffer))
            (let ((new-buffer (let ((find-file-hook
                                     (seq-difference
                                      find-file-hook
                                      easysession-exclude-from-find-file-hook)))
                                (condition-case err
                                    (find-file-noselect buffer-path t)
                                  (error
                                   (easysession--warning
                                    "Failed to restore the buffer '%s': %s"
                                    buffer-name
                                    (error-message-string err))
                                   nil)))))
              ;; We are going to be using the base buffer to make sure that the
              ;; buffer that was returned by `find-file-noselect' is a base
              ;; buffer and not a clone
              (setq buffer (or (buffer-base-buffer new-buffer) new-buffer))))

          (unless (buffer-base-buffer buffer)
            (if (not (buffer-live-p buffer))
                (easysession--warning "Failed to restore the buffer '%s': %s"
                                      buffer-name buffer-path)
              ;; Ensure that buffer name is buffer-name
              (let ((uniquify-buffer-name-style nil)) ; Disable uniquify
                (easysession--ensure-buffer-name buffer buffer-name))

              ;; Restore buffer narrowing if present
              (when new-format-p
                (easysession--restore-buffer-state buffer
                                                   buffer-info)))))))))

(defun easysession--handler-load-indirect-buffers (session-data)
  "Load indirect buffers from the SESSION-DATA variable."
  (dolist (item (assoc-default "indirect-buffers" session-data))
    (let ((indirect-buffer-name (alist-get 'indirect-buffer-name item))
          (base-buffer-name (alist-get 'base-buffer-name item)))
      (when (and indirect-buffer-name
                 base-buffer-name)
        (let ((base-buffer (get-buffer base-buffer-name))
              (indirect-buffer (get-buffer indirect-buffer-name)))
          (when (and (not (buffer-live-p indirect-buffer))
                     (buffer-live-p base-buffer))
            (condition-case err
                (progn
                  (setq indirect-buffer (with-current-buffer base-buffer
                                          (clone-indirect-buffer
                                           indirect-buffer-name nil)))

                  (if (not (buffer-live-p indirect-buffer))
                      (easysession--warning
                       "Failed to restore the indirect buffer/clone: %s"
                       indirect-buffer-name)
                    ;; Restore indirect buffer
                    (let ((uniquify-buffer-name-style nil)) ; Disable uniquify
                      (easysession--ensure-buffer-name indirect-buffer
                                                       indirect-buffer-name))

                    ;; Restore buffer narrowing if present
                    (easysession--restore-buffer-state indirect-buffer item)))
              (error
               (easysession--warning
                "Failed to restore indirect buffer/clone '%s': %s"
                indirect-buffer-name (error-message-string err))))))))))

(defun easysession--get-base-buffer-info (buffer)
  "Return base buffer metadata for BUFFER.

If BUFFER is a live base buffer associated with a path, return an alist with the
buffer name, buffer path, and narrowing information.

If BUFFER is not a base buffer or has no associated path, return nil."
  (unless (buffer-base-buffer buffer)
    (with-current-buffer buffer
      (let ((path (if (derived-mode-p 'dired-mode)
                      default-directory
                    (buffer-file-name)))
            (uniquify-base-name (and (fboundp 'uniquify-buffer-base-name)
                                     (uniquify-buffer-base-name))))
        (when path
          ;; File visiting buffer and base buffers (not carbon copies)
          `((buffer-name . ,(buffer-name))
            (uniquify-base-name . ,uniquify-base-name)
            (buffer-path . ,path)
            (narrowing-bounds . ,(easysession--buffer-narrowing-bounds
                                  buffer))))))))

(defun easysession--handler-save-file-editing-buffers (buffers)
  "Collect and categorize file editing buffers from the provided list.
BUFFERS is the list of buffers to process. This function identifies buffers
that are associated with files (file editing buffers) and those that are
not. It returns an alist with the following structure."
  (let ((file-editing-buffers '())
        (remaining-buffers '()))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (let ((base-buffer-info (easysession--get-base-buffer-info buf)))
          (if base-buffer-info
              (push base-buffer-info file-editing-buffers)
            (push buf remaining-buffers)))))
    `((key . "path-buffers")
      (value . ,file-editing-buffers)
      (remaining-buffers . ,remaining-buffers))))

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

(defun easysession--handler-load-managed-major-modes (session-data)
  "Load managed major mode buffers from SESSION-DATA safely.
Failures restoring individual buffers are logged but do not stop other buffers."
  (let ((managed-mode-buffers (assoc-default "managed-major-modes"
                                             session-data)))
    (when managed-mode-buffers
      (dolist (item managed-mode-buffers)
        (let ((buffer-name (alist-get 'buffer-name item))
              (mode (alist-get 'major-mode item)))
          (when (and buffer-name mode)
            (let ((props (cdr (assq mode easysession--managed-major-modes))))
              (when (and props (not (get-buffer buffer-name)))
                (let ((restore-fn (plist-get props :restore))
                      (validate-fn (plist-get props :validate)))
                  (when (and restore-fn
                             (or (null validate-fn) (funcall validate-fn item)))
                    (let ((default-directory
                           (or (alist-get 'default-directory item)
                               default-directory)))
                      (condition-case err
                          (funcall restore-fn item)
                        (error
                         (easysession--warning
                          "Failed to restore %s buffer '%s': %s"
                          mode buffer-name (error-message-string err)))))))))))))))

(defun easysession--handler-save-managed-major-modes (buffers)
  "Collect and categorize managed mode buffers from the provided list.
BUFFERS is the list of buffers to process. This function identifies buffers
with managed modes and separates them from other buffers."
  (let ((managed-mode-buffers '())
        (remaining-buffers '()))
    (dolist (buf buffers)
      (let ((managed-mode-buffer-info
             (easysession--get-managed-major-mode-buffer-info buf)))
        (if managed-mode-buffer-info
            (push managed-mode-buffer-info managed-mode-buffers)
          (push buf remaining-buffers))))
    `((key . "managed-major-modes")
      (value . ,managed-mode-buffers)
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
  (append easysession--save-handlers
          easysession--builtin-save-handlers))

(defun easysession-get-load-handlers ()
  "Return a list of all built-in and user-defined load handlers."
  (append easysession--load-handlers
          easysession--builtin-load-handlers))

(defun easysession-add-managed-major-mode (mode &rest props)
  "Add a managed major mode.

MODE must be a non-nil symbol representing a major mode.

PROPS is a keyword-value property list. Supported keys are:

:restore  (Required) A function called during session loading. It
          receives an alist containing `buffer-name', `major-mode',
          `default-directory', and a `data' key for mode-specific
          state.
:save     (Optional) A function that returns an alist of mode-specific
          data to be persisted.
:validate (Optional) A function used to verify the integrity of
          restored data.

If MODE is already managed, the new properties replace the existing
registration."
  (unless (and (symbolp mode) mode)
    (error "[easysession] MODE must be a non-nil symbol"))

  (unless (plist-get props :restore)
    (error "[easysession] :restore function is required for mode %S" mode))

  (setq easysession--managed-major-modes
        (cons (cons mode props)
              (assq-delete-all mode easysession--managed-major-modes))))

(defun easysession-remove-managed-major-mode (mode)
  "Unregister MODE from the collection of managed major modes.

MODE must be a symbol representing a valid major mode previously registered via
`easysession-add-managed-major-mode'. Upon execution, the associated restoration
logic is purged from the internal registry, returning the specified major mode
to an unmanaged state.

If MODE is not present in the registry, the operation terminates silently
without altering the current configuration."
  (unless (and (symbolp mode) mode)
    (error "[easysession] MODE must be a non-nil symbol"))

  (setq easysession--managed-major-modes
        (assq-delete-all mode easysession--managed-major-modes)))

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
         ,(intern (concat "easysession--handler-load-" key)) (session-data)
       ,(format "Load handler for restoring: %s."
                key)
       (let ((handler-data (assoc-default ,key session-data)))
         (when handler-data
           (funcall ,handler-func handler-data))))

     (easysession-add-load-handler
      ',(intern (concat "easysession--handler-load-" key)))))

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
         (push (cons 'key ,key) result)))))

(defmacro easysession-define-handler (key load-handler-func save-handler-func)
  "Add both load and save handlers for a given KEY.

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
  `(let ((fn ',(intern (concat "easysession--handler-load-" key))))
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
(defun easysession-save-session-and-close-frames ()
  "Save the session and close all frames without stopping the Emacs daemon.

Useful in daemon mode, this simulates quitting Emacs: buffers are saved, the
EasySession state is saved, and all frames except the initial terminal frame are
closed.

From the perspective of EasySession, this is functionally equivalent to an
application shutdown: the session is fully saved and unloaded. When a new frame
is later initialized by the Emacs daemon, EasySession restores the state as if
the process had been freshly started."
  (interactive)
  (when (yes-or-no-p "[easysession] Save session and close all frames? ")
    (save-some-buffers)
    (easysession-unload)
    ;; Close all frames
    (dolist (frame (frame-list))
      (when (and (frame-live-p frame)
                 (or (not (daemonp))
                     (not (string-equal (terminal-name (frame-terminal frame))
                                        "initial_terminal"))))
        (ignore-errors
          (delete-frame frame t))))))

;;;###autoload
(defun easysession-setup ()
  "Initialize `easysession' for session persistence.

If Emacs is running as a daemon, add `easysession-load-including-geometry' to
`server-after-make-frame-hook' so that session restoration occurs for each new
frame. Otherwise, add it to `emacs-startup-hook' to restore the session at
startup.

Also enable `easysession-save-mode' on startup to automatically save sessions.
Hook priorities are controlled by `easysession-setup-add-hook-depth'.

This function prepares `easysession' for automatic loading and saving of frames,
buffers, and session data."
  (when easysession-setup-load-session
    (if (daemonp)
        ;; Dameon mode
        (progn
          (when (seq-some (lambda (frame)
                            (frame-parameter frame 'client))
                          (frame-list))
            (easysession--setup-load-session))

          (add-hook 'server-after-make-frame-hook
                    #'easysession--setup-load-session
                    easysession-setup-add-hook-depth))
      ;; Graphical mode
      (add-hook 'emacs-startup-hook
                #'easysession--setup-load-session
                easysession-setup-add-hook-depth)))

  ;; Save the current session every `easysession-save-interval'
  (add-hook 'emacs-startup-hook #'easysession-save-mode
            easysession-setup-add-hook-depth))

;;;###autoload
(defun easysession-visible-buffer-list ()
  "Return a list of all buffers considered visible in the current session.

A buffer is included if it satisfies any of the following:
- It is the *scratch* buffer (included as a special case).
- It is currently displayed in a visible window.
- It is associated with a visible tab in `tab-bar-mode', if enabled.

The returned list contains live buffers only."
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (or
                  ;; Exceptions
                  (member (buffer-name buffer)
                          easysession-visible-buffer-list-include-names)

                  ;; Buffers and indirect buffers
                  (let ((base-buffer (buffer-base-buffer buffer)))
                    (cond
                     ;; Indirect buffers
                     (base-buffer
                      (and
                       (buffer-live-p base-buffer)

                       (or
                        ;; Is the indirect buffer visible?
                        (easysession--buffer-is-visible buffer)

                        ;; Is the base buffer visible?
                        (easysession--buffer-is-visible base-buffer))))

                     ;; Normal buffers
                     (t
                      (easysession--buffer-is-visible buffer))))))
        (push buffer visible-buffers)))
    visible-buffers))

;;;###autoload
(defun easysession-reset ()
  "Kill all buffers and close all frames, tabs, and windows."
  (interactive)
  ;; Hooks
  (run-hooks 'easysession-before-reset-hook)

  ;; Kill all buffers
  (let ((protected-names '("*scratch*" "*Messages*")))
    (mapc (lambda (buffer)
            (let ((name (buffer-name buffer))
                  (file (buffer-file-name buffer)))
              (when (and (not (member name protected-names))
                         (not (string-prefix-p " " name))
                         (or (not file)
                             (not (buffer-modified-p buffer))))
                (kill-buffer buffer))))
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
        (user-error
         "[easysession] No session is active. Load a session with `easysession-switch-to'"))

      (easysession--prompt-session-name
       (format "Rename session '%s' to: "
               easysession--current-session-name)
       nil
       nil
       easysession--current-session-name))))
  (unless easysession--current-session-name
    (user-error
     "[easysession] No session is active. Load a session with `easysession-switch-to'"))

  (unless new-session-name
    (user-error "[easysession] You need to specify the new session name"))

  (let* ((old-path (easysession-get-session-file-path
                    easysession--current-session-name))
         (new-path (easysession-get-session-file-path new-session-name)))
    (unless (file-regular-p old-path)
      (user-error "[easysession] No such file or directory: %s" old-path))

    (rename-file old-path new-path)
    (setq easysession--current-session-name new-session-name)))

;;;###autoload
(defun easysession-delete (session-names)
  "Delete one or more sessions.
SESSION-NAMES is a string or a list of session names."
  (interactive (list
                (easysession--prompt-multiple-session-names
                 "Delete session(s): "
                 nil nil nil)))

  (setq session-names
        (delete-dups
         (copy-sequence
          (cond
           ((null session-names) nil)
           ((stringp session-names) (list session-names))
           (t session-names)))))

  (unless session-names
    (user-error "[easysession] No sessions selected"))

  (let ((session-files (mapcar
                        (lambda (name)
                          (cons name (easysession--session-file name)))
                        session-names)))
    (dolist (entry session-files)
      (unless (cdr entry)
        (user-error
         "[easysession] The session '%s' cannot be deleted because it doesn't exist"
         (car entry))))

    (when (and (called-interactively-p 'any)
               (> (length session-names) 1)
               (not
                (yes-or-no-p
                 (format "[easysession] Delete the sessions %s? "
                         (string-join session-names ", ")))))
      (user-error "[easysession] Deletion aborted"))

    (dolist (entry session-files)
      (let* ((file (cdr entry))
             (buffer (find-buffer-visiting file)))
        (when buffer
          (kill-buffer buffer))
        (delete-file file nil)))

    (when (called-interactively-p 'any)
      (easysession--message
       "Deleted session%s: %s"
       (if (> (length session-names) 1) "s" "")
       (string-join session-names ", ")))))

;;;###autoload
(defun easysession-load (&optional session-name)
  "Load a session.

If SESSION-NAME is non-nil, that session is loaded. Otherwise, the function
loads the current session if set, or defaults to the \"main\" session."
  (interactive
   (list (easysession--prompt-session-name
          "Load session: "
          (unless easysession-switch-to-exclude-current
            (or easysession--current-session-name
                ""))
          (and easysession-switch-to-exclude-current
               easysession--session-loaded))))
  (setq easysession-load-in-progress nil)
  (setq easysession--session-loaded nil)
  (unwind-protect
      (condition-case err
          (progn
            (let* ((session-name (or session-name
                                     easysession--current-session-name
                                     ;; The default session loaded when none is
                                     ;; specified is 'main'.
                                     "main"))
                   (session-file (easysession--session-file session-name))
                   ;; (uniquify-buffer-name-style nil)
                   )  ; Disable uniquify
              (setq easysession-load-in-progress session-name)

              (if (not session-file)
                  (easysession-set-current-session-name session-name)
                ;; Load and evaluate session
                (let ((session-data
                       (let ((coding-system-for-read 'utf-8-emacs)
                             (file-coding-system-alist nil))
                         (with-temp-buffer
                           (insert-file-contents session-file)

                           (when (= (buffer-size) 0)
                             (error
                              "[easysession] %s: Failed to read session information from %s"
                              session-name
                              session-file))

                           (goto-char (point-min))
                           (read (current-buffer))))))

                  ;; Load buffers first because the cursor, window-start, or
                  ;; hscroll might be altered by packages such as saveplace.
                  ;; This will allow the frameset to modify the cursor later on.
                  (run-hooks 'easysession-before-load-hook)

                  (dolist (handler (easysession-get-load-handlers))
                    (when handler
                      (cond
                       ((and (symbolp handler)
                             (fboundp handler))
                        (funcall handler session-data))

                       (t
                        (error
                         "[easysession] The following load handler is not a defined function: %s"
                         handler)))))

                  ;; Load the frame set
                  (easysession--load-frameset
                   session-data
                   (bound-and-true-p easysession-frameset-restore-geometry))

                  (when (called-interactively-p 'any)
                    (easysession--message "Session loaded: %s" session-name))

                  (easysession-set-current-session-name session-name)

                  (setq easysession--session-loaded t)

                  (run-hooks 'easysession-after-load-hook)))))
        (error
         (error "[easysession] easysession-load error: %s"
                (error-message-string err))))
    ;; Unwind protect
    (setq easysession-load-in-progress nil)))

;;;###autoload
(defun easysession-unload ()
  "Unload the currently loaded session.
If a session is active and marked as loaded, the session state is saved before
unloading.

The function then clears all internal indicators of an active session by
resetting the current session identifier and load flag.

This operation only affects in-memory state. Session data on disk is preserved."
  (interactive)
  (unwind-protect
      (when (and easysession--current-session-name
                 easysession--session-loaded)
        (easysession-save))
    (setq easysession--daemon-session-loaded nil)
    (setq easysession--current-session-name nil)
    (setq easysession--session-loaded nil)))

;;;###autoload
(defun easysession-edit (session-name)
  "Edit a session in `emacs-lisp-mode`.
If SESSION-NAME is nil, defaults to the current session.
Signal an error if the session file does not exist.
Returns the buffer visiting the session file.
The buffer is read-only if `easysession-edit-read-only` is non-nil."
  (interactive
   (list (easysession--prompt-session-name
          "Edit session: "
          nil
          nil
          easysession--current-session-name)))
  (let* ((session-file (easysession-get-session-file-path session-name))
         (buffer-before (get-file-buffer session-file)))
    (unless (file-regular-p session-file)
      (user-error "Session file does not exist: %s" session-file))
    (find-file session-file)

    (unless buffer-before
      (let ((buffer (get-file-buffer session-file)))
        (with-current-buffer buffer
          (lisp-mode)
          (when easysession-edit-read-only
            (read-only-mode 1))
          (current-buffer))))))

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
          (and easysession-switch-to-exclude-current
               easysession--session-loaded))))
  (let ((easysession-frameset-restore-geometry t))
    (easysession-switch-to session-name)))

;;;###autoload
(defun easysession-save (&optional session-name)
  "Save the current session.
SESSION-NAME is the name of the session."
  (interactive
   (list (easysession--prompt-session-name
          "Save session as: "
          (or easysession--current-session-name
              "")
          nil
          easysession--current-session-name)))
  (when (and (not session-name)
             (not easysession--current-session-name))
    (user-error "%s%s"
                "[easysession] No session is active. "
                "Load a session with `easysession-switch-to'"))
  (unwind-protect
      (progn
        (setq easysession-save-in-progress t)
        (run-hooks 'easysession-before-save-hook)
        (let* ((session-name (if session-name
                                 session-name
                               easysession--current-session-name))
               (session-file (easysession-get-session-file-path session-name))
               (data-frameset (easysession--save-frameset session-name))
               (data-frameset-geometry (easysession--save-frameset
                                        session-name t))
               (session-data nil)
               (session-dir (file-name-directory session-file))
               (uniquify-buffer-name-style nil))  ; Disable uniquify
          ;; Frameset
          (push (cons "frameset" data-frameset) session-data)
          (push (cons "frameset-geo" data-frameset-geometry) session-data)

          ;; Buffers and file buffers
          (let* ((buffers (funcall easysession-buffer-list-function)))
            (dolist (handler (easysession-get-save-handlers))
              (if (not (and handler
                            (symbolp handler)
                            (fboundp handler)))
                  (error "The following save handler is not a defined function: %s"
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

                      ;; Generate legacy list of buffers
                      (when (string= key "path-buffers")
                        (let (legacy-list-buffers)
                          (dolist (item value)
                            (let ((path (alist-get 'buffer-path item))
                                  (name (alist-get 'buffer-name item)))
                              (when (and path name)
                                (push (cons name path) legacy-list-buffers))))

                          (push (cons "buffers" legacy-list-buffers) session-data)))

                      ;; The following optimizes buffer processing by updating
                      ;; the list of buffers for the next iteration By setting
                      ;; buffers to the remaining-buffers returned by each
                      ;; handler function, it ensures that each subsequent
                      ;; handler only processes buffers that have not yet been
                      ;; handled. This approach avoids redundant processing of
                      ;; buffers that have already been classified or processed
                      ;; by previous handlers, resulting in more efficient
                      ;; processing. As a result, each handler operates on a
                      ;; progressively reduced set of buffers.
                      (setq buffers remaining-buffers)))))))

          (push (cons "file-format-version" easysession-file-version) session-data)
          (push (cons "mtime" (format-time-string "%Y-%m-%d %H:%M:%S %Z"))
                session-data)
          (push
           (cons
            "comment"
            "EasySession session file - https://github.com/jamescherti/easysession.el")
           session-data)

          (unless (file-directory-p session-dir)
            (make-directory session-dir :parents))

          (let* ((print-escape-newlines t)
                 (print-length nil)
                 (print-level nil)
                 (float-output-format nil)
                 (quote-sexp (easysession--serialize-to-quoted-sexp session-data))
                 (print-quoted t))
            (with-temp-buffer
              (prin1 (cdr quote-sexp) (current-buffer))
              (let ((coding-system-for-write 'utf-8-emacs)
                    (write-region-annotate-functions nil)
                    (write-region-post-annotation-function nil))
                (when easysession-save-pretty-print
                  (if (fboundp 'elisp-autofmt-buffer)
                      (elisp-autofmt-buffer)
                    (pp-buffer)))
                (write-region (point-min) (point-max) session-file nil 'silent)
                nil))

            (when (called-interactively-p 'any)
              (if (string= session-name easysession--current-session-name)
                  (easysession--message "Session saved: %s" session-name)
                (easysession--message "Session saved as: %s" session-name))))

          (run-hooks 'easysession-after-save-hook)))
    (setq easysession-save-in-progress nil)))

(defalias 'easysession-save-as 'easysession-save)
(make-obsolete 'easysession-save-as 'easysession-save "1.1.7")

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
          (format "%s to session: "
                  (if (and easysession--current-session-name
                           easysession-switch-to-save-session)
                      "Save and switch"
                    "Switch"))
          (unless easysession-switch-to-exclude-current
            (or easysession--current-session-name
                ""))
          (and easysession-switch-to-exclude-current
               easysession--session-loaded))))

  (setq session-name (or session-name
                         easysession--current-session-name))

  (unless session-name
    (user-error
     "[easysession] A session name must be provided to `easysession-switch-to'"))

  (let* ((new-session-file (easysession-get-session-file-path session-name))
         (new-session-exists (file-exists-p new-session-file))
         (session-reloaded (and easysession--current-session-name
                                (string= session-name
                                         easysession--current-session-name)))
         saved
         new-session)
    (when (or new-session-exists
              (or (not easysession-confirm-new-session)
                  (yes-or-no-p
                   (format
                    "[easysession] Session '%s' does not exist. Would you like to create it? "
                    session-name))))
      (when (and easysession--current-session-name
                 easysession-switch-to-save-session
                 (or (or (not easysession--session-loaded)
                         (not session-reloaded))
                     (yes-or-no-p
                      (format "[easysession] Do you want to save the current session '%s' before reloading it?"
                              easysession--current-session-name))))
        (easysession-save easysession--current-session-name)
        (setq saved t))

      (condition-case err
          (easysession-load session-name)
        (error
         (user-error "[easysession] Failed to load session '%s': %s"
                     session-name (error-message-string err))))

      (easysession-set-current-session-name session-name)

      (when (and (not session-reloaded)
                 (not (file-exists-p new-session-file)))
        (run-hooks 'easysession-new-session-hook)
        (easysession-save session-name)
        (setq new-session t))

      (cond
       (session-reloaded
        (easysession--message "Reloaded session: %s" session-name))
       (saved
        (easysession--message "Saved and switched to %ssession: %s"
                              (if new-session "new " "") session-name))
       (t (easysession--message "Switched to %ssession: %s"
                                (if new-session "new " "") session-name))))))

;;;###autoload
(define-minor-mode easysession-save-mode
  "Persist and restore your sessions."
  :global t
  :lighter (:eval
            (if easysession-save-mode-lighter-show-session-name
                easysession-save-mode-lighter-session-name-spec
              easysession-save-mode-lighter))
  :group 'easysession
  (if easysession-save-mode
      (progn
        (when (daemonp)
          ;; This ensures the session is saved before the last client frame is
          ;; closed in daemon mode, allowing correct restoration when a new
          ;; frame is created.
          (add-hook 'delete-frame-functions
                    #'easysession--persist-session-on-frame-delete-maybe))

        (when easysession--timer
          (cancel-timer easysession--timer)
          (setq easysession--timer nil))

        (when easysession-save-interval
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
