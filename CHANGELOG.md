# easysession - Changelog

URL: https://github.com/jamescherti/easysession.el
Author: [James Cherti](https://www.jamescherti.com/)

## WIP

* easysession-save: Serialization now reliably produces `(QUOTE . SEXP)` pairs, preserving the exact structure of saved values. This ensures correct quoting of complex Emacs Lisp objects, including structs (`#s(...)`) and other otherwise unreadable or unloadable entities (`#>`), preventing errors when restoring sessions.

* Fix Emacs daemon mode issue

* Added the `easysession-visible-buffer-list` function, allowing session persistence and restoration to be limited to buffers that are currently visible in windows or associated with visible tab-bar tabs.

* Refactored `easysession-load` to use direct buffer-stream deserialization rather than string-based evaluation, reducing memory overhead and computational pressure when restoring large session files.

* Enhanced session buffer restoration logic (`easysession--handler-load-file-editing-buffers` and `easysession--handler-load-indirect-buffers`) to improve robustness and structural clarity.

* Added two interactive confirmations to `easysession-switch-to`:

  * Prompt the user before creating a new session, requiring explicit approval to switch to a session that does not yet exist.
  * When reloading the current session, prompt the user to confirm saving it before reloading it.

* Contributions by Emre Yolcu (@emreyolcu on GitHub):

  * Added Magit integration through the `easysession-magit` extension (`extensions/easysession-magit.el`). This introduces `easysession-magit-mode`, allowing Magit status, log, and diff buffers to be persisted and restored across sessions. Log and diff buffers retain their arguments, including selected revisions and ranges, ensuring continuity when working with Git repositories.
  * Introduced a mode registration system for persisting and restoring custom major modes across EasySession sessions. Users can register and unregister modes using `easysession-add-managed-major-mode` and `easysession-remove-managed-major-mode`.

* Add support for deleting multiple sessions at once via `easysession-delete`.

* Do not run `jit-lock-fontify-now` when `redisplay-skip-fontification-on-input` is non-nil.

* Support the `desktop-dont-save` frame parameter.

* Convert warnings to errors in `easysession-load`, `easysession-delete` for missing sessions

* Contribution by Herman He (@hermanhel on GitHub): Added support for persisting and restoring the narrowing state of both base and indirect buffers.

* Add the following to the session file: comment, file-format-version, and mtime.

* Add interactive session selection prompts to `easysession-load` and `easysession-save`.  Make `easysession-save-as` obsolete in favor of `easysession-save`

## 1.1.7

* Fix #48: Prevent clearing `file-name-handler-alist` during execution

## 1.1.6

* Update the `easysession-load` function to set the session name when the session file is absent
* Revise docstrings and refresh `README.md` for improved clarity and completeness

## 1.1.5

* Add the `easysession-scratch` extension to enable persisting and restoring the scratch buffer in session files
* Ensure the session name is set only after the session has been successfully loaded
* Improve geometry restoration
* Enable restoring TTY sessions to GUI
* Refine mode line and enhance error message clarity
* Close #39: Add a command to switch sessions and restore window geometry
* Fix #45: Update modeline and current session only if a session is loaded
* Resolve warning: Assignment to the free variable `trusted-content`
* Update handler functions to improve reliability
* Add pre-commit hooks for Emacs Lisp to enforce code quality
* Improve `easysession-save-handler-dolist-buffers` macro for buffer handling
* Add macro declarations: `(declare (indent 0) (debug t))`
* Rename `defcustom` to `easysession-switch-to-save-session`
* Introduce `defcustom`: `easysession-switch-to-inhibit-save`
* Introduce new functions: `easysession-set-current-session-name` and `easysession-reset`
* Revise docstrings and update `README.md` for clarity and completeness
* Remove the `.images/` directory to clean up unused files
* Ensure `easysession-frameset-restore-geometry` resets automatically after `easysession-load`
* Modify `easysession-save-as` to save a session without switching to it
* Remove `&optional` from `easysession-rename` for clarity

## 1.1.4

* Add macros to simplify the definition of load/save handlers, enabling users to create custom handlers and extend EasySession more easily
* Optimize EasySession functions to improve the performance of session loading and saving operations.
* Contribution by Artem Tseranu: Allow excluding the current session when switching sessions
* Rename session prompts to include the current session name
* Update `.github` files, docstrings, and GitHub Actions
* Improve session saving by making `write-region` more efficient and reliable for file output.
* Enhance how the session file is read from disk
* Fix `easysession-save-as` `(interactive)` form and update rename session to include the session name in the prompt
* Add a `.nosearch` file to the `tests/` directory
* Add `CHANGELOG.md`

## 1.1.3

* Enhance and refactor the source code
* Improve session loading and saving
* Enhance unit tests
* Replace the `f` package with built-in functions to reduce dependencies
* Fix: Adjust settings to ensure EasySession restores all frames
* Fix: Prevent the Dirvish package from breaking the session file
* Fix: Refactor frameset filtering into a dedicated function
* Fix: `lsp-ui-doc`: Failed to evaluate session information
* Add option to exclude specific functions from `find-file-hook` when restoring a file
* Replace `(dired-current-directory)` with `default-directory`
* Add variable: `easysession-frameset-restore-geometry`
* Ensure `easysession-load` sets `easysession--load-error` to `t` on failure
* Remove direct `require` and replace it with `default-directory`
* Add function: `easysession-path`
* Rename functions and variables such (`easysession--is-loading`, `easysession-get-session-name`, `easysession-get-session-file-path`)
* Enhance functions to get session file path and session name

## 1.1.2

* Ensure access to frames during the Emacs shutdown process
* Enable `tab-bar-mode` if the loaded session includes a `tab-bar`
* Removed reliance on `kill-emacs-hook` for cleanup, as frames were no longer accessible at that stage.
* Add a check to save handlers
* Add an error message when the handler type is not a function

## 1.1.1

* Refactor the load indirect buffers function: simplify logic and improve readability
* Enhance restoring frameset (customizable and disabled when running as a daemon)
* Contribution by Enzo Gurgel: Display the session name in the mode lighter.
* Make easysession functions utilize the `session-name` argument
* Make unit tests delete the test session
* Make `easysession-save-as` ask the user to enter a session when `session-name` is not specified
* Make `easysession-delete` return `t` when the session is successfully deleted
* Remove docstring warning
* Add autoload to interactive functions
* Add easysession-frameset-restore-force-display, easysession-frameset-restore-force-onscreen, additional frameset-restore options, easysession-frameset-restore-cleanup-frames, easysession-frameset-restore-reuse-frames
* Change the default value of `easysession-frameset-restore-force-onscreen` to `(display-graphic-p)`
* Ensure handlers return a non-nil value

## 1.1.0

* Implement handlers allowing EasySession to become extensible. It allows users to add their own handlers that can include entries in the session file.
* Fix: Fontify buffers when `redisplay-skip-fontification-on-input` is non-nil
* Create unit tests for EasySession using GitHub Actions and Cask
* Add predicate that determines if the session is saved automatically
* Update docstrings
* Renames `easysession-set-current-session` to `easysession--set-current-session`.
* Change lighter to EasySes
* Ignore `jit-lock-fontify-now` errors

## 1.0.5

* Fix issue that prevented EasySession from saving/loading frame geometry
* Use `bound-and-true-p` to check if `easysession--load-geometry` is set to `t`
* Add a defcustom containing a function to retrieve buffers for persistence and restoration
* Fix warnings
* Add `:nowarn` to `find-file-noselect`

## 1.0.4

* Optimize loading file editing buffers and indirect buffers
* Improve detection of indirect buffers
* Improve error handling and messages
* Enhance `easysession-set-current-session` session name check
* Ensure `easysession--handler-load-base-buffers` loads base buffers
* Fix: Ensure indirect buffer names match frameset buffer names
* Fix: Add the `easysession-quiet` defcustom
* Simplify the `easysession--handler-load-base-buffers` function
* Add `easysession-before-save-hook` and `easysession-after-save-hook`
* Add `:group` to `easysession-save-interval` and rename `easysession-timer` to `easysession--timer`
* Make easysession use `frameset-persistent-filter-alist` instead of `frameset-filter-alist`
* Make `easysession--init-frame-parameters-filters` parameter mandatory
* Fix `frameset-filter-alist` bug and improve session reading
* Rename variables (`easysession--is-valid-session-name` and `easysession-after-new-session-created-hook`)

## 1.0.3

* Handle frameset and buffer errors when loading a session, and update README.md
* Add `auto-save-interval` and `easysession--load-geometry`
* Correct `easysession-save-interval` variable name
* Add the `easysession--is-loading-p` variable
* Remove `interactive` from `easysession-load-including-geometry`
* Fix warnings

## 1.0.2

* New feature: save/load geometry
* Add the `easysession-rename` function
* Fix the `make-directory` error when a file's parent directory is missing
* Fix issue with `frameset-restore` caused by a change in the file format
* Fix error: Cannot open load file: No such file or directory: f
* Add new options, variables, and functions: `easysession-persist-geometry`, `easysession--get-geometry-frameset-filter-alist`, `frameset--text-pixel-width`, `frameset--text-pixel-height`, and `easysession-load-including-geometry`
* Fix warning: `called-interactively-p` called with 0 arguments but requires 1
* Fix warning: unused lexical argument `session-name`

## 1.0.1

* Fix border color and border width issues
* Add support for indirect buffers
* Rename variables and update comments
* Add list of geometry parameters
* Fix warnings
* Optimization: Decrease the frequency of calls to `easysession--init-frame-parameters-filters` to improve efficiency.

## 1.0.0

* Initial version of EasySession
* Add docstrings
* Add `easysession-mode`
* Add additional hooks
* Remove the `emacs-startup-hook` function
* Fix warning: the function `dired-current-directory` is not known to be defined
