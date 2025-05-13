# easysession.el - Easily persist and restore your Emacs editing sessions
![Build Status](https://github.com/jamescherti/easysession.el/actions/workflows/ci.yml/badge.svg)
[![MELPA](https://melpa.org/packages/easysession-badge.svg)](https://melpa.org/#/easysession)
[![MELPA Stable](https://stable.melpa.org/packages/easysession-badge.svg)](https://stable.melpa.org/#/easysession)
![License](https://img.shields.io/github/license/jamescherti/easysession.el)
![](https://raw.githubusercontent.com/jamescherti/easysession.el/main/.images/made-for-gnu-emacs.svg)

The **easysession** Emacs package is a session manager for Emacs that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), and Emacs frames. It offers a convenient and effortless way to manage Emacs editing sessions and utilizes built-in Emacs functions to persist and restore frames.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [easysession.el - Easily persist and restore your Emacs editing sessions](#easysessionel---easily-persist-and-restore-your-emacs-editing-sessions)
  - [Features](#features)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Customization](#customization)
    - [How to only persist and restore visible buffers](#how-to-only-persist-and-restore-visible-buffers)
    - [How to persist and restore global variables?](#how-to-persist-and-restore-global-variables)
    - [How to make the current session name appear in the mode-line?](#how-to-make-the-current-session-name-appear-in-the-mode-line)
    - [How to create an empty session setup](#how-to-create-an-empty-session-setup)
    - [How to configure easysession-save-mode to automatically save only the "main" session and let me manually save others?](#how-to-configure-easysession-save-mode-to-automatically-save-only-the-main-session-and-let-me-manually-save-others)
    - [Configuring EasySession with Emacs daemon mode](#configuring-easysession-with-emacs-daemon-mode)
    - [How to make EasySession kill all buffers before loading a session?](#how-to-make-easysession-kill-all-buffers-before-loading-a-session)
    - [How to create custom load and save handlers for non-file-visiting buffers](#how-to-create-custom-load-and-save-handlers-for-non-file-visiting-buffers)
  - [Frequently asked questions](#frequently-asked-questions)
    - [How does the author use easysession?](#how-does-the-author-use-easysession)
    - [What does EasySession offer that desktop.el doesn't?](#what-does-easysession-offer-that-desktopel-doesnt)
    - [Why not just improve and submit patches to desktop.el?](#why-not-just-improve-and-submit-patches-to-desktopel)
    - [How does it compare to activities.el?](#how-does-it-compare-to-activitiesel)
    - [Why not use one of the other third-party session packages?](#why-not-use-one-of-the-other-third-party-session-packages)
    - [Are there any testimonials from users?](#are-there-any-testimonials-from-users)
  - [License](#license)
  - [Links](#links)

<!-- markdown-toc end -->

## Features

Key features include:
- Quickly switch between sessions while editing without disrupting the frame geometry, enabling you to resume work immediately.
- Save and load file editing buffers, indirect buffers/clones, dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), the Emacs frames (with or without their position and size).
- Automatically save sessions by activating the mode with `easysession-save-mode` to ensure that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- Design focused on performance, simplicity, and efficiency.
- Helper functions: Switch to a session (i.e., load and change the current session) with `easysession-switch-to`, load the Emacs editing session with `easysession-load`, save the Emacs editing session with `easysession-save` and `easysession-save-as`, delete the current Emacs session with `easysession-delete`, and rename the current Emacs session with `easysession-rename`.
- Customizable: Users can implement their own handlers to manage non-file-editing buffers, enabling the creation of custom functions for restoring buffers.
- The ability to exclude specific functions from being executed in `find-file-hook` when Easysession restores a file.
- Display the currently loaded session in the modeline or as a lighter.
- Predicate that determines if the session is saved automatically.

## Installation

To install **easysession** from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install **easysession** from MELPA:

``` emacs-lisp
(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))
```

Note that:
- `easysession-load-including-geometry` is not needed after Emacs is loaded if you do not want EasySession to move or resize the Emacs frame when switching sessions. Instead, use `easysession-switch-to` or `easysession-load` to switch to another session or reload the current session without resizing or moving the Emacs frames.
- `easysession-mode-line` determines whether the current session name appears in the mode line by adding EasySession to `mode-line-misc-info`. Alternatively, the `easysession-save-mode-lighter-show-session-name` can be set to `t` to make EasySession display the session name in the lighter.
- `easysession-save-mode` ensures that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- The `easysession-save-interval` variable determines the interval between automatic session saves. Setting it to nil disables timer-based autosaving, causing `easysession-save-mode` to save only when Emacs exits.
- The author added 102 and 103 to `add-hook` in the code snippet above to ensure that the session is loaded after all other packages. (Using the depth 102 and 103 is particularly useful for those using [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d), where some optimizations restore `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
- When using Emacs in daemon mode (`emacs --daemon`), if using the `after-init-hook` results in issues on startup, an alternative approach is to use `server-after-make-frame-hook`. This hook ensures that the session is loaded once the client frame is created.

## Usage

It is recommended to use the following functions:
- `easysession-switch-to` to switch to another session or `easysession-load` to reload the current one,
- `easysession-save-as` to save the current session as the current name or another name.

To facilitate session management, consider using the following key mappings: `C-c l` for switching sessions with `easysession-switch-to`, and `C-c s` for saving the current session with `easysession-save-as`:
``` emacs-lisp
(global-set-key (kbd "C-c l") 'easysession-switch-to)
(global-set-key (kbd "C-c s") 'easysession-save-as)
```

## Customization

### How to only persist and restore visible buffers

By default, all file visiting buffers, dired buffers, and indirect buffers are persisted and restored.

To ensure that only visible buffers are saved and restored in your sessions, follow these steps:
- Create a function named `my-easysession-visible-buffer-list` to retrieve all buffers currently visible in your Emacs session. This function identifies buffers displayed in windows or tab-bar tabs.
- Set the variable `easysession-buffer-list-function` to use the newly defined function. This configuration ensures that only the buffers currently visible in windows or tab-bar tabs are persisted and restored.

Here is the Lisp code:
``` emacs-lisp
(defun my-easysession-visible-buffer-list ()
"Return a list of all visible buffers in the current session.
This includes buffers visible in windows or tab-bar tabs."
(let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
    (when (or
            ;; Windows
            (get-buffer-window buffer 'visible)
            ;; Tab-bar windows
            (and (bound-and-true-p tab-bar-mode)
                (fboundp 'tab-bar-get-buffer-tab)
                (tab-bar-get-buffer-tab buffer t nil)))
        (push buffer visible-buffers)))
    visible-buffers))

(setq easysession-buffer-list-function #'my-easysession-visible-buffer-list)
```

(`get-buffer-window` checks if a buffer is visible in any window. `tab-bar-get-buffer-tab` checks if the buffer is visible in a tab-bar tab window. The function collects all visible buffers into the visible-buffers list and returns it.)

### How to persist and restore global variables?

To persist and restore global variables in Emacs, you can use the built-in `savehist` Emacs package. This package is designed to save and restore minibuffer histories, but it can also be configured to save other global variables:
``` emacs-lisp
(use-package savehist
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))
```

(Each element added to `savehist-additional-variables` is a variable that will be persisted across Emacs sessions that use `savehist`.)

The **easysession** package can leverage `savehist` save the restore the current session name:
```emacs-lisp
(add-to-list 'savehist-additional-variables 'easysession--current-session-name)
```

### How to make the current session name appear in the mode-line?

You can display the current session name in the mode line by setting the following variable to t:
```emacs-lisp
(setq easysession-mode-line-misc-info t)
```

### How to create an empty session setup

To set up a minimal environment when easysession creates a new session, you can define a function that closes all other tabs, deletes all other windows, and switches to the scratch buffer. The following Emacs Lisp code demonstrates how to achieve this:

``` emacs-lisp
(defun my-empty-easysession ()
  "Set up a minimal environment when easysession creates a new session."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs))
  (delete-other-windows)
  (scratch-buffer))

(add-hook 'easysession-new-session-hook #'my-empty-easysession)
```

NOTE: The `easysession-new-session-hook` functions are called when the user switches to a non-existent session using the `easysession-switch-to` function.


### How to configure easysession-save-mode to automatically save only the "main" session and let me manually save others?

To set up `easysession-save-mode` to automatically save only the "main" session and allow you to manually save other sessions, add the following code to your configuration:
```emacs-lisp
(defun my-easysession-only-main-saved ()
  "Only save the main session."
  (when (string= "main" (easysession-get-current-session-name))
    t))
(setq easysession-save-mode-predicate 'my-easysession-only-main-saved)
```

### Configuring EasySession with Emacs daemon mode

When using Emacs in daemon mode (`emacs --daemon`), loading sessions needs to be triggered appropriately. If using the `after-init-hook` results in issues on startup, an alternative approach is to use `server-after-make-frame-hook`. This hook ensures that the session is loaded once the client frame is created.

Here is an example:
```emacs-lisp
(use-package easysession
  :ensure t
  :config
  (defun my-setup-easy-session ()
    (easysession-load-including-geometry)
    (easysession-save-mode)
    (remove-hook 'server-after-make-frame-hook #'my-setup-easy-session))

  (add-hook 'server-after-make-frame-hook #'my-setup-easy-session))
```

([read this discussion](https://github.com/jamescherti/easysession.el/discussions/15) for more information.)

### How to make EasySession kill all buffers before loading a session?

Here is a code snippet shared by u/capuche on Reddit that closes all buffers before loading a session:

``` emacs-lisp
(defun kill-old-session-buffers ()
  (save-some-buffers t)
  (mapc #'kill-buffer
        (cl-remove-if
         (lambda (buffer)
           (string= (buffer-name buffer) "*Messages*"))
         (buffer-list)))
  (delete-other-windows))
(add-hook 'easysession-before-load-hook #'kill-old-session-buffers)
(add-hook 'easysession-new-session-hook #'kill-old-session-buffers)
```

NOTE: The `easysession-new-session-hook` functions are called when the user switches to a non-existent session using the `easysession-switch-to` function.

### How to create custom load and save handlers for non-file-visiting buffers

EasySession is customizable. Users can implement their own handlers to manage non-file-editing buffers, enabling the creation of custom functions for restoring buffers.

Here is an example:
```elisp
;; NOTE: You cannot use the following reserved Easy Session keys:
;;       buffers, indirect-buffers, frameset, frameset-geo.
(setq my-easysession-buffers-key "MY-BUFFERS")

(defun my-easysession-load-handler (session-data)
  "Load handler for restoring specific buffers from SESSION-DATA."
  (let ((data (assoc-default my-easysession-buffers-key session-data)))
    (when data
      (message "[MY EASYSESSION TEST] LOAD HANDLER: Loaded data: %s" data)

      ;; TODO: Add code to process and restore session data associated
      ;; with `my-easysession-buffers-key'
      )))

(defun my-easysession-save-handler (buffers)
  "Save handler for saving specific buffers from the given BUFFERS list."
  (let ((saved-buffers '())
        (remaining-buffers buffers))
    (dolist (buf buffers)
      (with-current-buffer buf
        (let ((buffer-name (buffer-name)))
          (message "[MY EASYSESSION TEST] SAVE HANDLER: Processing buffer %s"
                   buffer-name)
          ;; TODO: Replace t with a condition like: (derived-mode-p 'MY-MODE)
          (if t
              ;; TODO: Replace "MY-BUFFER-DATA" with your data. It does not
              ;;       have to be a string; it can be a list, hash table, or
              ;;       any other data structure.
              (let ((buffer-data (cons buffer-name "MY-BUFFER-DATA")))
                (push buffer-data saved-buffers)
                (message "[MY EASYSESSION TEST] SAVE HANDLER: Buffer %s saved"
                         buffer-name))
            ;; If the buffer should not be saved, add it to remaining-buffers
            (push buf remaining-buffers)))))
    ;; Return an alist with the saved data and remaining buffers
    (list
     (cons 'key my-easysession-buffers-key)
     (cons 'buffers saved-buffers)
     (cons 'remaining-buffers remaining-buffers))))

(defun my-easysession-setup-handlers ()
  "Configure EasySession load and save handlers."
  (easysession-add-load-handler 'my-easysession-load-handler)
  (easysession-add-save-handler 'my-easysession-save-handler))

(add-hook 'easysession-before-load-hook #'my-easysession-setup-handlers)
(add-hook 'easysession-before-save-hook #'my-easysession-setup-handlers)
```

Replace `TODO` with the appropriate code and remove `[MY EASYSESSION TEST]` after debugging is complete.

## Frequently asked questions

### How does the author use easysession?

The author uses easysession by setting up each session to represent a distinct project or a specific "view" on a particular project, including various tabs (built-in tab-bar), window splits, dired buffers, and file editing buffers. This organization allows for the creation of dedicated environments for different tasks or aspects of a project, such as development, debugging, specific issue, and documentation. The author switches between projects and views of the same projects multiple times a day, and easysession helps significantly by allowing quick transitions between them.

### What does EasySession offer that desktop.el doesn't?

While `desktop.el` is a foundational session management tool for Emacs, it has several limitations:
- It primarily saves Emacs' state on exit and restores it on startup, making it difficult to switch between different session files during an editing session.
- The `desktop.el` package does not allow the user to easily choose whether to load sessions with or without modifying the Emacs frame geometry. This last feature is important in easysession because it allows switching between sessions without the annoyance of changing the window position or size.
- The `desktop.el` package saves and restores major modes and important global variables, which can prevent some packages from initializing correctly. For example, the `vdiff` package may stop working after comparing two files and reloading Emacs and the `desktop.el` session. This issue has also occurred with a few other packages.
- The `desktop.el` package can be bulky and slow in operation.
- The `desktop.el` package lacks support for saving and restoring indirect buffers (clones). Indirect buffers are secondary buffers that share the same content as an existing buffer but can have different point positions, narrowing, folds, and other buffer-local settings. This allows users to view and edit the same file or text content in multiple ways simultaneously without duplicating the actual data. There are third-party packages, such as desktop+, that extend desktop.el to restore indirect buffers. However, packages like desktop+ are still based on desktop.el and can cause the issues described above.

In contrast, easysession offers enhanced functionality:
- It supports saving and loading various buffer types, including indirect buffers (clones).
- It allows users to load or save different sessions while actively editing, without the need to restart Emacs.
- It excels in speed and efficiency, enabling seamless session management within Emacs.

### Why not just improve and submit patches to desktop.el?

It is preferable for EasySession to remain a third-party plugin, as this provides more flexibility for implementing new features. EasySession relies on the same built-in functions as desktop.el (e.g., frameset) but includes additional features that enhance the experience of persisting and restoring sessions. EasySession is also customizable, allowing users to implement their own handlers to persist and restore new types of non-file-visiting buffers.

### How does it compare to activities.el?

- EasySession is designed for loading, saving, and switching entire sessions, while Activities focuses on managing "activities" and allows for multiple activities within a single session.
- EasySession supports restoring indirect buffers, whereas Activities does not.
- EasySession allows you to choose whether to restore the geometry (position, width, and height) of your frames.
- EasySession relies on built-in Emacs functions for saving and loading frames and tab-bar tabs, ensuring compatibility and support from Emacs developers. In contrast, Activities uses custom functions, such as those for restoring the tab-bar.
- EasySession is customizable. Users can implement their own handlers to manage non-file-backed buffers, enabling the creation of custom functions for restoring such buffers.

### Why not use one of the other third-party session packages?

There are some existing packages, such as minimal-session-saver, save-visited-files, sesman, and psession. However, these packages have the following limitations:
- None of them can restore indirect buffers (clones). Indirect buffers, which can be created using `clone-indirect-buffer`, are secondary buffers that share the same content as an existing buffer but can have different point positions, narrowing, folds, and other buffer-local settings. This allows users to view and edit the same file or text content in multiple ways simultaneously without duplicating the actual data.
- The minimal-session-saver and save-visited-files packages are no longer maintained and cannot restore the frameset and the tab-bar.
- Sesman is designed to implement some IDE features in Emacs.
- Psession cannot switch between sessions quickly, with or without modifying the the Emacs frame geometry. This last feature is important in easysession because it allows switching between sessions without the annoyance of changing the window position or size.

Easysession can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames geometry). It is similar to Vim or Neovim sessions because it loads and restores your editing environment, including buffers, windows, tabs, and other settings, allowing you to resume work exactly where you left off.

### Are there any testimonials from users?

- [UnitaryInverse on Reddit](https://www.reddit.com/r/emacs/comments/1jah0e4/comment/mho5kqj/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1): I have started using easysession more and more on my Spacemacs setup and it great! I can have a “lab notes” setup, a coding/simulation setup (I’m a physicist), a course planning setup for the courses I teach, and a personal setup all in one. Each one with custom windows setup so I spend SO MUCH less time splitting and moving windows. What a great package.
- [Hungariantoast on Reddit](https://www.reddit.com/r/emacs/comments/1i93ly5/comment/m980q04/): "I have a single raylib-experiments repository that I have been writing a bunch of separate, miniature gamedev projects in. This package has made the process of creating, managing, and restoring each of those little coding sessions such a breeze. Thanks for writing it"
- [ghoseb on GitHub](https://github.com/jamescherti/easysession.el/issues/21): Thanks a lot for your amazing packages! Easysession works great 🎉

## License

The easysession Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
- [easysession.el @MELPA](https://melpa.org/#/easysession)
- There is also a Vim version of Easysession: [vim-easysession](https://github.com/jamescherti/vim-easysession)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
