# easysession.el - Easily persist and restore your Emacs editing sessions
[![MELPA](https://melpa.org/packages/easysession-badge.svg)](https://melpa.org/#/easysession)
![](https://raw.githubusercontent.com/jamescherti/easysession.el/main/.images/made-for-gnu-emacs.svg)
![Build Status](https://github.com/jamescherti/easysession.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/easysession.el)

The `easysession.el` Emacs package is a lightweight session manager for Emacs that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), and Emacs frames. It offers a convenient and effortless way to manage Emacs editing sessions and utilizes built-in Emacs functions to persist and restore frames.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [easysession.el - Easily persist and restore your Emacs editing sessions](#easysessionel---easily-persist-and-restore-your-emacs-editing-sessions)
    - [Features](#features)
    - [Installation](#installation)
    - [Usage](#usage)
    - [Frequently asked questions](#frequently-asked-questions)
        - [Configuring EasySession with Emacs daemon mode](#configuring-easysession-with-emacs-daemon-mode)
        - [How to only persist and restore visible buffers](#how-to-only-persist-and-restore-visible-buffers)
        - [How to persist and restore global variables?](#how-to-persist-and-restore-global-variables)
        - [How to create an empty session setup](#how-to-create-an-empty-session-setup)
        - [How to configure easysession-save-mode to automatically save only the "main" session and let me manually save others?](#how-to-configure-easysession-save-mode-to-automatically-save-only-the-main-session-and-let-me-manually-save-others)
        - [How to make EasySession kill all buffers before loading a session?](#how-to-make-easysession-kill-all-buffers-before-loading-a-session)
        - [How does the author use easysession?](#how-does-the-author-use-easysession)
        - [Why not use the desktop.el?](#why-not-use-the-desktopel)
        - [Why not use one of the other third-party session packages?](#why-not-use-one-of-the-other-third-party-session-packages)
    - [License](#license)
    - [Links](#links)

<!-- markdown-toc end -->

## Features

Key features include:
- Minimalist design focused on performance and simplicity, avoiding unnecessary complexity.
- Quickly switch between sessions while editing without disrupting the frame geometry, enabling you to resume work immediately.
- Save and load file editing buffers, indirect buffers/clones, dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), the Emacs frames (with or without their position and size).
- Automatically save sessions by activating the mode with `easysession-save-mode` to ensure that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- Helper functions: Switch to a session (i.e., load and change the current session) with `easysession-switch-to`, load the Emacs editing session with `easysession-load`, save the Emacs editing session with `easysession-save` and `easysession-save-as`, delete the current Emacs session with `easysession-delete`, and rename the current Emacs session with `easysession-rename`.

## Installation

To install the `easysession` from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install `easysession` from MELPA:

``` emacs-lisp
(use-package easysession
  :ensure t
  :custom
  ;; Interval between automatic session saves
  (easysession-save-interval (* 10 60))
  ;; Make the current session name appear in the mode-line
  (easysession-mode-line-misc-info t)
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 102))
```

Note that:
- `easysession-load-including-geometry` is not needed after Emacs is loaded if you do not want EasySession to move or resize the Emacs frame when switching sessions. Instead, use `easysession-switch-to` or `easysession-load` to switch to another session or reload the current session without resizing or moving the Emacs frames.
- `easysession-mode-line` determines whether the current session name appears in the mode line by adding EasySession to `mode-line-misc-info`. Alternatively, the `easysession-save-mode-lighter-show-session-name` can be set to `t` to make EasySession display the session name in the lighter.
- The `easysession-save-mode` ensures that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- The `easysession-save-interval` variable determines the interval between automatic session saves. Setting it to nil disables timer-based autosaving, causing `easysession-save-mode` to save only when Emacs exits.
- The author added `102` to `add-hook` to ensure that the session is loaded after everything else. Using `102` is particularly useful for those using [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d), where some settings such as `file-name-handler-alist` are restored at depth `101` during `emacs-startup-hook`.

## Usage

It is recommended to use the following functions:
- `easysession-switch-to` to switch to another session or `easysession-load` to reload the current one,
- `easysession-save-as` to save the current session as the current name or another name.

To facilitate session management, consider using the following key mappings: `C-c l` for switching sessions with `easysession-switch-to`, and `C-c s` for saving the current session with `easysession-save-as`:
``` emacs-lisp
(global-set-key (kbd "C-c l") 'easysession-switch-to)
(global-set-key (kbd "C-c s") 'easysession-save-as)
```

## Frequently asked questions

### Configuring EasySession with Emacs daemon mode

When using Emacs in daemon mode (`emacs --daemon`), loading sessions needs to be triggered appropriately. If using the after-init-hook results in issues on startup, an alternative approach is to use `server-after-make-frame-hook`. This hook ensures that the session is loaded once the client frame is created.

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

The `easysession` package can leverage `savehist` save the restore the current session name:
``` emacs-lisp
(add-to-list 'savehist-additional-variables 'easysession--current-session-name)
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

### How to configure easysession-save-mode to automatically save only the "main" session and let me manually save others?

To set up `easysession-save-mode` to automatically save only the "main" session and allow you to manually save other sessions, add the following code to your configuration:
```emacs-lisp
(defun my-easysession-only-main-saved ()
  "Only save the main session."
  (when (string= "main" (easysession-get-current-session-name))
    t))
(setq easysession-save-mode-predicate 'my-easysession-only-main-saved)
```

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

### How does the author use easysession?

The author uses `easysession.el` by setting up each session to represent a distinct project or a specific "view" on a particular project, including various tabs (built-in tab-bar), window splits, dired buffers, and file editing buffers. This organization allows for the creation of dedicated environments for different tasks or aspects of a project, such as development, debugging, specific issue, and documentation. The author switches between projects and views of the same projects multiple times a day, and `easysession.el` helps significantly by allowing quick transitions between them.

### Why not use the desktop.el?

While `desktop.el` is a foundational session management tool for Emacs, it has several limitations:
- It can be bulky and slow in operation.
- It primarily saves Emacs' state on exit and restores it on startup, making it difficult to switch between different session files during an editing session.
- The `desktop.el` package saves and restores major modes and important global variables, which can prevent some packages from initializing correctly. For example, the `vdiff` package may stop working after comparing two files and reloading Emacs and the `desktop.el` session. This issue has also occurred with a few other packages.
- The `desktop.el` package lacks support for saving and restoring indirect buffers (clones). Indirect buffers are secondary buffers that share the same content as an existing buffer but can have different point positions, narrowing, folds, and other buffer-local settings. This allows users to view and edit the same file or text content in multiple ways simultaneously without duplicating the actual data. There are third-party packages, such as desktop+, that extend desktop.el to restore indirect buffers. However, packages like desktop+ are still based on desktop.el and can cause the issues described above.

In contrast, `easysession.el` offers enhanced functionality:
- It supports saving and loading various buffer types, including indirect buffers (clones).
- It allows users to load or save different sessions while actively editing, without the need to restart Emacs.
- It excels in speed and efficiency, enabling seamless session management within Emacs.

### Why not use one of the other third-party session packages?

There are some existing packages, such as minimal-session-saver, save-visited-files, sesman, and psession. However, these packages have the following limitations:
- None of them can restore indirect buffers (clones). Indirect buffers, which can be created using `clone-indirect-buffer`, are secondary buffers that share the same content as an existing buffer but can have different point positions, narrowing, folds, and other buffer-local settings. This allows users to view and edit the same file or text content in multiple ways simultaneously without duplicating the actual data.
- The minimal-session-saver and save-visited-files packages are no longer maintained and cannot restore the frameset and the tab-bar.
- Sesman is designed to implement some IDE features in Emacs.
- Psession cannot switch between sessions quickly, with or without modifying the the Emacs frame geometry. This last feature is important in easysession.el because it allows switching between sessions without the annoyance of changing the window position or size.

Easysession is lightweight and can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, the tab-bar, and the Emacs frames (with or without the Emacs frames geometry). It is similar to Vim or Neovim sessions because it loads and restores your editing environment, including buffers, windows, tabs, and other settings, allowing you to resume work exactly where you left off.

## License

The `easysession` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
- [easysession.el @MELPA](https://melpa.org/#/easysession)
- There is also a Vim version of Easysession: [vim-easysession](https://github.com/jamescherti/vim-easysession)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for `ansible-lint`.
