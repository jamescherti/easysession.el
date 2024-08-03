# easysession.el - Easily persist and restore your Emacs editing sessions
[![MELPA](https://melpa.org/packages/easysession-badge.svg)](https://melpa.org/#/easysession)
![](https://raw.githubusercontent.com/jamescherti/easysession.el/main/.images/made-for-gnu-emacs.svg)

The `easysession.el` Emacs package is a lightweight session manager for Emacs that can persist and restore file editing buffers, indirect buffers/clones, Dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), and Emacs frames. It offers a convenient and effortless way to manage Emacs editing sessions and utilizes built-in Emacs functions to persist and restore frames.

## Features

Key features include:
- Minimalist design focused on performance and simplicity, avoiding unnecessary complexity.
- Quickly switch between sessions while editing without disrupting the frame geometry, enabling you to resume work immediately.
- Save and load file editing buffers, indirect buffers/clones, dired buffers, windows/splits, the built-in tab-bar (including tabs, their buffers, and windows), the Emacs frames (with or without their position and size).
- Automatically save sessions by activating the mode with `easysession-save-mode` to ensure that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- Helper functions: Switch to a session (i.e., load and change the current session) with `easysession-switch-to`, load the Emacs editing session with `easysession-load`, save the Emacs editing session with `easysession-save` and `easysession-save-as`, delete the current Emacs session with `easysession-delete`, and rename the current Emacs session with `easysession-rename`.

## Installation

The `easysession` package can be installed from MELPA by adding the following to your Emacs init file:
```
(use-package easysession
  :ensure t
  :custom
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 98)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 99))
```

Note that:
- `easysession-load-including-geometry` is not needed after Emacs is loaded if you do not want EasySession to move or resize the Emacs frame when switching sessions. Instead, use `easysession-switch-to` or `easysession-load` to switch to another session or reload the current session without resizing or moving the Emacs frames.
- The `easysession-save-mode` ensures that the current session is automatically saved every `easysession-save-interval` seconds and when emacs quits.
- The `easysession-save-interval` variable determines the interval between automatic session saves. Setting it to nil disables timer-based autosaving, causing `easysession-save-mode` to save only when Emacs exits.

## Usage

It is recommended to use the following functions:
- `easysession-switch-to` to switch to another session or `easysession-load` to reload the current one,
- `easysession-save-as` to save the current session as the current name or another name.

To facilitate session management, consider using the following key mappings: `C-c l` for switching sessions with `easysession-switch-to`, and `C-c s` for saving the current session with `easysession-save-as`:
```
(global-set-key (kbd "C-c l") 'easysession-switch-to)
(global-set-key (kbd "C-c s") 'easysession-save-as)
```

## Frequently asked questions

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

### Why not use the desktop.el?

While `desktop.el` is a foundational session management tool for Emacs, it has several limitations:
- It can be bulky and slow in operation.
- It primarily saves Emacs' state on exit and restores it on startup, making it difficult to switch between different session files during an editing session.
- It lacks support for saving and restoring indirect buffers (clones). Indirect buffers are secondary buffers that share the same content as an existing buffer but can have different point positions, narrowing, folds, and other buffer-local settings. This allows users to view and edit the same file or text content in multiple ways simultaneously without duplicating the actual data.
- The `desktop.el` package saves and restores major modes and important global variables, which can prevent some packages from initializing correctly. For example, the `vdiff` package may stop working after comparing two files and reloading Emacs and the `desktop.el` session. This issue has also occurred with a few other packages.

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

### How does the author use easysession?

The author uses `easysession.el` by setting up each session to represent a distinct project or a specific "view" on a particular project, including various tabs (built-in tab-bar), window splits, dired buffers, and file editing buffers. This organization allows for the creation of dedicated environments for different tasks or aspects of a project, such as development, debugging, and documentation. The author switches between projects and views of the same projects multiple times a day, and `easysession.el` helps significantly by allowing quick transitions between them.

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
- [easysession.el @MELPA](https://melpa.org/#/easysession)
- There is also a Vim version of Easysession: [vim-easysession](https://github.com/jamescherti/vim-easysession)
