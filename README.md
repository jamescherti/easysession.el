# easysession.el - Easily persist and restore your Emacs editing sessions

The Emacs package `easysession.el` offers a convenient and effortless way to persist and restore Emacs editing sessions. It can significantly increase productivity and save a lot of time for users who frequently switch between different projects and those who frequently open and close the Emacs editor.

In addition to its automatic session management capabilities, the `easysession.el` Emacs package also offers a variety of useful Emacs commands that allow users to save, load, list and delete session.

## Installation

### Install using straight.el
To install the `easysession` using `straight.el`:
1. If you haven't already done so, [add the `straight.el` bootstrap code to your init file ](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started)

2. Add the following code to your Emacs init file:
```
(use-package easysession
  :ensure t
  :straight (easysession :type git
                         :host github
                         :repo "jamescherti/easysession.el")
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 100)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 100))
```

## Features

- Save and load file editing buffers, indirect buffers/clones, dired buffers, tab-bar, the Emacs frame, and it is extensible (users can add other handlers).
- Automatically save sessions by activating the mode with `easysession-save-mode`.
  This mode saves the current editing session when Emacs is closed.
- Save the Emacs editing session with `easysession-save` and `easysession-save-as`.
- Load the Emacs editing session with `easysession-load`.
- Switch to a session (i.e., load and change the default session) with `easysession-switch-to`.
- Delete the current Emacs session with `easysession-delete`.

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Comparison with alternatives

### desktop.el (built-in)

While `desktop.el` is a foundational session management tool for Emacs, it has several limitations:
- It can be bulky and slow in operation.
- It primarily saves Emacs' state on exit and restores it on startup, making it difficult to switch between different session files during an editing session.
- It lacks support for saving and restoring indirect buffers (clones).

In contrast, `easysession.el` offers enhanced functionality:
- It supports saving and loading various buffer types, including indirect buffers (clones).
- It allows users to load or save different sessions while actively editing, without the need to restart Emacs.
- It excels in speed and efficiency, enabling seamless session management within Emacs.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
- There is also a Vim version of Easysession: [vim-easysession](https://github.com/jamescherti/vim-easysession)
