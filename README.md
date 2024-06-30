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
  :straight (easysession
             :type git
             :host github
             :repo "jamescherti/easysession.el")
  :config
  (add-hook 'emacs-startup-hook #'easysession-mode))
```

## Features

- Automatically save and restore the Emacs editing session,
- Automatically save the current editing session when Emacs is closed or when a file is save,
- Manually save the current Emacs editing session,
- Switch to a different session,
- List the available sessions,
- Delete the current Emacs session,
- Specify the directory where all the saved sessions are located.

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
