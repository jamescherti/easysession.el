# easysession

Easily persist and restore your editing sessions effortlessly.

## Installation

### Install using package-vc
```
(package-vc-install
 '(easysession
   :url "https://github.com/jamescherti/easysession.el"))
(add-hook 'emacs-startup-hook #'easysession-mode)
```

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


## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [easysession.el @GitHub](https://github.com/jamescherti/easysession.el)
