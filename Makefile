#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
#

export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa-stable\" . \"https://stable.melpa.org/packages/\") package-archives) \
  (customize-set-variable 'package-archive-priorities \
                          '((\"gnu\"    . 99) (\"nongnu\" . 80) \
                            (\"melpa-stable\" . 70) (\"melpa\"  . 0))) \
  (package-initialize) \
  (package-refresh-contents) \
  (dolist (pkg '(package-lint)) \
   (unless (assoc pkg package-archive-contents) \
     (package-refresh-contents)) \
   (unless (package-installed-p pkg) \
     (package-install pkg))))"

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: package-lint
package-lint:
	cask emacs -Q --eval ${INIT_PACKAGES} -batch -f package-lint-batch-and-exit easysession.el

.PHONY: test
test:
	if test -d tests; then cask emacs --batch -L . -L tests -l tests/test-easysession.el -f ert-run-tests-batch-and-exit; else true; fi
