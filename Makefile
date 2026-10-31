#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

INIT_PACKAGES="(progn \
  (require 'package) \
  (setq package-user-dir \"$(CASK_DIR)/elpa\") \
  (push '(\"melpa-stable\" . \"https://stable.melpa.org/packages/\") package-archives) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (customize-set-variable 'package-archive-priorities \
						  '((\"gnu\"          . 99) (\"nongnu\" . 80) \
							(\"melpa-stable\" . 70) (\"melpa\"  . 0))) \
  (package-initialize) \
  (unless package-archive-contents \
	(package-refresh-contents)) \
  (dolist (pkg '(package-lint)) \
	(unless (package-installed-p pkg) \
	  (package-install pkg))))"

$(CASK_DIR): Cask easysession.el
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask exec emacs -Q --batch -L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $$(cask files); \
		(ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: package-lint
package-lint: cask
	cask exec emacs -Q --batch -L . \
		--eval ${INIT_PACKAGES} \
		-f package-lint-batch-and-exit $$(cask files)

.PHONY: test
test: cask
	if test -d tests; then \
		cask exec emacs -Q --batch -L . -L tests \
			-l tests/test-easysession.el \
			-f ert-run-tests-batch-and-exit; \
	fi
