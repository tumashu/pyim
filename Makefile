# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: test clean deps compile

EMACS_GENERIC_OPTS=--quick --directory . --directory .deps
EMACS_BATCH_OPTS:=--batch $(EMACS_GENERIC_OPTS)
RM=@rm -rf

XR_URL="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/xr.el?h=externals/xr"
ASYNC_URL="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/async.el?h=externals/async"
POSFRAME_URL="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/posframe.el?h=externals/posframe"
POPUP_URL="https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/popup.el?h=elpa/popup"
BASEDICT_URL="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/pyim-basedict.el?h=externals/pyim-basedict"

## Download pyim-basedict.pyim file from pyim-basedict v0.5.0, which commit
## is: 7495c974ada99f9fed96d8e85d8b97dabce9532c
BASEDICT_PYIM_URL="https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/pyim-basedict.pyim?h=externals/pyim-basedict&id=7495c974ada99f9fed96d8e85d8b97dabce9532c"

clean:
	$(RM) pyim-tests-temp-*
	$(RM) *.elc

deps:
	@mkdir -p .deps;
	@if [ ! -f .deps/xr.el ]; then curl -L $(XR_URL) > .deps/xr.el; fi;
	@if [ ! -f .deps/async.el ]; then curl -L $(ASYNC_URL) > .deps/async.el; fi;
	@if [ ! -f .deps/posframe.el ]; then curl -L $(POSFRAME_URL) > .deps/posframe.el; fi;
	@if [ ! -f .deps/popup.el ]; then curl -L $(POPUP_URL) > .deps/popup.el; fi;
	@if [ ! -f .deps/pyim-basedict.el ]; then curl -L $(BASEDICT_URL) > .deps/pyim-basedict.el; fi;
	@if [ ! -f .deps/pyim-basedict.pyim ]; then curl -L $(BASEDICT_PYIM_URL) > .deps/pyim-basedict.pyim; fi;

compile: deps
	$(RM) *.elc
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/pyim-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

# test: compile deps clean
test: compile deps
	@$(EMACS) $(EMACS_BATCH_OPTS) --load ./tests/pyim-tests.el
	$(RM) pyim-tests-temp-*

runemacs: deps
	@$(EMACS) $(EMACS_GENERIC_OPTS) --load ./tests/pyim-emacs-init.el
