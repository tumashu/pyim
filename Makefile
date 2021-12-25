# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

.PHONY: deps
deps:
	@mkdir -p deps;
	@if [ ! -f deps/xr.el ]; then curl -L https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/xr.el?h=externals/xr > deps/xr.el; fi;
	@if [ ! -f deps/async.el ]; then curl -L https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/async.el?h=externals/async > deps/async.el; fi;
	@if [ ! -f deps/popup.el ]; then curl -L https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/popup.el?h=elpa/popup > deps/popup.el; fi;
	@if [ ! -f deps/pyim-basedict.pyim ]; then curl -L https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/pyim-basedict.pyim?h=externals/pyim-basedict > deps/pyim-basedict.pyim; fi;
	@if [ ! -f deps/pyim-basedict.el ]; then curl -L https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/pyim-basedict.el?h=externals/pyim-basedict > deps/pyim-basedict.el; fi;

.PHONY: test
test: deps clean
	@$(EMACS) -batch --quick --directory . --directory ./deps --load ./tests/pyim-tests.el
