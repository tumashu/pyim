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
	@if [ ! -f deps/xr.el ]; then curl -L https://github.com/mattiase/xr/raw/master/xr.el > deps/xr.el; fi;
	@if [ ! -f deps/async.el ]; then curl -L https://github.com/jwiegley/emacs-async/raw/master/async.el > deps/async.el; fi;
	@if [ ! -f deps/popup.el ]; then curl -L https://github.com/auto-complete/popup-el/raw/master/popup.el > deps/popup.el; fi;


.PHONY: test
test: deps clean
	@$(EMACS) -batch -Q -L . -l deps/xr.el -l deps/async.el -l deps/popup.el -l pyim.el -l tests/pyim-tests.el
