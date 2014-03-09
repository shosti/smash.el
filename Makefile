all: test

test: compile
	cask exec ert-runner -l smash.elc

smash.elc: smash.el
	cask exec emacs --batch -f batch-byte-compile smash.el

compile: smash.elc

clean:
	rm -f smash.elc

.PHONY: all test clean
