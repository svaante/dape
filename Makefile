export EMACS ?= $(shell which emacs)

ELFILES = dape.el dape-tests.el
ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

%.elc: %.el
	@echo Compiling $<
	@${EMACS} -batch -q -no-site-file -L . -f batch-byte-compile $<

test: $(ELCFILES)
	@${EMACS} -batch -l ert $(foreach file, $^, -l $(file)) -f ert-run-tests-batch-and-exit

clean:
	@rm -f *.elc
