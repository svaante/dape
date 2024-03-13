export EMACS ?= $(shell which emacs)

JSONRPC = jsonrpc-1.0.25/jsonrpc.el

ELFILES = dape.el dape-tests.el
ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

$(JSONRPC):
	@curl "https://elpa.gnu.org/packages/$(@D).tar" -o $(@D).tar
	@tar -xvf $(@D).tar
	@rm $(@D).tar

%.elc: %.el $(JSONRPC)
	@echo Compiling $<
	@${EMACS} -Q \
	          -batch \
                  -no-site-file \
                  -L . \
                  --eval="(package-install-file \"$(JSONRPC)\")" \
                  -f batch-byte-compile $<

check: $(JSONRPC) $(ELCFILES)
	@${EMACS} -Q \
		  -batch \
		  -l ert \
		  --eval="(package-install-file \"$(JSONRPC)\")" \
		  $(foreach file, $^, -l $(file)) \
		  -f ert-run-tests-batch-and-exit

clean:
	@rm -f *.elc
	@rm -fr $(dir $(JSONRPC))
