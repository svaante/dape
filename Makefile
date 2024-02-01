export EMACS ?= $(shell which emacs)

JSONRPC = jsonrpc-1.0.19/jsonrpc.el

ELFILES = dape.el dape-tests.el
ELCFILES = $(addsuffix .elc, $(basename $(ELFILES)))

all: $(ELCFILES)

$(DEPS):
	@curl "https://elpa.gnu.org/packages/$(@D).tar.lz" -o $(@D).tar.lz
	@tar -xvf $(@D).tar.lz
	@rm $(@D).tar.lz

%.elc: %.el
	@echo Compiling $<
	@${EMACS} -Q \
	          -batch \
                  -no-site-file \
                  -L . \
                  --eval="(package-install-file \"$(JSONRPC)\")" \
                  -f batch-byte-compile $<

check: $(DEPS) $(ELCFILES)
	@${EMACS} -Q                              \
		  -batch 			  \
		  -l ert 			  \
		  --eval="(package-install-file \"$(JSONRPC)\")" \
		  $(foreach file, $^, -l $(file)) \
		  -f ert-run-tests-batch-and-exit

clean:
	@rm -f *.elc
	@rm -fr $(dir $(DEPS))
