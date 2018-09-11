.PHONY: expander fasl

## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander

expander:
	$(MAKE) -C $(EXPANDER_DIR) bytecode
	racket linkl-expand.rkt -e --output ../expander.rktl.linklet $(EXPANDER_DIR)/compiled/exp.zo
fasl:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	racket -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -B -t fasl.rkt -o $(EXPANDER_DIR)/compiled/fasl.zo
	racket linkl-expand.rkt -e --output ../fasl.rktl.linklet $(EXPANDER_DIR)/compiled/fasl.zo
