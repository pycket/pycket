.PHONY: expander regexp fasl

## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander
REGEXP_DIR := $(PLTHOME)/racket/src/regexp

expander:
	$(MAKE) -C $(EXPANDER_DIR) bytecode
	racket linkl-expand.rkt -e --output ../expander.rktl.linklet $(EXPANDER_DIR)/compiled/exp.zo
regexp:
	$(MAKE) -C $(REGEXP_DIR) bytecode
	racket linkl-expand.rkt -e --output ../regexp.rktl.linklet $(REGEXP_DIR)/compiled/regexp.zo
fasl:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	racket -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -B -t fasl.rkt -o $(EXPANDER_DIR)/compiled/fasl.zo
	racket linkl-expand.rkt -e --output ../fasl.rktl.linklet $(EXPANDER_DIR)/compiled/fasl.zo
