.PHONY: expander

## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander
THIS_DIR := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

expander:
	$(MAKE) -C $(EXPANDER_DIR) bytecode
	racket linkl-expand.rkt -e --output ../expander.rktl.linklet $(EXPANDER_DIR)/compiled/exp.zo
