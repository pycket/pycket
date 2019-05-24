.PHONY: expander regexp fasl

## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander
REGEXP_DIR := $(PLTHOME)/racket/src/regexp

expander: expander-sexp
	racket linklet-sexp-to-fasl.rkt --version --output ../expander.fasl expander.rktl

expander-json: expander-sexp
	racket linklet-sexp-to-json.rkt --output ../expander.rktl.linklet expander.rktl

expander-sexp:
	$(MAKE) -C $(EXPANDER_DIR) expander-src-generate
	cp $(EXPANDER_DIR)/compiled/expander.rktl .

fasl: fasl-sexp
	racket linklet-sexp-to-fasl.rkt --output ../fasl.fasl fasl.rktl

fasl-json: fasl-sexp
	racket linklet-sexp-to-json.rkt --output ../fasl.rktl.linklet fasl.rktl

fasl-sexp:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	racket -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -t fasl.rkt -o fasl.rktl

regexp:
	$(MAKE) -C $(REGEXP_DIR) regexp-src-generate
	cp $(REGEXP_DIR)/compiled/regexp.rktl .
	racket linklet-sexp-to-json.rkt --output ../regexp.rktl.linklet regexp.rktl

expander-bytecode:
	$(MAKE) -C $(EXPANDER_DIR) bytecode
	racket linkl-expand.rkt -e --output ../expander.rktl.linklet $(EXPANDER_DIR)/compiled/exp.zo

regexp-bytecode:
	$(MAKE) -C $(REGEXP_DIR) bytecode
	racket linkl-expand.rkt -e --output ../regexp.rktl.linklet $(REGEXP_DIR)/compiled/regexp.zo

fasl-bytecode:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	racket -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -B -t fasl.rkt -o $(EXPANDER_DIR)/compiled/fasl.zo
	racket linkl-expand.rkt -e --output ../fasl.rktl.linklet $(EXPANDER_DIR)/compiled/fasl.zo
