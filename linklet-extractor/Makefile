.PHONY: expander regexp fasl

ZUO := zuo
RACKET := $(PLTHOME)/racket/bin/racket

# Target to check if zuo is installed
.PHONY: check_zuo_installed
check_zuo_installed:
	@if ! command -v $(ZUO) > /dev/null; then \
		echo "Error: $(ZUO) is not installed or not in your PATH."; \
		exit 1; \
	fi

.PHONY: check_racket_installed
check_racket_installed:
	@if [ ! -x "$(RACKET)" ]; then \
		echo "Error: $(RACKET) is not installed or not executable."; \
		exit 1; \
	fi

## Assumes PLTHOME
EXPANDER_DIR := $(PLTHOME)/racket/src/expander
REGEXP_DIR := $(PLTHOME)/racket/src/regexp

expander: expander-sexp
	$(RACKET) linklet-sexp-to-fasl.rkt --version --output ../expander.fasl compiled/expander.rktl

expander-json: expander-sexp
	$(RACKET) linklet-sexp-to-json.rkt --output ../expander.rktl.linklet compiled/expander.rktl

expander-sexp:
	$(ZUO) $(EXPANDER_DIR) expander-src

fasl: fasl-sexp
	$(RACKET) linklet-sexp-to-fasl.rkt --output ../fasl.fasl fasl.rktl

fasl-json: fasl-sexp
	$(RACKET) linklet-sexp-to-json.rkt --output ../fasl.rktl.linklet fasl.rktl

fasl-sexp:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	$(RACKET) -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -t fasl.rkt -o fasl.rktl

regexp: regexp-sexp
	$(RACKET) linklet-sexp-to-fasl.rkt --output ../regexp.fasl compiled/regexp.rktl

regexp-json: regexp-sexp
	$(RACKET) linklet-sexp-to-json.rkt --output ../regexp.rktl.linklet compiled/regexp.rktl

regexp-sexp:
	$(ZUO) $(REGEXP_DIR) regexp-src

expander-bytecode:
	$(MAKE) -C $(EXPANDER_DIR) bytecode
	$(RACKET) linkl-expand.rkt -e --output ../expander.rktl.linklet $(EXPANDER_DIR)/compiled/exp.zo

regexp-bytecode:
	$(MAKE) -C $(REGEXP_DIR) bytecode
	$(RACKET) linkl-expand.rkt -e --output ../regexp.rktl.linklet $(REGEXP_DIR)/compiled/regexp.zo

fasl-bytecode:
	raco make -v $(EXPANDER_DIR)/bootstrap-run.rkt
	$(RACKET) -t $(EXPANDER_DIR)/bootstrap-run.rkt -- -c compiled/cache-src ++knot read - -s -x -B -t fasl.rkt -o $(EXPANDER_DIR)/compiled/fasl.zo
	$(RACKET) linkl-expand.rkt -e --output ../fasl.rktl.linklet $(EXPANDER_DIR)/compiled/fasl.zo
