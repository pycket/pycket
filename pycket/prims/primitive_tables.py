from pycket.values import W_Symbol
from pycket.prims.expose import define_nyi, prim_env, expose

DEBUG = False

sym = W_Symbol.make

# make_primitive_table takes a list of string
# and turns that into a list of W_Symbol
def make_primitive_table(ls_str):
    table = [None] * len(ls_str)
    for i, exposed_id in enumerate(ls_str):
        table[i] = sym(exposed_id)

    return table


place_str = [
    "place-break", "place-channel-get", "place-channel-put",
    "place?", "place-enabled?", "place-channel", "place-dead-evt",
    "place-kill", "place-message-allowed?", "place-channel?", "dynamic-place",
    "place-wait", "place-pumper-threads", "place-shared?"
]

paramz_str = [
    "check-for-break", "break-enabled-key", "parameterization-key", "cache-configuration",
    "exception-handler-key", "extend-parameterization", "reparameterize", "security-guard-check-file",
    "security-guard-check-file-link", "security-guard-check-network"
]

# Exports that are not exposed to Racket, but
# can be used in a linklet:
internal_str = [
    "call/cm", "extract-procedure", "set-ctl-c-handler!", "register-linklet-instantiate-continuation!",
    "impersonator-val", "impersonate-ref", "impersonate-set!", "struct-type-install-properties!",
    "structure-type-lookup-prefab-uid", "struct-type-constructor-add-guards", "register-struct-constructor!", "register-struct-predicate!",
    "register-struct-field-accessor!", "register-struct-field-mutator!", "struct-property-set!"
]

futures_str = [
    "futures-enabled?", "processor-count", "future", "future?",
    "touch", "would-be-future", "current-future", "make-fsemaphore",
    "fsemaphore?", "fsemaphore-post", "fsemaphore-wait", "fsemaphore-try-wait?",
    "fsemaphore-count", "reset-future-logs-for-tracing!", "mark-future-trace-end!"
]

flfxnum_str = [
    "fx->fl", "fl->fx", "fxabs", "fx+",
    "fx-", "fx*", "fxquotient", "fxremainder",
    "fx+/wraparound", "fx-/wraparound", "fx*/wraparound", "fxlshift/wraparound",
    "fxmodulo", "fxand", "fxior", "fxxor",
    "fxnot", "fxrshift", "fxlshift", "fx>=",
    "fxrshift/logical", "fxpopcount", "fxpopcount32", "fxpopcount16",
    "fx>", "fx=", "fx<", "fx<=",
    "fxmin", "fxmax", "fxvector?", "fxvector",
    "make-fxvector", "shared-fxvector", "make-shared-fxvector", "fxvector-length",
    "fxvector-ref", "fxvector-set!", "fl+",
    "fl-", "fl*", "fl/", "flabs",
    "flsqrt", "flexp", "fllog", "flsin", "flsingle",
    "flcos", "fltan", "flasin", "flacos",
    "flatan", "flfloor", "flceiling", "flround",
    "fltruncate", "flexpt", "fl=", "fl<",
    "fl<=", "fl>", "fl>=", "flmin",
    "flmax", "->fl", "fl->exact-integer", "flvector?",
    "flvector", "make-flvector", "shared-flvector", "make-shared-flvector",
    "flvector-length", "flvector-ref", "flvector-set!",
    "flreal-part", "flimag-part", "make-flrectangular",
    "flbit-field"
]

extfl_str = [
    "extflmin", "extflatan", "extfl+", "extflmax",
    "extflfloor", "extfl<=", "extflvector", "extfl->floating-point-bytes",
    "extfltruncate", "->extfl", "extflsin", "extflonum?",
    "extflacos", "extflvector-ref", "extflexp", "extflabs",
    "extflonum-available?", "extfl<", "extfl->exact", "extfl->fx",
    "extfl->inexact", "extflvector-set!", "make-extflvector", "extflcos",
    "extflvector-length", "extfl/", "extflceiling", "floating-point-bytes->extfl",
    "extfl>=", "make-shared-extflvector", "extflround", "extfl->exact-integer",
    "real->extfl", "extflexpt", "fx->extfl", "shared-extflvector",
    "extfl>", "extfllog", "extflvector?", "extfl=",
    "extflsqrt", "extfl*", "extfl-", "extfltan",
    "extflasin"
]

network_str = [
    "tcp-abandon-port", "tcp-accept", "tcp-accept-evt", "tcp-accept-ready?",
    "tcp-accept/enable-break", "tcp-addresses", "tcp-close", "tcp-connect",
    "tcp-connect/enable-break", "tcp-listen", "tcp-listener?", "tcp-port?",
    "udp?", "udp-bind!", "udp-bound?", "udp-close",
    "udp-connect!", "udp-connected?", "udp-multicast-interface", "udp-multicast-join-group!",
    "udp-multicast-leave-group!", "udp-multicast-loopback?", "udp-multicast-set-interface!", "udp-multicast-set-loopback!",
    "udp-multicast-set-ttl!", "udp-multicast-ttl", "udp-open-socket", "udp-receive!", "udp-ttl", "udp-set-ttl!",
    "udp-receive!*", "udp-receive!-evt", "udp-receive!/enable-break", "udp-receive-ready-evt",
    "udp-set-receive-buffer-size!",
    "udp-send", "udp-send*", "udp-send-evt", "udp-send-ready-evt",
    "udp-send-to", "udp-send-to*", "udp-send-to-evt", "udp-send-to/enable-break",
    "udp-send/enable-break"
]

foreign_str = [
    "assert-ctype-representation",
    "_bool", "_bytes", "_double", "_double*",
    "_fixint", "_fixnum", "_float", "_fpointer",
    "_gcpointer", "_int16", "_int32", "_int64",
    "_int8", "_longdouble", "_path", "_pointer",
    "_scheme", "_stdbool", "_string/ucs-4", "_string/utf-16",
    "_symbol", "_ufixint", "_ufixnum", "_uint16",
    "_uint32", "_uint64", "_uint8", "_void",
    "compiler-sizeof", "cpointer-gcable?", "cpointer-tag", "cpointer?",
    "ctype-alignof", "ctype-basetype", "ctype-c->scheme", "ctype-scheme->c",
    "ctype-sizeof", "ctype?", "end-stubborn-change", "extflvector->cpointer",
    "ffi-call", "ffi-callback", "ffi-callback?", "ffi-lib",
    "ffi-lib-name", "ffi-lib?", "ffi-obj", "ffi-obj-lib",
    "ffi-obj-name", "ffi-obj?", "flvector->cpointer", "free",
    "ffi-lib-unload",
    "free-immobile-cell", "lookup-errno", "make-array-type", "make-cstruct-type",
    "make-ctype", "make-late-weak-box", "make-late-weak-hasheq", "make-sized-byte-string", "ffi-maybe-call-and-callback-core",
    "make-stubborn-will-executor", "make-union-type", "malloc", "malloc-immobile-cell",
    "memcpy", "memmove", "memset", "offset-ptr?",
    "prop:cpointer", "ptr-add", "ptr-add!", "ptr-equal?",
    "ptr-offset", "ptr-ref", "ptr-set!", "saved-errno",
    "set-cpointer-tag!", "set-ptr-offset!", "vector->cpointer",
    "ffi-callback-maker", "ffi-call-maker", "make-late-will-executor"
]

linklet_str = [
    "linklet?", "compile-linklet", "recompile-linklet", "eval-linklet", "read-compiled-linklet", "instantiate-linklet",
    "linklet-import-variables", "linklet-export-variables", "instance?", "make-instance", "instance-name", "instance-data",
    "instance-variable-names", "instance-variable-value", "instance-set-variable-value!", "instance-unset-variable!",
    "linklet-directory?", "hash->linklet-directory", "linklet-directory->hash", "linklet-bundle?", "hash->linklet-bundle", "linklet-add-target-machine-info",
    "linklet-bundle->hash", "variable-reference?", "variable-reference->instance", "variable-reference-constant?",
    "primitive-table", "variable-reference-from-unsafe?",
    "compiled-position->primitive", "linklet-virtual-machine-bytes",
    "read-linklet-bundle-hash", "write-linklet-bundle-hash", "instance-describe-variable!",
    "primitive-lookup", "linklet-summarize-target-machine-info",
    "linklet-cross-machine-type",
]

unsafe_str = [
    "unsafe-car", "unsafe-cdr", "unsafe-list-tail",
    "unsafe-list-ref", "unsafe-cons-list", "unsafe-fx+",
    "unsafe-fx-", "unsafe-fx*", "unsafe-fxquotient",
    "unsafe-fxremainder", "unsafe-fxmodulo", "unsafe-fxabs",
    "unsafe-fxand", "unsafe-fxior", "unsafe-fxxor",
    "unsafe-fxnot", "unsafe-fxrshift", "unsafe-fxlshift",
    "unsafe-fx=", "unsafe-fx<", "unsafe-fx>",
    "unsafe-fx>=", "unsafe-fx<=", "unsafe-fxmin",
    "unsafe-fxmax", "unsafe-fl+", "unsafe-fl-",
    "unsafe-fl*", "unsafe-fl/", "unsafe-flabs",
    "unsafe-fl=", "unsafe-fl<", "unsafe-fl>",
    "unsafe-fl>=", "unsafe-fl<=", "unsafe-flmin",
    "unsafe-flmax", "unsafe-fx->fl", "unsafe-fl->fx",
    "unsafe-flrandom", "unsafe-flsqrt", "unsafe-make-flrectangular",
    "unsafe-flreal-part", "unsafe-flimag-part", "unsafe-extfl*",
    "unsafe-extfl+", "unsafe-extfl-", "unsafe-extfl/",
    "unsafe-extfl<", "unsafe-extfl<=", "unsafe-extfl=",
    "unsafe-extfl>", "unsafe-extfl>=", "unsafe-extflabs",
    "unsafe-extflmax", "unsafe-extflmin", "unsafe-extflsqrt",
    "unsafe-extfl->fx", "unsafe-fx->extfl", "unsafe-extflvector-length",
    "unsafe-extflvector-ref", "unsafe-extflvector-set!", "unsafe-unbox*",
    "unsafe-set-box*!", "unsafe-set-box!", "unsafe-unbox",
    "unsafe-box*-cas!", "unsafe-mcar", "unsafe-mcdr",
    "unsafe-set-mcar!", "unsafe-set-mcdr!", "unsafe-vector-ref",
    "unsafe-vector-set!", "unsafe-vector*-ref", "unsafe-vector*-set!",
    "unsafe-vector*-cas!", "unsafe-vector-length", "unsafe-vector*-length",
    "unsafe-vector-copy",
    "unsafe-fxvector-length", "unsafe-fxvector-ref", "unsafe-fxvector-set!",
    "unsafe-flvector-length", "unsafe-flvector-ref", "unsafe-flvector-set!",
    "unsafe-s16vector-ref", "unsafe-s16vector-set!", "unsafe-u16vector-ref",
    "unsafe-u16vector-set!", "unsafe-f64vector-ref", "unsafe-f64vector-set!",
    "unsafe-f80vector-set!", "unsafe-f80vector-ref", "unsafe-bytes-length",
    "unsafe-bytes-ref", "unsafe-bytes-set!", "unsafe-string-length",
    "unsafe-string-set!", "unsafe-string-ref", "unsafe-struct-ref",
    "unsafe-struct-set!", "unsafe-struct*-ref", "unsafe-struct*-set!",
    "unsafe-immutable-hash-iterate-key+value", "unsafe-immutable-hash-iterate-pair",
    "unsafe-immutable-hash-iterate-value", "unsafe-immutable-hash-iterate-key",
    "unsafe-immutable-hash-iterate-first", "unsafe-immutable-hash-iterate-next",
    "unsafe-mutable-hash-iterate-key+value", "unsafe-mutable-hash-iterate-pair",
    "unsafe-mutable-hash-iterate-value", "unsafe-mutable-hash-iterate-key",
    "unsafe-mutable-hash-iterate-first", "unsafe-mutable-hash-iterate-next",
    "unsafe-weak-hash-iterate-key+value", "unsafe-weak-hash-iterate-pair",
    "unsafe-weak-hash-iterate-value", "unsafe-weak-hash-iterate-key",
    "unsafe-weak-hash-iterate-first", "unsafe-weak-hash-iterate-next",
    "unsafe-ephemeron-hash-iterate-key+value", "unsafe-ephemeron-hash-iterate-pair",
    "unsafe-ephemeron-hash-iterate-value", "unsafe-ephemeron-hash-iterate-key",
    "unsafe-ephemeron-hash-iterate-first", "unsafe-ephemeron-hash-iterate-next",
    "unsafe-chaperone-procedure", "unsafe-impersonate-procedure", "unsafe-impersonate-vector", "unsafe-impersonate-hash",
    "unsafe-chaperone-vector", "unsafe-undefined", "check-not-unsafe-undefined",
    "check-not-unsafe-undefined/assign", "prop:chaperone-unsafe-undefined",
    "chaperone-struct-unsafe-undefined",
    "unsafe-start-breakable-atomic",
    "unsafe-end-breakable-atomic", "unsafe-in-atomic?", "unsafe-set-on-atomic-timeout!",
    "unsafe-thread-at-root",
    "unsafe-register-process-global", "unsafe-make-security-guard-at-root",
    "unsafe-abort-current-continuation/no-wind", "unsafe-call-with-composable-continuation/no-wind",
    "unsafe-poller", "unsafe-poll-fd",
    "unsafe-poll-ctx-fd-wakeup", "unsafe-poll-ctx-eventmask-wakeup", "unsafe-poll-ctx-milliseconds-wakeup",
    "unsafe-signal-received", "unsafe-set-sleep-in-thread!",
    "unsafe-file-descriptor->port", "unsafe-socket->port",
    "unsafe-file-descriptor->semaphore", "unsafe-socket->semaphore",
    "unsafe-port->file-descriptor", "unsafe-port->socket",
    "unsafe-get-place-table", "unsafe-call-in-os-thread",
    "unsafe-make-os-semaphore", "unsafe-os-semaphore-post", "unsafe-os-semaphore-wait",
    "unsafe-os-thread-enabled?", "unsafe-struct*-cas!",
    "unsafe-add-post-custodian-shutdown",
    "unsafe-root-continuation-prompt-tag",
    "unsafe-make-place-local",
    "unsafe-place-local-ref",
    "unsafe-place-local-set!",
    "unsafe-char<?", "unsafe-char<=?", "unsafe-char=?", "unsafe-char>=?", "unsafe-char>?",
    "unsafe-char->integer",
    "unsafe-add-global-finalizer", "unsafe-add-collect-callbacks",
    "unsafe-remove-collect-callbacks"
]

# This table omits anything that the expander implements itself,
# since the expander will export its own variant instead of the
# `kernel-table` variant.
kernel_str = [
    "*", "+", "-",
    "/", "<", "<=",
    "=", ">", ">=",
    "quotient", "quotient/remainder", "remainder",
    "abort-current-continuation", "abs", "absolute-path?",
    "add1", "acos", "alarm-evt",
    "andmap", "angle",
    "append", "apply", "arithmetic-shift",
    "asin", "assoc", "assq",
    "assv", "atan", "banner",
    "bitwise-and", "bitwise-bit-set?", "bitwise-bit-field",
    "bitwise-ior", "bitwise-not", "bitwise-xor",
    "boolean?", "box",
    "box-cas!", "box-immutable", "box?",
    "break-enabled", "break-thread", "build-path",
    "build-path/convention-type", "byte-ready?", "byte-pregexp",
    "byte-pregexp?", "byte-regexp", "byte-regexp?",
    "byte?", "bytes", "bytes->immutable-bytes",
    "bytes->list", "bytes->path", "bytes->path-element",
    "bytes->string/latin-1", "bytes->string/locale", "bytes->string/utf-8",
    "bytes-append", "bytes-close-converter", "bytes-convert",
    "bytes-convert-end", "bytes-converter?", "bytes-copy",
    "bytes-copy!", "bytes-fill!", "bytes-length",
    "bytes-open-converter", "bytes-ref", "bytes-set!",
    "bytes-utf-8-index", "bytes-utf-8-length", "bytes-utf-8-ref",
    "bytes>?", "bytes<?", "bytes=?",
    "bytes?",
    "caadr", "call-in-nested-thread", "call-with-composable-continuation",
    "call-with-continuation-barrier", "call-with-continuation-prompt", "call-with-current-continuation", "call-in-continuation",
    "call-with-escape-continuation", "call-with-immediate-continuation-mark", "call-with-input-file",
    "call-with-output-file", "call-with-semaphore", "call-with-semaphore/enable-break",
    "call-with-values", "ceiling", "channel?",
    "channel-put-evt?", "chaperone?",
    "chaperone-of?", "chaperone-box", "chaperone-continuation-mark-key",
    "chaperone-channel", "chaperone-evt", "chaperone-hash",
    "chaperone-procedure", "chaperone-procedure*", "chaperone-prompt-tag",
    "chaperone-struct", "chaperone-struct-type", "chaperone-vector",
    "chaperone-vector*", "char->integer", "char-alphabetic?",
    "char-downcase", "char-foldcase", "char-general-category",
    "char-graphic?", "char-blank?", "char-iso-control?",
    "char-numeric?", "char-ready?", "char-lower-case?",
    "char-punctuation?", "char-symbolic?", "char-title-case?",
    "char-upper-case?", "char-upcase", "char-titlecase",
    "char-whitespace?", "char-utf-8-length", "char<=?",
    "char<?", "char=?", "char>=?",
    "char>?", "char?", "char-ci<=?",
    "char-ci<?", "char-ci=?", "char-ci>=?",
    "char-ci>?", "checked-procedure-check-and-extract",
    "cleanse-path", "close-input-port", "close-output-port",
    "collect-garbage", "complex?",
    "compile-allow-set!-undefined", "compile-enforce-module-constants", "compile-context-preservation-enabled",
    "complete-path?", "continuation-marks", "continuation-mark-key?",
    "continuation-mark-set?", "continuation-mark-set-first", "continuation-mark-set->list",
    "continuation-mark-set->list*", "continuation-mark-set->context", "continuation-prompt-available?",
    "continuation-prompt-tag?", "continuation?", "copy-file",
    "cos", "current-code-inspector", "current-command-line-arguments",
    "current-compile-realm",
    "current-continuation-marks", "current-directory",
    "current-directory-for-user", "current-drive", "current-environment-variables",
    "current-error-port", "current-evt-pseudo-random-generator", "current-force-delete-permissions",
    "current-gc-milliseconds", "current-get-interaction-input-port", "current-inexact-milliseconds", "current-inexact-monotonic-milliseconds",
    "current-input-port", "current-inspector", "current-load-extension",
    "current-load-relative-directory", "current-locale", "current-logger",
    "current-memory-use", "current-milliseconds", "current-output-port",
    "current-preserved-thread-cell-values", "current-print",
    "current-process-milliseconds", "current-prompt-read", "current-pseudo-random-generator",
    "current-read-interaction", "current-seconds", "current-security-guard",
    "current-subprocess-custodian-mode", "current-thread-group",
    "current-thread-initial-stack-size", "current-write-relative-directory", "custodian?",
    "custodian-box?", "custodian-box-value", "custodian-limit-memory",
    "custodian-managed-list", "custodian-memory-accounting-available?", "custodian-require-memory",
    "custodian-shutdown-all", "custom-print-quotable?", "custom-print-quotable-accessor",
    "custom-write?", "custom-write-accessor", "datum-intern-literal",
    "default-continuation-prompt-tag", "delete-directory", "delete-file",
    "denominator", "directory-exists?", "directory-list",
    "display", "dump-memory-stats",
    "dynamic-wind", "environment-variables-ref", "environment-variables-set!",
    "environment-variables-copy", "environment-variables-names", "environment-variables?",
    "eof", "eof-object?", "ephemeron?",
    "ephemeron-value", "eprintf", "eq-hash-code",
    "eq?", "equal-hash-code", "equal-always-hash-code", "equal-secondary-hash-code",
    "equal?", "equal-always?", "equal?/recur", "eqv?",
    "eqv-hash-code", "error", "error-display-handler",
    "error-escape-handler", "error-print-context-length", "error-print-source-location",
    "error-print-width", "error-value->string-handler", "eval-jit-enabled",
    "even?", "exact-integer?",
    "exact-nonnegative-integer?", "exact-positive-integer?", "exact?",
    "exact->inexact", "executable-yield-handler", "exit",
    "exit-handler", "exn-continuation-marks", "exn-message",
    "exn?", "expand-user-path", "exp",
    "explode-path", "expt", "file-exists?",
    "file-or-directory-modify-seconds", "file-or-directory-identity",
    "file-or-directory-permissions", "file-or-directory-type",
    "file-position", "file-position*", "file-size",
    "file-stream-buffer-mode", "file-stream-port?", "file-truncate",
    "filesystem-change-evt", "filesystem-change-evt?", "filesystem-change-evt-cancel",
    "filesystem-root-list", "find-system-path", "fixnum?",
    "flonum?", "floor", "floating-point-bytes->real",
    "flush-output", "for-each", "format",
    "fprintf", "gcd", "gensym",
    "get-output-bytes", "get-output-string", "global-port-print-handler",
    "get-installation-name",
    "handle-evt?", "hash", "hashalw",
    "hash-clear", "hash-clear!", "hash-copy",
    "hash-count", "hash-eq?", "hash-eqv?",
    "hash-equal?", "hash-equal-always?", "hash-for-each", "hash-iterate-first",
    "hash-iterate-key", "hash-iterate-key+value", "hash-iterate-next",
    "hash-iterate-pair", "hash-iterate-value", "hash-keys-subset?",
    "hash-map", "hash-placeholder?", "hash-ref", "hash-ref-key",
    "hash-remove", "hash-remove!", "hash-set",
    "hash-set!", "hash-weak?", "hash-strong?", "hash-ephemeron?", "hash?",
    "hasheq", "hasheqv", "imag-part",
    "immutable?", "impersonate-box", "impersonate-channel",
    "impersonate-continuation-mark-key", "impersonate-hash", "impersonate-procedure",
    "impersonate-procedure*", "impersonate-prompt-tag", "impersonate-struct",
    "impersonate-vector", "impersonate-vector*", "impersonator?",
    "impersonator-ephemeron", "impersonator-of?", "impersonator-property?",
    "impersonator-prop:application-mark", "impersonator-property-accessor-procedure?", "inexact?",
    "inexact-real?", "inexact->exact", "input-port?",
    "inspector-superior?", "inspector?", "integer->char",
    "integer->integer-bytes", "integer-bytes->integer", "integer-length",
    "integer-sqrt", "integer-sqrt/remainder", "integer?",
    "interned-char?", "kill-thread", "lcm",
    "length", "link-exists?", "list",
    "list*", "list->bytes", "list->string",
    "list->vector", "list-ref", "list-tail",
    "list?", "list-pair?", "load-on-demand-enabled",
    "locale-string-encoding", "log", "logger?",
    "logger-name", "log-all-levels", "log-level?",
    "log-level-evt", "log-max-level", "log-message",
    "log-receiver?", "magnitude", "make-bytes",
    "make-continuation-mark-key", "make-continuation-prompt-tag",
    "make-custodian", "make-custodian-box", "make-derived-parameter",
    "make-directory", "make-environment-variables", "make-ephemeron",
    "make-ephemeron-hash", "make-ephemeron-hasheq", "make-ephemeron-hasheqv",
    "make-file-or-directory-link", "make-hash", "make-hashalw", "make-hash-placeholder",
    "make-hasheq", "make-hasheq-placeholder", "make-hasheqv",
    "make-hasheqv-placeholder", "make-input-port", "make-immutable-hash",
    "make-immutable-hashalw", "make-immutable-hasheq",
    "make-immutable-hasheqv", "make-impersonator-property",
    "make-inspector", "make-known-char-range-list", "make-logger",
    "make-log-receiver", "make-output-port", "make-parameter",
    "make-pthread-parameter",
    "make-phantom-bytes", "make-pipe", "make-placeholder",
    "make-plumber", "make-polar", "make-prefab-struct",
    "make-pseudo-random-generator", "make-reader-graph", "make-rectangular",
    "make-security-guard", "make-shared-bytes",
    "make-sibling-inspector", "make-string", "make-struct-field-accessor",
    "make-struct-field-mutator", "make-struct-type", "make-struct-type-property",
    "make-thread-cell", "make-thread-group", "make-vector",
    "make-weak-box", "make-weak-hash", "make-weak-hasheq",
    "make-weak-hashalw", "make-ephemeron-hashalw",
    "make-weak-hasheqv", "make-will-executor", "map",
    "max", "memq", "memv", "min", "modulo",
    "most-positive-fixnum", "most-negative-fixnum",
    "nack-guard-evt", "negative?", "never-evt",
    "newline", "not", "null",
    "null?", "number->string", "number?",
    "numerator", "object-name", "odd?",
    "open-input-bytes", "open-input-file", "open-input-output-file",
    "open-input-string", "open-output-bytes", "open-output-file",
    "open-output-string", "ormap", "output-port?",
    "parameter?", "parameter-procedure=?", "parameterization?",
    "path->bytes", "path->complete-path", "path->directory-path",
    "path->string", "path-convention-type", "path-element->bytes",
    "path-element->string", "path-for-some-system?", "path?",
    "path<?", "peek-byte", "peek-byte-or-special",
    "peek-bytes", "peek-bytes!", "peek-bytes-avail!",
    "peek-bytes-avail!*", "peek-bytes-avail!/enable-break", "peek-char-or-special",
    "peek-char", "peek-string", "peek-string!",
    "phantom-bytes?", "pipe-content-length", "placeholder?",
    "placeholder-get", "placeholder-set!",
    "plumber-flush-all", "plumber-flush-handle?",
    "plumber?", "poll-guard-evt", "port-closed?",
    "port-closed-evt", "port-commit-peeked", "port-count-lines!",
    "port-count-lines-enabled", "port-counts-lines?", "port-file-identity",
    "port-file-unlock", "port-next-location", "port-display-handler",
    "port-print-handler", "port-progress-evt", "port-provides-progress-evts?",
    "port-read-handler", "set-port-next-location!", "port-try-file-lock?",
    "port-write-handler", "port-writes-atomic?", "port-writes-special?",
    "positive?", "prefab-key->struct-type", "prefab-key?",
    "prefab-struct-type-key+field-count",
    "prefab-struct-key", "pregexp", "pregexp?",
    "primitive?", "primitive-closure?",
    "primitive-result-arity", "printf", "print",
    "print-as-expression", "print-boolean-long-form", "print-box",
    "print-graph", "print-hash-table", "print-mpair-curly-braces",
    "print-pair-curly-braces", "print-reader-abbreviations", "print-struct",
    "print-syntax-width", "print-vector-length", "print-unreadable",
    "procedure-arity", "procedure-arity-mask",
    "procedure-arity?", "procedure-arity-includes?",
    "procedure-extract-target", "procedure-impersonator*?",
    "procedure-reduce-arity", "procedure-reduce-arity-mask",
    "procedure-rename", "procedure-realm",
    "procedure-result-arity", "procedure->method",
    "procedure?", "procedure-specialize", "procedure-struct-type?",
    "procedure-closure-contents-eq?", "progress-evt?", "prop:arity-string",
    "prop:authentic", "prop:checked-procedure", "prop:custom-print-quotable",
    "prop:custom-write", "prop:equal+hash",
    "prop:impersonator-of", "prop:incomplete-arity", "prop:method-arity-error",
    "prop:procedure", "prop:object-name", "prop:output-port",
    "prop:input-port", "prop:sealed", "pseudo-random-generator?", "pseudo-random-generator->vector",
    "pseudo-random-generator-vector?", "random", "random-seed",
    "raise", "raise-user-error", "rational?",
    "read-accept-bar-quote", "read-byte", "read-byte-or-special",
    "read-bytes", "read-bytes!", "read-bytes-avail!",
    "read-bytes-avail!*", "read-bytes-avail!/enable-break", "read-bytes-line",
    "read-case-sensitive", "read-char", "read-char-or-special",
    "read-line", "read-on-demand-source", "read-string",
    "read-string!", "real?", "real-part",
    "real->double-flonum", "real->floating-point-bytes", "real->single-flonum", "single-flonum-available?",
    "regexp", "regexp-match", "regexp-match/end",
    "regexp-match-positions", "regexp-match-positions/end", "regexp-match-peek",
    "regexp-match-peek-immediate", "regexp-match-peek-positions", "regexp-match-peek-positions/end",
    "regexp-match-peek-positions-immediate", "regexp-match-peek-positions-immediate/end", "regexp-match?",
    "regexp-max-lookbehind", "regexp-replace", "regexp-replace*",
    "regexp?", "relative-path?", "rename-file-or-directory",
    "replace-evt", "resolve-path", "reverse",
    "round", "seconds->date", "security-guard?",
    "semaphore?", "semaphore-peek-evt?",
    "semaphore-try-wait?",
    "semaphore-wait/enable-break", "set-box!", "set-box*!", "set-phantom-bytes!",
    "shared-bytes", "shell-execute", "simplify-path",
    "sin", "single-flonum?", "sleep",
    "split-path", "sqrt", "string",
    "string->bytes/latin-1", "string->bytes/locale", "string->bytes/utf-8",
    "string->immutable-string", "string->list", "string->number",
    "string->path", "string->path-element", "string->symbol",
    "string->uninterned-symbol", "string->unreadable-symbol",
    "string-append", "string-append-immutable"
    "string-ci=?", "string-ci<=?", "string-ci<?",
    "string-ci>=?", "string-ci>?", "string-copy",
    "string-copy!", "string-downcase", "string-fill!",
    "string-foldcase", "string-length", "string-locale-downcase",
    "string-locale-ci<?", "string-locale-ci=?", "string-locale-ci>?",
    "string-locale-upcase", "string-locale<?", "string-locale=?",
    "string-locale>?", "string-normalize-nfc", "string-normalize-nfd",
    "string-normalize-nfkc", "string-normalize-nfkd", "string-port?",
    "string-ref", "string-set!", "string-titlecase",
    "string-upcase", "string-utf-8-length", "string<=?",
    "string<?", "string=?", "string>=?",
    "string>?", "string?", "struct->vector",
    "struct-type?", "struct?", "struct-accessor-procedure?",
    "struct-mutator-procedure?", "struct-constructor-procedure?", "struct-info",
    "struct-predicate-procedure?", "struct-type-info", "struct-type-make-constructor",
    "struct-type-make-predicate", "struct-type-property-accessor-procedure?", "struct-type-property?",
    "sub1", "subbytes", "subprocess?",
    "subprocess", "subprocess-group-enabled", "subprocess-kill",
    "subprocess-pid", "subprocess-status", "subprocess-wait",
    "substring", "symbol->string", "symbol->immutable-string", "symbol-interned?",
    "symbol-unreadable?", "symbol<?", "symbol?",
    "sync/enable-break",
    "sync/timeout/enable-break", "system-big-endian?", "system-idle-evt",
    "system-language+country", "system-library-subpath", "system-path-convention-type",
    "system-type", "tan", "terminal-port?",
    "time-apply", "thread/suspend-to-kill",
    "thread?", "thread-cell?", "thread-cell-ref",
    "thread-cell-set!", "thread-cell-values?", "thread-dead?",
    "thread-group?",
    "thread-receive", "thread-receive-evt",
    "thread-resume-evt", "thread-rewind-receive", "thread-running?",
    "thread-send", "thread-receive", "thread-suspend",
    "thread-try-receive", "thread-wait",
    "true-object?", "truncate", "unbox", "unbox*",
    "uncaught-exception-handler", "unquoted-printing-string", "unquoted-printing-string?",
    "unquoted-printing-string-value", "values", "vector",
    "vector->immutable-vector", "vector->list", "vector->pseudo-random-generator",
    "vector->pseudo-random-generator!", "vector->values", "vector-cas!",
    "vector-copy", "vector-append", "vector-set/copy",
    "vector-copy!", "vector-fill!", "vector-immutable",
    "vector*-copy", "vector*-set/copy", "vector*-append",
    "vector-extend", "vector*-extend",
    "vector-length", "vector-ref", "vector-set!",
    "vector*-length", "vector*-ref", "vector*-set!",
    "vector-set-performance-stats!", "vector?", "version",
    "stencil-vector?", "stencil-vector-length", "stencil-vector-ref",
    "stencil-vector-mask",
    "void", "void?", "weak-box?",
    "weak-box-value", "will-execute", "will-executor?",
    "will-register", "will-try-execute", "with-input-from-file",
    "with-output-to-file", "write",
    "write-byte", "write-bytes", "write-bytes-avail",
    "write-bytes-avail*", "write-bytes-avail/enable-break", "write-bytes-avail-evt",
    "write-char", "write-special", "write-special-avail*",
    "write-special-evt", "write-string", "zero?",
    "keyword<?", "string->keyword", "keyword->string", "keyword->immutable-string",
    "keyword?", "cons", "pair?",
    "car", "cdr", "caar",
    "cadr", "cdar", "cddr",
    "caaar", "caadr", "cadar",
    "caddr", "cdaar", "cdadr",
    "cddar", "cdddr", "caaaar",
    "caaadr", "caadar", "caaddr",
    "cadaar", "cadadr", "caddar",
    "cadddr", "cdaaar", "cdaadr",
    "cdadar", "cdaddr", "cddaar",
    "cddadr", "cdddar", "cddddr",
    "mpair?", "mcons", "mcar",
    "mcdr", "set-mcar!", "set-mcdr!",
    "raise-argument-error", "raise-argument-error*", "raise-arguments-error", "raise-arguments-error*", "raise-result-error", "raise-result-error*",
    "raise-mismatch-error", "raise-range-error",
    "raise-range-error*", "raise-arity-error",
    "raise-result-arity-error", "raise-arity-mask-error",
    "raise-type-error", "struct:exn", "exn",
    "error-message->adjusted-string", "error-contract->adjusted-string",
    "error-syntax->string-handler", "error-syntax->name-handler",
    "exn?", "exn-message",
    "exn-continuation-marks", "struct:exn:break", "exn:break", "exn:break?",
    "exn:break-continuation", "struct:exn:break:hang-up", "exn:break:hang-up",
    "exn:break:hang-up?", "struct:exn:break:terminate", "exn:break:terminate",
    "exn:break:terminate?", "struct:exn:fail", "exn:fail",
    "exn:fail?", "struct:exn:fail:contract", "exn:fail:contract",
    "exn:fail:contract?", "struct:exn:fail:contract:arity", "exn:fail:contract:arity",
    "exn:fail:contract:arity?", "struct:exn:fail:contract:divide-by-zero", "exn:fail:contract:divide-by-zero",
    "exn:fail:contract:divide-by-zero?", "struct:exn:fail:contract:non-fixnum-result", "exn:fail:contract:non-fixnum-result",
    "exn:fail:contract:non-fixnum-result?", "struct:exn:fail:contract:continuation", "exn:fail:contract:continuation",
    "exn:fail:contract:continuation?", "struct:exn:fail:contract:variable", "exn:fail:contract:variable",
    "exn:fail:contract:variable?", "exn:fail:contract:variable-id", "struct:exn:fail:read",
    "exn:fail:read", "exn:fail:read?", "exn:fail:read-srclocs",
    "struct:exn:fail:read:eof", "exn:fail:read:eof", "exn:fail:read:eof?",
    "struct:exn:fail:read:non-char", "exn:fail:read:non-char", "exn:fail:read:non-char?",
    "struct:exn:fail:filesystem", "exn:fail:filesystem", "exn:fail:filesystem?",
    "struct:exn:fail:filesystem:exists", "exn:fail:filesystem:exists", "exn:fail:filesystem:exists?",
    "struct:exn:fail:filesystem:version", "exn:fail:filesystem:version", "exn:fail:filesystem:version?",
    "struct:exn:fail:filesystem:errno", "exn:fail:filesystem:errno", "exn:fail:filesystem:errno?",
    "exn:fail:filesystem:errno-errno", "struct:exn:fail:network", "exn:fail:network",
    "exn:fail:network?", "struct:exn:fail:network:errno", "exn:fail:network:errno",
    "exn:fail:network:errno?", "exn:fail:network:errno-errno", "struct:exn:fail:out-of-memory",
    "exn:fail:out-of-memory", "exn:fail:out-of-memory?", "struct:exn:fail:unsupported",
    "exn:fail:unsupported", "exn:fail:unsupported?", "struct:exn:fail:user",
    "exn:fail:user", "exn:fail:user?", "prop:exn:srclocs",
    "exn:srclocs?", "exn:srclocs-accessor", "struct:srcloc",
    "srcloc", "srcloc?", "srcloc-source",
    "srcloc-line", "srcloc-column", "srcloc-position",
    "srcloc-span", "srcloc->string", "struct:date",
    "date?", "date",
    "date-second", "date-minute", "date-hour",
    "date-day", "date-month", "date-year",
    "date-week-day", "date-year-day", "date-dst?",
    "date-time-zone-offset", "struct:date*", "date*?",
    "date*", "date*-nanosecond",
    "date*-time-zone-name", "struct:arity-at-least", "arity-at-least",
    "arity-at-least?", "arity-at-least-value", "syntax?",
    "syntax-source", "syntax-line", "syntax-column",
    "syntax-position", "syntax-span", "syntax-e",
    "syntax->datum", "datum->syntax", "syntax-property",
    "syntax-property-symbol-keys",
    "current-compile-target-machine",
    "compile-target-machine?",
    "sha1-bytes", "sha224-bytes", "sha256-bytes",
    # mutability
    "immutable-string?", "immutable-bytes?", "immutable-vector?",
    "immutable-hash?", "immutable-box?",
    "mutable-string?", "mutable-bytes?", "mutable-vector?",
    "mutable-hash?", "mutable-box?",
]

pycket_extra_str = [
    "pycket:activate-debug", "pycket:deactivate-debug",
    "pycket:get-verbosity", "pycket:set-verbosity",
    "pycket:is-debug-active", "pycket:print",
    "pycket:activate-keyword", "pycket:deactivate-keyword",
    "pycket:eq?", "pycket:report-undefined-prims"
]

schemify_hooks = [
    "variable-ref", "variable-ref/no-check",
    "variable-set!/check-undefined", "variable-set!"
]

terminal_table = [
    "terminal-init",
    "terminal-read-char",
    "terminal-write-char",
    "terminal-char-width",
    "terminal-set-color",
    "terminal-flush",
    "terminal-get-screen-size",
    "terminal-raw-mode",
    "terminal-postoutput-mode",
    "terminal-signal-mode",
    "terminal-automargin-mode",
    "terminal-nanosleep",
    "terminal-pause",
    "terminal-get-clipboard",
    "terminal-move-cursor",
    "terminal-clear",
    "terminal-scroll-reverse",
    "terminal-bell",
    "terminal-carriage-return",
    "terminal-line-feed"
]

thread_str = [
    "thread",
    "thread-suspend-evt",
    "thread-dead-evt",
    "current-thread",
    "thread-resume",
    "make-semaphore",
    "semaphore-post",
    "semaphore-post-all",
    "semaphore-wait",
    "semaphore-peek-evt",
    "make-channel",
    "channel-put-evt",
    "wrap-evt",
    "handle-evt",
    "always-evt",
    "choice-evt",
    "sync",
    "sync/timeout",
    "evt?",
    "sync-atomic-poll-evt?",
    "prop:evt",
    "prop:secondary-evt",
    "poller",
    "poller-evt",
    "poll-ctx-poll?",
    "poll-ctx-select-proc",
    "poll-ctx-sched-info",
    "set-poll-ctx-incomplete?!",
    "control-state-evt",
    "async-evt",
    "current-sandman",
    "schedule-info-current-exts",
    "schedule-info-did-work!",
    "unsafe-start-atomic",
    "unsafe-end-atomic",
    "start-atomic/no-interrupts",
    "end-atomic/no-interrupts",
    "in-atomic-mode?",
    "current-custodian",
    "custodian-shut-down?",
    "current-plumber",
    "plumber-add-flush!",
    "plumber-flush-handle-remove!",
    "unsafe-custodian-register",
    "unsafe-custodian-unregister",
    "unsafe-make-custodian-at-root",
    "thread-push-kill-callback!",
    "thread-pop-kill-callback!",
    "unsafe-add-pre-poll-callback!",
    "set-get-subprocesses-time!",
    "prop:place-message",
]
kernel_str += thread_str

engine_str = [
    "make-engine",
    "engine-timeout",
    "engine-return",
    "engine-roots",
    "call-with-engine-completion",
    "current-process-milliseconds",
    "set-ctl-c-handler!",
    "set-break-enabled-transition-hook!",
    "continuation-marks",
    "poll-will-executors",
    "make-will-executor",
    "make-late-will-executor",
    "will-executor?",
    "will-register",
    "will-try-execute",
    "set-reachable-size-increments-callback!",
    "set-custodian-memory-use-proc!",
    "set-immediate-allocation-check-proc!",
    "exn:break/non-engine",
    "exn:break:hang-up/non-engine",
    "exn:break:terminate/non-engine",
    "poll-async-callbacks",
    "disable-interrupts",
    "enable-interrupts",
    "sleep",
    "get-wakeup-handle",
    "wakeup",
    "fork-place",
    "place-get-inherit",
    "start-place",
    "exit",
    "current-place-roots",
    "get-initial-place",
    "call-with-current-continuation-roots",
    "fork-pthread",
    "pthread?",
    "get-thread-id",
    "make-condition",
    "condition-wait",
    "condition-signal",
    "condition-broadcast",
    "make-mutex",
    "mutex-acquire",
    "mutex-release",
    "threaded?",
    "call-as-asynchronous-callback",
    "post-as-asynchronous-callback",
    "continuation-current-primitive",
    "prop:unsafe-authentic-override",
    "get-system-stats"
]

pthread_str = [
    "make-pthread-parameter",
    "unsafe-make-place-local",
    "unsafe-place-local-ref",
    "unsafe-place-local-set!",
    "unsafe-add-global-finalizer",
    # "unsafe-strip-impersonator",
    "immobile-cell-ref",
    "immobile-cell->address",
    "address->immobile-cell",
    "set-fs-change-properties!",
    "engine-block"
]

rktio_str = [
    "rktio_NULL",
    "rktio_filesize_ref",
    "rktio_timestamp_ref",
    "rktio_is_timestamp",
    "rktio_recv_length_ref",
    "rktio_recv_address_ref",
    "rktio_stat_to_vector",
    "rktio_identity_to_vector",
    "rktio_seconds_to_date*",
    "rktio_convert_result_to_vector",
    "rktio_to_bytes",
    "rktio_to_bytes_list",
    "rktio_to_shorts",
    "rktio_from_bytes_list",
    "rktio_free_bytes_list",
    "rktio_make_sha1_ctx",
    "rktio_make_sha2_ctx",
    "rktio_process_result_stdin_fd",
    "rktio_process_result_stdout_fd",
    "rktio_process_result_stderr_fd",
    "rktio_process_result_process",
    "rktio_status_running",
    "rktio_status_result",
    "rktio_pipe_results",
    "rktio_do_install_os_signal_handler",
    "rktio_get_ctl_c_handler"
]


# The reason for make_primitive_table is for turning these into list
# of symbols (to avoid making new objects everytime we look things up)

place = make_primitive_table(place_str)
paramz = make_primitive_table(paramz_str)
internal = make_primitive_table(internal_str)
futures = make_primitive_table(futures_str)
flfxnum = make_primitive_table(flfxnum_str)
extfl = make_primitive_table(extfl_str)
network = make_primitive_table(network_str)
foreign = make_primitive_table(foreign_str)
linklet = make_primitive_table(linklet_str)
unsafe = make_primitive_table(unsafe_str)
kernel = make_primitive_table(kernel_str)
pycket = make_primitive_table(pycket_extra_str + schemify_hooks)
terminal = make_primitive_table(terminal_table)
pthread = make_primitive_table(pthread_str)
thread = make_primitive_table(thread_str)
rktio = make_primitive_table(rktio_str)
engine = make_primitive_table(engine_str)

select_prim_table = {
    sym("#%linklet"): linklet,
    sym("#%kernel"): kernel,
    sym("#%paramz"): paramz,
    sym("#%unsafe"): unsafe,
    sym("#%foreign"): foreign,
    sym("#%futures"): futures,
    sym("#%place"): place,
    sym("#%flfxnum"): flfxnum,
    sym("#%extfl"): extfl,
    sym("#%pycket"): pycket,
    sym("#%network"): network,
    sym("#%terminal"): terminal,
    sym("#%pthread"): pthread,
    sym("#%thread"): thread,
    sym("#%rktio"): rktio,
    sym("#%engine"): engine,
}

def append_to_prim_table(table_name_str, prim_name_str):
    select_prim_table[sym(table_name_str)].append(sym(prim_name_str))

def add_prim_to_rktio(prim_name_str):
    append_to_prim_table("#%rktio", prim_name_str)

# Lists of actual functions indexed by the names above
prim_table_cache = {}

ALL_PRIMS = linklet_str + \
    kernel_str + \
    paramz_str + \
    unsafe_str + \
    foreign_str + \
    futures_str + \
    place_str + \
    flfxnum_str + \
    extfl_str + \
    pycket_extra_str + \
    schemify_hooks + \
    network_str + \
    terminal_table + \
    pthread_str + \
    rktio_str + \
    engine_str + \
    thread_str

if DEBUG:
    print("\n\nPriming all primitives in :\n")
    for table_name, _ in select_prim_table.items():
        print(table_name)

for prim_name_str in ALL_PRIMS:
    define_nyi(prim_name_str)


def report_undefined_prims():
    linklets = get_undef_prims_in(linklet_str)
    kernel = get_undef_prims_in(kernel_str)
    paramz = get_undef_prims_in(paramz_str)
    unsafe = get_undef_prims_in(unsafe_str)
    foreign = get_undef_prims_in(foreign_str)
    futures = get_undef_prims_in(futures_str)
    places = get_undef_prims_in(place_str)
    flfxnum = get_undef_prims_in(flfxnum_str)
    extfl = get_undef_prims_in(extfl_str)
    network = get_undef_prims_in(network_str)
    terminal = get_undef_prims_in(terminal_table)
    pthread = get_undef_prims_in(pthread_str)
    thread = get_undef_prims_in(thread_str)

    total = linklets + kernel + paramz + unsafe + foreign + \
        futures + places + flfxnum + extfl + network + terminal + pthread

    report = """
    linklets   : %s -- %s
    kernel     : %s -- %s
    paramz     : %s -- %s
    unsafe     : %s -- %s
    foreign    : %s -- %s
    futures    : %s -- %s
    places     : %s -- %s
    flfxnum    : %s -- %s
    extfl      : %s -- %s
    network    : %s -- %s
    terminal   : %s -- %s
    pthread    : %s -- %s
    thread     : %s -- %s
    TOTAL      : %s
    """ % (len(linklets), linklets,
            len(kernel), kernel,
            len(paramz), paramz,
            len(unsafe), unsafe,
            len(foreign), foreign,
            len(futures), futures,
            len(places), places,
            len(flfxnum), flfxnum,
            len(extfl), extfl,
            len(network), network,
            len(terminal), terminal,
            len(pthread), pthread,
            len(thread), thread,
            len(total))

    print(report)
    return 0


def get_undef_prims_in(table):
    from pycket.prims.expose import prim_env
    from pycket.values import W_Symbol, W_Prim
    ls = []
    for name in table:
        p = prim_env[sym(name)]
        if isinstance(p, W_Prim) and not p.is_implemented():
            ls.append(name)
    return ls
