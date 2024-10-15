#lang racket/base
(require racket/path racket/list
         racket/file
         racket/cmdline
         syntax/modread
         (prefix-in c: compiler/compile-file))
(provide compile-file compile-lib-path)

(define compile-file
  (case-lambda
    [(src)
     (with-module-reading-parameterization (lambda () (c:compile-file src)))]
    [(src dest)
     (with-module-reading-parameterization (lambda () (c:compile-file src dest)))]))

(define clean-count 0)

(define compiled-file-path (build-path "compiled" "pycket"))

(define racket-base-modules
  '("racket/private/stx"
    "racket/private/qq-and-or"
    "racket/private/gen-temp"
    "racket/private/cond"
    "racket/private/member"
    "racket/private/define-et-al"
    "racket/private/ellipses"
    "racket/private/sc"
    "racket/private/stxcase"
    "racket/private/template"
    "racket/private/stxloc"
    "racket/private/with-stx"
    "racket/private/letstx-scheme"
    "racket/private/stxcase-scheme"
    "racket/private/qqstx"
    "racket/private/norm-define"
    "racket/private/define"
    "racket/private/sort"
    "racket/private/case"
    "racket/private/logger"
    "racket/private/more-scheme"
    "racket/private/path"
    "racket/private/path-list"
    "racket/private/reading-param"
    "racket/repl"
    "racket/private/misc"
    "racket/private/kw-prop-key"
    "racket/private/name"
    "racket/private/procedure-alias"
    "racket/private/kw"
    "racket/private/stxparamkey"
    "racket/private/stxparam"
    "racket/stxparam-exptime"
    "racket/private/generic-methods"
    "racket/private/struct-info"
    "racket/private/struct-util"
    "racket/private/define-struct"
    "racket/require-transform"
    "racket/provide-transform"
    "racket/private/reqprov"
    "racket/private/modbeg"
    "racket/private/submodule"
    "racket/private/reverse"
    "racket/private/for"
    "racket/private/map"
    "racket/private/kernstruct"
    "racket/private/top-int"
    "racket/private/pre-base"
    "racket/runtime-config"
    "racket/private/hash"
    "racket/private/list"
    "racket/private/string"
    "racket/private/kw-file"
    "racket/private/namespace"
    "racket/private/struct"
    "racket/private/base"
    "racket/phase+space"
    "racket/base"
    "racket/private/performance-hint"
    "racket/private/executable-path"
    "racket/private/generic-interfaces"
    "racket/private/immediate-default"
    "syntax/wrap-modbeg"
    "racket/stxparam"
    "racket/private/cert"
    "racket/private/collect"
    "racket/private/old-path"
    "racket/private/kw-syntax-binding"))

(define racket-modules
  (append racket-base-modules
          '("racket/private/list-predicates"
            "racket/private/print-value-columns"
            "racket/private/math-predicates"
            "racket/list"
            "racket/private/arity"
            "racket/private/norm-arity"
            "racket/promise"
            "racket/private/promise"
            "racket/private/config"
            "syntax/readerr"
            "syntax/module-reader"
            "syntax/modread"
            "setup/cross-system"
            "setup/dirs"
            "setup/private/dirs"
            "compiler/private/mach-o"
            "compiler/private/winutf16"
            "planet/private/define-config"
            "planet/config"
            "racket/cmdline"
            "setup/path-relativize"
            "pkg/path"
            "racket/private/vector-wraps"
            "racket/unsafe/ops"
            "racket/flonum"
            "racket/fixnum"
            "racket/private/fixnum"
            "syntax/private/boundmap"
            "syntax/srcloc"
            "syntax/stx"
            "syntax/location"
            "racket/path"
            "racket/file"
            "racket/private/vector-wraps"
            "racket/unsafe/ops"
            "racket/private/port"
            "racket/private/portlines"
            "racket/struct-info"
            "racket/private/custom-write"
            "racket/pretty"
            "setup/main-collects"
            "setup/path-to-relative"
            "racket/contract/private/helpers"
            "racket/contract/private/prop"
            "racket/contract/private/blame"
            "racket/contract/private/generate-base"
            "racket/contract/private/generate"
            "racket/contract/private/guts"
            "racket/contract/private/misc"
            "racket/contract/private/arrow-common"
            "racket/contract/private/arr-i-parse"
            "racket/contract/private/arr-util"
            "racket/contract/private/arr-i"
            "racket/contract/private/arr-d"
            "racket/contract/private/and"
            "racket/contract/private/opt-guts"
            "racket/contract/private/list"
            "racket/contract/private/opt"
            "racket/contract/private/ds-helpers"
            "racket/contract/private/ds" ;; exn:fail : Reference to an undefined variable : build-optres22.1
            "racket/contract/private/arity-checking"
            "racket/contract/private/application-arity-checking"
            "racket/contract/private/arrow-higher-order"
            "racket/contract/private/kwd-info-struct"
            "racket/contract/private/box"
            "racket/contract/private/basic-opters"
            "racket/contract/private/opters"
            "racket/contract/private/arrow-val-first"
            "racket/contract/private/case-arrow" ;; fail
            "racket/contract/private/hash"
            "racket/contract/private/legacy"
            "racket/contract/private/orc"
            "racket/contract/private/out" ;; fail
            "racket/contract/private/object" ;; needs classes
            "racket/contract/private/parametric"
            "racket/contract/parametric"
            "racket/contract/private/provide" ;; fail
            "racket/contract/private/struct-dc" ;; infinite exn:fail : abort-current-continuation: no such prompt exists
            "racket/contract/private/struct-prop"
            "racket/contract/private/rand"
            "racket/contract/private/top-sort"
            "racket/contract/private/types"
            "racket/contract/private/unconstrained-domain-arrow"
            "racket/contract/private/vector"
            "racket/contract/private/base"
            "racket/contract/private/exists"
            "racket/contract/combinator"
            "racket/contract/base"
            "racket/private/serialize-structs"
            "racket/private/so-search"
            "racket/private/runtime-path-table"
            "racket/private/this-expression-source-directory"
            "racket/runtime-path" ;; exn:fail : Reference to an undefined variable : path->collects-relative10.1
            "racket/unsafe/undefined"
            "syntax/kerncase"
            "racket/private/class-wrapped"
            "racket/private/classidmap"
            "racket/private/class-undef"
            "racket/private/check"
            "racket/private/custom-hash" ;; fail
            "racket/private/generic" ;; fail
            "racket/private/local"
            "racket/private/place-local"
            "racket/private/primitive-table"
            "racket/private/sequence"
            "racket/private/small-scheme"
            "racket/private/streams"
            "racket/private/stream-cons"
            "racket/private/truncate-path"
            "racket/private/unit/keywords"
            "racket/private/unix-rand"
            "racket/private/serialize" ;; fail
            "racket/private/unit-compiletime" ;; fail
            "racket/private/unit/util" ;; fail
            "racket/private/set" ;; fail
            "racket/private/set-types" ;; fail
            "racket/private/shared-body" ;; fail
            "racket/private/tethered-installer" ;; fail
            "racket/private/unit/contract-syntax" ;; fail
            "racket/private/unit/contract" ;; fail
            "racket/private/unit/runtime"
            "racket/async-channel" ;; fail
            "racket/bool"
            "racket/block"
            "racket/bytes"
            "racket/control"
            "racket/date" ;; fail
            "racket/dict" ;; fail
            "racket/engine" ;; fail
            "racket/enter" ;; fail
            "racket/exn"
            "racket/fasl"
            "racket/format" ;; fail
            "racket/function"
            "racket/future"
            "racket/generator"
            "racket/generic" ;; fail
            "racket/hash" ;; fial
            "racket/help"
            "racket/include"
            "racket/init" ;; fail
            "racket/interactive"
            "racket/kernel"
            "racket/keyword-transform"
            "racket/language-info"
            "racket/lazy-require" ;; fail
            "racket/linklet"
            "racket/match" ;;fail
            "racket/local"
            "racket/main" ;; fail
            "racket/logging" ;; fail
            "racket/os" ;; fail
            "racket/performance-hint" ;; fail
            "racket/place" ;; fail
            "racket/provide-syntax"
            "racket/provide-transform"
            "racket/prefab"
            "racket/port" ;; fail
            "racket/random" ;; fail
            "racket/require"
            "racket/require-syntax"
            "racket/require-transform"
            "racket/rerequire" ;; fail
            "racket/sequence" ;; fail
            "racket/serialize" ;; fail
            "racket/set" ;; fail
            "racket/shared" ;; fail
            "racket/splicing"
            "racket/stream" ;; fail
            "racket/string"
            "racket/surrogate" ;; fail
            "racket/syntax"
            "racket/system"
            "racket/undefined"
            "racket/tcp"
            "racket/udp"
            "racket/trait" ;; fail
            "racket/vector"
            "racket/unit" ;; fail
            "racket/unit-exptime" ;; fail
            "racket/load/lang/reader"
            "racket/base/lang/reader"
            "racket/lang/reader" ;; fail
            "racket/kernel/init" ;; fail
            "racket/place/private/prop"
            "racket/place/private/th-place"
            "racket/place/private/coercion"
            "racket/place/private/async-bi-channel"
            "racket/match/runtime"
            "racket/match/stxtime"
            "racket/match/syntax-local-match-introduce"
            "racket/struct" ;; needs contracts
            "syntax/strip-context" ;; needs contracts
            "compiler/compile-file"
            "syntax/private/modhelp"
            "compiler/private/dep"
            "compiler/compilation-path"
            "setup/collects"
            "syntax/private/modresolve-noctc"
            "syntax/private/modcode-noctc"
            "file/sha1"
            "compiler/private/cm-minimal"
            )))


(define (split s)
  (define l (regexp-split "/" s))
  (cons (last l) (drop-right l 1)))

(define (compile-lib-path p)
  (let* ([p-list (split p)]
         [mod-name (format "~a.rkt" (car p-list))]
         [dirs (cdr p-list)]
         [p (apply collection-file-path mod-name dirs)]
         [compiled-file-name (format "~a_rkt.zo" (car p-list))]
         [zo-path (build-path (path-only p) compiled-file-path compiled-file-name)])
    (if (or (force-recompile)
            (<= (file-or-directory-modify-seconds zo-path #f (lambda () -inf.0))
                (file-or-directory-modify-seconds p #f)))
        (begin
          (printf "PYCKET COMPILE FILE -- compiling : ~a to ~s\n" p zo-path)
          (compile-file p #;zo-path))
        (printf "PYCKET COMPILE FILE -- PASS -- already exists : ~a\n" p))))


(define (compile-path p)
  (let* ([path-to-file.rkt (string->path p)]
         [p (path-only path-to-file.rkt)])
    (printf "PYCKET COMPILE FILE -- path-to-file : ~a -- p : ~a\n" path-to-file.rkt p)
    (if (not p)
        (compile-file (build-path (current-directory) path-to-file.rkt))
        (compile-file (build-path path-to-file.rkt)))))

(define (clean-file p)
  (let* ([p-list (split p)]
         [mod-name (format "~a.rkt" (car p-list))]
         [dirs (cdr p-list)]
         [p (apply collection-file-path (cons mod-name dirs))])
    (let* ((d (path-only p))
           (zo-name (format "~a_rkt.zo" (car p-list)))
           (zo-path (build-path d compiled-file-path zo-name)))
      (if (file-exists? zo-path)
          (begin
            (printf "REMOVING : ~a\n" zo-path)
            (set! clean-count (add1 clean-count))
            (delete-file zo-path))
          (printf "DOESN'T EXIST : ~a\n" zo-path)))))

(define force-recompile (make-parameter #f))


(module+ main
  (define batch #f)
  (define clean #f)
  (define lib-path? #f)
  (define base-only #f)
  (define stop-on-error? #f)

  ;; fake-parameterize
  (define old-ns (current-namespace))
  (define fails '())

  (command-line
   #:once-each
   [("-b" "--batch") "compile the Racket modules statically listed here"
                     (set! batch #t)]
   [("--clean") "remove all the generated pycket .zo files for racket modules"
                (set! clean #t)]
   [("--recompile") "clean and recompile the Racket libraries"
                    (begin (set! clean #t) (set! batch #t))]
   [("-f" "--force") "force re-generation of existing zo files (disabled by default)"
                     (force-recompile #t)]
   [("-l") "interpret paths as library paths"
           (set! lib-path? #t)]
   [("--stop-on-error") "stop on the first error"
                        (set! stop-on-error? #t)]
   [("--base-only") "only compile modules needed for `racket/base`"
                    (set! base-only #t)]
   #:args paths
   ;; do this with mutation because parameterization doesn't currently work
   (current-namespace (make-base-namespace))
   (let ([racket-modules (if base-only racket-base-modules racket-modules)])
     (when clean
       (for ([p (in-list racket-modules)])
         (clean-file p))
       (use-compiled-file-paths (list compiled-file-path)))

     ;; to do multiple
     (when batch
       (for ([p (in-list racket-modules)]
             #:break (and stop-on-error? (not (null? fails))))
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (let* ((msg (lambda (n)
                                          (string-append
                                            (substring
                                              (exn-message e)
                                              0
                                              (min (string-length (exn-message e)) n))
                                            " ...")))
                                   (msg-short (msg 40))
                                   (msg-long (msg 200))
                                   (mod.msg (cons p msg-short)))
                              (set! fails (cons mod.msg fails))
                              (printf "ERROR : ~a\n\n" (exn-message e))))])
           (compile-lib-path p))))

     ;; to compile individual paths
     (unless (null? paths)
       (for ([p (in-list paths)])
         (if lib-path?
             (compile-lib-path p)
             (compile-path p))))


     (unless (zero? clean-count)
       (printf "\n-- ~a out of ~a zo files removed --\n\n"
               clean-count (length racket-modules)))

     (if (null? fails)
         (printf "All DONE without errors.\n")
         (begin
           (printf "\n-- Here are the failed libraries -- ~a / ~a failed --\n\n"
                   (length fails) (length racket-modules))
           (for ([p (in-list fails)])
             (printf "~a ------------ ~a\n" (car p) (cdr p)))
           (printf "DONE.\n")))

     (current-namespace old-ns))
     ))
