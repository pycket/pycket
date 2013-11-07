(let();; Recently improved by Jason Hemann and Dan Friedman

;; The code of this system comprises two steps.  The first step is to run
;; a goal and check if the result fails to make sense: we term this
;; "fast fail".  If each goal goes to completion, then we have the reify
;; step.  The work of the reify step is to take the final state of the
;; computation and return a Scheme value.  This also comprises two steps.
;; The first step is to try every funtion in a cycle of functions that try
;; to make a new state, which is then fed to the next function in the cycle.
;; Each cycle function takes a state and returns a state: it cannot fail.
;; When one of the functions does not improve the state, it becomes the
;; function to reach without ever changing the state by the intervening
;; functions.  When no improvements can be made we have found a fixed point.
;; Each of these intervening cycle functions tried to change the state, but
;; couldn't.  Next we turn the value along with the non-empty fields of the
;; state into a list structure.  Reification does not appear to be a
;; bottleneck, since it is just turning an answer into something readable.
;; There may be many answers, and each answer has to be reified.

;; We have just added not-pairo to the system.  It appears to be working
;; and passes our test cases. It's field is called Np.


(define (exists f l)
  (if (null? l) #f
      (or (f (car l))
          (exists f (cdr l)))))

(define (for-all f l)
  (if (null? l) #t
      (and (f (car l))
           (exists f (cdr l)))))

(define (find f l)
  (if (null? l) 
      #f
      (if (f (car l))
          (car l)
          (find f (cdr l)))))

(define (memp f l)
  ())

(define rhs
  (lambda (pr)
    (cdr pr)))

(define lhs
  (lambda (pr)
    (car pr)))

(define-syntax case-value
  (syntax-rules ()
    ((_ u ((t1) e0) ((at dt) e1) ((t2) e2))
     (let ((t u))
       (cond
	 ((var? t) (let ((t1 t)) e0))
	 ((pair? t) (let ((at (car t)) (dt (cdr t))) e1))
	 (else (let ((t2 t)) e2)))))))

(define var
  (lambda (dummy)
    (vector dummy)))

(define var?
  (lambda (x)
    (vector? x)))

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define unify
  (lambda (u v S)
    (let ((u (walk u S))
          (v (walk v S)))
      (cond
        ((and (pair? u) (pair? v))
         (let ((S (unify (car u) (car v) S)))
           (and S
             (unify (cdr u) (cdr v) S))))
        (else (unify-nonpair u v S))))))

(define unify-nonpair
  (lambda (u v S)
    (cond
      ((eq? u v) S)
      ((var? u) (ext-S-check u v S))
      ((var? v) (ext-S-check v u S))
      ((equal? u v) S)
      (else #f))))

(define ext-S-check
  (lambda (x v S)
    (case-value v
      ((v) (ext-S x v S))
      ((au du) (cond
                 ((occurs-check x v S) #f)
                 (else (ext-S x v S))))
      ((v) (ext-S x v S)))))

(define ext-S
  (lambda (x v S)
    (cons `(,x . ,v) S)))

(define occurs-check
  (lambda (x v S)
    (case-value (walk v S)
      ((v) (eq? v x))
      ((av dv)
       (or (occurs-check x av S)
           (occurs-check x dv S)))
      ((v) #f))))

(define prefix-S
  (lambda (S0 S)
    (cond
      ((eq? S0 S) '())
      (else (cons (car S0)
              (prefix-S (cdr S0) S))))))

(define walk*
  (lambda (v S)
    (case-value (walk v S)
      ((v) v)
      ((av dv)
       (cons (walk* av S) (walk* dv S)))
      ((v) v))))

(define reify-R
  (lambda (v R)
    (case-value (walk v R)
      ((v) (let ((n (length R)))
             (let ((name (reify-name n)))
               (ext-S v name R))))
      ((av dv) (let ((R (reify-R av R)))
                 (reify-R dv R)))
      ((v) R))))

(define unify-safe
  (lambda (u v S)
    (let ((u (walk u S))
          (v (walk v S)))
      (cond
        ((and (pair? u) (pair? v))
         (let ((S (unify-safe (car u) (car v) S)))
           (and S
             (unify-safe (cdr u) (cdr v) S))))
        (else (unify-nonpair-safe u v S))))))

(define unify-nonpair-safe
  (lambda (u v S)
    (cond
      ((eq? u v) S)
      ((var? u) (ext-S u v S))
      ((var? v) (ext-S v u S))
      (else S))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (x)
    (lambda (c)
      (let ((c (cycle c)))
        (let ((S (c->S c)))
          (let ((D (walk* (c->D c) S))
                (Y (walk* (c->Y c) S))
                (N (walk* (c->N c) S))
                (Np (walk* (c->Np c) S))
                (A (walk* (c->A c) S))
                (v (walk* x S)))
            (let ((R (reify-R v '())))
              (reify+
               v R c S D Y N Np A))))))))

(define reify+
  (lambda (v R c S D Y N Np A)
    (reify++ v R
      (D-subsumed
       (remp
          (lambda (d)
            (anyvar? (walk* d S) R))
          (drop-from-D-using-A S
            (c->Y c) (c->N c)
	    (c->Np c) (c->A c)
            (rem-xx-from-D D S))))
      (remp (lambda (y) (anyvar? y R))
        Y)
      (remp (lambda (n) (anyvar? n R))
        N)
      (remp (lambda (np) (anyvar? np R))
        Np)
      (remp (lambda (a) (anyvar? a R))
        A))))

(define reify++
  (lambda (v R D Y N Np A)
    (form (walk* v R) (walk* D R)
          (walk* Y R) (walk* N R)
          (walk* Np R) (A-subsumed (walk* A R)))))

(define form
  (lambda (v D Y N Np A)
    (let ((fd (sort-D D))
          (fy (sorter Y))
          (fn (sorter N))
          (fnp (sorter Np))
          (fa (sorter A)))
      (let ((fd (if (null? fd) fd
                    (let ((fd (drop-dot-D fd)))
                      `((=/= . ,fd)))))
            (fy (if (null? fy) fy `((sym . ,fy))))
            (fn (if (null? fn) fn `((num . ,fn))))
            (fnp (if (null? fnp) fnp `((not-pair . ,fnp))))
            (fa (if (null? fa) fa
                    (let ((fa (drop-dot fa)))
                      `((absento . ,fa))))))
        (cond
          ((and (null? fd) (null? fy)
                (null? fn) (null? fnp)
                (null? fa))
           v)
          (else (append `(,v) fd fnp fn fy fa)))))))

(define lex<=?
  (lambda (x y)
    (cond
      ((vector? x) #t)
      ((vector? y) #f)
      ((port? x) #t)
      ((port? y) #f)
      ((procedure? x) #t)
      ((procedure? y) #f)
      ((boolean? x)
       (cond
         ((boolean? y) (or (not x) (eq? x y)))
         (else #t)))
      ((boolean? y) #f)
      ((null? x) #t)
      ((null? y) #f)
      ((char? x)
       (cond
         ((char? y) (char<=? x y))
         (else #t)))
      ((char? y) #f)
      ((number? x)
       (cond
         ((number? y) (<= x y))
         (else #t)))
      ((number? y) #f)
      ((string? x)
       (cond
         ((string? y) (string<=? x y))
         (else #t)))
      ((string? y) #f)
      ((symbol? x)
       (cond
         ((symbol? y)
          (string<=? (symbol->string x)
                     (symbol->string y)))
         (else #t)))
      ((symbol? y) #f)
      ((pair? x)
       (cond
         ((pair? y)
          (cond
            ((equal? (car x) (car y))
             (lex<=? (cdr x) (cdr y)))
            (else (lex<=? (car x) (car y)))))))
      ((pair? y) #f)
      (else #t))))

(define sorter
  (lambda (ls)
    (list-sort lex<=? ls)))

(define sort-D
  (lambda (D)
    (sorter
      (map sort-d D))))

(define sort-d
  (lambda (d)
    (list-sort
      (lambda (x y)
        (lex<=? (car x) (car y)))
      (map sort-pr d))))

(define drop-dot
  (lambda (X)
    (map (lambda (t)
           (let ((a (lhs t))
                 (d (rhs t)))
             `(,a ,d)))
         X)))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

(define drop-dot-D
  (lambda (D)
    (map drop-dot D)))

(define lex<-reified-name?
  (lambda (r)
    (char<?
      (string-ref (datum->string r) 0)
      (string-ref "_" 0))))

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define rem-xx-from-D
 (lambda (D S)
   (remp not
     (map (lambda (d)
            (let ((d-l (lhs d))
                  (d-r (rhs d)))
              (let ((S0 (unify-safe d-l d-r S)))
                (let ((pfx (prefix-S S0 S)))
                  (cond
                    ((null? pfx) '())
                    (else pfx))))))
          D))))

(define anyvar?
  (lambda (u S)
    (case-value u
      ((u) (var? (walk u S)))
      ((au du) (or (anyvar? au S)
                   (anyvar? du S)))
      ((u) #f))))

(define drop-from-D-using-A
  (lambda (S Y N Np A D)
    (remp (lambda (d)
	    (exists
	      (A-superfluous-pr? S Y N Np A)
	      d))
	  D)))

(define A-superfluous-pr?
  (lambda (S Y N Np A)
    (lambda (pr)
      (let ((pr-a (walk (lhs pr) S))
            (pr-d (walk (rhs pr) S)))
        (cond
          ((exists
             (lambda (a)
               (let ((a-a (walk (lhs a) S))
                     (a-d (walk (rhs a) S)))
                 (terms-pairwise=?
                   pr-a pr-d a-a a-d S)))
             A)
           (for-all
             (stays-in-A? S Y N Np pr-a pr-d)
             A))
          (else #f))))))

(define stays-in-A?
  (lambda (S Y N Np pr-a pr-d)
    (lambda (a)
      (let ((a-a (walk (lhs a) S))
            (a-d (walk (rhs a) S)))
        (or
          (not
            (terms-pairwise=?
              pr-a pr-d a-a a-d S))
          (untyped-var? S Y N Np a-d)
          (pair? a-d))))))

(define terms-pairwise=?
  (lambda (pr-a pr-d t-a t-d S)
    (or (and (term=? pr-a t-a S)
             (term=? pr-d t-d S))
        (and (term=? pr-a t-d S)
             (term=? pr-d t-a S)))))

(define D-subsumed
  (lambda (D)
    (let D-subsumed ((D D) (D0 '()))
      (cond
        ((null? D) D0)
        ((or (D-subsumed? (car D) (cdr D))
             (D-subsumed? (car D) D0))
         (D-subsumed (cdr D) D0))
        (else (D-subsumed (cdr D)
                (cons (car D) D0)))))))

(define D-subsumed?
  (lambda (d D0)
    (cond
      ((null? D0) #f)
      (else
       (let ((d^ (unify* (car D0) d)))
         (or (and d^ (eq? d^ d))
             (D-subsumed? d (cdr D0))))))))

(define unify*
  (lambda (d S)
    (unify (map lhs d) (map rhs d) S)))

(define unify*-safe
  (lambda (d S)
    (unify-safe
      (map lhs d)
      (map rhs d)
      S)))

(define A-subsumed
  (lambda (A)
    (let A-subsumed ((A A) (A0 '()))
      (cond
        ((null? A) A0)
        (else
         (let ((l (lhs (car A)))
               (r (rhs (car A))))
           (cond
             ((or
                (A-subsumed? l r (cdr A))
                (A-subsumed? l r A0))
              (A-subsumed (cdr A) A0))
             (else
              (A-subsumed (cdr A)
                (cons (car A) A0))))))))))

(define A-subsumed?
  (lambda (l r A)
    (cond
      ((null? A) #f)
      (else
       (let ((l^ (lhs (car A)))
             (r^ (rhs (car A))))
         (or
           (and (eq? r r^) (member* l^ l))
           (A-subsumed? l r (cdr A))))))))

(define member*
  (lambda (x u)
    (cond
      ((eq? x u) #t)
      ((pair? u)
       (or (member* x (car u))
           (member* x (cdr u))))
      (else #f))))

(define-syntax lambdag@
  (syntax-rules (:)
    ((_ (c) e) (lambda (c) e))
    ((_ (c : S D Y N Np A) e)
     (lambda (c)
       (let ((S (c->S c)) (D (c->D c))
             (Y (c->Y c)) (N (c->N c))
             (Np (c->Np c)) (A (c->A c)))
         e)))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define mzero (lambda () #f))
(define unit (lambdag@ (c) c))
(define choice (lambda (c f) (cons c f)))
(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambdaf@ () e))))
(define empty-f (lambdaf@ () (mzero)))

(define State
  (lambda (S D Y N Np A)
    `(,S ,D ,Y ,N ,Np ,A)))

(define empty-c '(() () () () () ()))


(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((c-inf e))
       (cond
         ((not c-inf) e0)
         ((procedure? c-inf)  (let ((f^ c-inf)) e1))
         ((not (and (pair? c-inf)
                 (procedure? (cdr c-inf))))
          (let ((c^ c-inf)) e2))
         (else (let ((c (car c-inf)) (f (cdr c-inf)))
                 e3)))))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (c)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 c) g ...)))))))

(define bind
  (lambda (c-inf g)
    (case-inf c-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((c) (g c))
      ((c f) (mplus (g c) (lambdaf@ () (bind (f) g)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ...
            (lambdag@ (final-c)
              (let ((z ((reify x) final-c)))
                (choice z empty-f))))
          empty-c))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define take
  (lambda (n f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (take n f))
         ((c) (cons c '()))
         ((c f) (cons c
                  (take (and n (- n 1)) f))))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
         (mplus*
           (bind* (g0 c) g ...)
           (bind* (g1 c) g^ ...) ...))))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0
                    (lambdaf@ () (mplus* e ...))))))

(define mplus
  (lambda (c-inf f)
    (case-inf c-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((c) (choice c f))
      ((c f^) (choice c (lambdaf@ () (mplus (f) f^)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

(define c->S (lambda (c) (car c)))
(define c->D (lambda (c) (cadr c)))
(define c->Y (lambda (c) (caddr c)))
(define c->N (lambda (c) (cadddr c)))
(define c->Np (lambda (c) (cadddr (cdr c))))
(define c->A (lambda (c) (cadddr (cddr c))))

(define absento
  (lambda (u v)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((mem-check u v S) (mzero))
        (else
         (unit (State S D Y N Np `((,u . ,v) . ,A))))))))

(define mem-check
  (lambda (u t S)
    (let ((t (walk t S)))
      (cond
        ((pair? t)
         (or (term=? u t S)
             (mem-check u (car t) S)
             (mem-check u (cdr t) S)))
        (else (term=? u t S))))))

(define term=?
  (lambda (u v S)
    (let ((u (walk u S))
          (v (walk v S)))
      (cond
        ((and (pair? u) (pair? v))
         (and (term=? (car u) (car v) S)
              (term=? (cdr u) (cdr v) S)))
        (else (term=?-nonpair u v S))))))

(define term=?-nonpair
  (lambda (u v S)
    (cond
      ((eq? u v) #t)
      ((var? u) #f)
      ((var? v) #f)
      ((equal? u v) #t)
      (else #f))))

(define ground-non-type?
  (lambda (pred)
    (lambda (u S)
      (let ((u (walk u S)))
        (cond
          ((var? u) #f)
          (else (not (pred u))))))))

(define ground-non-symbol?
  (ground-non-type? symbol?))

(define ground-non-number?
  (ground-non-type? number?))

(define not-pair? (lambda (x) (not (pair? x))))

(define ground-pair?
  (ground-non-type? not-pair?))

(define symbolo
  (lambda (u)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((ground-non-symbol? u S) (mzero))
        ((mem-check u N S) (mzero))
        (else (unit (State S D `(,u . ,Y) N Np A)))))))

(define numbero
  (lambda (u)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((ground-non-number? u S) (mzero))
        ((mem-check u Y S) (mzero))
        (else (unit (State S D Y `(,u . ,N) Np A)))))))

(define not-pairo
  (lambda (u)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((ground-pair? u S) (mzero))
        (else (unit (State S D Y N `(,u . ,Np) A)))))))

(define =/=
  (lambda (u v)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (cond
             ((eq? S0 S) (mzero))
             (else
              (let ((d `(,u . ,v)))
                (unit
                  (State S `(,d . ,D) Y N Np A)))))))
        (else c)))))

(define ==
  (lambda (u v)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((unify u v S) =>
         (lambda (S0)
           (cond
             ((eq? S0 S) (unit c))
             (else
              (cond
                ((==fail-check S0 D Y N Np A)
                 (mzero))
                (else
                 (unit (State S0 D Y N Np A))))))))
        (else (mzero))))))

(define ==fail-check
  (lambda (S0 D Y N Np A)
    (or (atomic-fail-check S0 Y ground-non-symbol?)
        (atomic-fail-check S0 N ground-non-number?)
        (atomic-fail-check S0 Np ground-pair?)
        (symbolo-numbero-fail-check S0 Y N)
        (=/=-fail-check S0 D)
        (absento-fail-check S0 A))))

(define atomic-fail-check
  (lambda (S Np pred)
    (exists
      (lambda (np)
        (pred (walk np S) S))
      Np)))

(define symbolo-numbero-fail-check
  (lambda (S Y N)
    (let ((N (map (lambda (n) (walk n S)) N)))
      (exists
        (lambda (y)
          (exists (same-var? (walk y S)) N))
        Y))))

(define absento-fail-check
  (lambda (S A)
    (exists
      (lambda (a) (mem-check (lhs a) (rhs a) S))
      A)))

(define =/=-fail-check
  (lambda (S D)
    (exists
      (lambda (d) (term=? (lhs d) (rhs d) S))
      D)))

(define tagged?
  (lambda (S Y y^)
    (exists (lambda (y) (eqv? (walk y S) y^)) Y)))

(define untyped-var?
  (lambda (S Y N Np t)
    (let ((in-type? (lambda (y)
                      (eq? (walk y S) t))))
      (and (var? t)
           (not (exists in-type? Y))
           (not (exists in-type? N))
           (not (exists in-type? Np))))))

(define const?
  (lambda (S)
    (lambda (a)
      (not (var? (walk a S))))))

(define drop-from-N-b/c-const
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (const? S) N) =>
       (lambda (n)
         (State S D Y (remq1 n N) Np A)))
      (else c))))

(define drop-from-Y-b/c-const
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (const? S) Y) =>
       (lambda (y)
         (State S D (remq1 y Y) N Np A)))
      (else c))))

(define drop-from-Np-b/c-const
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (const? S) Np) =>
       (lambda (np)
         (State S D Y N (remq1 np Np) A)))
      ((memp (lambda (x) (not (walk x S))) Np) =>
       (lambda (np*)
         (State S D Y N (remq1 (car np*) Np) A)))
      (else c))))

(define remq1
  (lambda (elem ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) elem) (cdr ls))
      (else (cons (car ls)
              (remq1 elem (cdr ls)))))))

(define same-var?
  (lambda (v)
    (lambda (v^)
      (and (var? v) (var? v^) (eq? v v^)))))

(define find-dup
  (lambda (f S)
    (lambda (set)
      (let loop ((set set))
        (cond
          ((null? set) #f)
          (else
           (let ((e (car set)))
             (let ((e0 (walk e S)))
               (cond
                 ((find (lambda (e1)
                          ((f e0) (walk e1 S)))
                    (cdr set))
                  e)
                 (else
                  (loop (cdr set))))))))))))

(define drop-from-N-b/c-dup-var
  (lambdag@ (c : S D Y N Np A)
    (cond
      (((find-dup same-var? S) N) =>
       (lambda (n)
         (State S D Y (remq1 n N) Np A)))
      (else c))))

(define drop-from-Y-b/c-dup-var
  (lambdag@ (c : S D Y N Np A)
    (cond
      (((find-dup same-var? S) Y) =>
       (lambda (y)
         (State S D (remq1 y Y) N Np A)))
      (else c))))

(define drop-from-Np-b/c-dup-var
  (lambdag@ (c : S D Y N Np A)
    (cond
      (((find-dup same-var? S) Np) =>
       (lambda (np)
         (State S D Y N (remq1 np Np) A)))
      (else c))))

(define drop-var-from-Np-b/c-Y
  (lambdag@ (c : S D Y N Np A)
    (let ((Y (map (lambda (y) (walk y S)) Y)))
      (cond
        ((find (lambda (np)
                 (exists (same-var? (walk np S)) Y))
               Np) =>
               (lambda (np)
                 (State S D Y N (remq1 np Np) A)))
        (else c)))))

(define drop-var-from-Np-b/c-N
  (lambdag@ (c : S D Y N Np A)
    (let ((N (map (lambda (n) (walk n S)) N)))
      (cond
        ((find (lambda (np)
                 (exists (same-var? (walk np S)) N))
               Np) =>
               (lambda (np)
                 (State S D Y N (remq1 np Np) A)))
        (else c)))))

(define var-type-mismatch?
  (lambda (S Y N Np t1 t2)
    (cond
      ((num? S N t1)
       (not (num? S N t2)))
      ((sym? S Y t1)
       (not (sym? S Y t2)))
      ((not-pr? S Np t1)
       (not (not (pair? t2))))
      (else #f))))

(define term-ununifiable?
  (lambda (S Y N Np t1 t2)
    (let ((t1 (walk t1 S))
          (t2 (walk t2 S)))
      (cond
        ((or (untyped-var? S Y N Np t1)
             (untyped-var? S Y N Np t2)) #f)
        ((var? t1)
         (var-type-mismatch? S Y N Np t1 t2))
        ((var? t2)
         (var-type-mismatch? S Y N Np t2 t1))
        ((and (pair? t1) (pair? t2))
         (or (term-ununifiable? S Y N Np
               (car t1) (car t2))
             (term-ununifiable? S Y N Np
               (cdr t1) (cdr t2))))
        (else (not (eqv? t1 t2)))))))

(define num?
  (lambda (S N n)
    (let ((n (walk n S)))
      (cond
        ((var? n) (tagged? S N n))
        (else (number? n))))))

(define sym?
  (lambda (S Y y)
    (let ((y (walk y S)))
      (cond
        ((var? y) (tagged? S Y y))
        (else (symbol? y))))))

(define not-pr?
  (lambda (S Np np)
    (let ((np (walk np S)))
      (cond
        ((var? np) (tagged? S Np np))
        (else (not-pair? np))))))

(define drop-from-D-b/c-d1-occurs-d2
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (lambda (d)
               (tree-occurs-check (lhs d) (rhs d) S))
         D) => (lambda (d)
                 (State S (remq1 d D) Y N Np A)))
      (else c))))
;; here
(define split-a-move-to-D-b/c-pair
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((exists
         (lambda (a)
           (let ((tr (walk (rhs a) S)))
             (cond
               ((pair? tr)
                ((split-a-move-to-D tr a) c))
               (else #f))))
         A))
      (else c))))

(define split-a-move-to-D
  (lambda (tr a)
    (lambdag@ (c : S D Y N Np A)
      (let ((tl (walk (lhs a) S))
            (tr-a (car tr))
            (tr-d (cdr tr)))
        (let ((t1 `(,tl . ,tr-a))
              (t2 `(,tl . ,tr-d))
              (A (remq1 a A)))
          (let ((A `(,t1 . (,t2 . ,A))))
            (cond
              ((unify tl tr S) =>
               (lambda (S0)
                 (cond
                   ((eq? S0 S)
                    (State S D Y N Np A))
                   (else
                     (let ((D `(,a . ,D)))
                       (State S D Y N Np A))))))
              (else (State S D Y N Np A)))))))))

(define tree-occurs-check
  (lambda (d-a d-b S)
    (let ((d-a (walk d-a S))
          (d-b (walk d-b S)))
      (cond
        ((var? d-a)
         (occurs-check d-a d-b S))
        ((var? d-b)
         (occurs-check d-b d-a S))
        ((and (pair? d-a) (pair? d-b))
         (or
           (tree-occurs-check (car d-a) (car d-b) S)
           (tree-occurs-check (cdr d-a) (cdr d-b) S)))
        (else #f)))))

(define move-from-A-to-D-b/c-a2-Np
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((exists
         (lambda (a)
           (let ((a2 (rhs a)))
             ((movable-a? (walk a2 S) a2 a) c)))
         A))
      (else c))))

(define movable-a?
  (lambda (a2^ a2 a)
    (lambdag@ (c : S D Y N Np A)
      (cond
        ((and
           (not (untyped-var? S Y N Np a2^))
           (not (pair? a2^)))
           (let ((A (remq1 a A)))
             (cond
               ((unify (lhs a) a2 S) =>
                (lambda (S0)
                  (cond
                    ((eq? S0 S)
                     (State S D Y N Np A))
                    (else
                     (let ((D `(,a . ,D)))
                       (State S D Y N Np A))))))
               (else (State S D Y N Np A)))))
        (else #f)))))

(define drop-from-D-b/c-Y-or-N-or-Np
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (lambda (d)
               (term-ununifiable?
                 S Y N Np (lhs d) (rhs d)))
        D) =>
       (lambda (d)
         (State S (remq1 d D) Y N Np A)))
      (else c))))

(define drop-from-A-b/c-a2-occurs-a1
  (lambdag@ (c : S D Y N Np A)
    (cond
      ((find (lambda (a)
               (let ((a-a (walk (lhs a) S))
                     (a-d (walk (rhs a) S)))
                 (mem-check a-d a-a S)))
         A)
       => (lambda (a)
            (State S D Y N Np (remq1 a A))))
      (else c))))

(define LOF
  (list drop-from-Y-b/c-const
        drop-from-N-b/c-const
        drop-from-Np-b/c-const
        drop-var-from-Np-b/c-Y
        drop-var-from-Np-b/c-N
        drop-from-Y-b/c-dup-var
        drop-from-N-b/c-dup-var
        drop-from-Np-b/c-dup-var
        drop-from-D-b/c-Y-or-N-or-Np
        drop-from-A-b/c-a2-occurs-a1
        move-from-A-to-D-b/c-a2-Np
        split-a-move-to-D-b/c-pair))

(define len-LOF (length LOF))

(define cycler
  (lambda (c n fns)
    (cond
      ((zero? n) c)
      ((null? fns) (cycler c len-LOF LOF))
      (else
       (let ((c^ ((car fns) c)))
         (cond
           ((not (eq? c^ c))
            (cycler c^ len-LOF (cdr fns)))
           (else
            (cycler c (sub1 n) (cdr fns)))))))))

(define cycle
  (lambdag@ (c)
    (cycler c len-LOF LOF)))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
         (ifa ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* c-inf g ...))
         ((a f) (bind* c-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (c)
       (inc
         (ifu ((g0 c) g ...)
              ((g1 c) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((c-inf e))
       (case-inf c-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((c) (bind* c-inf g ...))
         ((c f) (bind* (unit c) g ...)))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (c : S D Y N Np A)
       (let ((x (walk* x S)) ...)
         ((fresh () g g* ...) c))))))

(define booleano
  (lambda (x)
    (conde
      ((== #f x) succeed)
      ((== #t x) succeed))))

(define onceo
  (lambda (g)
    (condu
      (g))))

(define prt
  (lambda args
    (lambdag@ (c)
      (begin
        (for-each
          (lambda (msg)
            (printf "~s~n" msg))
          args)
        (pretty-print c)
        c))))


(define eval-expo
  (lambda (exp env val)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (not-in-envo 'quote env)
         (absento 'closure v)
         (== v val)))
      ((fresh (a*)
         (== `(list . ,a*) exp)
         (not-in-envo 'list env)
         (absento 'closure a*)
         (proper-listo a* env val)))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) exp)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(closure ,x ,body ,env) val))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest)))
      ((== '() env)))))

(define proper-listo
  (lambda (exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) exp)
         (== `(,t-a . ,t-d) val)
         (eval-expo a env t-a)
         (proper-listo d env t-d))))))

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(run 2 (x) (fresh (p q)
         (=/= p q)
         (eval-expo p '() q)
         (eval-expo q '() p)
         (== `(,p ,q) x)))
)
