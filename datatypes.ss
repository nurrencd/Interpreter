;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of box?))
   (env environment?))
  (recursively-extended-env-record
   (proc-names (list-of symbol?))
   (bodiess (list-of expression?)) ;I think we have to box this, but we'll do this later
   (env environment?)))

;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id (lambda (n)
         (ormap (lambda (proc) (proc n)) (list number? boolean? string? char? symbol? list? vector?))))]
  [lambda-exp
   (id (list-of symbol?))
   (list-id (lambda (n) (or (null? n) (symbol? n))))
   (body (list-of expression?))]
  [let-exp
   (id (list-of symbol?))
   (value (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (id (list-of symbol?))
   (value (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (id (list-of symbol?))
   (value (list-of expression?))
   (body (list-of expression?))]
  [named-let-exp
   (id (list-of symbol?))
   (value (list-of expression?))
   (body (list-of expression?))]
  [if-exp
   (condition expression?)
   (true expression?)
   (false expression?)]
  [single-if-exp
   (condition expression?)
   (true expression?)]
  [set!-exp
   (id symbol?)
   (rand expression?)]
  [app-exp
   (rator expression?)
   (rand (list-of expression?))]
  [and-exp
   (rand (list-of expression?))]
  [or-exp
    (rand (list-of expression?))]
  [begin-exp
   (execs (list-of expression?))]
  [cond-exp
    (cond  (list-of expression?))
    (execs (list-of (list-of expression?)))
    (else  (list-of expression?))]
  [case-exp
   (val expression?)
   (cases (list-of (list-of expression?)))
   (execs (list-of (list-of expression?)))
   (else-exp (list-of expression?))]
  [while-exp
    (test expression?)
    (body (list-of expression?))]
  [for-exp
   (init (list-of expression?))
   (condition expression?)
   (update (list-of expression?))
   (body (list-of expression?))]
  [define-exp
    (id symbol?)
    (val expression?)])

;; datatype for procedures.  At first there is only one
;; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (syms (list-of symbol?))
   (list-id (lambda (x) (or (symbol? x) (null? x))))
   (proc (list-of expression?))
   (env environment?)])

(define-datatype continuation continuation?
  [if-k  (then-exp expression?)
         (else-exp expression?)
         (env environment?)
         (k continuation?)]
  [single-if-k (then-exp expression?)
               (env environment?)
               (k continuation?)]
  [rator-k (rands (list-of expression?))
           (env environment?)
           (k continuation?)]
  [rands-k (proc-value scheme-value?)
           (k continuation?)]
  [set!-k (id symbol?)
          (val expression?)
          (k continuation?)]
  [letrec-k (id (list-of symbol?))
            (value (list-of expression?))
            (body (list-of expression?))
            (k continuation?)]
  [while-k (test expression?)
           (body (list-of expression?))
           
  [map-k (proc-val procedure?)
         (car-ls scheme-value?)
         (k continuation?)]
  [map-proc-k (cdr-ls list?)
              (k continuation?)]
  [value-k])
