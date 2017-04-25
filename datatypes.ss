;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id (lambda (n) (or number? boolean? string? char? symbol? list? vector?)))]
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
   (value (list-of expression?))]
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
    (else  (list-of expression?))])

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
