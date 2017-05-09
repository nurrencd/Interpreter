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
   (bodiess (list-of (lambda (n) (expression? (unbox n)))))
   (env environment?)))

(define-datatype lambda-id lambda-id?
  [ref-id 
    (sym symbol?)]
  [val-id 
    (sym symbol?)])

;; Parsed expression datatypes
(define-datatype expression expression?
  [la-var-exp
    (depth (lambda (x) (or (number? x) (symbol? x))))
    (position number?)]
  [var-exp
   (id symbol?)]
  [lit-exp
   (id (lambda (n)
         (ormap (lambda (proc) (proc n)) (list number? boolean? string? char? symbol? list? vector?))))]
  [lambda-exp
   (id (list-of lambda-id?))
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
  [la-set!-exp
    (depth (lambda (x) (or (number? x) (symbol? x))))
    (position number?)
    (rand expression?)]
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
   (syms (list-of lambda-id?))
   (list-id (lambda (x) (or (symbol? x) (null? x))))
   (proc (list-of expression?))
   (env environment?)])
