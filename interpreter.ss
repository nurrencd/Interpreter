;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) (value-k))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum)
        (apply-k k datum)]

      [var-exp (id)
				(apply-env-ref env id ; look up its value.
      	   (lambda (x) (apply-k k (deref x))) ; procedure to call if id is in the environment
           (lambda () (apply-env-ref global-env id
                                     (lambda (x) (apply-k k (deref x)))
                                     (lambda () (eopl:error
                                                 'apply-env-ref
                                                 "variable not found in environment: ~s"
                                                 id)))))]

      [if-exp (condition true false)
        (eval-exp condition
                  env
                 (if-k true false env k))]
        ;      (let ([cond-val (eval-exp condition env)])
         ;       (if cond-val (eval-exp true env) (eval-exp false env)))]

      [single-if-exp (condition true)
                     (eval-exp condition
                               env
                               (single-if-k true env k))]

      [app-exp (rator rands)
               (eval-exp rator
                         env
                         (rator-k rands env k))]
;               (let ([proc-value (eval-exp rator env)]
 ;                    [args (eval-rands rands env)])
  ;               (apply-proc proc-value args))]

;      [letrec-exp (id vals body)
;        (let ([new-env (recursively-extended-env-record
;                         id vals env)])
;          (apply-to-bodies new-env body k))]

      [lambda-exp (syms list-id body)
               (apply-k k (closure syms list-id body env))]

;      [while-exp (test body)
;        (if (eval-exp test env)
;            (begin (apply-to-bodies env body k)
;                   (eval-exp exp env k)))]

;      [for-exp (init condition update body)
;               (begin
;                 (apply-to-bodies env init)
;                 (let loop ()
;                   (if (eval-exp condition env)
;                       (begin
;                         (apply-to-bodies env body)
;                         (apply-to-bodies env update)
;                         (loop)))))]

      [set!-exp (id rand)
                (apply-env-ref env id
                               (lambda (x) (eval-exp rand env (set!-k x k)))
                               (lambda () (apply-env-ref global-env id
                                                         (lambda (x) (eval-exp
                                                                      rand
                                                                      env
                                                                      (set!-k x k)))
                                                         (lambda () (eopl:error
                                                                     'apply-env-ref
                                                                     "variable not found in environment: ~s"
                                                                     id)))))]

      [define-exp (id val)
        (eval-exp val env (define-k id k))]

      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (n new-k) (eval-exp n env new-k)) rands k)))

;; Executes the items in procs with the environment env. Meant for use in the body of a lambda

(define apply-to-bodies
  (lambda (env procs k)
    (cond
     [(null? procs) (void)]
     [(null? (cdr procs))
      (eval-exp (car procs) env k)]
     [else
      (eval-exp (car procs) env (apply-to-bodies-k (cdr procs) env k))])))

;; Apply a procedure to its arguments.
;; At this point, we only have primitive procedures.
;; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cond [(proc-val? proc-value)
           (cases proc-val proc-value
                  [prim-proc (op) (apply-prim-proc op args k)]
                                        ; You will add other cases
                  [closure (syms list-id proc env)
                           (if (null? list-id)
                               (let ([new-env (extend-env syms args env)])
                                 (apply-to-bodies new-env proc k))
                               (let* ([newer-args (new-args args (length syms))]
                                      [new-env (extend-env (append syms (list list-id))
                                                           newer-args
                                                           env)])
                                 (apply-to-bodies new-env proc k)))]
                  [else
                   (error 'apply-proc
                          "Attempt to apply bad procedure: ~s"
                          proc-value)])]
          [(continuation? proc-value)
           (apply-k proc-value (car args))])))

(define syntax-expand
  (lambda (exp)
    (cases expression exp
           [var-exp (id)
                    exp]
           [lit-exp (id)
            exp]
           [lambda-exp (id list-id body)
                       (lambda-exp id list-id (map syntax-expand body))]
           [let-exp (id val body)
             (app-exp (lambda-exp id '() (map syntax-expand body)) (map syntax-expand val))]
           [let*-exp (id val body)
                     (if (null? (cdr id))
                         (syntax-expand (let-exp (list (car id)) (list (car val)) body))
                         (syntax-expand (let-exp (list (car id))
                                                 (list (car val))
                                                 (list (let*-exp (cdr id) (cdr val) body)))))]
           [and-exp (rand)
                    (if (null? (cdr rand))
                        (syntax-expand (car rand))
                        (syntax-expand (if-exp (car rand) (and-exp (cdr rand)) (lit-exp #f))))]
           [or-exp (rand)
                   (cond
                    [(null? rand) (lit-exp #f)]
                    [(null? (cdr rand))
                     (syntax-expand (car rand))]
                    [else (syntax-expand (let-exp (list 'res) 
                                                  (list (car rand))
                                                  (list (if-exp (var-exp 'res)
                                                                (var-exp 'res) 
                                                                (or-exp (cdr rand))))))])]
           [letrec-exp (id val body)
                       (let ([temp-vars (map (lambda (n) (gensym)) (make-list (length id) #f))])
                         (syntax-expand (let-exp id
                                                 (make-list (length id) (lit-exp #f))
                                                 (list (let-exp temp-vars
                                                          val
                                                          (append (map (lambda (n m)
                                                                         (set!-exp n (var-exp m)))
                                                                       id
                                                                       temp-vars)
                                                                  body))))))]
           [if-exp (condition true false)
                   (if-exp (syntax-expand condition) (syntax-expand true) (syntax-expand false))]
           [single-if-exp (condition true)
                          (single-if-exp (syntax-expand condition) (syntax-expand true))]
           [set!-exp (id rand)
                     (set!-exp id (syntax-expand rand))]
           [app-exp (rator rand)
                    (app-exp (syntax-expand rator) (map syntax-expand rand))]
           [begin-exp (execs)
                      (app-exp (lambda-exp '() '() (map syntax-expand execs)) '())]
           [cond-exp (conds execs else-exp)
                     (cond
                      [(null? conds) (syntax-expand (begin-exp else-exp))]
                      [(and (null? (cdr conds)) (null? else-exp))
                       (syntax-expand (single-if-exp (car conds)
                                                     (begin-exp (car execs))))]
                      [(null? (cdr conds))
                       (syntax-expand (if-exp (car conds)
                                              (begin-exp (car execs))
                                              (begin-exp else-exp)))]
                      [else
                       (syntax-expand (if-exp (car conds)
                                              (begin-exp (car execs))
                                              (cond-exp (cdr conds) (cdr execs) else-exp)))])]
           [case-exp (val cases execs else-exp)
                     (syntax-expand
                      (cond-exp
                       (map (lambda (n) (app-exp (var-exp 'member)
                                                 (list val (lit-exp (map unparse-exp n)))))
                            cases)
                       execs
                       else-exp))]
           [while-exp (test body)
                      (while-exp (syntax-expand test) (map syntax-expand body))]
           [for-exp (init condition update body)
                    (for-exp (map syntax-expand init)
                             (syntax-expand condition)
                             (map syntax-expand update)
                             (map syntax-expand body))]
           [named-let-exp (id value body)
                          (syntax-expand
                           (letrec-exp (list (1st id))
                                       (list (lambda-exp (cdr id) '() body))
                                       (list (app-exp (var-exp (1st id)) value))))]
           [define-exp (id val)
             (define-exp
               id
               (syntax-expand val))]
      )))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero?
                              not < > <= >= car cdr list null? assq eq? equal? atom?
                              length list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number? symbol?
                              set-car! set-cdr! vector-set! display newline
                              caar cadr cdar cddr
                              caaar caadr cadar caddr cdaar cdadr cddar cdddr
                              map apply member quotient
                              list-tail eqv? append call/cc exit-list))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*     ; a value (not an expression) with an identifier.
   (map prim-proc
        *prim-proc-names*)
   (empty-env)))

(define global-env init-env)

(define reset-global-env
  (lambda () (set! global-env init-env)))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args k)
    (let ([arg-len (length args)])
      (case prim-proc
        [(+) (apply-k k (apply + args))]
        [(-) (apply-k k (apply - args))]
        [(*) (apply-k k (apply * args))]
        [(add1) (apply-k k (+ (1st args) 1))]
        [(sub1) (apply-k k (- (1st args) 1))]
        [(cons) (if (= arg-len 2)
                    (apply-k k (cons (1st args) (2nd args)))
                    (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(=) (apply-k k (apply = args))]
        [(/) (apply-k k (apply / args))]
        [(<) (apply-k k (apply < args))]
        [(>) (apply-k k (apply > args))]
        [(<=) (apply-k k (apply <= args))]
        [(>=) (apply-k k (apply >= args))]
        [(zero?) (if (= 1 arg-len)
                     (apply-k k (zero? (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(not) (if (= 1 arg-len)
                    (apply-k k (not (1st args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(car) (if (= 1 arg-len )
                   (apply-k k (car (1st args)))
                   (error 'apply-prim-proc
                          "Cannot car empty list ~s"
                          args))]
        [(cdr) (if (= 1 arg-len)
                     (apply-k k (cdr (1st args)))
                     (error 'apply-prim-proc
                            "cannot cdr empty list")
                   )]
        [(list) (apply-k k args)]
        [(null?) (if (= arg-len 1)
                     (apply-k k (null? (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                     )]
        [(assq) (if (= arg-len 2)
                    (apply-k k (assq (1st args) (2nd args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                    )]
        [(equal?) (if (= arg-len 2)
                   (apply-k k (equal? (1st args) (2nd args)))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(eq?) (if (= arg-len 2)
                   (apply-k k (eq? (1st args) (2nd args)))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(equal?) (if (= arg-len 2)
                      (apply-k k (equal? (1st args) (2nd args)))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(atom?) (if (= arg-len 1)
                     (apply-k k (atom? (1st args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(length) (if (= arg-len 1)
                      (apply-k k (length (1st args)))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(list->vector) (if (= arg-len 1)
                            (apply-k k (list->vector (1st args)))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(list?) (if (= arg-len 1)
                     (apply-k k (list? (1st args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(pair?) (if (= arg-len 1)
                     (apply-k k (pair? (1st args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(procedure?) (if (= arg-len 1)
                          (apply-k k (or (proc-val? (1st args)) (continuation? (1st args))))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector->list) (if (= arg-len 1)
                            (apply-k k (vector->list (1st args)))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(vector) (apply-k k (apply vector args))]
        [(make-vector) (apply-k k (cond [(= arg-len 1)
                              (make-vector (1st args))]
                             [(= arg-len 2)
                              (make-vector (1st args) (2nd args))]
                             [else (error 'apply-prim-proc
                                           "Incorrect argument count in call ~s"
                                           prim-proc)]
                        ))]
        [(vector-ref) (if (= arg-len 2)
                          (apply-k k (vector-ref (1st args) (2nd args)))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector?) (if (= arg-len 1)
                       (apply-k k (vector? (1st args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(number?) (if (= arg-len 1)
                        (apply-k k (number? (1st args)))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(symbol?) (if (= arg-len 1)
                       (apply-k k (symbol? (1st args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(set-car!) (if (= arg-len 2)
                        (apply-k k (apply set-car! args))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(set-cdr!) (if (= arg-len 2)
                        (apply-k k (apply set-cdr! args))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(vector-set!) (if (= arg-len 3)
                           (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))
                           (error 'apply-prim-proc
                                  "Incorrect argument count in call ~s"
                                  prim-proc))]
        [(display) (if (= arg-len 1)
                       (apply-k k (display (1st args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(newline) (if (null? args)
                       (apply-k k (newline))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(caar) (if (= arg-len 1)
                       (apply-k k (caar (1st args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(cadr) (if (= arg-len 1)
                    (apply-k k (cadr (1st args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cdar) (if (= arg-len 1)
                    (apply-k k (cdar (1st args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cddr) (if (= arg-len 1)
                    (apply-k k (cddr (1st args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caaar) (if (= arg-len 1)
                    (apply-k k (caaar (1st args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caadr) (if (= arg-len 1)
                     (apply-k k (caadr (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cadar) (if (= arg-len 1)
                     (apply-k k (cadar (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(caddr) (if (= arg-len 1)
                     (apply-k k (caddr (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdaar) (if (= arg-len 1)
                     (apply-k k (cdaar (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdadr) (if (= arg-len 1)
                     (apply-k k (cdadr (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cddar) (if (= arg-len 1)
                     (apply-k k (cddar (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdddr) (if (= arg-len 1)
                     (apply-k k (cdddr (1st args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(map) (map-cps (lambda (n new-k) (apply-proc (1st args) (list n) new-k)) (2nd args) k)]
        [(apply) (apply-proc (1st args) (cadr args) k)]
        [(member) (apply-k k (apply member args))]
        [(quotient) (apply-k k (apply quotient args))]
        [(list-tail) (apply-k k (apply list-tail args))]
        [(eqv?) (apply-k k (apply eqv? args))]
        [(append) (apply-k k (apply append args))]
        [(call/cc) (apply-proc (1st args) (list k) k)]
        [(exit-list) args]
        [else (error 'apply-prim-proc 
                     "Bad primitive procedure name: ~s" 
                     prim-proc)]))))

(define rep      ;"read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;;;notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      (if (proc-val? answer)
          (display "<interpreter-procedure>")
          (eopl:pretty-print answer))
      (newline)
      (rep))))  ;tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

(define new-args
  (lambda (args n)
    (if (zero? n)
        (cons args '())
        (cons (car args) (new-args (cdr args) (sub1 n))))))

(define map-cps
  (lambda (proc ls k)
    (if (null? ls)
        (apply-k k '())
        (map-cps proc
                 (cdr ls)
                 (map-k proc (1st ls) k)))))

(define --cps
  (lambda (val k)
    (apply-k k (- val))))
