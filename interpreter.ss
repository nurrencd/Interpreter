;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

;; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
				(apply-env env id ; look up its value.
      	   (lambda (x) x) ; procedure to call if id is in the environment
           (lambda () (apply-env global-env id
                                 (lambda (x) x)
                                 (lambda () (eopl:error
                                             'apply-env
                                             "variable not found in environment: ~s"
                                             id)))))] 
      [if-exp (condition true false)
              (let ([cond-val (eval-exp condition env)])
                (if cond-val (eval-exp true env) (eval-exp false env)))]
      [single-if-exp (condition true)
                     (let ([cond-val (eval-exp condition env)])
                       (if cond-val (eval-exp true env)))]
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator env)]
                     [args (eval-rands rands env)])
                 (apply-proc proc-value args))]
;;     [let-exp (id value body)
;;              (let ([new-env (extend-env id
;;                                          (map
;;                                           (lambda (m) (eval-exp m env))
;;                                           value)
;;                                          env)])
;;                (car (last-pair
;;                      (map
;;                       (lambda (n) (eval-exp n new-env))
;;                       body))))]
      [lambda-exp (syms list-id body)
               (closure syms list-id body env)]
      [while-exp (test body)
        (if (eval-exp test env)
            (begin (apply-to-bodies env body)
                   (eval-exp exp env)))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (n) (eval-exp n env)) rands)))

;; Executes the items in procs with the environment env. Meant for use in the body of a lambda

(define apply-to-bodies
  (lambda (env procs)
    (if (null? (cdr procs))
        (eval-exp (car procs) env)
        (begin (eval-exp (car procs) env)
               (apply-to-bodies env (cdr procs))))))

;; Apply a procedure to its arguments.
;; At this point, we only have primitive procedures.  
;; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [closure (syms list-id proc env)
                (if (null? list-id)
                    (let ([new-env (extend-env syms args env)])
                      (apply-to-bodies new-env proc))
                    (let* ([newer-args (new-args args (length syms))]
                           [new-env (extend-env (append syms (list list-id))
                                               newer-args
                                               env)])
                      (apply-to-bodies new-env proc)))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

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
                    [else (syntax-expand (if-exp (car rand) (car rand) (or-exp (cdr rand))))])]
           [letrec-exp (id val body)
                       (letrec-exp id (map syntax-expand val) (map syntax-expand body))]
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
           [named-let-exp (id value body)
                          (syntax-expand
                           (letrec-exp (list (1st id))
                                       (list (lambda-exp (cdr id) body)
                                       (app-exp (1st id) values))))]
      )))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero?
                              not < > <= >= car cdr list null? assq eq? equal? atom?
                              length list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number? symbol?
                              set-car! set-cdr! vector-set! display newline
                              caar cadr cdar cddr
                              caaar caadr cadar caddr cdaar cdadr cddar cdddr
                              map apply member quotient))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*     ; a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env)))

(define global-env init-env)

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args)
    (let ([arg-len (length args)])
      (case prim-proc
        [(+) (apply + args)]
        [(-) (apply - args)]
        [(*) (apply * args)]
        [(add1) (+ (1st args) 1)]
        [(sub1) (- (1st args) 1)]
        [(cons) (if (= arg-len 2)
                    (cons (1st args) (2nd args))
                    (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                    )]
        [(=) (apply = args)]
        [(/) (apply / args)]
        [(<) (apply < args)]
        [(>) (apply > args)]
        [(<=) (apply <= args)]
        [(>=) (apply >= args)]
        [(zero?) (if (= 1 arg-len)
                     (zero? (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                     )]
        [(not) (if (= 1 arg-len)
                    (not (1st args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(car) (if (= 1 arg-len )
                   (car (1st args))
                   (error 'apply-prim-proc
                          "Cannot car empty list")
                   )]
        [(cdr) (if (= 1 arg-len)
                     (cdr (1st args))
                     (error 'apply-prim-proc
                            "cannot cdr empty list")
                   )]
        [(list) args]
        [(null?) (if (= arg-len 1)
                     (null? (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                     )]
        [(assq) (if (= arg-len 2)
                    (assq (1st args) (2nd args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                    )]
        [(equal?) (if (= arg-len 2)
                   (equal? (1st args) (2nd args))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(eq?) (if (= arg-len 2)
                   (eq? (1st args) (2nd args))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(equal?) (if (= arg-len 2)
                      (equal? (1st args) (2nd args))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(atom?) (if (= arg-len 1)
                     (atom? (1st args))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(length) (if (= arg-len 1)
                      (length (1st args))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(list->vector) (if (= arg-len 1)
                            (list->vector (1st args))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(list?) (if (= arg-len 1)
                     (list? (1st args))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(pair?) (if (= arg-len 1)
                     (pair? (1st args))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(procedure?) (if (= arg-len 1)
                          (proc-val? (1st args))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector->list) (if (= arg-len 1)
                            (vector->list (1st args))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(vector) (apply vector args)]
        [(make-vector) (cond [(= arg-len 1)
                              (make-vector (1st args))]
                             [(= arg-len 2)
                              (make-vector (1st args) (2nd args))]
                             [else (error 'apply-prim-proc
                                           "Incorrect argument count in call ~s"
                                           prim-proc)]
                        )]
        [(vector-ref) (if (= arg-len 2)
                          (vector-ref (1st args) (2nd args))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector?) (if (= arg-len 1)
                       (vector? (1st args))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(number?) (if (= arg-len 1)
                        (number? (1st args))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(symbol?) (if (= arg-len 1)
                       (symbol? (1st args))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(set-car!) (if (= arg-len 2)
                        (apply set-car! args)
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(set-cdr!) (if (= arg-len 2)
                        (apply set-cdr! args)
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(vector-set!) (if (= arg-len 3)
                           (vector-set! (1st args) (2nd args) (3rd args))
                           (error 'apply-prim-proc
                                  "Incorrect argument count in call ~s"
                                  prim-proc))]
        [(display) (if (= arg-len 1)
                       (display (1st args))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(newline) (if (null? args)
                       (newline)
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(caar) (if (= arg-len 1)
                       (caar (1st args))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(cadr) (if (= arg-len 1)
                    (cadr (1st args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cdar) (if (= arg-len 1)
                    (cdar (1st args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cddr) (if (= arg-len 1)
                    (cddr (1st args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caaar) (if (= arg-len 1)
                    (caaar (1st args))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caadr) (if (= arg-len 1)
                     (caadr (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cadar) (if (= arg-len 1)
                     (cadar (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(caddr) (if (= arg-len 1)
                     (caddr (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdaar) (if (= arg-len 1)
                     (cdaar (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdadr) (if (= arg-len 1)
                     (cdadr (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cddar) (if (= arg-len 1)
                     (cddar (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdddr) (if (= arg-len 1)
                     (cdddr (1st args))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(map) (apply map (cons (lambda n (apply-proc (1st args) n)) (cdr args)))]
        [(apply) (apply-proc (1st args) (cadr args))]
        [(member) (apply member args)]
        [(quotient) (apply quotient args)]
        [else (error 'apply-prim-proc 
                     "Bad primitive procedure name: ~s" 
                     prim-proc)]))))

(define rep      ;"read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;;;notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
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
