;; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

(define get-id
  (lambda (n)
    (cases lambda-id n
      [ref-id (sym)
        sym]
      [val-id (sym)
        sym])))


(define eval-id ; ALWAYS RETURNS A BOX
  (lambda (n env arg)
    (cases lambda-id n
      [ref-id (sym)
        (cases expression arg
          [la-var-exp (depth pos)
                    (apply-env-ref-global env 
                                          depth
                                          pos
                                         (lambda (x) x)
                                         (lambda ()
                                           (eopl:error 'eval-id
                                                       "variable not found in environment ~s"
                                                       var-sym)))]
          [else (eopl:error 'eval-id
                            "expected variable reference, got garbage: ~s"
                            arg)])]
      [val-id (sym)
        (box (eval-exp arg env))]
      )))
;; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [la-var-exp (depth pos)
        (apply-env-ref-global
            env depth pos ; look up its value.
      	   (lambda (x) (deref x)) ; procedure to call if id is in the environment
           (lambda () (eopl:error
                                  'var-exp
                                  "variable not found in environment: ~s"
                                  depth)))]
      [if-exp (condition true false)
              (let ([cond-val (eval-exp condition env)])
                (if cond-val (eval-exp true env) (eval-exp false env)))]
      [single-if-exp (condition true)
                     (let ([cond-val (eval-exp condition env)])
                       (if cond-val (eval-exp true env)))]
      [app-exp (rator rands)
               (let ([proc-value (eval-exp rator env)])
                 (apply-proc proc-value rands env))]
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
      [letrec-exp (id vals body)
        (let ([new-env (recursively-extended-env-record
                         id (map box vals) env)])
          (apply-to-bodies new-env body))]

      [lambda-exp (syms list-id body)
               (closure syms list-id body env)]
      [while-exp (test body)
        (if (eval-exp test env)
            (begin (apply-to-bodies env body)
                   (eval-exp exp env)))]
      [for-exp (init condition update body)
               (begin
                 (apply-to-bodies env init)
                 (let loop ()
                   (if (eval-exp condition env)
                       (begin
                         (apply-to-bodies env body)
                         (apply-to-bodies env update)
                         (loop)))))]
      [la-set!-exp (depth pos rand)
                (apply-env-ref-global env
                               depth
                               pos
                              (lambda (x) (set-ref! x (eval-exp rand env)))
                              (lambda () (eopl:error
                                                                     'apply-env-ref
                                                                     "variable not found in environment: ~s"
                                                                     depth)))] ;depth will hold symbol if not bound
      [define-exp (id val)
        (set! global-env (extend-env (list id) (list (box (eval-exp val env))) global-env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (n) (eval-exp n env)) rands)))

;; Executes the items in procs with the environment env. Meant for use in the body of a lambda

(define apply-to-bodies
  (lambda (env procs)
    (cond
     [(null? procs) (void)]
     [(null? (cdr procs))
      (eval-exp (car procs) env)]
     [else
      (begin (eval-exp (car procs) env) (apply-to-bodies env (cdr procs)))])))

;; Apply a procedure to its arguments.
;; At this point, we only have primitive procedures.
;; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args orig-env) ;added orig-env to evaluate by reference   
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args orig-env)]
      [closure (syms list-id proc env)
                (if (null? list-id)
                    (let ([new-env (extend-env (map get-id syms)
                                               (map (lambda (x y) (eval-id x orig-env y))
                                                    syms args) 
                                               env)])
                      (apply-to-bodies new-env proc))
                    (let* ([newer-args (new-args args (length syms))]
                           [new-env (extend-env (append (map get-id syms) (list list-id))
                                               newer-args
                                               env)])
                      (apply-to-bodies new-env proc)))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define syntax-expand
  (lambda (exp bind-ls)
    (cases expression exp
           [la-var-exp (depth pos)
             exp]
           [var-exp (id)
                    (let ([result (get-depth-pos bind-ls id)])
                      (la-var-exp (car result) (cadr result)))]
           [lit-exp (id)
            exp]
           [lambda-exp (id list-id body)
                       (lambda-exp id list-id (map (lambda (x) (syntax-expand x
                                                                             (cons (append (map get-id id) (list list-id)) bind-ls))) body))]
           [let-exp (id val body)
             (syntax-expand (app-exp (lambda-exp (map val-id id) '() body) val) bind-ls)]
           [let*-exp (id val body)
                     (if (null? (cdr id))
                         (syntax-expand (let-exp (list (car id)) (list (car val)) body) bind-ls)
                         (syntax-expand (let-exp (list (car id))
                                                 (list (car val))
                                                 (list (let*-exp (cdr id) (cdr val) body))) bind-ls))]
           [and-exp (rand)
                    (if (null? (cdr rand))
                        (syntax-expand (car rand) bind-ls)
                        (syntax-expand (if-exp (car rand) (and-exp (cdr rand)) (lit-exp #f)) bind-ls))]
           [or-exp (rand)
                   (cond
                    [(null? rand) (lit-exp #f)]
                    [(null? (cdr rand))
                     (syntax-expand (car rand) bind-ls)]
                    [else (syntax-expand (let-exp (list 'res) 
                                                  (list (car rand))
                                                  (list (if-exp (var-exp 'res)
                                                                (var-exp 'res) 
                                                                (or-exp (cdr rand))))) bind-ls)])]
           [letrec-exp (id val body)
                     (let ([new-bind-ls (cons id bind-ls)])
                       (letrec-exp id (map (lambda (x) (syntax-expand x new-bind-ls)) val) (map (lambda (x) (syntax-expand x new-bind-ls)) body)))]
           [if-exp (condition true false)
                   (if-exp (syntax-expand condition bind-ls) (syntax-expand true bind-ls) (syntax-expand false bind-ls))]
           [single-if-exp (condition true)
                          (single-if-exp (syntax-expand condition bind-ls) (syntax-expand true bind-ls))]
           [la-set!-exp (depth pos rand)
             exp]
           [set!-exp (id rand)
             (let ([result (get-depth-pos bind-ls id)])
                     (la-set!-exp (car result) (cadr result) (syntax-expand rand bind-ls)))]
           [app-exp (rator rand)
                    (app-exp (syntax-expand rator bind-ls) (map (lambda (x) (syntax-expand x bind-ls)) rand))]
           [begin-exp (execs)
                      (syntax-expand (app-exp (lambda-exp '() '() execs) '()) bind-ls)]
           [cond-exp (conds execs else-exp)
                     (cond
                      [(null? conds) (syntax-expand (begin-exp else-exp) bind-ls)]
                      [(and (null? (cdr conds)) (null? else-exp))
                       (syntax-expand (single-if-exp (car conds)
                                                     (begin-exp (car execs))) bind-ls)]
                      [(null? (cdr conds))
                       (syntax-expand (if-exp (car conds)
                                              (begin-exp (car execs))
                                              (begin-exp else-exp)) bind-ls)]
                      [else
                       (syntax-expand (if-exp (car conds)
                                              (begin-exp (car execs))
                                              (cond-exp (cdr conds) (cdr execs) else-exp)) bind-ls)])]
           [case-exp (val cases execs else-exp)
                     (syntax-expand
                      (cond-exp
                       (map (lambda (n) (app-exp (var-exp 'member)
                                                 (list val (lit-exp (map unparse-exp n)))))
                            cases)
                       execs
                       else-exp) bind-ls)]
           [while-exp (test body)
                      (while-exp (syntax-expand test bind-ls) (map (lambda (x) (syntax-expand x bind-ls)) body))]
           [for-exp (init condition update body)
                    (for-exp (map (lambda (x) (syntax-expand x bind-ls)) init)
                             (syntax-expand condition bind-ls)
                             (map (lambda (x) (syntax-expand x bind-ls)) update)
                             (map (lambda (x) (syntax-expand x bind-ls)) body))]
           [named-let-exp (id value body)
                          (syntax-expand
                           (letrec-exp (list (1st id))
                                       (list (lambda-exp (map val-id (cdr id)) '() body))
                                       (list (app-exp (var-exp (1st id)) value))) bind-ls)]
           [define-exp (id val)
             (define-exp
               id
               (syntax-expand val bind-ls))]
      )))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero?
                              not < > <= >= car cdr list null? assq eq? equal? atom?
                              length list->vector list? pair? procedure? vector->list
                              vector make-vector vector-ref vector? number? symbol?
                              set-car! set-cdr! vector-set! display newline
                              caar cadr cdar cddr
                              caaar caadr cadar caddr cdaar cdadr cddar cdddr
                              map apply member quotient
                              list-tail eqv? append))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
   *prim-proc-names*     ; a value (not an expression) with an identifier.
   (map box (map prim-proc
                 *prim-proc-names*))
   (empty-env)))

(define global-env init-env)

(define reset-global-env
  (lambda () (set! global-env init-env)))

;; Usually an interpreter must define each 
;; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args env)
    (let ([arg-len (length args)])
      (case prim-proc
        [(+) (apply + (map (lambda (n) (eval-exp n env)) args))]
        [(-) (apply - (map (lambda (n) (eval-exp n env)) args))]
        [(*) (apply * (map (lambda (n) (eval-exp n env)) args))]
        [(add1) (+ (eval-exp (1st args) env) 1)]
        [(sub1) (- (eval-exp (1st args) env) 1)]
        [(cons) (if (= arg-len 2)
                    (cons (eval-exp (1st args) env) (eval-exp (2nd args) env))
                    (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                    )]
        [(=) (apply = (map (lambda (x) (eval-exp x env)) args))]
        [(/) (apply / (map (lambda (x) (eval-exp x env)) args))]
        [(<) (apply < (map (lambda (x) (eval-exp x env)) args))]
        [(>) (apply > (map (lambda (x) (eval-exp x env)) args))]
        [(<=) (apply <= (map (lambda (x) (eval-exp x env)) args))]
        [(>=) (apply >= (map (lambda (x) (eval-exp x env)) args))]
        [(zero?) (if (= 1 arg-len)
                     (zero? (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                     )]
        [(not) (if (= 1 arg-len)
                    (not (1st (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(car) (if (= 1 arg-len )
                   (car (1st (map (lambda (x) (eval-exp x env)) args)))
                   (error 'apply-prim-proc
                          "Cannot car empty list")
                   )]
        [(cdr) (if (= 1 arg-len)
                     (cdr (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "cannot cdr empty list")
                   )]
        [(list) (map (lambda (x) (eval-exp x env)) args)]
        [(null?) (if (= arg-len 1)
                     (null? (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc)
                     )]
        [(assq) (if (= arg-len 2)
                    (assq (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                    )]
        [(equal?) (if (= arg-len 2)
                   (equal? (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(eq?) (if (= arg-len 2)
                   (eq? (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))
                   (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                   )]
        [(equal?) (if (= arg-len 2)
                      (equal? (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(atom?) (if (= arg-len 1)
                     (atom? (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(length) (if (= arg-len 1)
                      (length (1st (map (lambda (x) (eval-exp x env)) args)))
                      (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                      )]
        [(list->vector) (if (= arg-len 1)
                            (list->vector (1st (map (lambda (x) (eval-exp x env)) args)))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(list?) (if (= arg-len 1)
                     (list? (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(pair?) (if (= arg-len 1)
                     (pair? (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                     )]
        [(procedure?) (if (= arg-len 1)
                          (proc-val? (1st (map (lambda (x) (eval-exp x env)) args)))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector->list) (if (= arg-len 1)
                            (vector->list (1st (map (lambda (x) (eval-exp x env)) args)))
                            (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                            )]
        [(vector) (apply vector (map (lambda (x) (eval-exp x env)) args))]
        [(make-vector) (cond [(= arg-len 1)
                              (make-vector (1st (map (lambda (x) (eval-exp x env)) args)))]
                             [(= arg-len 2)
                              (make-vector (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))]
                             [else (error 'apply-prim-proc
                                           "Incorrect argument count in call ~s"
                                           prim-proc)]
                        )]
        [(vector-ref) (if (= arg-len 2)
                          (vector-ref (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)))
                          (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc)
                          )]
        [(vector?) (if (= arg-len 1)
                       (vector? (1st (map (lambda (x) (eval-exp x env)) args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(number?) (if (= arg-len 1)
                        (number? (1st (map (lambda (x) (eval-exp x env)) args)))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(symbol?) (if (= arg-len 1)
                       (symbol? (1st (map (lambda (x) (eval-exp x env)) args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(set-car!) (if (= arg-len 2)
                        (apply set-car! (map (lambda (x) (eval-exp x env)) args))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(set-cdr!) (if (= arg-len 2)
                        (apply set-cdr! (map (lambda (x) (eval-exp x env)) args))
                        (error 'apply-prim-proc
                               "Incorrect argument count in call ~s"
                               prim-proc))]
        [(vector-set!) (if (= arg-len 3)
                           (vector-set! (1st (map (lambda (x) (eval-exp x env)) args)) (2nd (map (lambda (x) (eval-exp x env)) args)) (3rd (map (lambda (x) (eval-exp x env)) args)))
                           (error 'apply-prim-proc
                                  "Incorrect argument count in call ~s"
                                  prim-proc))]
        [(display) (if (= arg-len 1)
                       (display (1st (map (lambda (x) (eval-exp x env)) args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(newline) (if (null? (map (lambda (x) (eval-exp x env)) args))
                       (newline)
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(caar) (if (= arg-len 1)
                       (caar (1st (map (lambda (x) (eval-exp x env)) args)))
                       (error 'apply-prim-proc
                              "Incorrect argument count in call ~s"
                              prim-proc))]
        [(cadr) (if (= arg-len 1)
                    (cadr (1st (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cdar) (if (= arg-len 1)
                    (cdar (1st (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(cddr) (if (= arg-len 1)
                    (cddr (1st (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caaar) (if (= arg-len 1)
                    (caaar (1st (map (lambda (x) (eval-exp x env)) args)))
                    (error 'apply-prim-proc
                           "Incorrect argument count in call ~s"
                           prim-proc))]
        [(caadr) (if (= arg-len 1)
                     (caadr (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cadar) (if (= arg-len 1)
                     (cadar (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(caddr) (if (= arg-len 1)
                     (caddr (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdaar) (if (= arg-len 1)
                     (cdaar (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdadr) (if (= arg-len 1)
                     (cdadr (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cddar) (if (= arg-len 1)
                     (cddar (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(cdddr) (if (= arg-len 1)
                     (cdddr (1st (map (lambda (x) (eval-exp x env)) args)))
                     (error 'apply-prim-proc
                            "Incorrect argument count in call ~s"
                            prim-proc))]
        [(map) (apply map (cons (lambda n (apply-proc (1st args)
                                                      (map (lambda (x) (syntax-expand x '()))
                                                           (map parse-exp n)) env))
                                (cdr args)))]
        [(apply) (apply-proc (1st args) (map (lambda (x) (syntax-expand x '()))
                                             (map parse-exp (cadr args))) env)]
        [(member) (apply member args)]
        [(quotient) (apply quotient args)]
        [(list-tail) (apply list-tail args)]
        [(eqv?) (apply eqv? args)]
        [(append) (apply append args)]
        [else (error 'apply-prim-proc 
                     "Bad primitive procedure name: ~s" 
                     prim-proc)]))))

(define rep      ;"read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;;;notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read)) '()))])
      (if (proc-val? answer)
          (display "<interpreter-procedure>")
          (eopl:pretty-print answer))
      (newline)
      (rep))))  ;tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x) '()))))

(define new-args
  (lambda (args n)
    (if (zero? n)
        (cons args '())
        (cons (car args) (new-args (cdr args) (sub1 n))))))
