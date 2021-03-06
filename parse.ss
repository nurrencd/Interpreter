;;Luke Wukusick

(load "chez-init.ss"); This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

;; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

;; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define get-let-val
  (lambda (n)
    (parse-exp (2nd n))))

(define get-last
  (lambda (n)
    (if (pair? (cdr n))
        (get-last (cdr n))
        (cdr n))))

(define only-symbols?
  (lambda (ls)
    (cond [(or (symbol? ls) (null? ls)) #t]
          [(or (and (list? (car ls)) (= (length (car ls)) 2) (eqv? 'ref (caar ls)))
               (symbol? (car ls))) 
           (only-symbols? (cdr ls))]
          [else #f])))

(define make-lambda-ids
  (lambda (n)
    (cond [(symbol? n) (val-id n)]
          [else (ref-id (cadr n))])))

(define get-rest
  (lambda (n)
    (if (pair? (cdr n))
        (cons (car n) (get-rest (cdr n)))
        (cons (car n) '()))))

(define cons-end
  (lambda (n to-add)
    (if (null? (cdr n))
        (cons (car n) to-add)
        (cons (car n) (cons-end (cdr n) to-add)))))

(define cond-parser-helper
  (lambda (exp conds execs)
    (cond [(and (null? (cdr exp)) (equal? 'else (caar exp))) (list conds execs (cdar exp))]
          [(null? (cdr exp)) (list (append conds (list (caar exp)))
                                   (append execs (list (cdar exp)))
                                   '())]
          [else (let ([first (caar exp)]
                      [second (cdar exp)])
                  (cond-parser-helper (cdr exp)
                                      (append conds (list first))
                                      (append execs (list second))))])))

(define let-error-check
  (lambda (n)
    (cond
     [(not ((list-of list?) (2nd n)))
      (eopl:error 'parse-exp "declarations in the expression ~s are not a proper list" n)]
     [(ormap (lambda (m) (not (equal? 2 (length m)))) (2nd n))
      (eopl:error 'parse-exp "declarations in the expression ~s must be of length 2" n)]
     [(ormap (lambda (m) (not (symbol? (car m)))) (2nd n))
      (eopl:error 'parse-exp "first members must be symbols in the expression ~s" n)]
     [(if (> 3 (length n))
          (eopl:error 'parse-exp "the expression ~s has an incorrect length" n))])))

(define parse-exp
  (lambda (datum)
    (cond
     [(or (number? datum) (vector? datum) (boolean? datum) (string? datum) (char? datum) (null? datum))
      (lit-exp datum)]
     [(pair? datum)
      (if (not (list? datum)) (eopl:error 'parse-exp "expression ~s is not a proper list" datum))
      (case (car datum)
       [(lambda)
        (if (> 3 (length datum))
            (eopl:error 'parse-exp "lambda expression: incorrect length ~s" datum))
        (if (not (or (null? (2nd datum)) (symbol? (2nd datum)) (only-symbols? (2nd datum))))
            (eopl:error 'parse-exp "lambda expression: identifiers must be symbols ~s" datum))
        (cond
         [(list? (2nd datum))
          (lambda-exp (map make-lambda-ids (2nd datum))
                      '()
                      (map parse-exp (cddr datum)))]
         [(symbol? (2nd datum))
          (lambda-exp '()
                      (2nd datum)
                      (map parse-exp (cddr datum)))]
         [(pair? (2nd datum))
          (lambda-exp (map make-lambda-ids (get-rest (2nd datum)))
                      (get-last (2nd datum))
                      (map parse-exp (cddr datum)))])]
       [(let)
        (if (> 3 (length datum)) (eopl:error 'parse-exp "let expression: incorrect length ~s" datum))
        (if (or (pair? (2nd datum)) (null? (2nd datum)))
            (begin (let-error-check datum)
                   (let-exp (map 1st (2nd datum))
                            (map get-let-val (2nd datum))
                            (map parse-exp (cddr datum))))
            (named-let-exp (cons (2nd datum) (map 1st (3rd datum)))
                           (map get-let-val (3rd datum))
                           (map parse-exp (cdddr datum))))]
       [(let*)
        (let-error-check datum)
        (let*-exp (map 1st (2nd datum))
                  (map get-let-val (2nd datum))
                  (map parse-exp (cddr datum)))]
       [(letrec)
        (let-error-check datum)
        (letrec-exp (map 1st (2nd datum))
                    (map get-let-val (2nd datum))
                    (map parse-exp (cddr datum)))]
       [(if)
        (if (or (< (length datum) 3) (< 4 (length datum)))
            (eopl:error 'parse-exp "if expression: incorrect length ~s" datum))
        (if (null? (cdddr datum))
            (single-if-exp (parse-exp (2nd datum))
                           (parse-exp (3rd datum)))
            (if-exp (parse-exp (2nd datum))
                    (parse-exp (3rd datum))
                    (parse-exp (4th datum))))]
       [(set!)
        (if (not (equal? (length datum) 3))
            (eopl:error 'parse-exp "set! expression: incorrect length ~s" datum))
        (set!-exp (2nd datum)
                  (parse-exp (3rd datum)))]
       [(quote)
        (if (not (equal? (length datum) 2))
            (eopl:error 'parse-exp "quote expression: incorrect length ~s" datum))
        (lit-exp (2nd datum))]
       [(and)
        (and-exp (map parse-exp (cdr datum)))]
       [(or)
        (or-exp (map parse-exp (cdr datum)))]
       [(begin)
        (begin-exp (map parse-exp (cdr datum)))]
       [(cond)
        (let ([result (cond-parser-helper (cdr datum) '() '())])
          (cond-exp (map parse-exp (car result))
                    (map (lambda (n) (map parse-exp n)) (cadr result))
                    (map parse-exp (caddr result))))]
       [(case)
        (let ([result (cond-parser-helper (cddr datum) '() '())])
          (case-exp (parse-exp (2nd datum))
                    (map (lambda (n) (map parse-exp n)) (car result))
                    (map (lambda (n) (map parse-exp n)) (cadr result))
                    (map parse-exp (caddr result))))]
       [(while)
        (while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
       [(for)
        (for-exp (map parse-exp (1st (2nd datum)))
                 (parse-exp (3rd (2nd datum)))
                 (map parse-exp (cddddr (2nd datum)))
                 (map parse-exp (cddr datum)))]
       [(define)
        (define-exp (2nd datum)
          (parse-exp (3rd datum)))]
       [else (app-exp (parse-exp (1st datum))
                      (map parse-exp (cdr datum)))])]
     [(symbol? datum) (var-exp datum)]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
           [var-exp (id) id]
           [lit-exp (id) id]
           [lambda-exp (id ls body)
                       (if (null? id)
                           (append (list 'lambda ls) (map unparse-exp body))
                           (append (list 'lambda (cons-end id ls)) (map unparse-exp body)))]
           [let-exp (id value body)
                    (append (list 'let (map list id (map unparse-exp value)))
                            (map unparse-exp body))]
           [let*-exp (id value body)
                     (append (list 'let* (map list id (map unparse-exp value)))
                             (map unparse-exp body))]
           [letrec-exp (id value body)
                       (append (list 'letrec (map list id (map unparse-exp value)))
                               (map unparse-exp body))]
           [named-let-exp (id value)
                          (list 'let
                                (1st id)
                                (map list (cdr id) (map unparse-exp value))
                                (map unparse-exp body))]
           [if-exp (condition true false)
                   (list 'if (unparse-exp condition) (unparse-exp true) (unparse-exp false))]
           [single-if-exp (condition true)
                          (list 'if (unparse-exp condition) (unparse-exp true))]
           [set!-exp (id rand)
                     (list 'set id (unparse-exp rand))]
           [app-exp (rator rand)
                    (cons (unparse-exp rator) (map unparse-exp rand))]
           [and-exp (rand)
                    (cons 'and (map unparse-exp rand))]
           [or-exp (rand)
                   (cons 'and (map unparse-exp rand))]
           [begin-exp (execs)
                      (cons 'begin (map unparse-exp execs))]
           [cond-exp (conds execs else)
                     (append (list 'cond) (map (lambda (n m) (cons (unparse-exp n) (map unparse-exp m))) conds execs) (list (cons 'else (map unparse-exp else))))]
           [case-exp (val cases execs else-exp)
                     (append (list 'case (unparse-exp val))
                             (map (lambda (n m) (cons (unparse-exp n) (map unparse-exp m))) conds execs)
                             (list (cons 'else (map unparse-exp else-exp))))]
           [while-exp (test body)
                      (cons 'while (cons (unparse-exp test) (map unparse-exp body)))]
           [for-exp (init condition update body)
                    (list 'for
                          (list (unparse-exp init) ': (unparse-exp condition) ': (unparse-exp update))
                          (unparse-exp body))]
           [define-exp (id val)
             (list 'define
                   id
                   (unparse-exp val))])))

