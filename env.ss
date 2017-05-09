;; Environment definitions for CSSE 304 Scheme interpreter.  
;; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define deref unbox)

(define set-ref! set-box!)

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env-ref-global
  (lambda (env depth pos succeed fail)
    (cond [(symbol? depth)
           (apply-env-ref global-env depth succeed fail)]
          [else
            (apply-env-ref-la env depth pos succeed fail)])))

(define apply-env-ref
  (lambda (env sym succeed fail) ; succeed and fail are "callback procedures,
    (cases environment env       ;  succeed is appluied if sym is found, otherwise
           [empty-env-record ()       ;  fail is applied.
                             (fail)]
           [extended-env-record (syms vals env)
                                (let ((pos (list-find-position sym syms)))
                                  (if (number? pos)
                                      (succeed (list-ref vals pos))
                                      (apply-env-ref env sym succeed fail)))]
           [recursively-extended-env-record (proc-names bodiess old-env)
             (let ([pos (list-find-position sym proc-names)])
               (if (number? pos)
                   (cases expression (deref (list-ref bodiess pos))
                     [lambda-exp (id list-id bodies)
                       (succeed (box (closure id list-id bodies env)))]
                     [else (void)])
                   (apply-env-ref old-env sym succeed fail)))])))

(define apply-env-ref-la
  (lambda (env depth pos succeed fail)
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
        (if (= depth 0)
            (succeed (list-ref vals pos))
            (apply-env-ref-la env (- depth 1) pos succeed fail))]
      [recursively-extended-env-record (proc-names bodiess old-env)
        (if (zero? depth)
            (cases expression (deref (list-ref bodiess pos))
              [lambda-exp (id list-id bodies)
                (succeed (box (closure id list-id bodies env)))]
              [else (void)])
            (apply-env-ref-la old-env (- depth 1) pos succeed fail))]
      )))