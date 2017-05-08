;; File for lexical address parsing
(define lexical-address
  (lambda (exp)
    (address-helper exp '())))

(define address-helper
  (lambda (exp bind-ls)
    (if (symbol? exp)
        (replace-with-address exp bind-ls)
        (case (car exp)
          [(lambda)
           (list 'lambda
                 (cadr exp)
                 (address-helper (caddr exp) (update-bind-ls bind-ls (cadr exp))))]
          [(set!)
           (list 'set!
                 (cadr exp)
                 (address-helper (caddr exp) bind-ls))]
          [(if)
           (cons 'if
                 (map (lambda (n) (address-helper n bind-ls)) (cdr exp)))]
          [(let)
           (list 'let
                 (map (lambda (n) (list (car n) (address-helper (cadr n) bind-ls))) (cadr exp))
                 (let ([bound-vars (map car (cadr exp))])
                   (address-helper (caddr exp) (update-bind-ls bind-ls bound-vars))))]
          [else (map
                (lambda (n) (address-helper n bind-ls))
                 exp)]))))

(define replace-with-address
  (lambda (var bind-ls)
    (let ([bound-var (find (lambda (m) (equal? var (car m))) bind-ls)])
       (if bound-var
           (cons ': (cdr bound-var))
           (list ': 'free var)))))

(define update-bind-ls
  (lambda (bind-ls new-ls)
    (append
     (increase-depth
      (filter (lambda (n)
                (not (member (car n) new-ls)))
              bind-ls))
     (make-bind-list new-ls))))

(define make-bind-list
  (lambda (ls)
    (let loop ([index 0] [ls ls])
      (if (null? ls) '()
          (cons (list (car ls) 0 index) (loop (add1 index) (cdr ls)))))))

(define increase-depth
  (lambda (ls)
    (if (null? ls) '()
        (cons (list (caar ls) (add1 (cadar ls)) (caddar ls)) (increase-depth (cdr ls))))))

