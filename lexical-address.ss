;; lexical-address for the interpreter

(define get-depth-pos
  (lambda (ls sym)
    
    (letrec ([helper (lambda (L S depth)
                       (if (null? L) 
                           (list S -1)
                       (let ([pos (list-find-position S (car L))])
                         (cond [(not pos) (helper (cdr L) S (+ depth 1))]
                               [else (list depth pos)]))))])
      (helper ls sym 0))))