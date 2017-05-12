(define apply-k
  (lambda (k val)
    (cases continuation k
           [if-k (then-exp else-exp env k)
                   (if val
                       (eval-exp then-exp env k)
                       (eval-exp else-exp env k))]
           [single-if-k (then-exp env k)
                        (if val
                            (eval-exp then-exp env k))]
           [rator-k (rands env k)
                    (eval-rands rands
                                env
                                (rands-k val k))]
           [rands-k (proc-value k)
                    (apply-proc proc-value val k)]
           ;; Auxiliary function continuations below
           [map-k (proc-val car-ls k)
                  (proc-val car-ls
                            (map-proc-k val k))]
           [map-proc-k (cdr-ls k)
                       (apply-k k (cons val cdr-ls))]
           [set!-k (x k)
                  (apply-k k (set-ref! x val))]
           [apply-to-bodies-k (cdr-procs env k)
                              (apply-to-bodies env cdr-procs k)]
           [define-k (id k)
             (apply-k k (set! global-env (extend-env (list id) (list val) global-env)))]
           [value-k ()
                    val])))
