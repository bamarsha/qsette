#lang rosette

(struct environment (variables state) #:transparent)

(define (example1 b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (if b
                                (x q))
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list))))

(define (interpret-stmt stmt env)
  (let ([variables (environment-variables env)]
        [state (environment-state env)])
    (match stmt
      [`(begin ,stmts ...) (foldl interpret-stmt env stmts)]
      [`(if ,expr ,stmt1 ,stmt2)
       (let-values ([(value env*) (interpret-expr expr env)])
         (if value
             (interpret-stmt stmt1 env*)
             (interpret-stmt stmt2 env*)))]
      [`(if ,expr ,stmt1)
       (interpret-stmt `(if ,expr ,stmt1 (begin)) env)]
      [`(mutable ,id ,expr)
       (interpret-stmt `(set ,id ,expr) env)]
      [`(set ,id ,expr)
       (let-values ([(value env*) (interpret-expr expr env)])
         (environment (dict-set (environment-variables env*) id value)
                      (environment-state env*)))]
      [`(using (,qubits ...) ,stmts ...)
       (let* ([next-id (if (empty? state) 0 (log (length state) 2))]
              [variables* (foldl (lambda (q id vs) (dict-set vs q id))
                           variables
                           qubits
                           (stream->list (stream-take (in-naturals next-id)
                                                      (length qubits))))]
              [state* (if (empty? state)
                          (build-list (expt 2 (length qubits))
                                      (lambda (i) (if (= 0 i) 1 0)))
                          (append state (build-list (* (length state)
                                                       (expt 2 (length qubits)))
                                                    (const 0))))])
         (foldl interpret-stmt (environment variables* state*) stmts))]
      [expr
       (let-values ([(value env*) (interpret-expr expr env)])
         env*)])))

(define (interpret-expr expr env)
  (let ([variables (environment-variables env)]
        [state (environment-state env)])
    (match expr
      [`(x ,q) (error "X gate not implemented")]
      [`(m ,q) (error "measurement not implemented")]
      [`(reset ,q) (error "reset not implemented")]
      [(? boolean?) (values expr env)]
      [id (values (dict-ref variables id) env)])))

(example1 #f)
