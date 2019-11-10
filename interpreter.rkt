#lang rosette

(require "matrix.rkt")

(provide environment
         interpret-stmt
         interpret-expr)

(struct environment (variables state) #:transparent)

(define identity-operator
  '((1 0)
    (0 1)))

(define select-zero
  '((1 0)
    (0 0)))

(define select-one
  '((0 0)
    (0 1)))

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
       (let* ([next-id (if (empty? state) 0 (num-qubits state))]
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
      [`(m ,q)
       (let-values ([(result state*) (measure state (dict-ref variables q))])
         (values result (environment variables state*)))]
      [`(reset ,q) (error "reset not implemented")]
      [(? boolean?) (values expr env)]
      [id (values (dict-ref variables id) env)])))

(define (apply-to-qubit operator qubit state)
  (let ([operators (build-list
                    (num-qubits state)
                    (lambda (i) (if (= i qubit) operator identity-operator)))])
    (matrix-multiply (foldl kronecker-product (car operators) (cdr operators))
                     (list->column-vector state))))

(define (measure state qubit)
  (let* ([zero-state (apply-to-qubit select-zero qubit state)]
         [zero-probability (vector-magnitude-sq zero-state)]
         [one-state (apply-to-qubit select-one qubit state)]
         [one-probability (vector-magnitude-sq one-state)])
    ; For now, just choose the result with higher probability.
    ; TODO: Remember both results somehow.
    (if (> zero-probability one-probability)
        (values #f (scale (/ 1 (sqrt zero-probability)) zero-state))
        (values #t (scale (/ 1 (sqrt one-probability)) one-state)))))

(define (num-qubits state)
  (exact-truncate (log (length state) 2)))
