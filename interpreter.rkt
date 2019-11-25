#lang rosette


(require "complex.rkt"
         "matrix.rkt")

(provide environment
         interpret-stmt
         interpret-expr)

(struct environment (variables state) #:transparent)

(define identity-gate 
  `((,(complex 1 0) ,(complex 0 0))
    (,(complex 0 0) ,(complex 1 0))))

(define x-gate
  `((,(complex 0 0) ,(complex 1 0))
    (,(complex 1 0) ,(complex 0 0))))

(define z-gate
  `((,(complex 1 0) ,(complex 1 0))
    (,(complex 0 0) ,(complex -1 0))))

(define h-gate
  `((,(complex (/ 1 (sqrt 2)) 0) ,(complex (/ 1 (sqrt 2)) 0))
    (,(complex (/ 1 (sqrt 2)) 0) ,(complex (/ -1 (sqrt 2)) 0))))

(define t-gate
  `((,(complex 1 0) ,(complex 0 0))
    (,(complex 0 0) ,(complex (/ 1 (sqrt 2)) (/ 1 (sqrt 2))))))

(define select-zero
  `((,(complex 1 0) ,(complex 0 0))
    (,(complex 0 0) ,(complex 0 0))))

(define select-one
  `((,(complex 0 0) ,(complex 0 0))
    (,(complex 0 0) ,(complex 1 0))))

(define (interpret-stmt stmt env)
  (if (not (environment? env))
      ; env is some return value instead, so just return it again.
      env
      ; env is actually an environment.
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
           (remove-qubits qubits
                          (foldl interpret-stmt (add-qubits qubits env) stmts))]
          [`(return ,expr)
           (let-values ([(value env) (interpret-expr expr env)])
             value)]
          [expr
           (let-values ([(value env*) (interpret-expr expr env)])
             env*)]))))

(define (interpret-expr expr env)
  (let* ([variables (environment-variables env)]
         [state (environment-state env)]
         [apply-gate
          (lambda (gate qubit)
            (values (void)
                    (environment variables
                                 (column-vector->list
                                  (apply-to-qubit gate
                                                  (dict-ref variables qubit)
                                                  state)))))])
    (match expr
      [`(x ,q) (apply-gate x-gate q)]
      [`(z ,q) (apply-gate z-gate q)]
      [`(h ,q) (apply-gate h-gate q)]
      [`(t ,q) (apply-gate t-gate q)]
      [`(m ,q)
       (match-let ([`(,result ,state*) (measure state (dict-ref variables q))])
         (values result (environment variables state*)))]
      [`(reset ,q)
       (values (void) (interpret-stmt `(if (m ,q)
                                           (x ,q))
                                      env))]
      [(? boolean?) (values expr env)]
      [id (values (dict-ref variables id) env)])))

(define (apply-to-qubit operator qubit state)
  (let ([operators (build-list
                    (num-qubits state)
                    (lambda (i) (if (= i qubit) operator identity-gate)))])
    (matrix-multiply (foldl kronecker-product (car operators) (cdr operators))
                     (list->column-vector state))))

(define (measure state qubit)
  ; The state vector is unnormalized, so remember to divide the probability by
  ; the state vector's magnitude squared.
  (let* ([state-mag-sq (vector-magnitude-sq (list->column-vector state))]
         [zero-state (apply-to-qubit select-zero qubit state)]
         [zero-probability (/ (vector-magnitude-sq zero-state) state-mag-sq)]
         [one-state (apply-to-qubit select-one qubit state)]
         [one-probability (/ (vector-magnitude-sq one-state) state-mag-sq)])
    ; For now, just choose the result with higher probability.
    ; TODO: Remember both results somehow.
    (if (>= zero-probability one-probability)
        `(#f ,(column-vector->list zero-state))
        `(#t ,(column-vector->list one-state)))))

(define (num-qubits state)
  (exact-truncate (log (length state) 2)))

(define (add-qubits qubits env)
  (let* ([variables (environment-variables env)]
         [state (environment-state env)]
         [next-id (if (empty? state) 0 (num-qubits state))]
         [variables* (foldl (lambda (q id vs) (dict-set vs q id))
                            variables
                            qubits
                            (stream->list
                             (stream-take (in-naturals next-id)
                                          (length qubits))))]
         [state* (if (empty? state)
                     (build-list (expt 2 (length qubits))
                                 (lambda (i)
                                   (if (= 0 i) (complex 1 0) (complex 0 0))))
                     (append state
                             (build-list (* (length state)
                                            (- (expt 2 (length qubits)) 1))
                                         (const (complex 0 0)))))])
    (environment variables* state*)))

(define (remove-qubits qubits env)
  (let* ([variables (environment-variables env)]
         [state (environment-state env)]
         [variables* (foldl (lambda (q vs) (dict-remove vs q))
                            variables
                            qubits)]
         [state* (take state (/ (length state) (expt 2 (length qubits))))])
    ; TODO: Check that released qubits are all zero?
    (environment variables* state*)))
