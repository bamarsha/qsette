#lang rosette

(require "complex.rkt"
         "matrix.rkt")

(provide environment
         interpret-stmt
         interpret-expr)

(struct environment (variables state probabilities) #:transparent)

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
                          (environment-state env*)
                          (environment-probabilities env*)))]
          [`(using (,qubits ...) ,stmts ...)
           (let-values ([(num env*) (using-qubits qubits env)])
             (let ([env** (foldl interpret-stmt env* stmts)])
               ; TODO: Remove qubit variables from the environment.
               (environment (environment-variables env**)
                            (release-qubits num (environment-state env**))
                            (environment-probabilities env**))))]
          [`(return ,expr)
           (let-values ([(value env*) (interpret-expr expr env)])
             `(,value ,(environment-probabilities env*)))]
          [expr
           (let-values ([(value env*) (interpret-expr expr env)])
             env*)]))))

(define (interpret-expr expr env)
  (let* ([variables (environment-variables env)]
         [state (environment-state env)]
         [probabilities (environment-probabilities env)]
         [apply-gate
          (lambda (gate qubit)
            (let-values ([(id env*) (interpret-expr qubit env)])
              (values
               (void)
               (environment (environment-variables env*)
                            (column-vector->list
                             (apply-to-qubit gate id (environment-state env*)))
                            (environment-probabilities env*)))))])
    (match expr
      [`(x ,q) (apply-gate x-gate q)]
      [`(z ,q) (apply-gate z-gate q)]
      [`(h ,q) (apply-gate h-gate q)]
      [`(t ,q) (apply-gate t-gate q)]
      [`(cnot ,controls ,target)
       (values
        (void)
        (environment variables
                     (column-vector->list
                      (apply-controlled x-gate
                                        `((,(dict-ref variables controls) . #t))
                                        (dict-ref variables target)
                                        state))
                     probabilities))]
      [`(controlled ,operator ,controls ,target)
       (let-values ([(gate) (match operator
                              ['x x-gate]
                              ['z z-gate]
                              ['h h-gate]
                              ['t t-gate])]
                    [(control-ids env*) (interpret-expr controls env)]
                    [(target-id env**) (interpret-expr target env)])
         (values
          (void)
          (environment (environment-variables env**)
                       (column-vector->list
                        (apply-controlled gate
                                          (map (lambda (id) `(,id . #t))
                                               control-ids)
                                          target-id
                                          (environment-state env**)))
                       (environment-probabilities env**))))]
      [`(m ,q)
       (let-values ([(id env*) (interpret-expr q env)])
         (match-let ([`(,result ,state* ,probability)
                      (measure (environment-state env*) id)])
           (values result
                   (environment (environment-variables env*)
                                state*
                                (dict-set (environment-probabilities env*)
                                          result probability)))))]
      [`(reset ,q)
       (values (void) (interpret-stmt `(if (m ,q)
                                           (x ,q))
                                      env))]
      [`(reset-all ,qs)
       (let-values ([(ids env*) (interpret-expr qs env)])
         (values (void)
                 (foldl interpret-stmt env*
                        (map (lambda (q) `(reset ,q)) ids))))]
      [`(= ,expr1 ,expr2)
       (let*-values ([(value1 env1) (interpret-expr expr1 env)]
                     [(value2 env2) (interpret-expr expr2 env1)])
         (values (equal? value1 value2) env2))]
      [`(index ,expr ,i)
       (let-values ([(value env*) (interpret-expr expr env)])
         (values (list-ref value i) env*))]
      [(? boolean?) (values expr env)]
      [(? integer?) (values expr env)]
      [id (values (dict-ref variables id) env)])))

(define (apply-operator operator state)
  (matrix-multiply operator (list->column-vector state)))

(define (expand-operator operator qubit size)
  (let ([operators (build-list
                    size
                    (lambda (i) (if (= i qubit) operator identity-gate)))])
    (foldl kronecker-product (car operators) (cdr operators))))

(define (control-operator operator controls target size)
  (let* ([controls-satisfied
          (lambda (basis)
            (andmap
             identity
             (dict-map controls
                       (lambda (qubit value)
                         (= (if value 1 0)
                            (bitwise-bit-field basis qubit (+ 1 qubit)))))))]
         [expanded-op (expand-operator operator target size)]
         [expanded-id (expand-operator identity-gate target size)])
    (transpose (map (lambda (basis op-column id-column)
                      (if (controls-satisfied basis)
                          op-column
                          id-column))
                    (stream->list (in-range (expt 2 size)))
                    (transpose expanded-op)
                    (transpose expanded-id)))))

(define (apply-to-qubit operator qubit state)
  (apply-operator (expand-operator operator qubit (num-qubits state)) state))

(define (apply-controlled operator controls target state)
  (apply-operator (control-operator operator controls target (num-qubits state))
                  state))

(define (measure state qubit)
  ; The state vector is unnormalized, so remember to divide the probability by
  ; the state vector's magnitude squared.
  (let* ([state-mag-sq (vector-magnitude-sq (list->column-vector state))]
         [zero-state (apply-to-qubit select-zero qubit state)]
         [one-state (apply-to-qubit select-one qubit state)]
         [probability (if (= 0 state-mag-sq)
                          0
                          (/ (vector-magnitude-sq one-state) state-mag-sq))])
    (define-symbolic* m boolean?)
    `(,m
      ,(column-vector->list (if m one-state zero-state))
      ,probability)))

(define (num-qubits state)
  (exact-truncate (log (length state) 2)))

(define (allocate-qubits num state)
  (let* ([next-id (if (empty? state) 0 (num-qubits state))]
         [state* (if (empty? state)
                     (build-list (expt 2 num)
                                 (lambda (i)
                                   (if (= 0 i) (complex 1 0) (complex 0 0))))
                     (append state
                             (build-list (* (length state) (- (expt 2 num) 1))
                                         (const (complex 0 0)))))])
    (values (stream->list (in-range next-id (+ num next-id))) state*)))

(define (release-qubits num state)
  ; TODO: Check that released qubits are all zero?
  (if (= num (num-qubits state))
      empty
      (take state (/ (length state) (expt 2 num)))))

(define (using-qubits initializers env)
  (let ([update-env
         (lambda (env name value state*)
           (environment (dict-set (environment-variables env) name value)
                        state*
                        (environment-probabilities env)))])
    (for/fold ([num 0]
               [env* env])
              ([initializer initializers])
      (match initializer
        [`[,name (qubit)]
         (let-values ([(ids state*)
                       (allocate-qubits 1 (environment-state env*))])
           (values (+ 1 num) (update-env env* name (car ids) state*)))]
        [`[,name (qubits ,size)]
         (let-values ([(ids state*)
                       (allocate-qubits size (environment-state env*))])
           (values (+ size num) (update-env env* name ids state*)))]))))
