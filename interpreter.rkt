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
         (let*-values ([(value env*) (interpret-expr expr env)]
                       [(variables*) (environment-variables env*)])
           (struct-copy environment env*
                        [variables (dict-set variables* id value)]))]
        [`(using (,qubits ...) ,stmts ...)
         (let*-values ([(num env*) (using-qubits qubits env)]
                       [(env**) (foldl interpret-stmt env* stmts)]
                       [(state**) (environment-state env**)])
           ; TODO: Remove qubit variables from the environment.
           (struct-copy environment env**
                        [state (release-qubits num state**)]))]
        [`(return ,expr)
         (let-values ([(value env*) (interpret-expr expr env)])
           `(,value ,(environment-probabilities env*)))]
        [expr
         (let-values ([(value env*) (interpret-expr expr env)])
           env*)])))

(define (interpret-expr expr env)
  (define (apply-gate gate qubit)
    (let*-values ([(id env*) (interpret-expr qubit env)]
                  [(state*) (environment-state env*)])
      (values (void)
              (struct-copy environment env*
                           [state (column-vector->list
                                   (apply-to-qubit gate id state*))]))))

  (match expr
    [`(x ,q) (apply-gate x-gate q)]
    [`(z ,q) (apply-gate z-gate q)]
    [`(h ,q) (apply-gate h-gate q)]
    [`(t ,q) (apply-gate t-gate q)]
    [`(cnot ,controls ,target)
     (let*-values ([(control-id env*) (interpret-expr controls env)]
                   [(target-id env**) (interpret-expr target env*)]
                   [(state**) (environment-state env**)])
       (values (void)
               (struct-copy environment env**
                            [state (column-vector->list
                                    (apply-controlled x-gate
                                                      `((,control-id . #t))
                                                      target-id
                                                      state**))])))]
    [`(controlled ,operator ,controls ,target)
     (let*-values ([(gate) (match operator
                             ['x x-gate]
                             ['z z-gate]
                             ['h h-gate]
                             ['t t-gate])]
                   [(control-ids env*) (interpret-expr controls env)]
                   [(target-id env**) (interpret-expr target env*)]
                   [(state**) (environment-state env**)])
       (values
        (void)
        (struct-copy environment env**
                     [state (column-vector->list
                             (apply-controlled gate
                                               (map (lambda (id) `(,id . #t))
                                                    control-ids)
                                               target-id
                                               state**))])))]
    [`(m ,q)
     (match-let*-values
         ([(id (environment variables* state* probabilities*))
           (interpret-expr q env)]
          [(`(,result ,state** ,probability)) (measure state* id)])
       (values result
               (environment variables*
                            state**
                            (dict-set probabilities* result probability))))]
    [`(reset ,q)
     (values (void) (interpret-stmt `(if (m ,q) (x ,q)) env))]
    [`(reset-all ,qs)
     (let-values ([(ids env*) (interpret-expr qs env)])
       (values (void)
               (foldl interpret-stmt
                      env*
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
    [id (values (dict-ref (environment-variables env) id) env)]))

(define (apply-operator operator state)
  (matrix-multiply operator (list->column-vector state)))

(define (expand-operator operator qubit size)
  (let ([operators (build-list size
                               (lambda (i)
                                 (if (= i qubit) operator identity-gate)))])
    (foldl kronecker-product (car operators) (cdr operators))))

(define (control-operator operator controls target size)
  (define (controls-satisfied basis)
    (andmap identity
            (dict-map controls
                      (lambda (qubit value)
                        (= (if value 1 0)
                           (bitwise-bit-field basis qubit (+ 1 qubit)))))))

  (let ([expanded-op (expand-operator operator target size)]
        [expanded-id (expand-operator identity-gate target size)])
    (transpose (map (lambda (basis op-column id-column)
                      (if (controls-satisfied basis) op-column id-column))
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
  (define (update-env env name value state)
    (struct-copy environment env
                 [variables (dict-set (environment-variables env) name value)]
                 [state state]))

  (for/fold ([num 0]
             [env* env])
            ([initializer initializers])
    (let ([state* (environment-state env*)])
      (match initializer
        [`[,name (qubit)]
         (let-values ([(ids state**) (allocate-qubits 1 state*)])
           (values (+ 1 num) (update-env env* name (car ids) state**)))]
        [`[,name (qubits ,size)]
         (let-values ([(ids state*) (allocate-qubits size state*)])
           (values (+ size num) (update-env env* name ids state*)))]))))
