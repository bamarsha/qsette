#lang rosette

(provide probability/p
         probability/v)

(define (probability/p result predicate)
  (match-let* ([`(,value ,probabilities) result]
               [solver (solve+)]
               [first-solution (solver (predicate value))]
               [all-solutions (cons first-solution
                                    (find-all-solutions probabilities
                                                        solver
                                                        first-solution))])
    (solver 'shutdown)
    ; TODO: Shouldn't need to clear the assertion store...
    (clear-asserts!)
    (apply + (map (model->probability value probabilities predicate)
                  all-solutions))))

(define (probability/v result expected)
  (probability/p result (lambda (x) (equal? x expected))))

(define (find-all-solutions probabilities solver previous)
  (let ([solution (solver (negate-random-outcomes probabilities previous))])
    (if (unsat? solution)
        null
        (cons solution (find-all-solutions probabilities solver solution)))))

(define (negate-random-outcomes probabilities solution)
  (andmap identity (hash-map (random-variables-only probabilities solution)
                             (lambda (key value)
                               (not (equal? key value))))))

(define ((model->probability value probabilities predicate) solution)
  (let ([rv (random-variables-only probabilities solution)])
    (if (predicate (evaluate value (sat rv)))
        (apply * (hash-map rv (lambda (key value)
                                ; TODO: Might need to evaluate this expression
                                ; using a model of all the probabilities.  The
                                ; final expression should only be in terms of
                                ; non-random variables.
                                (if value
                                    (dict-ref probabilities key)
                                    (- 1 (dict-ref probabilities key))))))
        0)))

(define (random-variables-only probabilities solution)
  (make-immutable-hash
   (filter (compose not void?)
           (hash-map (model solution)
                     (lambda (key value)
                       (when (dict-has-key? probabilities key)
                         (cons key value)))))))
