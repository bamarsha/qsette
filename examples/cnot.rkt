#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (cnot-gate b)
  (begin
    (mutable r #f)
    (using ([q1 (qubit)]
            [q2 (qubit)])
           (if b
               (x q1))
           (cnot q1 q2)
           (set r (m q2))
           (reset q1)
           (reset q2))
    (return r)))

(operation (cnot-gate-wrong b)
  (begin
    (mutable r #f)
    (using ([q1 (qubit)]
            [q2 (qubit)])
           (x q1)
           (cnot q1 q2)
           (set r (m q2))
           (reset q1)
           (reset q2))
    (return r)))
                 
(printf "Pr(cnot-gate(#f) = #f) = ~a\n" (probability/v (cnot-gate #f) #f))
(clear-asserts!)
(printf "Pr(cnot-gate(#t) = #t) = ~a\n" (probability/v (cnot-gate #t) #t))
(clear-asserts!)
(newline)

(define-symbolic x boolean?)
(printf "Pr(cnot-gate(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (cnot-gate x) x)))))
(newline)

(printf "Pr(cnot-gate-wrong(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (cnot-gate-wrong x) x)))))
