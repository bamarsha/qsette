#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (entanglement different partial)
  (begin
    (mutable r1 #f)
    (mutable r2 #f)
    (using ([q1 (qubit)]
            [q2 (qubit)])
           (h q1)
           (cnot q1 q2)  ; This entangles the two qubits.
           (if different ; This decides whether the qubits are always the same
               (x q1))   ; or always different.
           (if partial   ; This decides whether the qubits are maximally
               (begin    ; entangled or only partially entangled.
                 (h q1)
                 (t q1)
                 (h q1)))
           (set r1 (m q1))
           (set r2 (m q2)))
    (return (= r1 r2))))

(define-symbolic x boolean?)
(define probability1
  (probability/v (entanglement x #f) (not x)))
(verify (assert (= 1 probability1)))

(define-symbolic y boolean?)
(define probability2
  (probability/v (entanglement #f y) #t))
(verify (assert (<= 0.8 probability2)))
(verify (assert (= 1 probability2)))
