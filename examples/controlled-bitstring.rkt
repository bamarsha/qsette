#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (controlled-bitstring x)
  (begin
    (mutable r #f)
    (using ([qs (qubits 2)]
            [p (qubit)])
           (x (index qs 1))
           (controlled-on-bit-string x (int-as-bool-array x 2) qs p)
           (set r (m p))
           (reset-all qs)
           (reset p))
    (return r)))

(define-symbolic x (bitvector 2))
(printf "Solve: Pr(controlled-bitstring(x) = #t) = 1?\n~a\n"
        (solve (assert (= 1 (probability/v (controlled-bitstring x) #t)))))
(newline)
(printf "Verify: Pr(controlled-bitstring(x) = #t) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (controlled-bitstring x) #t)))))

