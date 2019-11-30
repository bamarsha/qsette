#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (x-gate-approx b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (h q)
           (if b
               (z q))
           (t q)
           (h q)
           (set r (m q))
           (reset q))
    (return r)))

(operation (x-gate-approx-wrong b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (h q)
           (t q)
           (h q)
           (set r (m q))
           (reset q))
    (return r)))
                  
(printf "Pr(x-gate-approx(#f) = #f) = ~a\n"
        (probability/v (x-gate-approx #f) #f))
(clear-asserts!)
(printf "Pr(x-gate-approx(#t) = #t) = ~a\n"
        (probability/v (x-gate-approx #t) #t))
(clear-asserts!)
(newline)

(define-symbolic x boolean?)
(printf "Pr(x-gate-approx(x) = x) >= 0.7?\n~a\n"
        (verify (assert (<= 0.7 (probability/v (x-gate-approx x) x)))))
(newline)

(printf "Pr(x-gate-approx(x) = x) >= 0.8?\n~a\n"
        (verify (assert (<= 0.8 (probability/v (x-gate-approx x) x)))))
(newline)

(printf "Pr(x-gate-approx-wrong(x) = x) >= 0.7?\n~a\n"
        (verify (assert (<= 0.7 (probability/v (x-gate-approx-wrong x) x)))))
(newline)

(printf "Pr(x-gate-approx-wrong(x) = x) >= 0.1?\n~a\n"
        (verify (assert (<= 0.1 (probability/v (x-gate-approx-wrong x) x)))))
