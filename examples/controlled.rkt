#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (controlled b1 b2)
  (begin
    (mutable r #f)
    (using ([qs (qubits 2)]
            [p (qubit)])
           (if b1
               (x (index qs 0)))
           (if b2
               (x (index qs 1)))
           (controlled x qs p)
           (set r (m p))
           (reset-all qs)
           (reset p))
    (return r)))
                  
(printf "Pr(controlled(#f, #t) = #t) = ~a\n"
        (probability/v (controlled #f #t) #t))
(clear-asserts!)
(newline)

(define-symbolic x boolean?)
(define-symbolic y boolean?)
(printf "Pr(controlled(x, y) = #t) > 0?\n~a\n"
        (solve (assert (< 0 (probability/v (controlled x y) #t)))))
(newline)
