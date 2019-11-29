#lang rosette

(require "../qsette.rkt"
         "../probability.rkt")

(operation (x-gate b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (if b
               (x q))
           (set r (m q))
           (reset q))
    (return r)))
                  

(operation (x-gate-wrong b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (x q)
           (set r (m q))
           (reset q))
    (return r)))

(printf "Pr(x-gate(#f) = #f) = ~a\n" (probability/v (x-gate #f) #f))
(clear-asserts!)
(printf "Pr(x-gate(#t) = #t) = ~a\n" (probability/v (x-gate #t) #t))
(clear-asserts!)
(newline)

(define-symbolic x boolean?)
(printf "Pr(x-gate(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (x-gate x) x)))))
(newline)

(printf "Pr(x-gate-wrong(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (x-gate-wrong x) x)))))

