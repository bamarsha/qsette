#lang rosette

(require "../qsette.rkt" "../probability.rkt")

(operation (negate b)
           (begin
             (if b (set b #f) (set b #t))
             (return b)))

(operation (x-gate b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (if b
               (x q))
           (set r (m q))
           (reset q))
    (return r)))

(operation (x-gate-neg b)
  (begin
    (mutable r #f)
    (using ([q (qubit)])
           (if (,negate b)
               (x q))
           (set r (m q))
           (reset q))
    (return r)))

(x-gate #f)
(x-gate #t)
(x-gate-neg #f)
(x-gate-neg #t)