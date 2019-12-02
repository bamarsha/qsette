#lang rosette

(require "../probability.rkt"
         "../qsette.rkt")

(operation (measure-integer n)
  (begin
    (mutable r 0)
    (mutable bs (int-as-bool-array n 3))
    (using ([qs (qubits 3)])
           (if (index bs 0)
               (x (index qs 0)))
           (if (index bs 1)
               (x (index qs 1)))
           (if (index bs 2)
               (x (index qs 2)))
           (set r (measure-integer qs))
           (reset-all qs))
    (return r)))

(define-symbolic n (bitvector 3))
(printf "Pr(measure-integer(n) = n) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (measure-integer n) n)))))
(newline)

(operation (measure-integer-wrong n)
  (begin
    (mutable r 0)
    (mutable bs (int-as-bool-array n 3))
    (using ([qs (qubits 3)])
           (if (index bs 0)
               (x (index qs 0)))
           (if (index bs 1)
               (x (index qs 1)))
           (if (index bs 2)
               (x (index qs 1)))
           (set r (measure-integer qs))
           (reset-all qs))
    (return r)))

(printf "Pr(measure-integer-wrong(n) = n) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (measure-integer-wrong n) n)))))