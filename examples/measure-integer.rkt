#lang rosette

(require "../interpreter.rkt"
         "../probability.rkt")

(define (measure-integer n)
  (interpret-stmt '(begin
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
                     (return r))
                  (environment (list `(n . ,n)) (list) (list))))

(define-symbolic n (bitvector 3))
(printf "Pr(measure-integer(n) = n) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (measure-integer n) n)))))
(newline)

(define (measure-integer-wrong n)
  (interpret-stmt '(begin
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
                     (return r))
                  (environment (list `(n . ,n)) (list) (list))))

(printf "Pr(measure-integer-wrong(n) = n) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (measure-integer-wrong n) n)))))
(newline)

(define (measure-integer-loop n)
  (interpret-stmt '(begin
                     (mutable r 0)
                     (mutable bs (int-as-bool-array n 3))
                     (using ([qs (qubits 3)])
                            (for (i 3)
                              (if (index bs i)
                                  (x (index qs i))))
                            (set r (measure-integer qs))
                            (reset-all qs))
                     (return r))
                  (environment (list `(n . ,n)) (list) (list))))

(printf "Pr(measure-integer(n) = n) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (measure-integer-loop n) n)))))