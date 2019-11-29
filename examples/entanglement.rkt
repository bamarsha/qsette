#lang rosette

(require "../interpreter.rkt"
         "../probability.rkt")

(define (entanglement b)
  (interpret-stmt '(begin
                     (mutable r1 #f)
                     (mutable r2 #f)
                     (using (q1 q2)
                            (h q1)
                            (cnot q1 q2)
                            (if b
                                (x q1))
                            (set r1 (m q1))
                            (set r2 (m q2))
                            (reset q1)
                            (reset q2))
                     (return (= r1 r2)))
                  (environment (list `(b . ,b)) (list) (list))))

(define (entanglement-wrong b)
  (interpret-stmt '(begin
                     (mutable r1 #f)
                     (mutable r2 #f)
                     (using (q1 q2)
                            (h q1)
                            ; (cnot q1 q2)
                            (if b
                                (x q1))
                            (set r1 (m q1))
                            (set r2 (m q2))
                            (reset q1)
                            (reset q2))
                     (return (= r1 r2)))
                  (environment (list `(b . ,b)) (list) (list))))

(define-symbolic x boolean?)
(printf "Pr(entanglement(x) = !x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (entanglement x) (not x))))))
(newline)

; entanglement-wrong doesn't actually entangle the qubits - so there's a 50%
; chance of getting the wrong answer.
(define probability-wrong (probability/v (entanglement-wrong x) (not x)))
(define cex (verify (assert (= 1 probability-wrong))))
(printf "Pr(entanglement-wrong(x) = !x) = 1?\n~a\n~a\n"
        cex
        (evaluate probability-wrong cex))
(newline)

(printf "Pr(entanglement-wrong(x) = !x) >= 0.5?\n~a\n"
        (verify (assert (<= 0.5 probability-wrong))))
