#lang rosette

(require "../interpreter.rkt"
         "../probability.rkt")

(define (x-gate b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (if b
                                (x q))
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

(define (x-gate-wrong b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (x q)
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

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
