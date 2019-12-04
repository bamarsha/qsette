#lang rosette

(require "interpreter.rkt")

(define (example1 b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (if b
                                (x q))
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list))))


(example1 #f)
(example1 #t)

(define-symbolic x boolean?)
(example1 x)
(verify (assert (= x (example1 x))))

(define (no-arg)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q1)
                            (set r (m q1)))
                     (return r))
                  (environment (list) (list))))

(no-arg)

(define (many-qbits)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q1 q2 q3 q4 q5)
                            (set r (m q1)))
                     (return r))))
