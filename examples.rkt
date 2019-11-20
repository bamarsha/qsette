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

(define (example1-wrong b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (x q)
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list))))

(printf "example1(#f) = ~a\n" (example1 #f))
(printf "example1(#t) = ~a\n" (example1 #t))

(newline)

(define-symbolic x boolean?)
(printf "verify example1: ~a\n"
        (verify (assert (equal? x (example1 x)))))
(printf "verify example1-wrong: ~a\n"
        (verify (assert (equal? x (example1-wrong x)))))

(define (no-arg)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q1)
                            (set r (m q1)))
                     (return r))
                  (environment (list) (list))))

(define (many-qbits)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q1 q2 q3 q4 q5)
                            (set r (m q1)))
                     (return r))))
