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

(define (simple-measure)
  (interpret-stmt '(using (q) (return (m q)))
                  (environment (list) (list))))

(simple-measure)

;
(example1 #f)
(example1 #t)
;
(define-symbolic x boolean?)
(example1 x)
(verify (assert (= x (example1 x))))
