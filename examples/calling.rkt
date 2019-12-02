#lang rosette

(require "../qsette.rkt" "../probability.rkt")

(operation (negate c)
           (begin
             (if c (set c #f) (set c #t))
             (return c)))

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

(operation (test b)
           (return (,negate b)))

(test #f)
(test #t)

(operation (test2 b)
           (begin
             (mutable r #f)
             (if b
                 (set r #t)
                 (set r (,negate b)))
             (return r)))

(test2 #t)
(test2 #f)

(operation (test3 b)
           (begin
             (mutable r #f)
             (using ([q (qubit)])
                    (set r (,negate b)))
             (return r)))

(operation (test4 b)
           (begin
             (mutable r #f)
             (using ([q (qubit)])
                    (set r b))
             (return r)))

(test4 #t)
(test4 #f)

(test3 #t)
(test3 #f)

(x-gate-neg #f)
(x-gate-neg #t)