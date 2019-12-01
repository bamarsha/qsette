#lang rosette

(require "qsette.rkt")

(operation (scope-test)
           (begin
             (mutable a #f)
             (if #t (begin
                      (set a #t)
                      (mutable b #f)))
             (begin
               (mutable c #t))
             (return a)
             (mutable d #f)))


(operation (simple1-test)
           (mutable a #t))

;(simple1-test)
;(scope-test)

(define (fun x)
  ((let-values ([(a b) (values x)])
     b)))

(define (dub x)
  (values x x))