#lang rosette

(require "qsette.rkt")

(operation (scope-test)
           (begin
             (mutable a #f)
             (if #t (begin
                      (set a #t)
                      (mutable b #f)))
             (begin
               (begin
                 (mutable c #t)))
             (using ([q1 (qubit)])
                    (mutable e #t))
             (return a)
             (mutable d #f)))

(scope-test)