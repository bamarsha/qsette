#lang rosette

(require "../qsette.rkt")

(operation (for-test)
           (begin
             (mutable r #f)
             (for (i 3)
               (set r #t))
             (return r)))

(for-test)

(operation (for-test2)
           (begin
             (mutable r #f)
             (mutable bs (int-as-bool-array ,(bv 4 3) 3))
             (for (i 1 2)
               (set r (index bs i)))
             (return r)))

(for-test2)