#lang rosette

; Should be a better way of doing this
(require (only-in racket (* racket/*))
         (only-in racket (+ racket/+))
         (only-in racket (> racket/>))
         (only-in racket (sqrt racket/sqrt))
         (only-in racket (conjugate racket/conjugate)))

(provide + * > conjugate to-complex from-complex)

(struct complex (real imag) #:transparent)

; Could have these accept racket and our numbers and just check type
; + Would not have to use to-complex in interpreter.rkt
; + Functions could accept either
; - Might hide some bugs to do with wrong types
(define (to-complex c)
  (if (not (number? c)) c
      (complex (real-part c) (imag-part c))))

(define (from-complex c)
  (if (number? c) c
      (racket/+ (complex-real c) (racket/* 0+1i (complex-imag c)))))

; This seems like it should be done with define-lift but maybe our structs arre getting in the way
(define (* . cs)
  (to-complex (apply racket/* (map from-complex cs))))

(define (+ . cs)
  (to-complex (apply racket/+ (map from-complex cs))))

; Needed for the comparison
(define (> c1 c2)
  (racket/> (from-complex c1) (from-complex c2)))

(define (conjugate c)
  (to-complex (racket/conjugate (from-complex c))))

(define (sqrt c)
  (to-complex (racket/sqrt (from-complex c))))

; TODO: delete these as they are no longer needed
(define (add c1 c2)
  (complex (+ (complex-real c1)
              (complex-real c2))
           (+ (complex-imag c1)
              (complex-imag c2))))

(define (mul c1 c2)
  (match-let*
      ([(complex a b) c1]
       [(complex c d) c2])
    (complex (- (* a c) (* b d))
             (+ (* a d) (* b c)))))
