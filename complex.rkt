#lang rosette

; Should be a better way of doing this
;(require (only-in racket (* racket/*))
;         (only-in racket (+ racket/+))
;         (only-in racket (> racket/>))
;         (only-in racket (sqrt racket/sqrt))
;         (only-in racket (conjugate racket/conjugate)))


;(provide + * > conjugate to-complex from-complex)

(provide complex/+ complex/* complex/conjugate complex complex-real complex-imag)

(struct complex (real imag) #:transparent)

(define (to-complex c)
  (if (not (number? c)) c
      (complex (real-part c) (imag-part c))))
;
;(define (from-complex c)
;  (if (number? c) c
;      (racket/+ (complex-real c) (racket/* 0+1i (complex-imag c)))))

; This seems like it should be done with define-lift but maybe our structs arre getting in the way
;(define (* . cs)
;  (to-complex (apply racket/* (map from-complex cs))))
;
;(define (+ . cs)
;  (to-complex (apply racket/+ (map from-complex cs))))
;
;; Needed for the comparison
;(define (> c1 c2)
;  (racket/> (from-complex c1) (from-complex c2)))
;
(define (complex/conjugate c)
  (match-let ([(complex a b) c])
    (complex a (- b))))
;
;(define (sqrt c)
;  (to-complex (racket/sqrt (from-complex c))))

(define (complex/+ . cs)
  (foldl add 0 cs))

(define (complex/* . cs)
  (foldl mul 1 cs))
  
(define (add c1 c2)
  (let ([c1 (to-complex c1)]
        [c2 (to-complex c2)])
    (complex (+ (complex-real c1)
                (complex-real c2))
             (+ (complex-imag c1)
                (complex-imag c2)))))

(define (mul c1 c2)
  (let ([c1 (to-complex c1)]
        [c2 (to-complex c2)])
    (match-let*
        ([(complex a b) c1]
         [(complex c d) c2])
      (complex (- (* a c) (* b d))
               (+ (* a d) (* b c))))))
