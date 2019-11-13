#lang rosette

(provide complex/+ complex/* complex/conjugate complex complex-real complex-imag)

(struct complex (real imag) #:transparent)

(define (to-complex c)
  (if (number? c) (complex (real-part c) (imag-part c)) c))

; TODO: decide if this is needed anywhere
;(define (from-complex c)
;  (if (number? c) c
;      (racket/+ (complex-real c) (racket/* 0+1i (complex-imag c)))))

(define (complex/conjugate c)
  (match-let ([(complex a b) c])
    (complex a (- b))))

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
