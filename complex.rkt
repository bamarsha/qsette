#lang rosette

(provide complex/+
         complex/*
         complex/conjugate
         complex
         complex-real
         complex-imag)

(struct complex (real imag) #:transparent)

(define (complex/+ . cs)
  (let ([add (lambda (x y)
               (complex (+ (complex-real x) (complex-real y))
                        (+ (complex-imag x) (complex-imag y))))])
    (foldl add (car cs) (cdr cs))))

(define (complex/* . cs)
  (let ([multiply (lambda (x y)
                    (match-let* ([(complex a b) x]
                                 [(complex c d) y])
                      (complex (- (* a c) (* b d))
                               (+ (* a d) (* b c)))))])
    (foldl multiply (car cs) (cdr cs))))

(define (complex/conjugate c)
  (match-let ([(complex a b) c])
    (complex a (- b))))
