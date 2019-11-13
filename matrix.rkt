#lang rosette

(provide transpose
         matrix-conjugate
         adjoint
         scale
         matrix-multiply
         inner-product
         vector-magnitude-sq
         kronecker-product
         list->column-vector
         column-vector->list
         matrix-map)

;(require rosette/lib/lift)
;(require (only-in racket [conjugate racket/conjugate] number?))
(require "complex.rkt")

; TODO: This doesn't work? :(
;(define-lift conjugate [(number?) conjugate])

(define (transpose a)
  (apply map list a))

(define (matrix-conjugate a)
  (matrix-map conjugate a))

(define (adjoint a)
  (matrix-conjugate (transpose a)))

(define (scale c a)
  (matrix-map (lambda (x) (* c x)) a))

(define (matrix-multiply a b)
  (for/list ([a-row a])
    (for/list ([b-row (transpose b)])
      (apply + (map * a-row b-row)))))

(define (inner-product a b)
  (matrix-multiply (adjoint a) b))

(define (vector-magnitude-sq v)
  (match (matrix-map from-complex (inner-product v v))
    [`((,x)) x]))

(define (kronecker-product a b)
  (let ([matrix-flatten (lambda (m) (apply map append m))])
    (apply append (map matrix-flatten
                       (matrix-map (lambda (x) (scale x b)) a)))))

(define (list->column-vector l)
  (map list l))

(define (column-vector->list v)
  (map (match-lambda [`(,x) x]) v))

(define (matrix-map f a)
  (map (lambda (row) (map f row)) a))


