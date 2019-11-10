#lang rosette

(define (transpose a)
  (apply map list a))

(define (conjugate a)
  (matrix-map (lambda (z) (make-rectangular (real-part z) (- (imag-part z))))
              a))

(define (adjoint a)
  (conjugate (transpose a)))

(define (scale c a)
  (matrix-map (lambda (x) (* c x)) a))

(define (matrix-multiply a b)
  (for/list ([a-row a])
    (for/list ([b-row (transpose b)])
      (apply + (map * a-row b-row)))))

(define (kronecker-product a b)
  (let ([matrix-flatten (lambda (m) (apply map append m))])
    (apply append (map matrix-flatten
                       (matrix-map (lambda (x) (scale x b)) a)))))

(define (list->column-vector l)
  (map list l))

(define (matrix-map f a)
  (map (lambda (row) (map f row)) a))
