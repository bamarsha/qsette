#lang rosette

(define example1a
  '(using (q)
          (x q)))

(define example1b
  '(using (q)
          (x q)
          (x q)
          (x q)))

(define example2a
  '(using (q)
          (x q)))

(define example2b
  '(using (q)
          (h q)
          (z q)
          (h q)))

(define example3a
  '(using (q1 q2)
          (swap q1 q2)))

(define example3b
  '(using (q1 q2)
          (cnot q1 q2)
          (cnot q2 q1)
          (cnot q1 q2)))

(define (interpret program)
  (match program
    [`(using ,x ,y ...) (printf "qubits: ~a, body: ~a" x y)]))

(interpret example1b)
