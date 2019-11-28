#lang rosette

(require "interpreter.rkt"
         "probability.rkt")

(define (example1 b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (if b
                                (x q))
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

(define (example1-wrong b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (x q)
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

(printf "Pr(example1(#f) = #f) = ~a\n" (probability/v (example1 #f) #f))
(clear-asserts!)
(printf "Pr(example1(#t) = #t) = ~a\n" (probability/v (example1 #t) #t))
(clear-asserts!)
(newline)

(define-symbolic x boolean?)
(printf "Pr(example1(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (example1 x) x)))))
(newline)

(printf "Pr(example1-wrong(x) = x) = 1?\n~a\n"
        (verify (assert (= 1 (probability/v (example1-wrong x) x)))))
(newline)

(define (example2 b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (h q)
                            (if b
                                (z q))
                            (t q)
                            (h q)
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

(define (example2-wrong b)
  (interpret-stmt '(begin
                     (mutable r #f)
                     (using (q)
                            (h q)
                            (t q)
                            (h q)
                            (set r (m q))
                            (reset q))
                     (return r))
                  (environment (list `(b . ,b)) (list) (list))))

(printf "Pr(example2(#f) = #f) = ~a\n" (probability/v (example2 #f) #f))
(clear-asserts!)
(printf "Pr(example2(#t) = #t) = ~a\n" (probability/v (example2 #t) #t))
(clear-asserts!)
(newline)

(printf "Pr(example2(x) = x) >= 0.7?\n~a\n"
        (verify (assert (<= 0.7 (probability/v (example2 x) x)))))
(newline)

(printf "Pr(example2(x) = x) >= 0.8?\n~a\n"
        (verify (assert (<= 0.8 (probability/v (example2 x) x)))))
(newline)

(printf "Pr(example2-wrong(x) = x) >= 0.7?\n~a\n"
        (verify (assert (<= 0.7 (probability/v (example2-wrong x) x)))))
(newline)

(printf "Pr(example2-wrong(x) = x) >= 0.1?\n~a\n"
        (verify (assert (<= 0.1 (probability/v (example2-wrong x) x)))))

;; (define (no-arg)
;;   (interpret-stmt '(begin
;;                      (mutable r #f)
;;                      (using (q1)
;;                             (set r (m q1)))
;;                      (return r))
;;                   (environment (list) (list) (list))))

;; (define (many-qbits)
;;   (interpret-stmt '(begin
;;                      (mutable r #f)
;;                      (using (q1 q2 q3 q4 q5)
;;                             (set r (m q1)))
;;                      (return r))))
