#lang rosette

(require "interpreter.rkt")

(struct qsette (ast proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc))

(define (interpret prog inputs)
  (match-define `(procedure (,_ ,args ...) ,S) prog)
  (interpret-stmt S (environment (map cons args inputs) (list) (list))))
  
(define-syntax (procedure stx)
  (syntax-case stx ()
    [(_ (id args ...) stmt)
     (syntax/loc stx
       (define id
         (let* ([ast `(procedure (id args ...) stmt)]
                [id (lambda (args ...) (interpret ast (list args ...)))])
           (qsette ast id))))]))