#lang rosette

(require "interpreter.rkt")

(provide (all-defined-out))
         
(struct qsette (ast proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc))

(define (interpret prog inputs)
  (match-define `(operation (,_ ,args ...) ,S) prog)
  (interpret-stmt S (environment (map cons args inputs) (list) (list))))
  
(define-syntax (operation stx)
  (syntax-case stx ()
    [(_ (id args ...) stmt)
     (syntax/loc stx
       (define id
         (let* ([ast `(operation (id args ...) stmt)]
                [id (lambda (args ...) (interpret ast (list args ...)))])
           (qsette ast id))))]))