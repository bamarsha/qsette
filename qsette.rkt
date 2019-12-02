#lang rosette

(require "interpreter.rkt")

(provide (all-defined-out))
         
(struct qsette (ast proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc))

(define (interpret prog inputs state)
  (match-define `(operation (,_ ,args ...) ,S) prog)
  ; TODO: This is a default void return if no return statement is provided
  (match (interpret-stmt S (environment (map cons args inputs) state (list)))
    [(cons ret env) (cons ret env)]
    [env (cons (void) env)]))
  
  
(define-syntax (operation stx)
  (syntax-case stx ()
    [(_ (id args ...) stmt)
     (syntax/loc stx
       (define id
         (let* ([ast `(operation (id args ...) stmt)]
                [id (lambda (args ... (state (list))) (interpret ast (list args ...) state))])
           (qsette ast id))))]))