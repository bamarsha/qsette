#lang rosette

(require "interpreter.rkt")

(provide (all-defined-out))
         
(struct qsette (ast proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc))

(define (interpret prog inputs env)
  (match-define `(operation (,_ ,args ...) ,S) prog)
  ; TODO: This is a default void return if no return statement is provided
  (match (interpret-stmt S (struct-copy environment env (variables (map cons args inputs))))
    [(cons ret env) (cons ret env)]
    [env (cons (void) env)]))
  
  
(define-syntax (operation stx)
  (syntax-case stx ()
    [(_ (id args ...) stmt)
     (syntax/loc stx
       (define id
         (let* ([ast `(operation (id args ...) stmt)]
                [id (lambda (args ... (env (environment (list) (list) (list)))) (interpret ast (list args ...) env))])
           (qsette ast id))))]))