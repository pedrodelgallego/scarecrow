#lang racket/base

;; This implementation is supposed to be metacircular in the sense of SICP

;; Replacing the define-struct
(define (make-box value)
  (cons 'box value))

(define (box-value box)
  (cdr box))

(define (set-box-value box new-value)
  (set-cdr! box new-value))

;; -------------------------------------    Define booleans
(define the-false-value #f)

;; -------------------------------------    Data Types Predicates.
(define (boolean? x )
  (or (eq? x the-false-value)
      (eq? x (not the-false-value)) ))

;; -----------------------------------   Evaluator.
;; (define (eval expr env)
;;   (match expr
;;     [`__environment__ env]
;;          
;;     [(? boolean?)                              ((Evaluator 'literal) expr)]
;;     [(? string?)                               ((Evaluator 'literal) expr)]
;;     [(? number?)                               ((Evaluator 'literal) expr)]
;;     [(? symbol?)                               ((Evaluator 'symbol)  expr env)]
;;     
;;     [`(set! ,key ,value)                       ((Evaluator 'set!)    key value)]
;;     [`(if ,ec ,et ,ef)                         ((Evaluator 'if)      ec et ef env )]     
;;     [`(let ,bindings ,body)                    ((Evaluator 'let)     bindings body env)]
;;     [`(define (,name . ,bindings) ,function)   ((Evaluator 'define)  name expr )]    
;;     [`(define ,name ,value)                    ((Evaluator 'set!)    name value)]
;;     [`(lambda ,bindings ,body)                 ((Evaluator 'lambda)  expr env)]
;;     [`(begin . ,expr)                          ((Evaluator 'begin)   expr env)]
;;     
;;     [`(,f . ,args)                             ((Evaluator 'apply-proc) (eval f env)
;;                                                                         (map ((Evaluator 'evlis) env) args)) ]    
;;     [_ error "Unknown expression type -- EVAL" expr] ))
;;

;; -----------------------------------   Evaluation Helpers

(define Evaluator
  (lambda (method)
    (case method
      [(symbol)  (lambda (expr env)     ((Env 'look-up) expr env))]
      [(set!)    (lambda (key value)    ((Env 'set!)    key value))]      
      [(define)  (lambda (name expr )   ((Env 'set!)    name (list 'closure expr)))]
      
      [(lambda)  (lambda (expr env)     (list 'closure expr env))]
      [(begin)   (lambda (expr env)     (last (map ((Evaluator 'evlis) env) expr)))]
      [(literal) (lambda (expr)          expr)]     
      [(if)      (lambda (ec et ef env) (if (eval ec env)
                                            (eval et env)
                                            (eval ef env)))]

      [(let)     (lambda (bindings body env)  (eval body ((Env 'extended-env*) env
                                                              (map car bindings)
                                                              (map ((Evaluator 'evlis) env) (map cadr bindings)))))]
                  
      [(evlis)   (lambda (env)   (lambda (exp) (eval exp env)))
] 
      [(apply-proc) (lambda (f values)   
                      (match f
                        [`(closure (lambda ,vs ,body) ,env)
                         (eval body ((Env 'extended-env*) env vs values))]
                      
                        [`(closure (define (,name . ,vs) ,body) )
                         (eval body ((Env 'extended-env*) env.global vs values))]
                        
                        [_ (f values)] ))] )))      

        
;; -----------------------------------   Environment
(define-struct box ([value #:mutable]))

(define env.global (make-immutable-hash '()))

(define Env
  (lambda(method)
    (case method
      [(set)             (lambda (env key value)  (hash-set env key value))]
      [(set!)            (lambda (key value)      (set-box-value! (hash-ref env.global key) value))]
      [(look-up)         (lambda (expr env)     (box-value (hash-ref env expr)))]
      [(extended-env*)   (lambda (env vars values)
                           (match `(,vars ,values)
                               [`((,v . ,vars) (,val . ,values))
                                ((Env 'extended-env*)  ((Env 'set) env v (make-box val))  vars values)]
                               
                               [`(() ())  env] ))] )))