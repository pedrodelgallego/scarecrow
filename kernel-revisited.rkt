#lang racket/base

(require racket/match)

;; -----------------------------------   Evaluator.
(define (eval expr env)
  (match expr
    [`__environment__ env]
         
    [(? boolean?)                              ((Evaluator 'literal) expr)]
    [(? string?)                               ((Evaluator 'literal) expr)]
    [(? number?)                               ((Evaluator 'literal) expr)]
    [(? symbol?)                               ((Evaluator 'symbol)  expr env)]
    
    [`(set! ,key ,value)                       ((Evaluator 'set!)    key value)]
    [`(if ,ec ,et ,ef)                         ((Evaluator 'if)      ec et ef env )]     
    [`(let ,bindings ,body)                    ((Evaluator 'let)     bindings body env)]
    [`(define (,name . ,bindings) ,function)   ((Evaluator 'define)  name expr )]    
    [`(define ,name ,value)                    ((Evaluator 'set!)    name value)]
    [`(lambda ,bindings ,body)                 ((Evaluator 'lambda)  expr env)]
    [`(begin . ,expr)                          ((Evaluator 'begin)   expr env)]
    
    [`(,f . ,args)                             ((Evaluator 'apply-proc) (eval f env)
                                                                        (map ((Evaluator 'evlis) env) args)) ]    
    [_ error "Unknown expression type -- EVAL" expr] ))


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

;; -----------------------------------   Primitives
(define-syntax definitial 
  (syntax-rules ()    
    [(definitial name)
     (set! env.global ((Env 'set) env.global name (make-box null))) ]
    
    [(definitial name value)
     (set! env.global ((Env 'set) env.global name (make-box value))) ]))    

(define-syntax-rule (defprimitive name value arity)
  (definitial name 
    (lambda (values) 
      (if (= arity (length values))
          (apply value values)      
          (error "Incorrect arity"
                 (list 'name values))))))

(define-syntax-rule (defpredicate  name value arity)
  (defprimitive name
    (lambda values (or (apply value values) #f))
    arity ) )

(define (last lst)
  (if (pair? lst)
      (if (pair? (cdr lst))
          (last (cdr lst))
          (car lst))
      (error "parameter should be a non empty list")))

;; -----------------------------------
(define (eval-program  program)
  (evaluate (cons 'begin program)))
  
(define (evaluate program)
  (if (list? program)
      (map define->bindings program)
      '())
  (eval program env.global))

(define (define->bindings define)  
  (match define
    [`(define (,name . ,bindings ) ,body)   (definitial name)]
    [`(define ,name ,value)  (definitial name)]
    [else    '()]))

(definitial #t #t)
(definitial #f #t)
(definitial 'nil '())

(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defpredicate 'pair? pair? 1)
(defpredicate 'boolean? boolean? 1)
(defpredicate 'symbol? symbol? 1)
(defpredicate 'procedure? procedure? 1)
(defprimitive 'eq? eq? 2)
(defpredicate 'eq? eq? 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defpredicate '= = 2)
(defpredicate '> > 2)
(defpredicate '< < 2)    
(defprimitive '* * 2)
(defpredicate '<= <= 2)
(defpredicate '>= >= 2)
(defprimitive 'remainder remainder 2)
(defprimitive 'display display 1)

;; (module-path-index-resolve (car (identifier-binding  #'define-syntax-rule)))
(provide evaluate env.global eval-program)