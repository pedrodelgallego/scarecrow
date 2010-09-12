#lang racket/base

(require racket/match)

;; -----------------------------------   Evaluator.
(define (eval expr env)
  ;; (display "evaluating ") (display expr) (newline)
  (match expr
    [`__environment__ env]
         
    [(? boolean?)                              (evaluator 'literal expr)]
    [(? string?)                               (evaluator 'literal expr)]
    [(? number?)                               (evaluator 'literal expr)]    
    [(? symbol?)                               (evaluator 'symbol  env expr)]
                                                
    [`(set! ,key ,value)                       (evaluator 'set!    key value)]
    
    [`(define (,name . ,bindings) ,function)   (evaluator 'define  name expr )]
    [`(define ,name ,value)                    (evaluator 'set!    name value)]
    
    [`(if ,ec ,et ,ef)                         (evaluator 'if      env ec et ef)]
    [`(let ,bindings ,body)                    (evaluator 'let     env bindings body)]
    [`(lambda ,bindings ,body)                 (evaluator 'lambda  env expr )]
    [`(begin . ,expr)                          (evaluator 'begin   env expr)]
    
    [`(,f . ,args)                             (evaluator 'apply-proc  env
                                                                       (eval f env)
                                                                       (map ((evaluator 'evlis) env) args)) ]
    [_ error "Unknown expression type -- EVAL" expr] ))


;; -----------------------------------   Evaluation

(define (Evaluator)
  (define (evlis env) (lambda (exp) (eval exp env)))
  
  (define (*literal* expr)          expr)
  (define (*symbol*  env expr)      ((Env 'look-up) expr env) )
  (define (*set!*    key value)     ((Env 'set!)    key value))
  (define (*define*  name expr)     ((Env 'set!)    name (list 'closure expr)))
    
  (define (*if*      env ec et ef)  (if (eval ec env) (eval et env) (eval ef env)))
  (define (*lambda*  env expr)      (list 'closure expr env))
  (define (*begin*   env expr)      (last (map ((evaluator 'evlis) env) expr)))
  (define (*let* env bindings body) (eval body ((Env 'extended-env*) env
                                                              (map car bindings)
                                                              (map ((evaluator 'evlis) env) (map cadr bindings)))))
  
  (define (apply-proc env f values)
    (match f
       [`(closure (lambda ,vs ,body) ,env)
        (eval body ((Env 'extended-env*) env vs values))]
           
       [`(closure (define (,name . ,vs) ,body) )
        (eval body ((Env 'extended-env*) env.global vs values))]
       
       [_ (f values)] ))
  
  (lambda (method . args)
    (case method
      [(literal) (apply *literal*  args)]      
      [(symbol)  (apply *symbol*   args)]
      [(set!)    (apply *set!*     args)]
      [(define)  (apply *define*   args)]
      [(if)      (apply *if*       args)]
      [(lambda)  (apply *lambda*   args)]
      [(begin)   (apply *begin*    args)]
      [(let)     (apply *let*      args)]
      
      [(evlis)   evlis] 
      [(apply-proc) (apply apply-proc args)   ] )))

(define evaluator (Evaluator))
        
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

(define (define->bindings definitions)
  (match definitions
    [`(define (,name . ,bindings ) ,body)   (definitial name)]
    [`(define ,name ,value)                 (definitial name)]
    [_    '()]))

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