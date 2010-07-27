#lang racket/base

(require racket/match)

;; -------------------------------------    Define booleans
(define the-false-value #f)

;; -------------------------------------    Data Types Predicates.
(define (boolean? x )
  (or (eq? x the-false-value)
      (eq? x (not the-false-value)) ))

;; -----------------------------------   Evaluator.
(define (eval expr env)
  (match expr         
    [(? boolean?) expr]
    [(? string?)  expr]
    [(? number?)  expr]
    [(? symbol?)  (env.look-up expr env)]
    
    [`(set! ,key ,value ) (env.set!  key value)]
    
    [`(if ,ec ,et ,ef) (if (eval ec env)
                           (eval et env)
                           (eval ef env))]

    [`(let ,bindings ,body )
        (eval body (extended-env* env
                                  (map car bindings)
                                  (map (evlis env) (map cadr bindings))))]

    [`(define (,name . ,bindings) ,function )     
        (begin (definitial name (list 'closure expr))
               (list 'closure expr))]
    
    [`(define ,name ,value)
      (env.set! name value)]

    [`(lambda ,bindings ,body) 
         (list 'closure expr env)]

    [`(begin . ,expr)
          (last (map (evlis env) expr)) ]
       
    [`(,f . ,args)
        (apply-proc (eval f env)
                   (map (evlis env) args)) ]
    
    [_ error "Unknown expression type -- EVAL" expr] ))

; applies a procedure to arguments:
(define (apply-proc f values)   
  (match f
    [`(closure (lambda ,vs ,body) ,env)
     (eval body (extended-env* env vs values))]
    
    [_ (f values)]))

(define (evlis  env) 
  (lambda (exp) (eval exp env)))

;; -----------------------------------   Environment
(define-struct cell ([value #:mutable]))
(define env.global (make-immutable-hash '()))

(define (env.set   env key value)  (hash-set env key value))
(define (env.set!      key value)
  (set-cell-value! (hash-ref env.global key) value))

(define (env.look-up expr env )
  (cell-value (hash-ref env expr)))

(define (extended-env* env vars values)
  (match `(,vars ,values)
     [`((,v . ,vars) (,val . ,values))
          (extended-env*  (env.set env v (make-cell val))  vars values)]
         
     [`(() ())  env] ))

;; -----------------------------------   Primitives
(define-syntax definitial 
  (syntax-rules ()    
    [(definitial name)
     (set! env.global (env.set env.global name (make-cell null))) ]
    
    [(definitial name value)
     (set! env.global (env.set env.global name (make-cell value))) ]))    

(define-syntax-rule (defprimitive name value arity)
  (definitial name 
    (lambda (values) 
      (if (= arity (length values))
          (apply value values)      
          (error "Incorrect arity"
                 (list 'name values))))))

(define-syntax-rule (defpredicate  name value arity)
  (defprimitive name
    (lambda values (or (apply value values) the-false-value))
    arity ) )

(define (last lst)
  (if (pair? lst)
      (if (pair? (cdr lst))
          (last (cdr lst))
          (car lst))
      (error "parameter should be a non empty list")))

;; ----------------------------------- 
(define (evaluate program)
  (if (list? program)
      (map define->bindings program)
      '())
  (eval program env.global))    

(define (puts text) (display text)(newline))

(define (define->bindings define)  
  (match define
    [`(define (,name . ,bindings ) ,body)   (definitial name)]
    [`(define ,name ,value)  (definitial name)]
    [else    '()]))


(definitial #t #t)
(definitial #f the-false-value)
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
(provide eval env.global evaluate)