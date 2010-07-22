#lang racket/base

(require racket/match)

;; -------------------------------------    Define booleans
(define the-false-value #f)

;; -------------------------------------    Data Types Predicates.
(define (boolean? x )
  (or (eq? x the-false-value) (eq? x (not the-false-value)) ))

;; -----------------------------------   Evaluator.
(define (eval expr env)
  (match expr
         ;; Evaluate the basic data types.
    [(? boolean?) expr]         
    [(? string?)  expr]
    [(? number?)  expr]
    [(? symbol?)  (env.look-up expr env)]
    [`(set! ,key ,value ) (env.set! key value)]

    [`(if ,ec ,et ,ef) (if (eval ec env)
                           (eval et env)
                           (eval ef env))] 
    
    [`(begin ,e1 )   (begin (display e1) (display (length e1)) (eprogn e1 env))]
        
    [`(lambda ,args ,body) (make-function args  body env)]
        
    [`(,f . ,args)     ((eval f env) 
                         (map (evlis env) args))] ))


;;; ===================================================================

(define (make-function variables body env)
  (lambda (values)
     (eprogn body (extended-env* env variables values)) ) )

; extends an environment with several bindings:
(define (extended-env* env vars values)
  (match `(,vars ,values)
     [`((,v . ,vars) (,val . ,values))
          (extended-env*  (env.set (clone-env env) v val)  vars values)]
         
     [`(() ())  env] ))


(define (eprogn expr env)
  (display (length expr))
  (if (not (null? expr))
      (begin (eval (car expr) env)
             (eprogn (cdr expr) env) )
      '()))


;; -----------------------------------   Enviroment

(define env.empty (make-weak-hash))
(define env.global env.empty)

(define (env.set! key value)
  (hash-set! env.global key value)
  env.global)

(define (env.set env  key value)
  (hash-set! env key value)
  env)

(define (env.look-up expr env )
  (hash-ref env expr))

; a handy wrapper for Currying eval:
(define (evlis  env) 
  (lambda (exp) (eval exp env)))


(define (clone-env x)
  (make-weak-hash (hash-map x (lambda (x  y) (cons x y)))))

;; -----------------------------------   Primitives
(define-syntax definitial 
  (syntax-rules ()    
    [(definitial name)
      (hash-set! env.global name null) ]    
    [(definitial name value)
     (hash-set! env.global name value) ] ) )

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

;; ----------------------------------- 

(define (evaluate program)  
  (eval program env.global))

(definitial #t #t)
(definitial #f the-false-value)
(definitial 'nil '())

(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defpredicate 'pair? pair? 1)
(defpredicate 'boolean? boolean? 1)
(defpredicate 'symbol? symbol? 1)
(defprimitive 'eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defpredicate 'eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defpredicate '= = 2)
(defpredicate '> > 2)
(defpredicate '< < 2)               ; cf. exercice \ref{exer-predicate}\endlisp
(defprimitive '* * 2)
(defpredicate '<= <= 2)
(defpredicate '>= >= 2)
(defprimitive 'remainder remainder 2)
(defprimitive 'display display 1)

(provide eval env.global evaluate)