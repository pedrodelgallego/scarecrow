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

    [`(lambda ,vars , body)
       (list 'fake-lambda expr env)]
    
    [(list 'begin expr ...)
        (last (map (evlis env) expr))]
    
    [`(,f . ,args)
       (apply-proc (eval f env)
                   (map (evlis env) args)) ]  ))

; applies a procedure to arguments:
(define (apply-proc f values)
  (match f
         
    [`(fake-lambda (lambda ,vs ,body) ,env)  (eval body (extended-env* env vs values))]

    ;;  [(list _ vs body env)  (eval body (extended-env* env vs values))]
    
    [_ (f values)]))


; extends an environment with several bindings:
(define (extended-env* env vars values)
  (match `(,vars ,values)
     [`((,v . ,vars) (,val . ,values))
          (extended-env*  (env.set (clone-env env) v val)  vars values)]
         
     [`(() ())  env] ))

; a handy wrapper for Currying eval:
(define (evlis  env) 
  (lambda (exp) (eval exp env)))

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

(define (last lst)
  (if (pair? lst)
      (if (pair? (cdr lst))
          (last (cdr lst))
          (car lst))
      (error "parameter should be a non empty list")))

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