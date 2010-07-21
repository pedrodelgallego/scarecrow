#lang racket/base

(require racket/match)

;; -------------------------------------    Define booleans

(define (the-false-value) #f)
(define (the-true-value) (not (the-false-value)))

;; -------------------------------------    Data Types Predicates.

(define (boolean? x )
  (or (eq? x (the-false-value)) (eq? x (the-true-value))))

;; -----------------------------------   Evaluator.
(define (eval expr env)
  (match expr
    ;; Evaluate the basic data types.      
    [(? boolean?) expr]
    [(? string?)  expr]
    [(? number?)  expr]
    [(? symbol?)  (env.look-up expr env)]
    [`(set! ,key ,value ) (env.set! env key value)]

    [`(if ,ec ,et ,ef)    (if (eval ec env)
                              (eval et env)
                              (eval ef env))]
    
    [`(begin ,e1 ,e2)     (begin (eval e1 env)
                                 (eval e2 env))]))



;; -----------------------------------   Enviroment
(define env.empty (make-weak-hash))
(define env.initial env.empty)
(define env.global env.initial)
 

(define (env.set! env  key value)
  (hash-set! env key value))

(define (env.look-up expr env )
  (hash-ref env expr))


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


(definitial t #t)
(definitial f the-false-value)
(definitial nil '())

(definitial x)
(definitial y)
(definitial z)
(definitial a)
(definitial b)
(definitial c)
(definitial k)
(definitial foo)
(definitial bar)
(definitial hux)
(definitial fib)
(definitial fact)
(definitial visit)
(definitial length)
(definitial primes)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defpredicate pair? pair? 1)
(defpredicate symbol? symbol? 1)
(defprimitive eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defpredicate eq? eq? 2)           ; cf. exercice \ref{exer-predicate}
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defpredicate = = 2)
(defpredicate > > 2)
(defpredicate < < 2)               ; cf. exercice \ref{exer-predicate}\endlisp
(defprimitive * * 2)
(defpredicate <= <= 2)
(defpredicate >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

(provide eval env.global env.initial)