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

    ;; Set a value symbol value. 
    [`(set! ,key ,value ) (env-set! env key value)]
    
    ;; The if expression
    [`(if ,ec ,et ,ef)    (if (eval ec env)
                              (eval et env)
                              (eval ef env))]))



;; -----------------------------------   Enviroment
(define (env.empty) (hash))
(define (env.initial) (env.empty))

(define (env-set! env  key value)
  (hash-set env key value))




(provide eval env.initial)
    