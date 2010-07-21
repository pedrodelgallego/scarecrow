#lang racket

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
    [(? number?)  expr] ))

;; -----------------------------------   Enviroment
(define env.init '())

(provide eval)
    