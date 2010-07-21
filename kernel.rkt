#lang racket

(require racket/match)


;; Evaluator.

(define (eval expr env)
  (match expr
    ;; Evaluate the basic data types.      
    [(? boolean?) expr]
    [(? string?)  expr]
    [(? number?)  expr] ))
    