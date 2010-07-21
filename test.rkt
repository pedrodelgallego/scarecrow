#lang racket/base
  
(require rackunit "kernel.rkt")

;; Test Simple data types.
(check-equal? (eval #f '()) #f "Simple false atom ")
(check-equal? (eval #t '()) #t "Simple true atom")

(check-equal? (eval 1 '())       1       "Positive number")
(check-equal? (eval -123 '()) -123       "Negative number")

(check-equal? (eval "hola" '()) "hola"   "String")
