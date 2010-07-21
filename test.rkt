#lang racket/base
  
(require rackunit "kernel.rkt")



;; Test Simple data types.
(check-equal? (eval #f (env.initial)) #f "Simple false atom ")
(check-equal? (eval #t (env.initial)) #t "Simple true atom")

(check-equal? (eval 1 (env.initial))       1       "Positive number")
(check-equal? (eval -123 (env.initial)) -123       "Negative number")

(check-equal? (eval "hola" (env.initial)) "hola"   "String")

;; The If statement
(check-equal?  (eval '(if #t "hola" "adios") (env.initial)) "hola"    "false condition in if statament")
(check-equal?  (eval '(if #f "hola" "adios") (env.initial)) "adios"   "false condition in if statament")
(check-equal?  (eval '(if #f (+ 1 1) (- 1 1)) (env.initial)) 2        "Execute form in a  if statament true branch")
