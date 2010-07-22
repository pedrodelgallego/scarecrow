#lang racket/base
  
(require rackunit "kernel.rkt")

;; Test Simple data types. 
(check-equal? (eval #f env.global) #f "Simple false atom ")
(check-equal? (eval #t env.global) #t "Simple true atom")

(check-equal? (eval 1 env.global)       1       "Positive number")
(check-equal? (eval -123 env.global) -123       "Negative number")

(check-equal? (eval "hola" env.global) "hola"   "String")

;; The If statement
(check-equal?  (eval '(if #t "hola" "adios") env.global) "hola"      "false condition in if statament")
(check-equal?  (eval '(if #f "hola" "adios") env.global) "adios"     "false condition in if statament")

;; (check-equal?  (eval '(if #t (+ 1 1) (- 1 1)) env.global) 2          "Execute form in a if statament true branch")
;; (check-equal?  (eval '(if #f (+ 1 1) (- 1 1)) env.global) 0          "Execute form in a if statament false branch")
;; (check-equal?  (eval '(if (eq? 1 1) (+ 1 1) (- 1 1)) env.global) 2   "Execute form in condition if statament")
(check-equal? '(evaluate '(if (boolean? (eq? (= (- 2 1) (+ 1 0) )#t)) (- 1 (+ 1 1)) (- 1 1))-1  )) 
;; Enviroment

(check-equal? (eval '(begin (set! x "hola") x) env.global)  "hola" "Set up a symbol")