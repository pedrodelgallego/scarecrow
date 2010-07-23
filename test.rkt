#lang racket/base
  
(require rackunit "kernel.rkt")

(define (test description test-case result)  
  (display ".")
  (check-equal? (evaluate test-case) result description))

;; Test Simple data types. 
(test "the false value" #f #f)
(test "The true  value " #t #t)

(test "Positive numbers " 1 1)
(test "Negative numbers " -123 -123)

(test "String " "hola" "hola")

;; The If statement
(test "a true condition in if statament" '(if #t "hola" "adios") "hola")
(test "a false condition in if statament" '(if #f "hola" "adios") "adios")

(test "Execute form in a if statament true branch"
      '(if #t (+ 1 1) (- 1 1))
      2)

(test "Execute form in a if statament false branch"
      '(if #f (+ 1 1) (- 1 1))
      0 )

(test "Execute form in condition if statament"
      '(if (eq? 1 1) (+ 1 1) (- 1 1))
      2)   

(test "Check nested forms in a if statement" 
       '(if (boolean? (eq? (= (- 2 1) (+ 1 0) )#t)) (- 1 (+ 1 1)) (- 1 1)) 
       -1 )

;; Begin
(test "Set a variable from a 'begin scope"
      '(begin (set! x "hola") x)
      "hola")

(test "Set a variable from a 'lambda scope"
      '(begin (lambda (y) (set! x "hola")) x)
        "hola") 
      
;; Lambda 
(test  "Call a Lambda Function"
       '((lambda (y) y) 1)
       1 )

(test "Execute a lambda function"      
      '((lambda (y) (+ 1 y)) 1) 
      2 )

(test "Lambda do not polute the "
     '(begin (set! x "outter x") ((lambda(x) x) "inner x") x)
     "outter x")

(test "Nest begin clause"
      '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1)) (+ (+ 1 2) 100))
      103 )

(test "Nested begin clause"
      '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1))  1)
      1 )            

(test  "Nest begin clause"
       '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1)) (+ (+ 1 2) 100)) 
       103)

