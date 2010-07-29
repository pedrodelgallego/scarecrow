#lang racket/base
  
(require rackunit "kernel.rkt")

(define (test description test-case result)  
  (display ".")
  (check-equal? (evaluate test-case) result description))

;; Test Simple data types. 
(test "the false value" #f #f)
(test "The true  value " #t #t)

(test "Positive numbers." 1 1)
(test "Negative numbers." -123 -123)

(test "String." "hola" "hola")

;; The If statement
(test "a true condition in if statament." '(if #t "hola" "adios") "hola")
(test "a false condition in if statament." '(if #f "hola" "adios") "adios")

(test "Execute form in a if statament true branch."
      '(if #t (+ 1 1) (- 1 1))
      2)

(test "Execute form in a if statament false branch."
      '(if #f (+ 1 1) (- 1 1))
      0 )

(test "Execute form in condition if statament."
      '(if (eq? 1 1) (+ 1 1) (- 1 1))
      2)   

(test "Check nested forms in a if statement." 
       '(if (boolean? (eq? (= (- 2 1) (+ 1 0) )#t)) (- 1 (+ 1 1)) (- 1 1)) 
       -1 )

;; Begin
(test "Set a variable from a 'begin scope"
      '(begin (define x 1) (set! x "hola") x)
      "hola")

(test "Set a variable from a 'lambda scope"
      '(begin (lambda (y) (set! x "hola")) x)
      "hola") 
      

;; Lambda 
(test  "Call a Lambda Function."
       '((lambda (y) y) 1)
       1 )

(test "Execute a lambda function."      
      '((lambda (y) (+ 1 y)) 1) 
      2 )

(test "Lambda do not polute the "
     '(begin (set! x "outter x") ((lambda(x) x) "inner x") x)
     "outter x")

(test "Nest begin clause."
      '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1)) (+ (+ 1 2) 100))
      103 )

(test "Nested begin clause."
      '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1))  1)
      1 )            

(test  "Nest begin clause."
       '(begin (+ 1 1) (begin (+ 1 1) (+ 1 1)) (+ (+ 1 2) 100)) 
       103)

;; Let 
(test "simple let scope."
      '(let ((x 1) ) x)
      1)

(test "let accept procedure to set up lexical variables."
      '(let ((x (+ 1 1)) ) x)
      2)


;;(test "let accept procedure to set up lexical variables."
;;      '(let ((x (+ 1 1)) )
;;         (begin (set! x 3) 
;;               x))
;;     3)

(test "let create assign lambda to a lexical scoped variable."
      '(let ((increment (lambda (x) (+ 1 x))))
               (increment 1))
      2)

(test "A more complex interaction between scopes.  (+ (* (+ 4 5) 4) 3)"
      '(let ((x 3) (y 4))
        (+ (let ((x (+ y 5)))
             (* x y)) x ))
      39)

(test "Nested let scope"
      '(let ((x 1))
         (let ((x 3))
           x))
      3)

;; Other classic functions 
(test "A the factorial function"
      '(begin (define (fact n)
                (if (= 0 n)
                    1
                    (* n (fact (- n 1)))))
              (fact 3))
      6)
      
;; Extensions 

(test "inspect the environment"
      '__environment__
      env.global)
