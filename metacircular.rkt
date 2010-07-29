#lang racket/base

;; This implementation is supposed to be metacircular in the sense of SICP

;; Replacing the define-struct
(define (make-box value)
  (cons 'box value))

(define (box-value box)
  (cdr box))

(define (set-box-value box new-value)
  (set-cdr! box new-value))

;; -------------------------------------    Define booleans
(define the-false-value #f)

;; -------------------------------------    Data Types Predicates.
(define (boolean? x )
  (or (eq? x the-false-value)
      (eq? x (not the-false-value)) ))

