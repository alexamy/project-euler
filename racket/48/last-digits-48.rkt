#lang racket

(define (self-power n)
  (expt n n))

(define (sum lst)
  (apply + lst))

(define (take-right/string str n)
  (define chars (string->list str))
  (apply string (take-right chars (min n (length chars)))))

(define (answer/full range-max)
  (sum (map self-power (range 1 (add1 range-max)))))

(define (answer range-max last-count)
  (take-right/string (number->string (answer/full range-max)) last-count))

(define answer/euler (answer 1000 10))