#lang racket

(define string-reverse
  (compose list->string reverse string->list))

(define (palindrome? s)
  (string=? s (string-reverse s)))

(define (answer [max 1e6])
  (define (count? n)
    (and (palindrome? (number->string n 10))
         (palindrome? (number->string n 2))))
  (for/sum ([i (range max)] #:when (count? i)) i))