#lang racket

(define NUMBER-MAX 10000)
(define ITERATIONS 50)

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (palindrome? s)
  (string=? s (string-reverse s)))

(define (sum-with-reversed n)
  (let ([reversed (string->number (string-reverse (number->string n)))])
    (+ n reversed)))

(define (lychrel-rec? n i)
  (if (zero? i)
      #t
      (let ([next (sum-with-reversed n)])
       (if (palindrome? (number->string next))
        #f
        (lychrel-rec? next (sub1 i))))))

(define (lychrel? n) (lychrel-rec? n ITERATIONS))

(define answer
  (length (for/list ([i (range NUMBER-MAX)] #:when (lychrel? i)) i)))