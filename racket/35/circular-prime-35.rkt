#lang racket

(require (only-in math/number-theory prime?))

(define (circular-variants s)
  (for/list ([i (in-range (string-length s))])
    (string-append
     (substring s i (string-length s))
     (substring s 0 i))))

(define (prime/circular? n)
  (define circulars (map string->number (circular-variants (number->string n))))
  (andmap prime? circulars)) 

(define (answer [max 1e6] [pred? prime/circular?])
  (length (for/list ([i (in-range max)]
                     #:when (pred? i)) i)))