#lang racket

(require rackunit)

(define (sub2 n) (- n 2))

(define (group-count full-list count)
  (define (iter lst [res '()])
    (if (empty? lst)
        res
        (iter
         (drop lst count)
         (append res (list (take lst count))))))
  (iter full-list))

(define (spiral-start order)
  (define (iter order [sum 1])
    (define count (if (<= order 0) 1 (* order 4)))
    (if (< order 0)
        sum
        (iter (sub2 order) (+ sum count))))
  (iter (sub2 order)))

(define (spiral-parts order)
  (if (<= order 0)
      '((1))
      (let ([start (spiral-start order)])
        (group-count (range start (+ start (* order 4))) order))))

(define (spiral-sums order)
  (apply + (map last (spiral-parts order))))

(define (answer n)
  (let ([order (sub1 n)])
    (for/sum ([i (range 0 (+ order 2) 2)])
      (spiral-sums i))))

(define (answer/euler) (answer 1001))

(test-case
 "Spiral start"
 (check-equal? (spiral-start 0) 1)
 (check-equal? (spiral-start 2) 2)
 (check-equal? (spiral-start 4) 10)
 (check-equal? (spiral-start 6) 26))

(test-case
 "Group by count"
 (check-equal? (group-count '(2 3 4 5) 2) '((2 3) (4 5)))
 (check-equal? (group-count '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9))))

(test-case
 "Spiral lists"
 (check-equal? (spiral-parts 0) '((1)))
 (check-equal? (spiral-parts 2) '((2 3) (4 5) (6 7) (8 9)))
 (check-equal? (spiral-parts 4) '((10 11 12 13) (14 15 16 17) (18 19 20 21) (22 23 24 25))))

(test-case
 "Spiral lists sums"
 (check-equal? (spiral-sums 0) 1)
 (check-equal? (spiral-sums 2) 24)
 (check-equal? (spiral-sums 4) 76))

(test-case
 "Diagonal sums in spirals"
 (check-equal? (answer 3) 25)
 (check-equal? (answer 5) 101))
