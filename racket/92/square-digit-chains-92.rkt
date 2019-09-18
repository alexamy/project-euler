#lang racket

(define (char->number ch)
  (- (char->integer ch) 48))

(define (next-summed n)
  (for/sum ([digit (number->string n)])
    (sqr (char->number digit))))

(define (last-cyclic-rec n)
  (if (or (<= n 1) (= n 89))
      n
      (last-cyclic-rec (next-summed n))))

(define MAX 10000000)
(define MEMO (make-vector MAX #f))
(define TICK 0)

(define (last-cyclic-memo start n)
  (define memoed (vector-ref MEMO n))
  (cond
    [memoed memoed]
    [(or (<= n 1) (= n 89)) (vector-set! MEMO start n) n]
    [else (last-cyclic-memo start (next-summed n))]))

(define (last-cyclic n)
  (last-cyclic-memo n n))

(define (answer)
  (for ([i MAX])
    (when (= 89 (last-cyclic i))
        (set! TICK (add1 TICK))))
  TICK)