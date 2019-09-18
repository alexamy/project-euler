#lang racket

(struct date (day month year) #:transparent)

(define (leap-year? year)
  (define (divisible? n m) (zero? (modulo n m)))
  (if (divisible? year 100)
      (divisible? year 400)
      (divisible? year 4)))

(define (days-month date)
  (define (february-days year) (if (leap-year? year) 29 28))
  (match (date-month date)
    ['Jan 31]
    ['Feb (february-days (date-year date))]
    ['Mar 31]
    ['Apr 30]
    ['May 31]
    ['Jun 30]
    ['Jul 31]
    ['Aug 31]
    ['Sep 30]
    ['Oct 31]
    ['Nov 30]
    ['Dec 31]
    [_ (error "Unknown month")]))

(define (next-cyclic values)
  (define values-cyclic (append values (list (car values))))
  (λ (val) (cadr (member val values-cyclic))))

(define next-month
  (next-cyclic '(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)))

(define next-weekday
  (next-cyclic '(Mon Tue Wed Thu Fri Sat Sun)))

(define (next-day date-current)
  (let* ([day (date-day date-current)]
         [month (date-month date-current)]
         [year (date-year date-current)]
         [last-in-month? (= day (days-month date-current))]
         [last-in-year? (and (= day 31) (eq? month 'Dec))])
    (cond
      [last-in-year? (date 1 'Jan (add1 year))]
      [last-in-month? (date 1 (next-month month) year)]
      [else (date (add1 day) month year)])))

; for problem solving
(define (stop? date)
  (let ([day (date-day date)]
        [month (date-month date)]
        [year (date-year date)])
    (and (= day 1) (eq? month 'Jan) (= year 2001))))

(define (count? date weekday)
  (let ([day (date-day date)]
        [year (date-year date)])  
    (and (eq? weekday 'Sun) (= day 1) (> year 1900))))

(define (answer-count date weekday counter)
  (define tick (if (count? date weekday) 1 0))
  (if (stop? date)
      counter
      (answer-count (next-day date) (next-weekday weekday) (+ counter tick))))

(define (answer)
  (answer-count (date 1 'Jan 1900) 'Mon 0))
  