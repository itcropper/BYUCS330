#lang racket

(define (f x)
  3
  )

( f(/ 1 1))

;under eager: fail
;under lazy, returns '3'

(define (cube x)
  (* x x x)
  )

(cube (+ 1 2))

;eager: (*3 3 3)
;lazy (* (+ 1 2) (+ 1 2) (+ 1 2)), so lazy can sometimes be counter efficient

(f ( + 1 2))

;eager: 1 is added to 2 once
;lazy: 1 is added to 2 zero times.
;if you use something multiple times, lazy will slow things down because it has
; to evaluate it each time.
;smart lasziness (if you use it once, wait till needed, if more than that,
; save the value to use for later.