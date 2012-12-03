#lang lazy

(define (all-ints-from n)
  (cons n (all-ints-from ( + n 1)))
  )

(define all-pos-ints (all-ints-from 1))

(first all-pos-ints)

(define (getter n)
  (get-till n all-pos-ints)
  )

(define (get-till n list)
    (if (empty? list)
        empty
        (if (> n (first all-pos-ints))
            (first (all-pos-ints))
            (get-till n (rest all-pos-ints)))
  )
)