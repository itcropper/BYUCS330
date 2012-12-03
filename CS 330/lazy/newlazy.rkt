#lang lazy

(require racket/trace)

(define print-only-errors #f)




(define (build-infinite-list f)
   (map f natural-nums)
)

(define natural-nums
  (cons 0 (map add1 natural-nums))
)

(define (f x)
  (+ 5 x)
 )

(define evens (build-infinite-list (lambda (x) (* x 2))))
(define odds (build-infinite-list (λ(x) (+ 1 (* x 2)))))

