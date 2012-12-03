
#lang plai

(define-syntax-rule (prolog-expression?
                     failure-continuation)
  (let ((success-continuation "true")) (λ () success-continuation)))

(define (time-it f)
  (define start(current-inexact-milliseconds))
  (displayln f)
  (define end(current-inexact-milliseconds))
  (- end start))

(time-it (λ() (expt 2 5000)))

(define-syntax-rule (jor lhs rhs)
  (let ([lhs-v lhs])
        (if lhs-v lhs-v rhs)))
  
  (jor #t (/ 1 0))
  
  (jor (time-it (+ 1 1)) 7)
  
#;(define-syntax for
  (syntax-case()
    [(for from start to end do body ...)
     (syntax
      (let lop ([it start])
        (unless (= it end)
          body...
          (loop (add1 it)))))]))