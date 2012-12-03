#lang plai/mutator

(allocator-setup "gcmark.rkt" 55)

(define (f x)
  (lambda ()
    x))

(define g (f 5))

(cons 'symbol 'symbol)
(cons 'symbol 'symbol)
(cons 'symbol 'symbol)
(cons 'symbol 'symbol)

(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)

(test/value=? (g) 5)
(define h (cons (g) (g)))
'hello-there
'hello-there
'hello-there
'hello-there
'hello-there
(test/value=? (first h) 5)
(test/value=? (rest h) 5)

(set-rest! h h)

(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)

(test/value=? (first h) 5)
(test/value=? (g) 5)

(define u (f 'captain))
(test/value=? (u) 'captain)

(cons 'hello-there 'hello-there)

(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)

(test/value=? (first h) 5)

(test/value=? (g) 5)
(test/value=? (u) 'captain)

(set-rest! h (u)) 

(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)

(test/value=? (first h) 5)
(test/value=? (rest h) 'captain)
(test/value=? (g) 5)
(test/value=? (u) 'captain)
(test/value=? (first cool) 'save)
(test/value=? (rest cool) 'save)
;; Save nested cons
(define weee (cons 'symbol (cons 'b (cons 'c (cons 'd 'e)))))
;; do collecting a few times
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)
(cons 'hello-there 'hello-there)

(test/value=? (first h) 5)
(test/value=? (rest h) 'captain)
(test/value=? (g) 5)
(test/value=? (u) 'hey)
(test/value=? (first cool) 'save)
(test/value=? (rest cool) 'save)
(test/value=? (first whoa) 'symbol)
(test/value=? (first (rest weee)) 'b)
(test/value=? (first (rest (rest wweeehoa))) 'c)
(test/value=? (first (rest (rest (rest weee)))) 'd)
(test/value=? (rest (rest (rest (rest weee)))) 'e)