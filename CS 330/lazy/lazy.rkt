#lang lazy

(require racket/trace)

(define print-only-errors #f)

(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))


;CONTRACT
;(take-while p l) → (listof any/c)
;  p : (any/c . -> . boolean?)
;  l : (listof any/c)

;PURPOSE:
;Returns the prefix of l such that for all elements p returns true.
;This is not filter.

;DEFINITION:
(define (take-while func listIn)
  (if (empty? listIn)
      empty
      (if (func (first listIn))
          (cons (first listIn) (take-while func (rest listIn)))
          empty
          )
      )
  )

;TESTS
(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2)) (list 1 2 3 4))
(test (take-while (lambda (n) (< n 0)) (list 1 2 3 4 5 1 2)) empty)
(test (take-while (lambda (n) (not empty)) (list 1 2 3 4 5 1 2)) empty)
(test (take-while (lambda (n) (< n 0)) (list 0 0 0 0 0 0 0)) empty)
(test (take-while (lambda (n) (>= n 0)) (list 0 0 0 0 0 0 0)) 
      (list 0 0 0 0 0 0 0))


;;----------------------------------------------------------------------------
;CONTRACT:
;procedure
;(build-infinite-list f) → (listof any/c)
;  f : (exact-nonnegative-integer? . -> . any/c)

;PURPOSE:
;Lazily constructs the infinite list such that (list-ref 
;(build-infinite-list f) i) returns (f i).

;DEFINITION:
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


;TEST
;(trace build-infinite-list)
(test (take 5 evens) '(0 2 4 6 8 ))
(test (take 5 odds) '(1 3 5 7 9 ))
(test (list-ref (build-infinite-list f) 3)  (f 3))
(test (list-ref (build-infinite-list f) 3)  (f 3))

;;----------------------------------------------------------------------------

;CONTRACT
; (prime? n) → boolean?
;  n : exact-positive-integer?
  
;Purpose
;Returns true if n is prime.

(define natural-divisors
  (cons 2 (map add1 natural-divisors))
)
  
;DEFINITION
(define (prime? n)
  (if (or (= n 0)
          (= n 1)
          (= n 2))
      #t
      (if (< n 0)
          #f
          (andmap 
           (λ (x) (not 
                   (exact-positive-integer? 
                    (/ n x))))
           (take-while 
            (λ (z)(<= z (/ n 2))) 
            natural-divisors)
           )
          )
      )
  )


;TESTS
(test (prime? 3) #t)
(test (prime? 5) #t)
(test (prime? 7) #t)
(test (prime? 11) #t)
(test (prime? 0) #t)
(test (prime? 1) #t)
(test (prime? 2) #t)
(test (prime? 4) #f)
(test (prime? 5072) #f)
(test (prime? 5021) #t)
(test (prime? -4) #f)
(test (prime? -1) #f)
(test (prime? -101) #f)


;;--------------------------------------------------------------------------
;CONTRACT
;primes : (listof exact-positive-integer?)
  
;PURPOSE
;The list of all primes.
;You may find filter, prime?, and build-infinite-list useful.
  
  
;;DEFINITION
(define primes 
  (filter prime? natural-divisors)
)

(test (take 11 primes) (list 2 3 5 7 11 13 17 19 23 29 31))
(test (take 0 primes) empty)
#;(test (take 170 primes) (list     
      2      3      5      7     11     13     17     19     23     29 
     31     37     41     43     47     53     59     61     67     71 
     73     79     83     89     97    101    103    107    109    113 
    127    131    137    139    149    151    157    163    167    173 
    179    181    191    193    197    199    211    223    227    229 
    233    239    241    251    257    263    269    271    277    281 
    283    293    307    311    313    317    331    337    347    349 
    353    359    367    373    379    383    389    397    401    409 
    419    421    431    433    439    443    449    457    461    463 
    467    479    487    491    499    503    509    521    523    541 
    547    557    563    569    571    577    587    593    599    601 
    607    613    617    619    631    641    643    647    653    659 
    661    673    677    683    691    701    709    719    727    733 
    739    743    751    757    761    769    773    787    797    809 
    811    821    823    827    829    839    853    857    859    863 
    877    881    883    887    907    911    919    929    937    941 
    947    953    967    971    977    983    991    997   1009   1013))
  
;TESTS

;;-----------------------------------------------------------------------
;;CONTRACT
;primes/fast : (listof exact-positive-integer?)
  
;PURPOSE
;The list of all primes constructed with prime?/fast.
  
;;DEFINITION

(define primes/fast
 (cons 2 (filter prime?/fast (list-tail natural-nums 3))))
  
;;TESTS
;(test (take 11 primes/fast) (list 2 3 5 7 11 13 17 19 23 29 31))
  
;;--------------------------------------------------------------------------
;CONTRACT
;(prime?/fast n) → boolean
;  n : exact-positive-integer?
  
;PURPOSE
;Returns true if n is prime, but tests only prime factors from primes/fast.
  
;;DEFINITION
(define (prime?/fast n)
  (if (or (= n 0)
          (= n 1)
          (< n 0))
      #f
      (andmap 
       (λ (x) (not 
               (exact-positive-integer? (/ n x))))
       (take-while 
        (λ (x) (<= x (/ n 2)))
        primes/fast))
      )
  )

  
;;TESTS
(test (prime?/fast -4) #f)
(test (prime?/fast -1) #f)
(test (prime?/fast 0) #f)
(test (prime?/fast 1) #f)
(test (prime?/fast 2) #t)
(test (prime?/fast 3) #t)
(test (prime?/fast 17) #t)
(test (prime?/fast 15) #f)
(test (prime?/fast 100) #f)
(test (prime?/fast 9343) #t)

  


;;----------------------------------------------------------------------------
;;CONTRACT
;(build-table rows cols f) → (vectorof (vectorof any/c))
;rows : exact-positive-integer?
;cols : exact-positive-integer?
;f : (exact-nonnegative-integer? exact-nonnegative-integer? . -> . any/c

;PURPOSE
;Lazily constructs a vector such that 
;(vector-ref (vector-ref (build-table rows cols f) i) j)
;equals (f i j), when (< i rows) (< j cols).

;;DEFINITION
(define (build-table rows cols f)
  (build-vector rows
                (λ (x) (build-vector cols 
                                     (λ (y) (f x y))))))

(test (vector-ref (vector-ref (build-table 4 4 (λ (x y) (+ x y))) 0)  0) 0)
(test (vector-ref (vector-ref (build-table 4 4 (λ (x y) (+ x y))) 1)  2) 3)
(test (vector-ref (vector-ref (build-table 2 4 (λ (x y) (- x y))) 1)  1) 0)
(test (vector-ref (vector-ref (build-table 5 5 (λ (x y) (* x y))) 3)  4) 12)
  
;;TESTS

;;----------------------------------------------------------------------------
;;CONTRACT
;(lcs-length s1 s2) → exact-nonnegative-integer?
;  s1 : string?
;  s2 : string?
  
;PURPOSE
;Computes the length of the longest common 
;subsequence of two strings s1 and s2.
  
;;DEFINITION
  
;;TESTS
