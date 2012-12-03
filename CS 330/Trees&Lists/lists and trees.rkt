;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lists and trees|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;CONTRACT
;(check-temps1 temps) → boolean?

;PURPOSE:
;uses a list of temp mesurements to tell if all 
;numbers are between 5 and 95

;DEFINITION
(define (check-temps1 temps)
  (if (empty? temps)
      true
      (if ( and (<= (first temps) 95) 
                (>= (first temps) 5))
          (check-temps1 (rest temps))
          false
          )
      )
  )

;TESTS
(define match (list 23 94 35 66 95 23))
(define no-match(list 23 95 34 2 90 3))
(define no-match2(list 0 0 0 0 0 0 0 0 0))


(check-expect(check-temps1 match) true)
(check-expect(check-temps1 no-match) false)
(check-expect(check-temps1 no-match2) false)

;------------------------------------------------

;CONTRACT:(check-temps temps low high) → boolean?

;PURPOSE:Reports the lowest and highest from a list

;DEFINITION:
(define (check-temps temps low high)
  (if (empty? temps)
      true
      (if ( and (<= (first temps) high) 
                (>= (first temps) low))
         (check-temps (rest temps) low high)
       false
     )
  )
)   
;TESTS
(define list1 (list 20 40 35 94 105 5 34))

(check-expect(check-temps list1 4 106) true)
(check-expect(check-temps list1 8 105) false)

;--------------------------------------------

;CONTRACT:(convert digits) → number?

;PURPOSE: creates a string of digits out of 
;a reverse list of digits

;DEFINITION:
(define (convert digits)
  (if (empty? digits)
  0
  (+ (* 10 (convert (rest digits))) (first digits))
  )
)

;TESTS
(define listb(list 1 2 3 4 5))
(define listc(list 5 4 3 2 1))

(check-expect(convert listb) 54321)
(check-expect(convert listc) 12345)


;---------------------------------------------

;CONTRACT: (average-price prices) → number?

;PURPOSE: takes in a list of numbers 
;and gets the average

;DEFINITION:
(define (average-price prices)
  (if (empty? prices)
      empty
      (/ 
       (sum-list prices)
       (length prices)
       )
   )
)
(define (sum-list prices2)
  (if (empty? prices2)
      0
      (+ (first prices2)
         (sum-list (rest prices2))
      )
   )
)

;TESTS
(define listd(list 1 2 3 4 5))
(define liste(list 100 200 250 300))

(check-within(average-price listd) 3 .01)
(check-within(average-price liste) 212.5 .01)

;-------------------------------------------------

;CONTRACT: ConvertFC: fahrenheit: 
;number? -> (listof number?)

;PURPOSE: Converts a list of numbers 
;from ferenheit to celcius
;C = (F - 32) * 1.8 OR
;C = (* (- (F)  32) 1.8 )

;DEFINITION
(define (convertFC degrees)
  (if (empty? degrees)
      empty
      (cons
       (* (- (first degrees) 32) (/ 5 9))
       (convertFC (rest degrees))
       )
  )
)

;TESTS
(define list-deg(list 32 20 212 84))
(define list-fake(list))
(check-within(convertFC list-deg) 
    (cons 0 (cons -6.6 (cons 100 (cons 28.8 empty)))) .1)
(check-expect(convertFC list-fake) empty)

;--------------------------------------------------------------------
;CONTRACT: eliminat-exp
;( ua:number? lotp:(listof number?) -> (listof number?)

;PURPOSE: Eliminates from lotp all
;toys thats price is greater than ua.

;DEFINITION:
(define (eliminate-exp ua lotp)
  (if (empty? lotp)
      empty
  ;else
  (if (> ua (first lotp))
      (cons
         (first lotp)
         (eliminate-exp ua (rest lotp))
       ) 
  ;else
      (eliminate-exp ua (rest lotp))
  )
  )
  
)

;TESTS
(define less-than-list(list 9 8 7 6 5 4 3 2 1))
(define other-list(list 2 6 4 4 2 5 7))

(check-expect(eliminate-exp 6 less-than-list)
    (cons 5 (cons 4 (cons 3 (cons 2 (cons 1 empty)))))
 )

(check-expect(eliminate-exp 3 other-list)
     (cons 2 (cons 2 empty))
 )

;----------------------------------------------
;CONTRACT:suffixes(l:list?)-> (listof list?)

;PURPOSE:Produces a list of a suffixes 'l'

;DEFINITION
(define (suffixes l)
  (if (empty? l)
      empty
      (cons
       (suffixes1 l)
       (suffixes (rest l))
      )
   )
)
(define (suffixes1 new-list)
  (if (empty? new-list)
      empty
      (cons
        (first new-list)
        (suffixes1 (rest new-list))
       )
     )
  )

;TESTS
(define list-suf(list 2 3 4 5))
(check-expect(suffixes list-suf) 
(cons
  (cons 2 (cons 3 (cons 4 (cons 5 empty))))
(cons
  (cons 3 (cons 4 (cons 5 empty)))
(cons 
  (cons 4 (cons 5 empty)) 
(cons 
   (cons 5 empty) empty))
 ))
)

;-----------------------------------------------------

;CONTRACT: count-person(ftree:number?)
;-> number?

;PURPOSE: Returns number of people in 
;the family tree

; Represents an unknown ancestor
(define-struct unknown())

;represents a person
(define-struct person
  (name birthyear eyecolor father mother))

;DEFINITION
(define (count-person ftree)
 (cond
   [(unknown? ftree) 
    0
   ]
   [else
    (+ 
     1 
     (count-person (person-mother ftree)) 
     (count-person (person-father ftree))
     )
   ]
  )
)

(define whoknows(make-unknown))
(define maradeth
  (make-person "maradeth" 1921 'bl whoknows whoknows))
(define Wilkens
  (make-person "Wilkens" 1920 'br whoknows whoknows))
(define jose
  (make-person "Jose" 1982 'bl maradeth Wilkens))
(define thePoolGuy
  (make-person "Sergio" 1984 'gr whoknows whoknows))
(define ben
  (make-person "Ben" 2001 'gr jose thePoolGuy))
(define jessica
  (make-person "jessica" 2001 'gr whoknows whoknows))
(define John
  (make-person "john" 2012 'gr jessica ben))


(check-expect (count-person ben) 5)
(check-expect (count-person jose) 3)
(check-expect (count-person John) 7)
;TESTS

;-----------------------------------------------

;CONTRACT: (sum-age tree) -> number?

;Purpose: returns the total age of everyone in the tree

;DEFINITION
(define (total-age tree)
   (cond
   [(unknown? tree) 
    0
   ]
   [else
    (+ 
     (- 2012 (person-birthyear tree))
     (total-age (person-mother tree)) 
     (total-age (person-father tree))
     )
   ]
  )
)

;CONTRACT: (average-age ftree) → number?

;PURPOSE: returns average of the values in a tree

;DEFINITION
(define (average-age tree)
  (/  (total-age tree) (count-person tree) )
  )


(check-expect(average-age Wilkens) 92)
(check-expect(average-age ben) 50.4)
(check-expect(average-age jose) 71)

;TESTS:

;---------------------------------------------------------------------

;(eye-colors ftree) → (listof symbol?)

;PURPOSE: Produces a list of eye colors in a family tree

;DEFINITION
(define (eye-colors ftree)
  (cond
    [(unknown? ftree)
     empty
     ]
    [else
     (cons 
      (person-eyecolor ftree) 
      (append 
       (eye-colors (person-mother ftree)) 
       (eye-colors (person-father ftree))))
    ]
  )
)

;TESTS:

(check-expect 
    (eye-colors maradeth) (cons 'bl empty))
(check-expect 
    (eye-colors ben) (cons
                      'gr
                      (cons
                       'gr
                       (cons
                        'bl
                        (cons 'br (cons 'bl empty))))))
(check-expect (eye-colors jose) 
              (cons 'bl (cons 'br (cons 'bl empty))))

;------------------------------ END -----------------------------------