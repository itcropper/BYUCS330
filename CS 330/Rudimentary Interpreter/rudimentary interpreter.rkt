#lang plai

(require racket/trace)

(print-only-errors) 

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  [id (name symbol?)])


;CONTRACT: (parse s-exp) -> WAE?
;PURPOSE: parses s-exp into a WAE according to the grammar:
;    WAE	 	=	 	number
; 	 	|	 	(+ WAE WAE)
; 	 	|	 	(- WAE WAE)
; 	 	|	 	(with ([id WAE]) WAE)
; 	 	|	 	id
;
;where number is a Racket 
;number and id is not '+, '-, or 'with.

;DEFINITION:
(define (parse-binding l)
  (if (= 2 (length l))
      (binding (first l) (parse (second l)))
      (error "too few arguemnts in binding")
      )
  )

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    
    [(list? sexp)
     
     (cond
       [(not(equal? 3 (length sexp))) 
        (error "wrong number of arguements")]
       
       [else
        (case (first sexp)
          [(+) (add (parse (second sexp))
                    (parse (third sexp)))]
          [(-) (sub (parse (second sexp))
                    (parse (third sexp)))]
          ; new from here
          [(with)
           
           (if (list? (first (second sexp)))
               (with (parse-binding (first (second sexp))) 
                     (parse (third sexp)))
               (error "Not a list of bindings"))
           ]
          [else (error "unrecognized symbol")]
          )]
       )]
    [else (error "Illegal syntax")]
    )
  )


;TESTS


(test (parse '5) (num 5))
;(parse '(- $ 3))

;(trace parse)
(test/exn (parse +) "Illegal syntax")
(test/exn (parse true) "Illegal syntax")

(test (parse 
       '(+ 1 2)) 
      (add (num 1) (num 2)))

(test/exn (parse 
           '(+ 1 2 3)) 
          "wrong number of arguements")

(test/exn (parse 
           '(* 3 2)) 
          "unrecognized symbol")

;(trace parse)
(test (parse 
       '(with ({x 5}) {+ 1 x})) 
      (with (binding 'x (num 5)) 
            (add (num 1) (id 'x))))

;(trace parse)
(test (parse 
       '(with ({x 5}) x)) 
      (with (binding 'x (num 5)) (id 'x)))

;(trace parse)
(test/exn (parse 
           '(with [2 1]  x))
          "Not a list of bindings")



;------------------------------------------

;CONTRACT: (calc e) → number?
;  e : WAE?

;PURPOSE:Consumes a WAE 
;representation of an 
;expression and computes 
;the corresponding numerical result.

;DEFINITION


(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [with (bind body)
          (calc (subst body
                       (binding-name bind)
                       (num (calc 
                             (binding-named-expr bind)))))]
    [id (v) (error 'calc "free identifier")])
  )



(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound bound-body)
          
          (if (symbol=? (binding-name bound) sub-id)
              (with (binding (binding-name bound)
                             (subst (binding-named-expr bound) sub-id val))
                    bound-body) 
              (with (binding (binding-name bound)
                             (subst (binding-name bound) sub-id val))
                    (subst bound-body sub-id  val)))]
    
    [id (v) (if (symbol=? v sub-id) val expr)])
  )


;TESTS
(test (calc (num 5) ) 5)
(test (calc (add (num 5) (num 5))) 10)


(test (calc (parse '5)) 5)
(test (calc (parse '(+ 4 1))) 5)

(test (calc (parse '(- 4 1))) 3)
(test (calc (parse '(- 4 (- 3 5)))) 6)
(test (calc (parse '(+ 4 (+ 3 5)))) 12)
;(trace calc)
(test (calc (parse '(with ({x 3}) {+ x 1})))4)
(test (calc (parse '(with ({x 5}) x)))5)

(test/exn (calc (parse 
                 '(with ({x}) {x}))) 
          "too few arguemnts in binding")

(test/exn (calc (parse 
                 '(with ({x 3}) {+}))) 
          "wrong number of arguements")

(test/exn (calc (parse'(+ x 3))) "calc: free identifier")

(test/exn (calc (parse 
                 '(with ({}) {}))) 
          "too few arguemnts in binding")

(test/exn (calc (parse 
                 '(^ 3 5))) 
          "unrecognized symbol")

(test (calc (parse 
             '(with ({x 
                      (with ({x 3}) {+ x 4})}) 
                    (+ x 5)))) 12)

(test (calc (parse 
             '(with ([x (+ 5 5)]) (with ([x 7]) (+ x x)))
             ))14)
(test (calc (parse 
             '(with ([x (+ 5 5)]) (+ x (with ([x 7]) (+ x x))))
             )) 24)

(test (calc (parse 
             '(with ([y 7]) (with ([y (+ y 2)]) (+ y 3)))
             )) 12)


;-------------------------------END------------------