#lang plai


(require racket/trace)
(print-only-errors)


(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])


(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])


(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])
;--------------------------------------------------

(define op-table
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        )
  )

(define other-ops
  (list (list 'fun fun)
        (list 'with with)
        (list 'if0 if0)
        )
  )

(define (find-other-ops op)
  ;(printf "~e\n" op)
  (if (assoc op other-ops)
      (second (assoc op other-ops))
      false)
  )

;CONTRACT:(lookup-op op) → (or/c procedure? false/c)
;  op : symbol?

;PURPOSE:extracts the definition 
;of an operator or false

;DEFINITION
(define (lookup-op op)
  (if(assoc op op-table)
     (second (assoc op op-table))
     false)
  )
;TESTS 

;(test (lookup-op '+) +)
;(test (lookup-op '-) -)
;(test (lookup-op '*) *)
;(test (lookup-op '/) /)
;(test (lookup-op '$) false)
;(test (lookup-op '_) false)
;(test (lookup-op '++) false)
;(test (lookup-op 1) false)


;------------;------------;----------

#;(CFWAE	 	=	 	number
 	 	|	 	(list '+ CFWAE CFWAE)
 	 	|	 	(list '- CFWAE CFWAE)
 	 	|	 	(list '* CFWAE CFWAE)
 	 	|	 	(list '/ CFWAE CFWAE)
 	 	|	 	id
 	 	|	 	(list 'if0 CFWAE CFWAE CFWAE)
 	 	|	 	(list 'with (list (list id CFWAE) ...) CFWAE)
 	 	|	 	(list 'fun (list id ...) CFWAE))
; 	 	|	 	(list CWFAE CWFAE ... 

;DEFINITION:
(define (parse-binding l)
  (if (list? l)
      (if (= 2 (length l))
          (binding (first l) (parse (second l)))
          (error "too few arguemnts in binding"))
      (error "Not a list of bindings")
      )
  )
(define (validSymbol? sym)
  (not (member sym (list 'with 'if0 '+ '- '/ '* 'fun 'app)))
  )

(define (isValidIf? in)
  (if (and (list? in)
           (= 4 (length in))
           (equal? 'if0 (first in)))
      #t
      #f
      )  
  )
;[binding (name symbol?) (named-expr CFWAE?)])
(define (is-valid-binding in)
  (if (and (list? in)
           (= 2 (length in))
           (symbol? (first in))
           (validSymbol? (first in)))
           #t
           #f
           )
  )



(define (isvalidOp op)
  (if (procedure? (lookup-op op))
      #t
      #f
  )
  )

;(test (isvalidOp 'if0) #t)

;[with (lob (listof Binding?)) (body CFWAE?)]

(define (isValidWith? in)
  (and (list? in)
           (= 3 (length in))
           (equal? 'with (first in))
           (list? (second in))
           (andmap is-valid-binding (second in))
           )
)

;(andmap is-valid-binding '((x 4) (y 5) (with 6)))


(define (create-list-of-bindings in)
  (if (empty? in)
      empty
      (cons (binding (first (first in)) 
                     (parse (second (first in)))
                     )
            (create-list-of-bindings (rest in)))
      )
  )
;(list 'fun (list id ...) CFWAE))
(define (isfun? wae)
  (and (list? wae)
       (= (length wae) 3)
       (equal? 'fun (first wae))
       (list? (second wae))
       (andmap symbol? (second wae))
       (andmap validSymbol? (second wae))
       )
  )
;(list CWFAE CWFAE ... 
(define (isapp? in)
  (and (list? in)
       (> (length in) 0)
       )
  )

(define (parse cfwae)
  (cond
    [(number? cfwae) (num cfwae)]
    
    [(and (validSymbol? cfwae)
           (symbol? cfwae))
      (id cfwae)]
    
    [(and (list? cfwae) ;binop?
          (= 3(length cfwae))
          (isvalidOp (first cfwae)))
          (binop (lookup-op (first cfwae))
                 (parse (second cfwae))
                 (parse (third cfwae)))]
    ;(list 'if0 CFWAE CFWAE CFWAE)
    [(isValidIf? cfwae) 
     (if0 (parse (second cfwae))
          (parse (third cfwae))
          (parse (fourth cfwae)))]

    [(isValidWith? cfwae)
     (with 
      (create-list-of-bindings (second cfwae)) 
      (parse (third cfwae)))]
;[fun (args (listof symbol?)) (body CFWAE?)]
    [(isfun? cfwae) 
     (fun (second cfwae) (parse (third cfwae)))]
    
    ;[app (f CFWAE?) (args (listof CFWAE?))])
    [(isapp? cfwae) 
     (app (parse (first cfwae)) (map parse (rest cfwae)))]
    [(error "Invalid Syntax")]
    )
  )





;TESTS:
;(test/exn (parse '(-  2)) 
;"wrong number of arguements")
(test (parse '5) (num 5))
(test (parse '$) (id '$))
(test/exn (parse 'if0) "Invalid Syntax")
(test/exn (parse 'with) "Invalid Syntax")
(test/exn (parse 'fun) "Invalid Syntax")
(test/exn (parse 'app) "Invalid Syntax")

(test (parse '(* 1 2)) (binop * (num 1) (num 2)))
(test/exn (parse '(- 1 2 3)) "Invalid Syntax")

(test/exn (parse '(with [x 1] x)) "Invalid Syntax")
(test (parse '5) (num 5))

(test/exn (parse '(/ 1 "fish"))"Invalid Syntax")

(test/exn (parse +) "Invalid Syntax")
(test/exn (parse true) "Invalid Syntax")
(test (parse '(+ 1 2)) (binop + (num 1) (num 2)))
(test/exn (parse '(+ 1 2 3)) "Invalid Syntax")
(test (parse '(* 3 2)) (binop * (num 3) (num 2)))

(test/exn (parse true) "Invalid Syntax")

(test/exn (parse '(+)) "Invalid Syntax")
(test/exn (parse '(-)) "Invalid Syntax")
(test/exn (parse '(/)) "Invalid Syntax")
(test/exn (parse '(*)) "Invalid Syntax")

(test (parse '(+ 5 5)) (binop + (num 5) (num 5)))
(test/exn (parse '(+ 5 5 5)) "Invalid Syntax")
(test/exn (parse '(+ 5)) "Invalid Syntax")
(test/exn (parse '(+ 1 "fish")) "Invalid Syntax")
(test/exn (parse '(+ "fish"  1 )) "Invalid Syntax")

(test (parse '(- 5 5)) (binop - (num 5) (num 5)))
(test/exn (parse '(- 5 5 5)) "Invalid Syntax")
(test/exn (parse '(- 5)) "Invalid Syntax")
(test/exn (parse '(- 1 "fish")) "Invalid Syntax")
(test/exn (parse '(- "fish"  1 )) "Invalid Syntax")

(test (parse '(/ 5 5)) (binop / (num 5) (num 5)))
(test/exn (parse '(/ 5 5 5)) "Invalid Syntax")
(test/exn (parse '(/ 5)) "Invalid Syntax")
(test/exn (parse '(/ 1 "fish")) "Invalid Syntax")
(test/exn (parse '(/ "fish"  1 )) "Invalid Syntax")

(test (parse '(* 5 5)) (binop * (num 5) (num 5)))
(test/exn (parse '(* 5 5 5)) "Invalid Syntax")
(test/exn (parse '(* 5)) "Invalid Syntax")
(test/exn (parse '(* 1 "fish")) "Invalid Syntax")
(test/exn (parse '(* "fish"  1 )) "Invalid Syntax")


(test (parse '(with ((x 4)) (+ 3 x)))
      (with (list (binding 'x (num 4))) 
            (binop + (num 3) (id 'x))))

(test (parse '(with () (+ x y))) 
            (with empty (binop + (id 'x) (id 'y))))

(test (parse '(with ((x 4) (y 3) (z 2)) (+ x y))) 
            (with (list (binding 'x (num 4))
                        (binding 'y (num 3))
                        (binding 'z (num 2))) 
                  (binop + (id 'x) (id 'y))))

(test/exn (parse 
         '(with ({x 5}) ({x 4}) (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({x 5}) ({x 4}) (+ 3 x y))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({if0 5}) (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({with 5}) (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({app 5}) (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({fun 5}) (+ 3 x))) 
          "Invalid Syntax")

(test/exn (parse 
         '(with ({x 5} {3 3}) (+ 3 x))) 
          "Invalid Syntax")
(test/exn (parse '(with))  "Invalid Syntax")

(test (parse '(if0 5 5 5)) 
      (if0 (num 5) (num 5) (num 5)))

(test/exn (parse '(if0 "test" 2 3)) 
      "Invalid Syntax")

(test/exn (parse '(if0  2 "test" 3)) 
      "Invalid Syntax")

(test/exn (parse '(if0 1 2 "test")) 
      "Invalid Syntax")

(test (parse '( 5 5 5 5)) (app (num 5) 
                               (list (num 5) 
                                     (num 5) 
                                     (num 5))))

(test/exn (parse empty) "Invalid Syntax")

(test/exn (parse empty) "Invalid Syntax")















