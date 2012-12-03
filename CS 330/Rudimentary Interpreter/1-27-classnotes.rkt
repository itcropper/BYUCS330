#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  [id (name symbol?)])


(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (if (< 3 (length sexp))
         (error parse '(+ 1 2 3) "More than two arguments")
         (case (first sexp)
           [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
           [(-) (sub (parse (second sexp))
                     (parse (third sexp)))]
           [else (error "Illegal syntax")]
           )
         )
     ]
    )
  )





(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [with (bound-id named-expr bound-body);what are these supposed to be?
          (calc (subst bound-body
                       bound-id
                       (num (calc named-expr))))]
    [id (v) (error ’calc ”free identifier”)])
  )



(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body);what are these supposed to be?
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)])
  )

;(map calc (map binding-named-expr bindings))

;Now lets add functions to out language:
;just to keep it simple, were only going to allow built in functions, not worrying about user defined functions

;were allowing the ability to call by name, one-parameter functions

;<F1WAE>::=

;   |{<id><WAE>}

;so, the next thing we need is: if youre going to allow a user to call functions, you have to build in the functions
;were defining things in terms of the target language (not racket language)
;so we'll need an internal representation for funcitions

;IE:
;(define fun-defs
;(list
; (fundef 'double
;      'n
;      (add (id 'n) (id 'n)))

;formal parameters: what you refer to them in the definition of the function so that you
;know what to refer to them as within the function
;the caller then passes in the actual parameter
;functions need:
      ;name
      ;arguements(s)
      ;body/code------

