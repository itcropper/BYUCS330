#lang plai

;could we talk more about concrete syntax vs abstract syntax?

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])


(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [app (fun-name arg-expr)
         (app fun-name (subst arg-expr sub-id val))]))

(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds) (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          #;(interp (subst bound-body
                         bound-id
                         (num (interp named-expr fun-defs ds)))
                  fun-defs ds)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr fun-defs ds)
                        ds))]
    [id (v) (error 'interp "free identiﬁer")]
    [app (name actual)
         (local 
           ([define the-fun-def (lookup-fundef name fun-defs)])
           (interp (subst (fundef-body the-fun)
                          (fundef-arg-name the-fun)
                          (num (interp arg-expr fun-defs)))
                   fun-defs ds))]
    )
  )