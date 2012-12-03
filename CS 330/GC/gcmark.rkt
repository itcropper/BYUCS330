#lang plai/collector

(define heap-ptr  'uninitialized-heap-ptr)

(define (init-allocator)
  (set! heap-ptr (sweep 0)))



(define (sweep x)
  (cond
    [(equal? (heap-ref x) 'marked-prim)
     (sweep (+ x 1))]
    [(equal? (heap-ref x) 'marked-cons)
     (sweep (+ x 2))]
    [else
     (if (equal? x (- (heap-size) 1))
         (begin
           (heap-set! x #f)
           x)
         (begin
           (heap-set!  x (+ x 1))
           (sweep (+ x 1))
           ))]))

;; This function returns an address with room for a prim
;; or it returns false, meaning there is no room
(define (find-space-prim x)
  (cond
    [(equal? x #f)
     #f]
    [(equal? (+ 1 x) (heap-ref x))
     x]
    [else
     (find-space-prim (heap-ref x))]))


(define (find-space-cons x)
  (cond
    [(or (equal? (heap-ref x) #f)
         (equal? (heap-ref (heap-ref x)) #f)
         (equal? (heap-ref (heap-ref (heap-ref x))) #f))
     #f]
    [(and
      (equal? (+ x 1) (heap-ref x))
      (equal? (+ x 2) (heap-ref (heap-ref x))))
     (begin 
       (set! heap-ptr (heap-ref (+ x 2)))
       x)]
    [else
     (find-space-prim (heap-ref x))
     ]
    )
  )

(define (collect r)
  (begin
    (do-mark r)
    (set! heap-ptr (sweep 0))
    (unmark r)
    )
  )

(define (do-mark r)
  (when (not (empty? r))
    (mark (read-root (first r)))
    (do-mark (rest r))
    ))

(define (mark location)
  (cond
    [(or (equal? (heap-ref location) 'marked-prim)
         (equal? (heap-ref location) 'marked-cons))
     #f]
    [(equal? (heap-ref location) 'prim)
     (heap-set! location 'marked-prim)]
    [(equal? (heap-ref location) 'cons)
     (begin
       (heap-set! location 'marked-cons)
       (mark (heap-ref (+ location 1)))
       (mark (heap-ref (+ location 2))))]
    [else
     (error "you cant do that!")
     ]
    )
  )

(define (unmark x)
  (cond
    [(equal? x (- (heap-size) 1)) #f]
    [(equal? (heap-ref x) 'marked-cons)
     (begin
       (heap-set! x 'marked-prim)
       (unmark (+ x 1)))]
    [else
     (unmark (+ x 1))]))



(define (points-to-me start target)
  (if(equal? (heap-ref start) target)
     start
     (points-to-me (heap-ref start) target)
     )
  )



(define (gc:alloc-flat p)
  (begin
    (when (equal? (find-space-prim heap-ptr) #f)
      (if (procedure? p)
          (collect (append (get-root-set) (procedure-roots p)))
          (collect (get-root-set))
          ))
    (define space (find-space-prim heap-ptr))
    (when (equal? space #f)
      (error 'gc:alloc-flat "out of memory!"))
    
    (mark (points-to-me space heap-ptr))
    
    (heap-set! space 'prim)
    (heap-set! (+ 1 space) p)
    
    
    )
  )

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (error 'gc:cons "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (heap-ref (+ 1 a)))

(define (gc:rest a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (heap-set! (+ 2 a) r))

(define (gc:flat? a)
  (eq? (heap-ref a) 'prim))

(define (gc:deref a)
  (heap-ref (+ 1 a)))