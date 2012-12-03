#lang plai/collector

(define heap-ptr  'uninitialized-heap-ptr)
(define half-heap 0)
(define in-first-half #t)
(define scan-pointer 0)

(define (init-allocator)
  (begin (set! half-heap (floor (/ (heap-size) 2))))
  (set! heap-ptr 0))

(define (cant-allocate-prim)
  (if in-first-half
      (> (+ heap-ptr 2) half-heap)
      (> (+ heap-ptr 2) half-size)
      )
  )

(define (cant-allocate-cons)
  (if in-first-half
      (> (+ heap-ptr 3) half-heap)
      (> (+ heap-ptr 3) heap-size)
      )
  )


(define (gc:alloc-flat p)
  (begin
    (when (cant-allocate-prim)
      (stop-and-copy p)
      (when (cant-allocate-prim)
        (error 'gc:alloc-flat "out of memory")))
    (collect p)
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (cant-allocate-cons)
      (stop-and-copy-cons f r)
      (when (cant-allocate-cons)
        (error 'gc:cons "out of memory")))
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

(define (copy-root-to-semispace root);thats better
  (cond
    [(gc:cons? (read-root root))
     (begin
       (define from-location (read-root root))
       ;;copy data
       (heap-set! heap-ptr 'cons)
       (heap-set! (+ 1 heap-pointer) (heap-ref (+ 1 from-location)))
       (heap-set! (+ 2 heap-pointer) (heap-ref (+ 2 from-location)))
       
       ; Set forwarding address
       
       (heap-set! (heap-ref from-location) 'copied-cons)
       
       ; Update root pointer
       ; Move heap pointer
       
       
       )];;just realized i never use from location anywhere
    [(gc:flat? (read-root root))
     (begin
       (define from-location (read-root root))
       ;;copy data
       (heap-set! heap-ptr 'prim)
       (heap-set! (+ 1 heap-pointer) (heap-ref (+ 1 from-location)))
       )
     ]
    )
  )


(define (scanner)
  (cond
    [(gc:cons?)
     
     ]
    [(gc:flat?)
     
     ]
    [else
     
     ]
    )
  )

(define (stop-and-copy-cons f r)
  (begin 
      (target-space (if in-first-half 0 1))
    (set! heap-ptr (floor (/ (heap-size) 2)))
    (set! scan-pointer heap-ptr )
    (map 
     (λ(x) 
       (begin 
         (copy-root-to-semispace x)
         (heap-set! scan-pointer (+ 3 scan-pointer)))) 
     (get-root-set f r))
    
    (scanner)
    
    (if(in-first-half)
     (set! in-first-half #f)
     (set! in-first-half #t)
    )
    
    
    )
  
     ;; ([(if (gc:cons? f) (set! scan-pointer (+ scan-pointer 3)))])
  ;;(free-less-than)checks if there x spaces of contiguous memory left 
  ;; in the free memory block
  
  
  ; heap half and heap-ptr have been initialized and assigned
  ; called when close to filling half of memory
  ; determine which side of memory youre  on
  
  
  (define (stop-and-copy root)
    (copy root)
    
    )
  
  ; go through 'in use' side and copy in use blocks to idle side
  ; mark copied memory block as copied
  ; increment in use pointer, increment ide pointer
  
  
  
  
  
  
  ;;Provided Functions
  
  ;(heap-value? v) → boolean?
  ;  v : any/c
  
  ;(location? v) → boolean?
  ;  v : any/c
  
  ;(heap-size) → number?
  
  ;(heap-set! addr v) → void
  ;  addr : location?
  ;  v : heap-value?
  
  ;(heap-ref addr) → heap-value?
  ; addr : location?
  
  ;(root? v) → boolean?
  ;  v : any/c
  
  ;(get-root-set root-identifier ...)
  ;  	root-identifier	 :  identifier?
  
  ;(read-root r) → location?
  ;   r : root?
  
  ;(set-root! r a) → void
  ;  r : root?
  ;  a : location?
  
  ;(procedure-roots proc) → (listof root?)
  ;  proc : procedure?
  
  ;(allocator-setup collector-file heap-size)
  ;  collector-file:	 	string?
  ;  heap-size     :	 	number?