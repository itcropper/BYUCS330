#lang web-server/insta
(require web-server/servlet-env)
(require web-server/servlet
         web-server/servlet-env)

#;(serve/servlet my-app
                 #:extra-files-paths
                 (list
                  (build-path "path to jquery")))


(define (page1)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "controller for shooters"))
                    (body
                     (h1 " What is the best controller for shooters?")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "answer"] [value "Arcade Stick"]))
                           "Arcade Stick "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Gamepad"]))
                           "Gamepad "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Keyboard"]))
                           "Keyboard "
                           (br)
                           (input ([type "submit"]))))))))])
    (cdr (assoc 'answer (url-query (request-uri req)))))
  )

(define (page2)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "What is the best gate?"))
                    (body
                     (h1 " What is the best gate?")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "answer"] [value "Circular"]))
                           "Circular "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Octangonal"]))
                           "Octangonal "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Square"]))
                           "Square "
                           (br)
                           (input ([type "submit"]))))))))])
    (cdr (assoc 'answer (url-query (request-uri req)))))
  )

(define (page3)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "horizontal or vertical scrolling"))
                    (body
                     (h1 " Do you prefer horizontal or vertical scrolling?")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "answer"] [value "Horizantal"]))
                           "Horizontal  "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Vertical"]))
                           "Vertical "
                           (br)
                           
                           (input ([type "submit"]))))))))])
    (cdr (assoc 'dev (url-query (request-uri req)))))
  )

(define (page4)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "best horizontal shooter?"))
                    (body
                     (h1 " What is the best horizontal shooter?")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "answer"] [value "Gradius V"]))
                           " Gradius V "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Sexy Parodius"]))
                           "Sexy Parodius "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Border Down"]))
                           "Border Down "
                           (br)
                           
                           (input ([type "radio"] [name "answer"] [value "Deathsmiles"]))
                           "Deathsmiles "
                           (br)
                           (input ([type "radio"][name "answer"][value "text selected"]) (input ([type "text"] [name "other"] [placeholder "other"])))
                           (br)
                           (input ([type "submit"]))))))))])
    
    (if (equal? (cdr (assoc 'answer (url-query (request-uri req)))) "text selected")
        (cdr (assoc 'answer (url-query (request-uri req))))
        (cdr (assoc 'answer (url-query (request-uri req))))
        )
    )
  )

(define (page5)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "better developer?"))
                    (body
                     (h1 " Which is the better developer?")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "dev"] [value "Cave"]))
                           " Cave  "
                           (br)
                           
                           (input ([type "radio"] [name "dev"] [value "Treasure"]))
                           "Treasure "
                           (br)
                           (input ([type "submit"]))))))))])
    (cdr (assoc 'dev (url-query (request-uri req)))))
  )

(define (page6)
  (let ([req
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "Cave’s best release?"))
                    (body
                     (h1 "What is Cave’s best release? ")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "cave"] [value "Dodonpachi"]))
                           " Dodonpachi "
                           (br)
                           
                           (input ([type "radio"] [name "cave"] [value "Ketsui kizuna jigoku tachi"]))
                           "Ketsui kizuna jigoku tachi "
                           (br)
                           
                           (input ([type "radio"] [name "cave"] [value "Mushihimesama Futari 1.5"]))
                           "Mushihimesama Futari 1.5 "
                           (br)
                           
                           (input ([type "radio"][name "cave"][value "text selected"]) (input ([type "text"] [name "other"] [placeholder "other"])))
                           (br)
                           (input ([type "submit"]))))))))])
    
    (if (equal? (cdr (assoc 'treasure (url-query (request-uri req)))) "text selected")
        (cdr (assoc 'other (url-query (request-uri req))))
        (cdr (assoc 'treasure (url-query (request-uri req))))
        )
    )
  )

(define (page7)
  (let ([req 
         (send/suspend
          (lambda (k-url)
            (response/xexpr
             `(html (head (title "Treasure’s best release?"))
                    (body
                     (h1 "What is Treasure’s best release? ")
                     (form ([action ,k-url])
                           (input ([type "radio"] [name "treasure"] [value "Radiant Silvergun"]))
                           " Radiant Silvergun "
                           (br)
                           
                           (input ([type "radio"] [name "treasure"] [value "Sin and Punishment"]))
                           " Sin and Punishment "
                           (br)
                           
                           (input ([type "radio"] [name "treasure"] [value "Ikaruga"]))
                           " Ikaruga "
                           (br)
                           
                           (input ([type "radio"] [name "treasure"] [value "Sin and Punishment: Star Successor"]))
                           " Sin and Punishment: Star Successor "
                           (br)
                           
                           (input ([type "radio"][name "treasure"][value "text selected"]) (input ([type "text"] [name "other"] [placeholder "other"])))
                           (br)
                           (input ([type "submit"]))))))))])
    
    (if (equal? (cdr (assoc 'treasure (url-query (request-uri req)))) "text selected")
        (cdr (assoc 'other (url-query (request-uri req))))
        (cdr (assoc 'treasure (url-query (request-uri req))))
        )
    
    )
  
  )

(define (results pg1 pg2 pg3 pg4 pg5 pg6 pg7)
  (response/xexpr
   `(html (head (title "Results"))
          (body
           (h1 "Answers!")
           (p "What is the best controller for shooters?")
           (p "")
           )
          )
   )
  )



(define (start initial-request)
  (define response1 (page1))
  
  (define response2 (if (equal? response1 "Arcade Stick")
                        (page2)
                        #f))
  (define response3 (page3))
  (define response4 
    (if (equal? response3 "Horizontal")

        (page5))))
        
        (define response5 (page5))
    (define pg6 (page6))
    (define pg7 (page7))
    
    ;;(page7))
    
    
    )

