;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |c. replicate-elm-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; replicate-elm-starter.rkt


;; lox natural -> lox
;; produce list where each element replicated n times
(check-expect (replicate-elm (list "a" "b" "c") 2) (list "a" "a" "b" "b" "c" "c"))
(check-expect (replicate-elm (list "a" "b" "c") 1) (list "a" "b" "c"))
(check-expect (replicate-elm (list "a" "b" "c") 3) (list "a" "a" "a"
                                                         "b" "b" "b"
                                                         "c" "c" "c"))


(define (replicate-elm lox1 n)
  ;; acc: 
  ;;
  ;; (replicate-elm (list "a" "b" "c") 2) ; outercall
  ;;
  ;; (replicate-elm (list "a" "b" "c") 2)  ;; replicate a twice
  ;; (replicate-elm (list "a" "b" "c") 1)  ;; replicate a once 
  ;; (replicate-elm (list "a" "b" "c") 0)  ;; finished replicating a
  ;; (replicate-elm (list     "b" "c") 2)  ;; replicate b twice 
  ;; (replicate-elm (list     "b" "c") 1)
  ;; (replicate-elm (list     "b" "c") 0)
  ;; (replicate-elm (list         "c") 2)
  ;; 
  (local [(define (replicate-elm lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (zero? acc)
                       (replicate-elm (rest lox) n)
                       (cons (first lox)
                             (replicate-elm lox (sub1 acc))))]))]
(replicate-elm lox1 n)))
                                                                 
