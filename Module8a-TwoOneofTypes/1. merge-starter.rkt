;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1. merge-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; merge-starter.rkt

;; DATA DEFINITION

;; ListOfNumbers is one of:
;; - empty
;; - (cons number listofnumber)

(define LON1 (list 1 2 3))

#;
(define (fn-for-lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; function definition
;; listofnumber listofnumber -> listofnumber
;; produce sorted (ascending) list of number by combining two sorted listofnumbers
(check-expect (merge empty empty) empty)
(check-expect (merge (list 1) empty) (list 1))
(check-expect (merge empty (list 1)) (list 1))
(check-expect (merge (list 1 2) (list 3 4)) (list 1 2 3 4))
(check-expect (merge (list 1 3) (list 2 4)) (list 1 2 3 4))

;(define (merge empty empty) empty)

(define (merge lsta lstb)
  (cond [(empty? lsta) lstb]
        [(empty? lstb) lsta]
        [else
         (if (< (first lsta) (first lstb))
             (cons (first lsta) (merge (rest lsta) lstb))
             (cons (first lstb) (merge lsta (rest lstb))))]))