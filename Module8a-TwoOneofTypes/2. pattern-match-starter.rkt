;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2. pattern-match-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; pattern-match-starter.rkt

;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

(define (fn-for-pattern p)
  (cond [(empty? p) (...)]
        [(... (string=? (...) first p))
              (fn-for-pattern (rest p))]
        [(... (string=? (...) first p))
              (fn-for-pattern (rest p))]))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; ==========
;; Functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))

;; Pattern ListOfString -> Boolean
;; if listofstring matches pattern produce true else false

(define (match pattern los)
  (cond [(and (empty? pattern) (empty? los)) true]
        [(or (empty? pattern) (empty? los)) false]
        [(and (string=? (first pattern) "N") (alphabetic? (first los)))
             (match (rest pattern) (rest los))]
        [(and (string=? (first pattern) "A") (alphabetic? (first los)))
             (match (rest pattern) (rest los))]
        [else false]))