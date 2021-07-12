;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |d. skipn-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; skipn-starter.rkt


;; lox natural -> lox
;; produce new list that includes first element and every nth element
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 2) (list "a" "d"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 3) (list "a" "e"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 4) (list "a" "f"))
(check-expect (skipn (list "a" "b" "c" "d" "e" "f") 1) (list "a" "c" "e"))

;(define (skipn lox) empty)

(define (skipn lox1 n)
  ;; acc: position of first lox in lox1
  ;; (skipn (list 1 2 3) 1) 1
  ;; (skipn (list   2 3) 2) 2
  ;; (skipn (list     3) 3) 3
  (local [(define (skipn lox acc)
            (cond [(empty? lox) empty]
                  [else
                   (if (= (modulo acc (+ n 1)) 1)
                       (cons (first lox)
                       (skipn (rest lox) (add1 acc)))
                       (skipn (rest lox) (add1 acc)))]))]
    (skipn lox1 1)))