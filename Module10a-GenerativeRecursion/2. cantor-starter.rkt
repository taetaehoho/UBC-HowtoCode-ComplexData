;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2. cantor-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; cantor-starter.rkt

(define CUTOFF 2)
(define HEIGHT 10)
(define SEP (rectangle 1 5 "solid" "white"))

;; integer -> image
;; produces cantor set of width integer
(check-expect (cantor CUTOFF) (rectangle CUTOFF 10 "solid" "blue"))
(check-expect (cantor (* CUTOFF 3)) (local [(define sub (rectangle CUTOFF HEIGHT "solid" "blue"))
                                            (define org (rectangle (* CUTOFF 3) HEIGHT "solid" "blue"))
                                            (define wht (rectangle CUTOFF HEIGHT "solid" "white"))]
                                      (above org SEP (beside sub wht sub))))


;(define (cantor w) (square 0 "solid" "white"))

(define (cantor d)
  (cond [(<= d CUTOFF) (rectangle d HEIGHT "solid" "blue")]
        [else
         (local [(define sub (cantor (/ d 3)))
                 (define wht (rectangle (/ d 3) HEIGHT "solid" "white"))
                 (define org (rectangle d HEIGHT "solid" "blue"))]
           (above org SEP
                  (beside sub wht sub)))]))

;; cantor with P
;; produces cantor set with p% whitespace (calling with 1/3 will produce same image) 

(define (cantorp d p)
  (cond [(<= d CUTOFF) (rectangle d HEIGHT "solid" "blue")]
        [else
         (local [(define sub (cantorp (/ (- d (* d p)) 2) p))
                 (define wht (rectangle (* d p) HEIGHT "solid" "white"))
                 (define org (rectangle d HEIGHT "solid" "blue"))]
           (above org SEP
                  (beside sub wht sub)))]))
