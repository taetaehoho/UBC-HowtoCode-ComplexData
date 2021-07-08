;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1. wide-only-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; wide-only-starter.rkt

(define IMG1 (rectangle 10 20 "solid" "black"))
(define IMG2 (rectangle 20 10 "solid" "black"))
(define IMG3 (square 20 "solid" "black"))
(define IMG4 (rectangle 100 20 "solid" "red"))

;;listofimages is one of
;; - empty
;; - (cons image loi)

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) empty]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; function
;; wide-only

;; loi -> loi

(check-expect (wide-only (list IMG1 IMG2 IMG3 IMG4)) (list IMG2 IMG4))

(define (wide-only loi) (local [(define (wide? img)
                                  (< (image-height img) (image-width img)))]
                         (filter wide? loi)))