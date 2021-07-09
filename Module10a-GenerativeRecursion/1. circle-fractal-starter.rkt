;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1. circle-fractal-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; circle-fractal-starter.rkt

;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIV 5)

;; function design
;; integer -> image
;; purpose. creates fractal circle of size integer
(check-expect (frac TRIV) (circle TRIV "solid" "blue"))
(check-expect (frac (/ (* TRIV 5) 2)) (local [(define sub (circle (* (/ (* TRIV 5) 2) STEP) "solid" "blue"))
                                              (define org (circle (/ (* TRIV 5) 2) "solid" "blue"))]
                                      (above sub
                                             (beside (rotate 90 sub) org (rotate -90 sub))
                                                     (rotate 180 sub))))

(check-expect (frac (/ TRIV STEP STEP)) (local [(define sub (leaf (/ TRIV STEP)))
                                              (define org (circle (/ TRIV STEP STEP) "solid" "blue"))]
                                      (above sub
                                             (beside (rotate 90 sub) org (rotate -90 sub))
                                                     (rotate 180 sub))))


;(define (frac n) (square 0 "solid" "white"))

(define (frac n)
  (cond [(<= n TRIV) (circle n "solid" "blue")]
        [else (local [(define sub (leaf (* n STEP)))
                         (define org (circle n "solid" "blue"))]
                   (above sub
                        (beside (rotate 90 sub) org (rotate -90 sub))
                             (rotate 180 sub)))]))


;; function design
;; integer -> image
;; purpose create the top leaf of the fractal pattern
(check-expect (leaf TRIV) (circle TRIV "solid" "blue"))
(check-expect (leaf (/ (* TRIV 5) 2)) (local [(define sub (circle (* (/ (* TRIV 5) 2) STEP) "solid" "blue"))
                                              (define org (circle (/ (* TRIV 5) 2) "solid" "blue"))]
                                      (above sub
                                             (beside (rotate 90 sub) org (rotate 270 sub)))))

;(define (frac s) (square 0 "solid" "white"))

(define (leaf d)
  (cond [(<= d TRIV) (circle d "solid" "blue")]
        [else
         (local [(define sub (leaf (* d STEP)))
                 (define org (circle d "solid" "blue"))]
         (above sub
                (beside (rotate 90 sub) org (rotate 270 sub))))]))

;; base case (<= d TRIV)
;; reduction step: (* d STEP)
;; termination argument
;; - if d > 0 then as d * STEP will converge to 0 / TRIV