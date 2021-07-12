;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |d. strictly-decreasing-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; strictly-decreasing-starter.rkt


;; lom -> boolean
;; produce true if lon strictly decreasing false if otherwise
(check-expect (sd? (list 1 2)) false)
(check-expect (sd? (list 2 1)) true)
(check-expect (sd? (list 3 2 1)) true)
(check-expect (sd? (list 5 4 3 2 1)) true)
(check-expect (sd? (list 5 4 3 1 2)) false)
(check-expect (sd? (list 4 2 1 3)) false)

;(define (sd? lon) true)

(define (sd? lon1)
  ;; acc:
  ;; (sd? (list 3 2 1)) ; outercall 
  ;; (sd? (list 3 2 1) ?)
  ;; (sd? (list   2 1) 3) ; the acc is the previous first lon
  ;; (sd? (list     1) 2)
 
  (local [(define (sd? lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if (> acc (first lon))
                       (sd? (rest lon) (first lon))
                       false)]))]
    (sd? (rest lon1) (first lon1))))