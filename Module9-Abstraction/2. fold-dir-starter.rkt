;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2. fold-dir-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; fold-dir-starter.rkt

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:

;; (string Y Z -> X) (X Y -> Y) (image Z -> Z) Y Z dir -> X
;; artificial fold function for dir 
(check-expect (fold-dir make-dir cons cons empty empty D6) D6)
(check-expect (local [(define (c1 n lod loi) (cons n lod))
                      (define (c2 lod rlod) (append lod rlod))
                      (define (c3 img rloi) empty)]
                        (fold-dir c1 c2 c3 empty empty D6))
              (list "D6" "D4" "D5"))

(define (fold-dir c1 c2 c3 b1 b2 dir)
  (local [(define (fn-for-dir dir)            ; -> X
          (c1  (dir-name dir)
               (fn-for-lod (dir-sub-dirs dir))
               (fn-for-loi (dir-images dir))))

        (define (fn-for-lod lod)              ; -> Y
          (cond [(empty? lod) b1]
                [else
                 (c2  (fn-for-dir (first lod))
                      (fn-for-lod (rest lod)))]))

        (define (fn-for-loi loi)             ; -> Z
          (cond [(empty? loi) b2]
                [else
                 (c3 (first loi)
                     (fn-for-loi (rest loi)))]))]
  (fn-for-dir dir)))
                    





;; Dir -> integer
;; consumers Dir and produces number of images in directory and subdirectory
(check-expect (number-of-images D6) 3)
(check-expect (number-of-images D4) 2)

;(define (number-of-images dir) 0)

(define (number-of-images dir)
  (local [(define (c1 n rlod rloi) (+ rloi rlod))
          (define (c2 rdir rlod) (+ rlod rdir))
          (define (c3 img rloi) (+ 1 rloi))]
    (fold-dir c1 c2 c3 0 0 dir)))

#;
(define (fold-dir c1 c2 c3 b1 b2 dir)
  (local [(define (fn-for-dir dir)            ; -> X
          (c1  (dir-name dir)
               (fn-for-lod (dir-sub-dirs dir))       ; rlod - total images in subdir should be
               (fn-for-loi (dir-images dir))))       ;output int (total images in the loi)

        (define (fn-for-lod lod)              ; -> Y
          (cond [(empty? lod) b1]
                [else
                 (c2  (fn-for-dir (first lod))    ; output int (total images in loi) + total int img in first sub-dir
                      (fn-for-lod (rest lod)))])) ; rlod - total image in rest of subdir

        (define (fn-for-loi loi)             ; -> Z
          (cond [(empty? loi) b2]
                [else
                 (c3 (first loi)                   ; add output int recursively 
                     (fn-for-loi (rest loi)))]))]))  ; output int base is 0
            



;; dir string -> boolean
;; looks in all dirs and sub-dirs for string if its there produce true else false

(check-expect (lookstring "D6" D6) true)
(check-expect (lookstring "D4" D6) true)
(check-expect (lookstring "D5" D6) true)
(check-expect (lookstring "D6" D5) false)

(define (lookstring s dir)
  (local [(define (c1 n rlod rloi)
            (or (string=? n s)
                  rlod))
          (define (c2 rdir rlod) (or rdir rlod))
          (define (c3 img rloi) false)]
    (fold-dir c1 c2 c3 false false dir)))

#;
(define (fold-dir c1 c2 c3 b1 b2 dir)
  (local [(define (fn-for-dir dir)            ; -> X
          (c1  (dir-name dir)
               (fn-for-lod (dir-sub-dirs dir))       ; if string=? s n are not same look for it in here 
               (fn-for-loi (dir-images dir))))       ; irrelevant

        (define (fn-for-lod lod)              ; -> Y
          (cond [(empty? lod) b1]
                [else
                 (c2  (fn-for-dir (first lod))       ; if name is first directory - true or 
                      (fn-for-lod (rest lod)))]))    ; 

        (define (fn-for-loi loi)             ; -> Z
          (cond [(empty? loi) b2]
                [else
                 (c3 (first loi)                     ; irrelevant 
                     (fn-for-loi (rest loi)))]))]))  ; irrelevant
            





