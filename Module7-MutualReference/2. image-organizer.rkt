;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2. image-organizer-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; image-organizer-starter.rkt

;; =================
;; Constants:

(define BLANK (square 0 "solid" "white"))
(define LABEL-SIZE 24)
(define LABEL-COLOR "black")

;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

#;
(define (fn-for-dir d)
  (... (dir-name d)
       (fn-for-lod (dir-sub-dirs d))
       (fn-for-loi (dir-images d))))

;; Dir is compound: 3 types
;; - atomic distinct: name
;; - mutual reference: sub-dirs is listofdirs
;; - reference: images is listofimages

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

;; listofdir is one of:
;; - compound 2 types:
;; - atomic distinct: empty
;; - reference: (fist lod) is dir
;; - self-reference: (rest lod) is listofdir
                     
;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest-loi)))]))
;;listofimage is one of:
;; compound: 2 types
;; - atomic distinct: empty
;; - 

(define I1 (square 10 "solid" "red"))
(define I2 (square 10 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; =================
;; Functions:
  
;; dir -> integer
;; listofdir -> integer??
;; listofimage -> integer??

(check-expect (total-area--dir D4) 200)
(check-expect (total-area--dir D5) (* 13 14))
(check-expect (total-area--lod (list D4 D5)) (+ 200 (* 13 14)))
(check-expect (total-area--lod empty) 0)

(define (total-area--dir d)
  (+   (total-area--lod (dir-sub-dirs d))
       (total-area--loi (dir-images d))))

(define (total-area--lod lod)
  (cond [(empty? lod) 0]
        [else
         (+  (total-area--dir (first lod))
              (total-area--lod (rest lod)))]))

(define (total-area--loi loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-width (first loi)) (image-height (first loi)))
              (total-area--loi (rest loi)))]))


;; dir -> image
;; listofdir -> listofimage
;; listofimage -> image
;; purpose. take directory and produce series of images that it contains

(check-expect (render--dir D4) (beside (text "D4" LABEL-SIZE LABEL-COLOR)
                                (beside I1 I2 BLANK)))

(check-expect (render--loi (list I1 I2)) (beside I1 I2 BLANK))
(check-expect (render--loi empty) BLANK)
(check-expect (render--lod empty) BLANK)

(check-expect (render--dir D6)
              (beside (text "D6" LABEL-SIZE LABEL-COLOR)
                      (above (render--dir D4)
                             (render--dir D5)
                             BLANK)))

(define (render--dir d)
  (beside (text (dir-name d) LABEL-SIZE LABEL-COLOR)
         (above (render--loi (dir-images d))
         (render--lod (dir-sub-dirs d)))))

(define (render--loi loi)
  (cond [(empty? loi) BLANK]
        [else
         (beside (first loi)
                 (render--loi (rest loi)))]))

(define (render--lod lod)
  (cond [(empty? lod) BLANK]
        [else
         (above (render--dir (first lod))
              (render--lod (rest lod)))]))

