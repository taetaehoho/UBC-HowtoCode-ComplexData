;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt

(define-struct user (name verified? follows))

(define H1 (make-user "A" true (list (make-user "B" false empty))))


(define H2 
  (shared ((-0- (make-user "A" true (list (make-user "B" false (list -0-))))))
    -0-)) 


(define H3
  (shared ((-A- (make-user "A" true (list -B-)))
           (-B- (make-user "B" true (list -C-)))
           (-C- (make-user "C" true (list -A-))))
    -A-))

(define H4
  (shared ((-A- (make-user "A" true (list -B- -D-)))
           (-B- (make-user "B" true (list -C- -E-)))
           (-C- (make-user "C" true (list -B-)))
           (-D- (make-user "D" true (list -E-)))
           (-E- (make-user "E" true (list -F- -A-)))
           (-F- (make-user "F" true (list))))
    -A-))


;; data definition - user
;; user is (make-user (string boolean listof user)

(define TERRY (make-user "Terry" true (list "Hoing bo")))
(define EMILY (make-user "Emily" false (list "Hoing bo")))
(define HOING (make-user "Hoing" true (list "Terry" "Emily")))

(define C1 (shared ((-A- (make-user "A" true (list -B-)))
                    (-B- (make-user "B" false (list -A- -C-)))
                    (-C- (make-user "C" true (list -B-))))
             -A-))

;; template - tail recursive on graph (context preserving, worklist accumulator)
#;
(define (fn-for-chirper c0)
  ;; todo is (list of user) a worklist accumulator (what we have to do left)
  ;; visited (list of name) is a context preserving accumulator 
  (local [(define (fn-for-user u todo visited)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-follows u) todo)
                            (cons (user-name u) visited))))
          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-user c0 ... ...)))

;; most follows
;; User -> User
;; produce user with the most followers
(check-expect (fn-for-chirper (shared ((-A- (make-user "A" true (list -B-)))
                                       (-B- (make-user "B" false (list -A- -C-)))
                                       (-C- (make-user "C" true (list -B-))))
                                -A-))
              (shared ((-A- (make-user "A" true (list -B-)))
                       (-B- (make-user "B" false (list -A- -C-)))
                       (-C- (make-user "C" true (list -B-))))
                -B-))

(check-expect (fn-for-chirper (shared ((-A- (make-user "A" true (list -B-)))
                                       (-B- (make-user "B" false (list -A- -C-)))
                                       (-C- (make-user "C" true (list -A-))))
                                -A-))
              (shared ((-A- (make-user "A" true (list -B-)))
                       (-B- (make-user "B" false (list -A- -C-)))
                       (-C- (make-user "C" true (list -A-))))
                -A-))

(check-expect (fn-for-chirper (shared ((-A- (make-user "A" true (list -C-)))
                                       (-B- (make-user "B" false (list -A- -C-)))
                                       (-C- (make-user "C" true (list -B-))))
                                -A-))
              (shared ((-A- (make-user "A" true (list -C-)))
                       (-B- (make-user "B" false (list -A- -C-)))
                       (-C- (make-user "C" true (list -B-))))
                -C-))

(check-expect (fn-for-chirper H1) (first (user-follows H1)))

(check-expect (fn-for-chirper H4) 
              (shared ((-A- (make-user "A" true (list -B- -D-)))
                       (-B- (make-user "B" true (list -C- -E-)))
                       (-C- (make-user "C" true (list -B-)))
                       (-D- (make-user "D" true (list -E-)))
                       (-E- (make-user "E" true (list -F- -A-)))
                       (-F- (make-user "F" true (list))))
                -E-))


;(define (most-followers user) user)

(define (fn-for-chirper c0)
  ;; todo is (list of user) a worklist accumulator (what we have to do left)
  ;; visited (list of name) is a context preserving accumulator
  ;; rsf is list of rles (list (user n) (user n) (user n))
  (local [(define-struct rle (u n))
          ;; rle keeps track of the user and the number of followers user has 
          
          (define (fn-for-user u todo visited rsf)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited rsf)
                (fn-for-lou (append (user-follows u) todo)
                            (cons (user-name u) visited)
                            (create-list u rsf))))
          (define (fn-for-lou todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited
                                rsf)]))
          ;; add one user to rsf
          (define (create-list u rsf)
            (foldr create-nuser rsf (user-follows u)))
          ;; add one new follower to rsf
          
          (define (create-nuser u lorle)
            (cond [(empty? lorle) (list (make-rle u 1))]
                  [else
                   (if (string=? (user-name u) (user-name (rle-u (first lorle))))
                       (cons (make-rle u
                                       (add1 (rle-n (first lorle))))
                             (rest lorle))
                       (cons (first lorle)
                             (create-nuser u (rest lorle))))]))

          (define (get-max rsf)
            (rle-u
             (foldr (lambda (e1 e2)
                      (if (> (rle-n e1) (rle-n e2))
                          e1
                          e2))
                    (first rsf)
                    (rest rsf))))]
    
    (get-max (fn-for-user c0 empty empty empty))))

