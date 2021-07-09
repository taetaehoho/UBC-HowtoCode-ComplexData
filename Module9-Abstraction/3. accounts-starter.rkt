;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |3. accounts-starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; accounts-starter.rkt

(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))



;; (Account -> Boolean) Accounts -> Accounts
;; remove accounts that satisfy boolean pred
(check-expect (local [(define (no? act) false)]
                     (remove-acts no? ACT1)) 
               ACT1)

(check-expect (local [(define (yes? act) true)]
              (remove-acts yes? (make-node 14 "Mr. Impossible" -9 false false)))
              false)

(check-expect (local [(define (prof? act) (has-prefix? "Dr." (node-name act)))]
              (remove-acts prof? (make-node 100 "Dr. Hoing" 200 false false))) false)



(define (remove-acts pred act)
  (cond [(false? act) false]
        [else
         (if (pred act)
             (join (remove-acts pred (node-l act))
                   (remove-acts pred (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-acts pred (node-l act))
                        (remove-acts pred (node-r act))))]))


;; Accounts -> Accounts
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))


(define (remove-profs act)
  (cond [(false? act) false]
        [else
         (if (has-prefix? "Prof." (node-name act))
             (join (remove-profs (node-l act))
                   (remove-profs (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-profs (node-l act))
                        (remove-profs (node-r act))))]))



;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))

;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))

;; remove-odd-characters
;; purpose. remove node where any accoutn holder has odd number name characters
(check-expect (remove-odd (make-node 1000 "hoing" 42 false false)) false)
(check-expect (remove-odd (make-node 1000 "hoin" 42 false false)) (make-node 1000 "hoin" 42 false false))

(define (remove-odd act) (local [(define (odd! act)
                                 (odd? (string-length (node-name act))))]
                         (remove-acts odd! act)))


;; (Integer String Integer X X -> X) X Act -> X 
;; abstract fold function for accounts

(define (fold-act fn b act) ; X
  (cond [(false? act) b]
        [else
         (fn (node-id act)
             (node-name act)
             (node-bal act)
             (fold-act fn b (node-l act))
             (fold-act fn b (node-r act)))]))

;; charge-fee
;; Account -> Account
;; decrements balance of account by 3
(check-expect (charge-fee (make-node 14 "Mr. Impossible" -9 false false)) (make-node 14 "Mr. Impossible" -12 false false))
(check-expect (charge-fee ACT42) (make-node 42 "Mr. Mom" -82
                                            (make-node 27 "Mr. Selatcia" 37
                                                       (make-node 14 "Mr. Impossible" -12 false false)
                                                       false)
                                            (make-node 50 "Miss 604" 13 false false)))

;(define (charge-fee false) false)

(define (charge-fee act) (local [(define (fn id name bal l r)
                                 (make-node id name (- bal 3) l r))]
                         (fold-act fn false act))) 

