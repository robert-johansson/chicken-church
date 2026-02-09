;;; 02-generative-models.scm — ProbMods Chapter 2: Generative Models
;;;
;;; Usage: csi -q -s probmods-examples/02-generative-models.scm
;;;
;;; Examples from https://probmods.org/generative-models.html
;;; Blocks that depend on the JS physics engine (worldWidth, animatePhysics,
;;; runPhysics) are omitted. Exercise stubs with "..." are omitted.

(include "church/church.scm")
(include "church/viz.scm")

;; ============================================================
;; Basic XRPs
;; ============================================================

;; A single flip — run multiple times to see different samples
(display "--- Single flip ---") (newline)
(display (flip)) (newline)
(newline)

;; Histogram of 1000 flips
(hist (repeat 1000 flip) "Flips")

;; Product of two Gaussians
(display "--- Gaussian product ---") (newline)
(display (* (gaussian 0 1) (gaussian 0 1))) (newline)
(newline)

;; Density of repeated Gaussian products
(define two-gaussians (lambda () (* (gaussian 0 1) (gaussian 0 1))))
(density (repeat 100 two-gaussians) "Product of two Gaussians")

;; Noisy double
(define noisy-double (lambda (x) (if (flip) x (+ x x))))
(display "--- Noisy double of 3 ---") (newline)
(display (noisy-double 3)) (newline)
(newline)

;; ============================================================
;; Example: Flipping Coins
;; ============================================================

;; Fair coin
(define fair-coin (lambda () (if (flip 0.5) 'h 't)))
(hist (repeat 20 fair-coin) "fair coin")

;; Trick coin
(define trick-coin (lambda () (if (flip 0.95) 'h 't)))
(hist (repeat 20 trick-coin) "trick coin")

;; Higher-order coin maker
(define (make-coin weight) (lambda () (if (flip weight) 'h 't)))
(set! fair-coin (make-coin 0.5))
(set! trick-coin (make-coin 0.95))
(define bent-coin (make-coin 0.25))

(hist (repeat 20 fair-coin) "20 fair coin flips")
(hist (repeat 20 trick-coin) "20 trick coin flips")
(hist (repeat 20 bent-coin) "20 bent coin flips")

;; Bending a coin
(define (bend coin)
  (lambda () (if (equal? (coin) 'h)
                 ((make-coin 0.7))
                 ((make-coin 0.1)))))

(set! fair-coin (make-coin 0.5))
(set! bent-coin (bend fair-coin))
(hist (repeat 100 bent-coin) "bent coin")

;; Distribution of heads counts from weighted coin
(let ()
  (define make-flip-coin (lambda (weight) (lambda () (flip weight))))
  (define coin (make-flip-coin 0.8))
  (define data (repeat 1000 (lambda () (sum (map (lambda (x) (if x 1 0)) (repeat 10 coin))))))
  (hist data "Distribution of coin flips"))

;; ============================================================
;; Example: Causal Models in Medical Diagnosis
;; ============================================================

;; Simple version
(let ()
  (define lung-cancer (flip 0.01))
  (define cold (flip 0.2))
  (define cough (or cold lung-cancer))
  (display "--- Simple medical diagnosis ---") (newline)
  (display (list "cough:" cough)) (newline)
  (newline))

;; Complex version
(let ()
  (define lung-cancer (flip 0.01))
  (define TB (flip 0.005))
  (define stomach-flu (flip 0.1))
  (define cold (flip 0.2))
  (define other (flip 0.1))

  (define cough
    (or (and cold (flip 0.5))
        (and lung-cancer (flip 0.3))
        (and TB (flip 0.7))
        (and other (flip 0.01))))

  (define fever
    (or (and cold (flip 0.3))
        (and stomach-flu (flip 0.5))
        (and TB (flip 0.1))
        (and other (flip 0.01))))

  (define chest-pain
    (or (and lung-cancer (flip 0.5))
        (and TB (flip 0.5))
        (and other (flip 0.01))))

  (define shortness-of-breath
    (or (and lung-cancer (flip 0.5))
        (and TB (flip 0.2))
        (and other (flip 0.01))))

  (display "--- Complex medical diagnosis ---") (newline)
  (display (list "cough" cough
                 "fever" fever
                 "chest-pain" chest-pain
                 "shortness-of-breath" shortness-of-breath))
  (newline)
  (newline))

;; ============================================================
;; Prediction, Simulation, and Probabilities
;; ============================================================

;; Two flips
(display "--- Two flips ---") (newline)
(display (list (flip) (flip))) (newline)
(newline)

;; Histogram of random pairs
(define (random-pair) (list (flip) (flip)))
(hist (repeat 1000 random-pair) "return values")

;; Product rule
(let ()
  (define A (flip))
  (define B (flip))
  (define C (list A B))
  (display "--- Product rule ---") (newline)
  (display C) (newline)
  (newline))

;; Dependent flips
(let ()
  (define A (flip))
  (define B (flip (if A 0.3 0.7)))
  (display "--- Dependent flips ---") (newline)
  (display (list A B)) (newline)
  (newline))

;; Sum rule / marginalization
(display "--- or of two flips ---") (newline)
(display (or (flip) (flip))) (newline)
(newline)

;; ============================================================
;; Stochastic Recursion
;; ============================================================

(define (geometric p)
  (if (flip p)
      0
      (+ 1 (geometric p))))

(hist (repeat 1000 (lambda () (geometric 0.6))) "Geometric of 0.6")

;; ============================================================
;; Persistent Randomness: mem
;; ============================================================

;; Without mem — eye color changes each call
(define (eye-color person) (uniform-draw '(blue green brown)))
(display "--- Eye color without mem ---") (newline)
(display (list
          (eye-color 'bob)
          (eye-color 'alice)
          (eye-color 'bob)))
(newline)
(newline)

;; Equality of two flips vs two memoized flips
(display "--- Two flips equal? ---") (newline)
(display (equal? (flip) (flip))) (newline)
(newline)

(define mem-flip (mem flip))
(display "--- Two memoized flips equal? ---") (newline)
(display (equal? (mem-flip) (mem-flip))) (newline)
(newline)

;; With mem — eye color is persistent per person
(set! eye-color
  (mem (lambda (person) (uniform-draw '(blue green brown)))))
(display "--- Eye color with mem ---") (newline)
(display (list
          (eye-color 'bob)
          (eye-color 'alice)
          (eye-color 'bob)))
(newline)
(newline)

;; Memoized coin flips — infinite sequence, each persistent
(define flip-n (mem (lambda (n) (flip))))
(display "--- Memoized flip sequence ---") (newline)
(display (list (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548))
               (list (flip-n 1) (flip-n 12) (flip-n 47) (flip-n 1548))))
(newline)
(newline)

;; ============================================================
;; Example: Bayesian Tug of War
;; ============================================================

(define strength (mem (lambda (person) (gaussian 0 1))))

(define lazy (lambda (person) (flip 0.25)))

(define (pulling person)
  (if (lazy person) (/ (strength person) 2) (strength person)))

(define (total-pulling team)
  (sum (map pulling team)))

(define (winner team1 team2)
  (if (< (total-pulling team1) (total-pulling team2)) team2 team1))

(display "--- Tug of war tournament ---") (newline)
(display (list "Tournament results:"
               (winner '(alice bob) '(sue tom))
               (winner '(alice bob) '(sue tom))
               (winner '(alice sue) '(bob tom))
               (winner '(alice sue) '(bob tom))
               (winner '(alice tom) '(bob sue))
               (winner '(alice tom) '(bob sue))))
(newline)
(newline)

;; ============================================================
;; Exercises
;; ============================================================

;; Exercise 1: Three programs with the same marginal distribution
(display "--- Exercise 1: Three equivalent programs ---") (newline)
(display "1a: ") (display (if (flip) (flip 0.7) (flip 0.1))) (newline)
(display "1b: ") (display (flip (if (flip) 0.7 0.1))) (newline)
(display "1c: ") (display (flip 0.4)) (newline)
(newline)

;; Exercise 2: define vs define-thunk
(let ()
  (define foo (flip))
  (display "--- Exercise 2a: (define foo (flip)) ---") (newline)
  (display (list foo foo foo)) (newline)
  (newline))

(let ()
  (define (foo) (flip))
  (display "--- Exercise 2b: (define (foo) (flip)) ---") (newline)
  (display (list (foo) (foo) (foo))) (newline)
  (newline))

;; Exercise 3: Medical diagnosis with functions (no mem)
(let ()
  (define (lung-cancer person) (flip 0.01))
  (define (cold person) (flip 0.2))
  (define (cough person) (or (cold person) (lung-cancer person)))
  (display "--- Exercise 3: Diagnosis without mem ---") (newline)
  (display (list (cough 'bob) (cough 'alice))) (newline)
  (newline))

;; Exercise 4: Bend a coin — work through evaluation
(let ()
  (define (make-coin weight) (lambda () (if (flip weight) 'h 't)))
  (define (bend coin)
    (lambda () (if (equal? (coin) 'h)
                   ((make-coin 0.7))
                   ((make-coin 0.1)))))
  (define fair-coin (make-coin 0.5))
  (define bent-coin (bend fair-coin))
  (hist (repeat 100 bent-coin) "Exercise 4: bent coin"))

;; Exercise 5: Modified tug of war
(let ()
  (define strength (mem (lambda (person) (if (flip) 5 10))))
  (define lazy (lambda (person) (flip (/ 1 3))))
  (define (total-pulling team)
    (sum
     (map (lambda (person) (if (lazy person) (/ (strength person) 2) (strength person)))
          team)))
  (define (winner team1 team2)
    (if (< (total-pulling team1) (total-pulling team2)) team2 team1))

  (display "--- Exercise 5: Modified tug of war ---") (newline)
  (display "expr 1: ") (display (winner '(alice) '(bob))) (newline)
  (display "expr 2: ") (display (equal? '(alice) (winner '(alice) '(bob)))) (newline)
  (display "expr 3: ") (display (and (equal? '(alice) (winner '(alice) '(bob)))
                                     (equal? '(alice) (winner '(alice) '(fred))))) (newline)
  (display "expr 4: ") (display (and (equal? '(alice) (winner '(alice) '(bob)))
                                     (equal? '(jane) (winner '(jane) '(fred))))) (newline)
  (newline))

;; Exercise 6: Geometric distribution
(let ()
  (define (geometric p)
    (if (flip p)
        0
        (+ 1 (geometric p))))
  (hist (repeat 1000 (lambda () (geometric 0.6))) "Exercise 6: Geometric distribution"))
