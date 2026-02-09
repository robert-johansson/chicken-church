;;; social-cognition.scm — Bayesian Social Cognition
;;;
;;; A Church program that models how we infer people's hidden personality
;;; traits from observed behavior, using a generative model of actions.
;;;
;;; Exercises many Church features: ERPs, mem, rejection-query, mh-query,
;;; enumeration-query, DPmem, and visualization.
;;;
;;; Compile:  csc -O2 probmods-examples/social-cognition.scm -o social-cognition
;;; Run:      ./social-cognition
;;;
;;; Or interpreted:  csi -q -s probmods-examples/social-cognition.scm

(import (chicken process-context))
(import (chicken condition))
(import (chicken time))

(include "church/church.scm")
(include "church/viz.scm")

;; ============================================================
;; Part 1: Exact enumeration — simple trait inference
;;
;; A person is either generous or stingy. Generous people share
;; 80% of the time, stingy people share 20% of the time.
;; We observe someone sharing — what do we infer?
;; ============================================================

(display "========================================") (newline)
(display "Part 1: Trait inference (enumeration)") (newline)
(display "========================================") (newline)
(newline)

(let ()
  (define result
    (enumeration-query
     (define generous (flip 0.5))
     (define share-prob (if generous 0.8 0.2))
     (define shared (flip share-prob))
     generous
     (condition shared)))
  (barplot (list (car result) (cadr result)) "P(generous | shared)"))

;; What if we observe sharing 3 times in a row?
(let ()
  (define result
    (enumeration-query
     (define generous (flip 0.5))
     (define share-prob (if generous 0.8 0.2))
     generous
     (condition (and (flip share-prob) (flip share-prob) (flip share-prob)))))
  (barplot (list (car result) (cadr result)) "P(generous | shared 3 times)"))

;; ============================================================
;; Part 2: Rejection sampling — causal attribution
;;
;; Two possible causes for a kind act: genuine kindness or
;; ulterior motive. Infer the likely cause given observations.
;; ============================================================

(display "========================================") (newline)
(display "Part 2: Causal attribution (rejection)") (newline)
(display "========================================") (newline)
(newline)

(let ()
  (define (infer-motive)
    (rejection-query
     (define kind-person (flip 0.6))
     (define has-motive (flip 0.3))
     (define did-kind-act (or (and kind-person (flip 0.9))
                              (and has-motive (flip 0.7))
                              (flip 0.05)))
     (list kind-person has-motive)
     (condition did-kind-act)))
  (hist (repeat 500 infer-motive)
        "Inferred cause of kind act (kind? motive?)"))

;; ============================================================
;; Part 3: MH inference — continuous trait model
;;
;; People have a continuous "friendliness" trait drawn from a
;; Gaussian. Friendliness determines probability of smiling,
;; helping, and greeting. We observe some behaviors and infer
;; the trait value.
;; ============================================================

(display "========================================") (newline)
(display "Part 3: Continuous traits (MH)") (newline)
(display "========================================") (newline)
(newline)

;; Sigmoid function to map continuous trait to probability
(define (sigmoid x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

;; Infer friendliness given: smiled=yes, helped=yes, greeted=no
(let ()
  (define samples
    (mh-query 2000 5
     (define friendliness (gaussian 0 1))
     (define smile-prob (sigmoid (* 2.0 friendliness)))
     (define help-prob (sigmoid (* 1.5 friendliness)))
     (define greet-prob (sigmoid friendliness))
     friendliness
     (condition (and (flip smile-prob)
                     (flip help-prob)
                     (not (flip greet-prob))))))
  (density samples "Friendliness posterior (smiled, helped, didn't greet)"))

;; Compare: infer friendliness given all positive observations
(let ()
  (define samples
    (mh-query 2000 5
     (define friendliness (gaussian 0 1))
     (define smile-prob (sigmoid (* 2.0 friendliness)))
     (define help-prob (sigmoid (* 1.5 friendliness)))
     (define greet-prob (sigmoid friendliness))
     friendliness
     (condition (and (flip smile-prob)
                     (flip help-prob)
                     (flip greet-prob)))))
  (density samples "Friendliness posterior (smiled, helped, greeted)"))

;; ============================================================
;; Part 4: Persistent traits with mem — multi-person model
;;
;; Each person has a stable personality. We model a social
;; network and infer who is trustworthy from observed
;; cooperation patterns.
;; ============================================================

(display "========================================") (newline)
(display "Part 4: Social network (mem + MH)") (newline)
(display "========================================") (newline)
(newline)

(let ()
  (define samples
    (mh-query 1000 10

     ;; Each person has a persistent trustworthiness
     (define trustworthy (mem (lambda (person) (flip 0.5))))

     ;; Cooperation depends on both parties' trustworthiness
     (define (cooperates? a b)
       (if (trustworthy a)
           (flip 0.85)  ; trustworthy people usually cooperate
           (flip 0.15))) ; untrustworthy people rarely do

     ;; Query: is Alice trustworthy?
     (trustworthy 'alice)

     ;; Evidence: Alice cooperated with Bob, Carol, and Dave
     ;; But Bob did NOT cooperate with Carol
     (condition (and (cooperates? 'alice 'bob)
                     (cooperates? 'alice 'carol)
                     (cooperates? 'alice 'dave)
                     (not (cooperates? 'bob 'carol))))))

  (hist samples "P(Alice is trustworthy | evidence)"))

;; ============================================================
;; Part 5: DPmem — discovering personality types
;;
;; Using Dirichlet Process memoization to cluster people into
;; an unknown number of personality types. Each type has a
;; characteristic behavior profile.
;; ============================================================

(display "========================================") (newline)
(display "Part 5: Personality clustering (DPmem)") (newline)
(display "========================================") (newline)
(newline)

(let ()
  ;; DPmem assigns people to clusters (personality types)
  ;; with a Chinese Restaurant Process
  (define get-type (DPmem 1.0 (lambda (person) (flip 0.5))))

  ;; Sample 10 people's types
  (define people '(alice bob carol dave eve frank grace hank iris jack))
  (define types (map get-type people))

  (display "Person -> Type assignments (DPmem, alpha=1.0):") (newline)
  (for-each (lambda (p t)
              (display "  ")
              (display p) (display ": type-")
              (display (if t "A" "B"))
              (newline))
            people types)
  (newline)

  ;; Count cluster sizes
  (let ((type-a (length (filter identity types)))
        (type-b (length (filter not types))))
    (display (list "Type A:" type-a "Type B:" type-b)) (newline)
    (newline)))

;; Repeat to show the CRP gives different clusterings
(let ()
  (define cluster-sizes
    (repeat 500 (lambda ()
      (define get-type (DPmem 1.5 (lambda (person)
                                     (uniform-draw '(extrovert introvert ambivert)))))
      (define people '(p1 p2 p3 p4 p5 p6 p7 p8))
      (define types (map get-type people))
      (length (unique types)))))
  (hist cluster-sizes "Number of personality types discovered (8 people, alpha=1.5)"))

;; ============================================================
;; Part 6: Theory of mind — recursive social reasoning
;;
;; Alice is choosing between two restaurants. Bob recommends
;; restaurant A. We model Bob as a rational agent who might
;; be helpful or self-interested, and infer his true preference.
;; ============================================================

(display "========================================") (newline)
(display "Part 6: Theory of mind (nested query)") (newline)
(display "========================================") (newline)
(newline)

(let ()
  (define (infer-bob-preference)
    (rejection-query
     ;; Bob's true preference (unknown to us)
     (define bob-prefers-A (flip 0.5))
     ;; Is Bob helpful or self-interested?
     (define bob-helpful (flip 0.7))
     ;; Bob's recommendation strategy
     (define bob-recommends-A
       (if bob-helpful
           bob-prefers-A              ; helpful: recommends what he likes
           (not bob-prefers-A)))      ; self-interested: sends us elsewhere

     ;; What does Bob really prefer?
     bob-prefers-A

     ;; We observed: Bob recommends restaurant A
     (condition bob-recommends-A)))

  (hist (repeat 1000 infer-bob-preference)
        "P(Bob prefers A | Bob recommends A)"))

;; More complex: what if we also know the restaurants' qualities?
(let ()
  (define (infer-about-bob)
    (rejection-query
     ;; Restaurant qualities
     (define quality-A (if (flip 0.6) 'good 'bad))
     (define quality-B (if (flip 0.4) 'good 'bad))

     ;; Bob knows the qualities and has a preference
     (define bob-prefers-A (flip (if (equal? quality-A 'good) 0.8 0.2)))
     (define bob-helpful (flip 0.7))

     (define bob-recommends-A
       (if bob-helpful bob-prefers-A (not bob-prefers-A)))

     ;; Query both Bob's helpfulness and restaurant A's quality
     (list bob-helpful quality-A)

     ;; Bob recommends A, and we independently heard A might be good
     (condition (and bob-recommends-A
                     (equal? quality-A 'good)))))

  (hist (repeat 1000 infer-about-bob)
        "P(helpful?, quality-A | recommends A, A is good)"))

;; ============================================================
;; Part 7: Tug of war with learning — compare prior vs posterior
;; ============================================================

(display "========================================") (newline)
(display "Part 7: Tug of war learning") (newline)
(display "========================================") (newline)
(newline)

;; Prior on strength
(let ()
  (define prior-samples
    (repeat 2000 (lambda () (gaussian 0 1))))
  (density prior-samples "Strength prior (Gaussian 0,1)"))

;; Posterior: Alice beat Bob twice, what's Alice's strength?
(let ()
  (define samples
    (mh-query 2000 10
     (define alice-strength (gaussian 0 1))
     (define bob-strength (gaussian 0 1))

     (define (team-pull person)
       (if (flip 0.25)
           (/ (if (equal? person 'alice) alice-strength bob-strength) 2)
           (if (equal? person 'alice) alice-strength bob-strength)))

     alice-strength

     (condition (and (> (team-pull 'alice) (team-pull 'bob))
                     (> (team-pull 'alice) (team-pull 'bob))))))

  (density samples "Alice's strength (after beating Bob twice)"))

;; ============================================================
;; Timing summary
;; ============================================================

(display "========================================") (newline)
(display "All examples completed successfully.") (newline)
(display "========================================") (newline)
