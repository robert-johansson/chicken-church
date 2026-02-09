;;; test/run-tests.scm — Comprehensive Church test suite
;;;
;;; Ported from webchurch/test/*.church

(include "church/church.scm")

(define tests-passed 0)
(define tests-failed 0)
(define test-failures '())

(define (check name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (set! test-failures (cons name test-failures))
        (display "FAIL: ") (display name) (newline)
        (display "  expected: ") (write expected) (newline)
        (display "  actual:   ") (write actual) (newline))))

(define (check-approx name expected actual tolerance)
  (if (< (abs (- (exact->inexact expected) (exact->inexact actual))) tolerance)
      (begin
        (set! tests-passed (+ tests-passed 1)))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (set! test-failures (cons name test-failures))
        (display "FAIL: ") (display name) (newline)
        (display "  expected: ~") (write expected) (display " +/- ") (write tolerance) (newline)
        (display "  actual:   ") (write actual) (newline))))

;; ============================================================
;; Basic arithmetic (from basic.church)
;; ============================================================

(check "plus" 2 (+ 1 1))
(check "plus-mult" 7 (+ (* 2 3) 1))
(check "min-2" 1 (min 1 2))
(check "min-many" -100 (min 5 4 1 2 3 -100))
(check "max-many" 5 (max 5 4 1 2 3 -100))
(check "max-2" 2 (max 1 2))

;; ============================================================
;; List operations (from basic.church)
;; ============================================================

(check "null?-empty" #t (null? '()))
(check "null?-nonempty" #f (null? '(())))
(check "null?-cdr-single" #t (null? (cdr (list 3))))
(check "null?-0" #f (null? 0))
(check "null?-false" #f (null? #f))

(check "length-5" 5 (length (list 1 2 3 4 5)))
(check "length-0" 0 (length (list)))
(check "length-nested" 2 (length (list (list (list 1) '1a) 2)))

(check "first-pair" 1 (first '(1 . 2)))
(check "rest-pair" 2 (rest '(1 . 2)))
(check "rest-rest-dotted" 3 (rest (rest '(1 2 . 3))))

(check "first" 1 (first '(1 2 3 4 5 6 7 8)))
(check "second" 2 (second '(1 2 3 4 5 6 7 8)))
(check "third" 3 (third '(1 2 3 4 5 6 7 8)))
(check "fourth" 4 (fourth '(1 2 3 4 5 6 7 8)))
(check "fifth" 5 (fifth '(1 2 3 4 5 6 7 8)))
(check "sixth" 6 (sixth '(1 2 3 4 5 6 7 8)))
(check "seventh" 7 (seventh '(1 2 3 4 5 6 7 8)))

(check "length-empty" 0 (length '()))
(check "length-1" 1 (length '(1)))
(check "length-2" 2 (length '(1 2)))
(check "length-nested2" 2 (length '(1 (2))))
(check "length-deep" 1 (length '((1 2))))

(check "reverse-empty" '() (reverse '()))
(check "null?-reverse-empty" #t (null? (reverse '())))
(check "reverse" '(f e d c b a) (reverse '(a b c d e f)))
(check "reverse-nested" '((d e f) c b a) (reverse '(a b c (d e f))))

(check "make-list-3" '(foo foo foo) (make-list 3 'foo))
(check "make-list-0" '() (make-list 0 'a))

;; eq? and equal?
(check "eq?-same-sym" #t (eq? 'a 'a))
(check "eq?-diff-sym" #f (eq? 'aa 'a))
(check "eq?-num-string" #f (eq? 3 "3"))

(check "equal?-nums" #t (equal? 1 1))
;; Note: In Church, symbols and strings are the same (both are JS strings).
;; In native Scheme, they are different types. This is an expected difference.
;; (check "equal?-sym-str" #t (equal? 'a "a"))  ; skipped — Church-specific
(check "equal?-lists" #t (equal? '(1 2 3) '(1 2 3)))
(check "equal?-diff-lists" #f (equal? '(1 2 3 (4)) '(1 2 3 (5))))
(check "equal?-deep-lists" #t (equal? '(1 2 3 ((4))) '(1 2 3 ((4)))))

(check "member-found" '(1 2 3) (member 1 '(1 2 3)))
(check "member-not-found" #f (member '() '(1 2 3)))
(check "member-list" '((c d) (e f) (a b)) (member '(c d) '((g h) (c d) (e f) (a b))))
(check "member-false" '(#f) (member #f '(#t #f)))

;; ============================================================
;; iota (from basic.church)
;; ============================================================

(check "iota-1" '(0) (iota 1))
(check "iota-10" '(0 1 2 3 4 5 6 7 8 9) (iota 10))
(check "iota-start" '(2 3 4 5 6 7 8 9 10 11) (iota 10 2))
(check "iota-empty" '() (iota 0 2))
(check "iota-negative" '(-6 -5 -4 -3 -2) (iota 5 -6))
(check "iota-step" '(0 20 40 60 80 100 120 140 160 180) (iota 10 0 20))

;; ============================================================
;; Boolean operations (from basic.church)
;; ============================================================

(check "and-all-true" #t (church-and #t #t #t #t))
(check "and-one-false" #f (church-and #t #t #t #f))
(check "and-empty" #t (church-and))
(check "or-empty" #f (church-or))
(check "or-all-false" #f (church-or #f #f #f #f))
(check "or-one-true" #t (church-or #f #f #f #t))

(check "all-empty" #t (all '()))
(check "all-true" #t (all '(#t #t #t #t #t #t)))
(check "all-one-false" #f (all '(#t #t #t #t #t #f)))
(check "none-empty" #t (none '()))
(check "none-all-true" #f (none '(#t #t #t #t #t #t)))
(check "none-all-false" #t (none '(#f #f #f #f #f #f)))
(check "some-empty" #f (some '()))
(check "some-all-false" #f (some '(#f #f #f #f #f #f)))
(check "some-one-true" #t (some '(#f #f #t #f #f #f)))

;; ============================================================
;; Comparisons (from basic.church)
;; ============================================================

(check "> true" #t (church-> 3 0))
(check "> false" #f (church-> 3 4))
(check ">= just-above" #t (church->= 3.1 3))
(check ">= equal" #t (church->= 3 3))
(check ">= below" #f (church->= 3 3.1))
(check "<= above" #t (church-<= 3 3.1))
(check "<= equal" #t (church-<= 3 3))
(check "<= below" #f (church-<= 3.1 3))

(check "= all-same" #t (church-= 1 1 1 1 1))
(check "= first-diff" #f (church-= 4 1 1 1 1))
(check "= empty" #t (church-=))

(check ".>. decreasing" #t (pw-greater 5 4 3 2 1))
(check ".>. not-strict" #f (pw-greater 5 4 3 2 2))
(check ".>=. non-strict" #t (pw-geq 5 4 3 2 2))
(check ".<. increasing" #t (pw-less 1 2 3 4 5))
(check ".<. not-strict" #f (pw-less 1 2 3 4 4))
(check ".<=. non-strict" #t (pw-leq 1 2 3 4 4))

(check "soft-equal-true" #t (soft-equal 3 4 2))
(check "soft-equal-false" #f (soft-equal 100 4 2))

;; ============================================================
;; Functional operations (from basic.church)
;; ============================================================

(check "list-ref-0" 0 (list-ref (iota 50) 0))
(check "list-ref-1" 1 (list-ref (iota 50) 1))
(check "list-elt-20" 19 (list-elt (iota 20) 20))

(check "take-3" '(1 2 3) (take '(1 2 3 4 5) 3))
(check "take-overflow" '(1 2 3 4 5) (take '(1 2 3 4 5) 6))
(check "take-0" '() (take '(1 2 3 4 5) 0))

(check "but-last" '(1 2 3) (but-last '(1 2 3 4)))

(check "apply-sum" 5050 (apply + (iota 101)))

(check "sort" '(1 2 3 4 5 6 7 8 9) (sort '(3 8 7 6 4 5 1 2 9)))

(check "unique" '(a b c) (unique '(a a a a a a b c c a)))
(check "nub" '(a b c) (nub '(a a a a a a b c c a)))

;; ============================================================
;; Gensym (from basic.church)
;; ============================================================

(let ((g (make-gensym)))
  (check "gensym-10" '(g0 g1 g2 g3 g4 g5 g6 g7 g8 g9) (repeat 10 g)))

;; ============================================================
;; Type predicates (from basic.church)
;; ============================================================

(check "list?-pair" #f (list? (cons 1 2)))
(check "list?-proper" #t (list? (cons 1 (cons 2 '()))))
(check "pair?-empty" #f (pair? '()))
(check "pair?-num" #f (pair? 3))
(check "pair?-list" #t (pair? (list 1 2 3)))
(check "pair?-single" #t (pair? (list 1)))

;; ============================================================
;; Map / Filter / Fold (from basic.church)
;; ============================================================

(check "map-empty" '() (map (lambda (x) (* x x)) '()))

(check "nested-map"
       '((101 201 301) (102 202 302) (103 203 303) (104 204 304))
       (map (lambda (x) (map (lambda (y) (+ x y)) '(100 200 300)))
            '(1 2 3 4)))

(check "map-2-lists" '(5 12 21 32) (map (lambda (x y) (* x y)) '(1 2 3 4) '(5 6 7 8 9)))

(check "filter" '(4 5 6 7 8) (filter (lambda (x) (> x 3)) '(1 2 3 4 5 6 7 8 3)))
(check "filter-none" '() (filter (lambda (x) #f) '(1 2 3 4 5 6 7 8 3)))

(check "fold-and" #f (fold (lambda (x acc) (and x acc)) #t '(#t #t #f)))
(check "fold-cons" '(3 2 1) (fold cons '() '(1 2 3)))
(check "fold-multi" 18 (fold + 0 '(1 2 3) '(2 4 6)))

(check "repeat" '(a a a a a a a a a a) (repeat 10 (lambda () 'a)))

(check "filter-gt3" '(4 5 6 7 9) (filter (lambda (x) (> x 3)) '(1 2 3 4 5 6 7 2 1 9)))

(check "partition" '((4 5 6 7 9) (1 2 3 2 1))
       (partition (lambda (x) (> x 3)) '(1 2 3 4 5 6 7 2 1 9)))

;; ============================================================
;; Set operations (from basic.church)
;; ============================================================

(check "union" '(1 2 3 4 5 6 7 9) (sort (union '(1 2 3 2 1) '(1 2 3 4 5 6 7 2 1 9) '(1 2 6 7 9))))
(check "intersection" (sort '(1 2)) (sort (intersection '(1 2 3 2 1) '(1 2 3 4 5 6 7 2 1 9) '(1 2 6 7 9))))
(check "difference" '(1 3 4) (sort (difference '(1 2 3 4 5) '(5 2 10))))

;; ============================================================
;; Statistics (from basic.church)
;; ============================================================

(check "sum-big" 123456 (sum (make-list 123456 1)))
(check "mean-ones" 1 (mean (make-list 123456 1)))
(check "variance-1" 6 (variance '(1 4 7)))
(check "variance-neg" 6 (variance '(-1 -4 -7)))
;; variance '(9 50 57 18) => population variance
(check-approx "variance-pop" 416.25 (exact->inexact (variance '(9 50 57 18))) 0.01)
(check "prod-ones" 1 (prod (make-list 123456 1)))

;; ============================================================
;; Lambda (from basic.church)
;; ============================================================

(check "lambda-0" 1 ((lambda () 1)))
(check "lambda-1" 1 ((lambda (x) x) 1))
(check "lambda-2" 3 ((lambda (x y) (+ x y)) 1 2))
(check "lambda-define" 1 ((lambda () (define foo 1) foo)))
;; Variadic lambda
(check "lambda-variadic" 6 ((lambda x (sum x)) 1 2 3))

;; ============================================================
;; Let forms (from basic.church)
;; ============================================================

(check "let" 3 (let ((x 1) (y 2)) (+ x y)))
(check "let*" '(1 2) (let* ([x 1] [y (+ x 1)]) (list x y)))
(check "letrec"
       #t
       (letrec ([is-even? (lambda (n) (if (= n 0) #t (is-odd? (- n 1))))]
                [is-odd? (lambda (n) (if (= n 0) #f (is-even? (- n 1))))])
         (is-odd? 131)))

;; Named let
(check "named-let"
       '((6 1 3) (-5 -2))
       (let loop ((numbers '(3 -2 1 6 -5))
                  (nonneg '())
                  (neg '()))
         (cond ((null? numbers) (list nonneg neg))
               ((>= (car numbers) 0)
                (loop (cdr numbers) (cons (car numbers) nonneg) neg))
               ((< (car numbers) 0)
                (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

;; ============================================================
;; Define (from basic.church)
;; ============================================================

(define test-x 1)
(check "define-var" 1 test-x)

(define (test-f0) 1)
(check "define-fn-0" 1 (test-f0))

(define (test-f1 x) x)
(check "define-fn-1" 1 (test-f1 1))

(define (test-f2 x y) (* x y))
(check "define-fn-2" 6 (test-f2 2 3))

;; Variadic define
(define (test-fv . x) x)
(check "define-variadic" '(1 2 3) (test-fv 1 2 3))

;; ============================================================
;; Conditionals (from basic.church)
;; ============================================================

(check "if-true" 1 (if #t 1))
(check "if-true-else" 1 (if #t 1 2))
(check "if-false-else" 2 (if #f 1 2))

(check "cond-1" 1 (cond (#t 1)))
(check "cond-2" 2 (cond (#f 1) (#t 2)))
(check "cond-else" 3 (cond (#f 1) (#f 2) (else 3)))

;; ============================================================
;; Assoc (from basic.church)
;; ============================================================

(check "assoc-found" '(a b c) (assoc 'a '((1 2) (a b c))))
(check "assoc-not-found" #f (assoc 'b '((a b c))))

;; ============================================================
;; String operations (from basic.church)
;; ============================================================

(check "string->number-0" 0 (string->number "0"))
(check "string->number-int" 123456 (string->number "123456"))
(check "string->number-float" -123456.789 (string->number "-123456.789"))
(check "number->string" "123" (number->string 123))
(check "number->string-neg" "-123456" (number->string -123456))
(check "string-split" '("abc" "def" "ghi") (string-split "abc.def.ghi" "."))
(check "string-split-none" '("abcdefghi") (string-split "abcdefghi" "."))

;; ============================================================
;; Mem (from basic.church)
;; ============================================================

(let ((f (mem (lambda (x) (gaussian 0 1)))))
  (check "mem-same-arg" #t (= (f 'a) (f 'a))))

;; ============================================================
;; Rejection query (from conditioning.church)
;; ============================================================

;; A+B+C=3 means all must be 1
(check "rejection-abc3"
       1
       (rejection-query
        (define A (if (flip) 1 0))
        (define B (if (flip) 1 0))
        (define C (if (flip) 1 0))
        (define D (+ A B C))
        A
        (= D 3)))

;; ============================================================
;; Enumeration query (from conditioning.church + support-change.church)
;; ============================================================

;; P(A=1 | D>=2) = 0.75
(let* ((dist (enumeration-query
              (define A (if (flip) 1 0))
              (define B (if (flip) 1 0))
              (define C (if (flip) 1 0))
              (define D (+ A B C))
              A
              (>= D 2)))
       (vals (car dist))
       (probs (cadr dist))
       (p1 (let loop ((v vals) (p probs))
             (cond ((null? v) 0)
                   ((= (car v) 1) (car p))
                   (else (loop (cdr v) (cdr p)))))))
  (check-approx "enum-A|D>=2" 0.75 p1 0.001))

;; Support change test from support-change.church
(let* ((dist (enumeration-query
              (define x (+ 0 (uniform-draw (if (flip) '(0) '(0 1)))))
              x
              #t))
       (vals (car dist))
       (probs (cadr dist))
       ;; Find P(x=0)
       (p0 (let loop ((v vals) (p probs))
             (cond ((null? v) 0)
                   ((= (car v) 0) (car p))
                   (else (loop (cdr v) (cdr p)))))))
  (check-approx "enum-support-change-p0" 0.75 p0 0.001))

;; ============================================================
;; MH query (from conditioning.church)
;; ============================================================

;; Deterministic: A+B+C=3 ⟹ A=1
(check "mh-abc3"
       1
       (mean (mh-query
              20 1
              (define A (if (flip) 1 0))
              (define B (if (flip) 1 0))
              (define C (if (flip) 1 0))
              (define D (+ A B C))
              A
              (= D 3))))

;; Statistical: P(A=1 | D>=2) ≈ 0.75
(let ((result (exact->inexact
               (mean (mh-query
                      5000 10
                      (define A (if (flip) 1 0))
                      (define B (if (flip) 1 0))
                      (define C (if (flip) 1 0))
                      (define D (+ A B C))
                      A
                      (>= D 2))))))
  (check-approx "mh-A|D>=2" 0.75 result 0.05))

;; ============================================================
;; MH edge cases (from mh.church)
;; ============================================================

;; flip(0) inside MH — https://github.com/probmods/webchurch/issues/54
(let* ((get-samp (lambda ()
                   (mh-query
                    1 1
                    (define x (uniform-draw '(a b c d e)))
                    x
                    (flip (if (equal? x 'a) 1 0)))))
       (n 20)
       (result (equal? (flatten (repeat n get-samp))
                       (make-list n 'a))))
  (check "mh-flip0" #t result))

;; ============================================================
;; Support change in MH (from support-change.church)
;; ============================================================

(let ((samps (mh-query
              10000 1
              (define x (+ 0 (uniform-draw (if (flip) '(0) '(0 1)))))
              x
              #t)))
  (check-approx "mh-support-change" 0.25 (exact->inexact (mean samps)) 0.05))

;; ============================================================
;; Conditional interface (from basic.church)
;; ============================================================

(check "conditional-enumerate"
       '(1 1 1 1 1)
       (repeat 5 (conditional '(enumerate)
                   (define x (if (flip) 1 0))
                   (define y (if (flip) 1 0))
                   (+ x y)
                   (church-and (= x 1) (= y 0)))))

(check "conditional-mh"
       '(1 1 1 1 1)
       (repeat 5 (conditional '(mh 5)
                   (define x (if (flip) 1 0))
                   (define y (if (flip) 1 0))
                   (+ x y)
                   (church-and (= x 1) (= y 0)))))

(check "conditional-rejection"
       '(1 1 1 1 1)
       (repeat 5 (conditional '(rejection)
                   (define x (if (flip) 1 0))
                   (define y (if (flip) 1 0))
                   (+ x y)
                   (church-and (= x 1) (= y 0)))))

;; ============================================================
;; Summary
;; ============================================================

(newline)
(display "========================================") (newline)
(display "Test Results: ") (display tests-passed) (display " passed, ")
(display tests-failed) (display " failed") (newline)
(when (> tests-failed 0)
  (display "Failures:") (newline)
  (for-each (lambda (name) (display "  - ") (display name) (newline))
            (reverse test-failures)))
(display "========================================") (newline)

(when (> tests-failed 0)
  (exit 1))
