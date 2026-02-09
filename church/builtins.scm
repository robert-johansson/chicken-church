;;; church/builtins.scm — Gap-fill builtins not native to Chicken Scheme
;;;
;;; These implement Church built-in functions that don't exist in standard
;;; Chicken Scheme or SRFI-1. Many Church builtins (car, cdr, cons, map,
;;; filter, etc.) are already native Scheme.

(import (srfi 1))   ; Extended list operations

;; ============================================================
;; Aliases for Scheme builtins to match Church naming
;; ============================================================

;; pair/cons aliases (Scheme already has cons, car, cdr)
(define pair cons)
(define first car)
(define rest cdr)

;; Positional accessors
(define (second lst) (cadr lst))
(define (third lst) (caddr lst))
(define (fourth lst) (cadddr lst))
(define (fifth lst) (car (cddddr lst)))
(define (sixth lst) (cadr (cddddr lst)))
(define (seventh lst) (caddr (cddddr lst)))

;; but-last / initial
(define (but-last lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (but-last (cdr lst)))))
(define initial but-last)

;; ============================================================
;; Church's list? / pair? / null? semantics
;; ============================================================

;; Scheme's list? and pair? already work correctly for native cons cells.
;; null? already works.

;; ============================================================
;; List operations
;; ============================================================

;; list-ref is native in Chicken Scheme

;; list-elt (1-indexed)
(define (list-elt lst n)
  (if (< n 1) (error "The n argument to list-elt should be an integer >= 1"))
  (list-ref lst (- n 1)))

;; list-index / position: find index of x in lst
(define (list-index-church lst x)
  (let loop ((l lst) (i 0))
    (cond
     ((null? l) -1)
     ((equal? x (car l)) i)
     (else (loop (cdr l) (+ i 1))))))
(define position list-index-church)

;; make-list (Church version: (make-list n x))
;; SRFI-1 has make-list but it may differ, so define explicitly
(define (church-make-list n x)
  (let loop ((i 0) (acc '()))
    (if (>= i n) acc
        (loop (+ i 1) (cons x acc)))))

;; update-list: return new list with element at index n replaced by value
(define (update-list lst n value)
  (let loop ((l lst) (i 0))
    (cond
     ((null? l) (error "list index too big in update-list"))
     ((= i n) (cons value (cdr l)))
     (else (cons (car l) (loop (cdr l) (+ i 1)))))))

;; map-at / f-at: apply f to element at index i
(define (map-at lst i f)
  (update-list lst i (f (list-ref lst i))))
(define f-at map-at)

;; ============================================================
;; iota — arithmetic progression
;; ============================================================

;; Church's iota: (iota count [start] [step])
(define (church-iota count . args)
  (let ((start (if (null? args) 0 (car args)))
        (step (if (or (null? args) (null? (cdr args))) 1 (cadr args))))
    (let loop ((i 0) (val start) (acc '()))
      (if (>= i count)
          (reverse acc)
          (loop (+ i 1) (+ val step) (cons val acc))))))

;; range: (range start end) — inclusive
(define (range start end)
  (church-iota (+ (- end start) 1) start 1))

;; ============================================================
;; take / drop (may exist in SRFI-1 but define to match Church exactly)
;; ============================================================

(define (church-take lst n)
  (let loop ((l lst) (i 0) (acc '()))
    (if (or (>= i n) (null? l))
        (reverse acc)
        (loop (cdr l) (+ i 1) (cons (car l) acc)))))

(define (church-drop lst n)
  (let loop ((l lst) (i 0))
    (if (or (>= i n) (null? l))
        l
        (loop (cdr l) (+ i 1)))))

;; ============================================================
;; sort — sort a list with optional comparator
;; ============================================================

;; Church's sort: (sort lst [cmp]) where cmp returns a number > 0, 0, or < 0
;; Default sort is numeric ascending
(define (church-sort lst . args)
  (if (null? args)
      ;; Default: numeric ascending sort
      (list-sort < lst)
      ;; With comparator: cmp(a,b) > 0 means a should come before b
      (let ((cmp (car args)))
        (list-sort (lambda (a b) (> (cmp a b) 0)) lst))))

;; Merge sort implementation
(define (list-sort less? lst)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car a) (car b))
           (cons (car a) (merge (cdr a) b)))
          (else
           (cons (car b) (merge a (cdr b))))))
  (define (split lst)
    (let loop ((l lst) (a '()) (b '()) (toggle #t))
      (if (null? l)
          (values a b)
          (if toggle
              (loop (cdr l) (cons (car l) a) b #f)
              (loop (cdr l) a (cons (car l) b) #t)))))
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (let-values (((a b) (split lst)))
        (merge (list-sort less? a) (list-sort less? b)))))

;; ============================================================
;; Set operations
;; ============================================================

;; union: merge multiple lists, keeping unique elements
(define (union . lsts)
  (delete-duplicates (apply append lsts)))

;; intersection: elements common to all lists
(define (intersection . lsts)
  (if (null? lsts) '()
      (delete-duplicates
       (fold (lambda (lst acc)
               (filter (lambda (x) (member x lst)) acc))
             (car lsts)
             (cdr lsts)))))

;; difference: elements in first list not in any other
(define (difference . lsts)
  (if (null? lsts) '()
      (let ((rest-items (apply append (cdr lsts))))
        (filter (lambda (x) (not (member x rest-items)))
                (car lsts)))))

;; ============================================================
;; unique / nub
;; ============================================================

(define (unique lst . args)
  (let ((eq-fn (if (null? args) equal? (car args))))
    (let loop ((l lst) (acc '()))
      (if (null? l)
          (reverse acc)
          (if (any (lambda (a) (eq-fn (car l) a)) acc)
              (loop (cdr l) acc)
              (loop (cdr l) (cons (car l) acc)))))))

(define (nub lst)
  (delete-duplicates lst))

;; ============================================================
;; Statistics
;; ============================================================

(define (sum lst) (apply + lst))

(define (prod lst) (apply * lst))

(define (mean lst)
  (/ (apply + lst) (length lst)))

(define (variance lst)
  (let* ((n (length lst))
         (mu (mean lst)))
    (/ (apply + (map (lambda (x) (expt (- x mu) 2)) lst))
       n)))

;; ============================================================
;; Boolean/logic operations (variadic versions)
;; ============================================================

;; Church's and/or are functions, not special forms
;; But in Scheme and/or ARE special forms. We provide function versions.
(define (church-and . args)
  (every identity args))

(define (church-or . args)
  (any identity args))

(define (church-not b)
  (not b))

(define (all lst)
  (every identity lst))

(define (none lst)
  (not (any identity lst)))

(define (some lst)
  (any identity lst))
(define church-any some)

;; ============================================================
;; Comparison operators (Church-style variadic)
;; ============================================================

;; Church's > tests x > all y's
(define (church-> x . ys)
  (every (lambda (y) (> x y)) ys))

(define (church-< x . ys)
  (every (lambda (y) (< x y)) ys))

(define (church->= x . ys)
  (every (lambda (y) (>= x y)) ys))

(define (church-<= x . ys)
  (every (lambda (y) (<= x y)) ys))

(define (church-= . args)
  (or (null? args)
      (null? (cdr args))
      (every (lambda (x) (= (car args) x)) (cdr args))))

;; Pairwise comparisons
(define (pw-greater . args)
  (let loop ((a args))
    (or (null? (cdr a))
        (and (> (car a) (cadr a))
             (loop (cdr a))))))
(define .>. pw-greater)

(define (pw-less . args)
  (let loop ((a args))
    (or (null? (cdr a))
        (and (< (car a) (cadr a))
             (loop (cdr a))))))
(define .<. pw-less)

(define (pw-geq . args)
  (let loop ((a args))
    (or (null? (cdr a))
        (and (>= (car a) (cadr a))
             (loop (cdr a))))))
(define .>=. pw-geq)

(define (pw-leq . args)
  (let loop ((a args))
    (or (null? (cdr a))
        (and (<= (car a) (cadr a))
             (loop (cdr a))))))
(define .<=. pw-leq)

;; soft-equal: check if y is within tolerance of x
(define (soft-equal y x tol)
  (and (> y (- x tol)) (< y (+ x tol))))
(define soft= soft-equal)

;; ============================================================
;; Conversion functions
;; ============================================================

(define (boolean->number b) (if b 1 0))
(define (number->boolean x) (not (= x 0)))
(define (bang-bang x) (if x #t #f))

(define (string->number-church s)
  (let ((x (string->number s)))
    (or x #f)))

;; number->string is native
;; string->symbol is native
;; symbol->string is native

(define (stringify x)
  (cond
   ((string? x) x)
   ((number? x) (number->string x))
   ((symbol? x) (symbol->string x))
   ((boolean? x) (if x "#t" "#f"))
   ((list? x) (string-append "(" (string-join (map stringify x) " ") ")"))
   ((pair? x) (string-append "(" (stringify (car x)) " . " (stringify (cdr x)) ")"))
   (else (error "stringify: cannot convert" x))))

(define (string-join lst sep)
  (if (null? lst) ""
      (fold (lambda (s acc) (string-append acc sep s))
            (car lst)
            (cdr lst))))

;; ============================================================
;; String operations
;; ============================================================

(define (regexp-split str sep)
  ;; Simple string split (not full regexp)
  (let loop ((s str) (acc '()) (current ""))
    (cond
     ((= (string-length s) 0)
      (reverse (cons current acc)))
     ((and (>= (string-length s) (string-length sep))
           (string=? (substring s 0 (string-length sep)) sep))
      (loop (substring s (string-length sep))
            (cons current acc)
            ""))
     (else
      (loop (substring s 1)
            acc
            (string-append current (string (string-ref s 0))))))))
(define string-split regexp-split)

(define (string-slice s start . args)
  (let ((end (if (null? args) (string-length s) (car args))))
    (substring s start (min end (string-length s)))))

(define church-string-length string-length)

;; string-append is native (and variadic)

;; ============================================================
;; Functional combinators
;; ============================================================

;; identity
(define (identity x) x)
(define id identity)

;; compose
(define (compose . fns)
  (if (null? fns)
      identity
      (let ((f (car fns))
            (rest (apply compose (cdr fns))))
        (lambda args (f (apply rest args))))))
(define o compose)

;; ============================================================
;; fold — Church's fold: (fold f init lst1 ...)
;; ============================================================

;; Church's fold applies right-to-left: f(lst_0, f(lst_1, ... f(lst_n, init)))
;; Actually looking at webchurch, it iterates left to right pushing cumulative:
;; for i in 0..n: cumulativeValue = fn(lst[i], cumulativeValue)
(define (church-fold fn init . lsts)
  (let ((arrs (map (lambda (l) l) lsts)))
    (let ((n (apply min (map length arrs))))
      (let loop ((i 0) (acc init))
        (if (>= i n)
            acc
            (loop (+ i 1)
                  (apply fn (append (map (lambda (arr) (list-ref arr i)) arrs)
                                    (list acc)))))))))

;; foldl: (foldl f init lst) — left fold
;; Church's foldl: f(accumulator, value)
(define (church-foldl fn init lst)
  (let loop ((l lst) (acc init))
    (if (null? l)
        acc
        (loop (cdr l) (fn acc (car l))))))

;; foldr: (foldr f init lst) — right fold
(define (church-foldr fn init lst)
  (if (null? lst)
      init
      (fn init (car lst))))

;; Actually Church foldr works as: f(accumulator, value) right to left
(define (church-foldr fn init lst)
  (let loop ((l (reverse lst)) (acc init))
    (if (null? l)
        acc
        (loop (cdr l) (fn acc (car l))))))

;; ============================================================
;; repeat — (repeat n f) calls f() n times, returns list
;; ============================================================

(define (repeat n fn)
  (let loop ((i 0) (acc '()))
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons (fn) acc)))))

;; ============================================================
;; for-each is native

;; ============================================================
;; Church's map — handles multiple lists
;; Native Scheme map already handles multiple lists.

;; ============================================================
;; member — Church returns sublist from match point, or #f
;; Scheme's member already does this with equal?.

;; ============================================================
;; assoc — already in Scheme

;; ============================================================
;; apply is native

;; ============================================================
;; flatten
;; ============================================================

(define (flatten lst)
  (cond
   ((null? lst) '())
   ((pair? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst))))
   (else
    (cons (car lst) (flatten (cdr lst))))))

;; ============================================================
;; zip / zipT / transpose
;; ============================================================

(define (zip . lsts)
  ;; Zip using longest list (pad with '())
  (let ((max-len (apply max (map length lsts))))
    (let loop ((i 0) (acc '()))
      (if (>= i max-len)
          (reverse acc)
          (loop (+ i 1)
                (cons (map (lambda (l)
                             (if (< i (length l))
                                 (list-ref l i)
                                 '()))
                           lsts)
                      acc))))))

(define (zipT . lsts)
  ;; Zip using shortest list (truncate)
  (let ((min-len (apply min (map length lsts))))
    (let loop ((i 0) (acc '()))
      (if (>= i min-len)
          (reverse acc)
          (loop (+ i 1)
                (cons (map (lambda (l) (list-ref l i)) lsts)
                      acc))))))

(define (transpose mat)
  (apply zip mat))

;; ============================================================
;; partition — (partition pred lst) → ((matches) (non-matches))
;; ============================================================

;; SRFI-1 partition returns two values; Church returns a list of two lists
(define (church-partition pred lst)
  (let loop ((l lst) (yes '()) (no '()))
    (cond
     ((null? l) (list (reverse yes) (reverse no)))
     ((pred (car l)) (loop (cdr l) (cons (car l) yes) no))
     (else (loop (cdr l) yes (cons (car l) no))))))

;; ============================================================
;; gensym / make-gensym
;; ============================================================

(define (make-gensym . args)
  (let ((prefix (if (null? args) "g" (car args)))
        (counter 0))
    (lambda ()
      (let ((sym (string->symbol (string-append
                                  (if (symbol? prefix) (symbol->string prefix) prefix)
                                  (number->string counter)))))
        (set! counter (+ counter 1))
        sym))))

(define church-gensym-default (make-gensym "g"))
(define (church-gensym) (church-gensym-default))

;; ============================================================
;; sample — apply a thunk
;; ============================================================

(define (sample thunk) (thunk))

;; ============================================================
;; Display / print
;; ============================================================

;; Church's display prints and returns void
(define (church-display . args)
  (for-each (lambda (x)
              (cond
               ((string? x) (display x))
               (else (write x)))
              (newline))
            args))
(define church-print church-display)
(define pn church-display)

;; ============================================================
;; I/O
;; ============================================================

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((chars '()))
        (let ((c (read-char)))
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

;; ============================================================
;; mem — Memoization
;; ============================================================

(define (mem f)
  (let ((table '()))
    (lambda args
      (let ((entry (assoc args table)))
        (if entry
            (cdr entry)
            (let ((result (apply f args)))
              (set! table (cons (cons args result) table))
              result))))))

;; ============================================================
;; get-time
;; ============================================================

(define (get-time)
  (current-milliseconds))

;; ============================================================
;; eq? / equal? are native Scheme
;; ============================================================

;; Church's = is numeric equality (already defined as church-=)
;; Church's eq? is Scheme's eq? (reference equality)
;; Church's equal? is Scheme's equal? (structural equality)
