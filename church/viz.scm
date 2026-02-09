;;; church/viz.scm — Text-based visualization stubs for ProbMods chapters
;;;
;;; Provides text-mode replacements for the visualization functions
;;; that ProbMods chapters expect (hist, density, barplot, scatter,
;;; lineplot, table). These are no-ops in webchurch's JS runtime but
;;; here we print useful text summaries.

;; ============================================================
;; hist — text histogram of a list of samples
;; ============================================================

(define (hist samples . args)
  (let ((title (if (null? args) "Histogram" (car args))))
    (display "--- ")
    (display title)
    (display " ---")
    (newline)
    ;; Count occurrences
    (let ((counts '()))
      (for-each
       (lambda (s)
         (let ((entry (assoc s counts)))
           (if entry
               (set-cdr! entry (+ (cdr entry) 1))
               (set! counts (cons (cons s 1) counts)))))
       samples)
      ;; Sort by key for consistent output
      (let ((sorted (list-sort (lambda (a b)
                                 (string<? (stringify (car a))
                                           (stringify (car b))))
                               counts)))
        (let ((max-count (apply max (map cdr sorted))))
          (for-each
           (lambda (entry)
             (let* ((label (stringify (car entry)))
                    (count (cdr entry))
                    (bar-len (max 1 (inexact->exact
                                     (round (* 40.0 (/ count max-count)))))))
               (display (string-pad-right label 20))
               (display " ")
               (display (make-string bar-len #\#))
               (display " ")
               (display count)
               (newline)))
           sorted))))
    (newline)))

;; ============================================================
;; density — summary stats for numeric samples
;; ============================================================

(define (density samples . args)
  (let ((title (if (null? args) "Density" (car args))))
    (display "--- ")
    (display title)
    (display " ---")
    (newline)
    (when (pair? samples)
      (let* ((nums (filter number? samples))
             (n (length nums)))
        (when (> n 0)
          (let ((mu (/ (apply + nums) n))
                (mn (apply min nums))
                (mx (apply max nums)))
            (display "  n=") (display n)
            (display "  mean=") (display (exact->inexact mu))
            (display "  min=") (display (exact->inexact mn))
            (display "  max=") (display (exact->inexact mx))
            (newline)))))
    (newline)))

;; ============================================================
;; barplot — display label/value pairs
;; ============================================================

(define (barplot data . args)
  (let ((title (if (null? args) "Barplot" (car args))))
    (display "--- ")
    (display title)
    (display " ---")
    (newline)
    (cond
     ;; (list labels values) format
     ((and (list? data) (= (length data) 2)
           (list? (car data)) (list? (cadr data)))
      (let ((labels (car data))
            (values (cadr data)))
        (for-each
         (lambda (l v)
           (display "  ")
           (display (stringify l))
           (display ": ")
           (display v)
           (newline))
         labels values)))
     ;; Association list format
     ((and (list? data) (pair? data) (pair? (car data)))
      (for-each
       (lambda (entry)
         (display "  ")
         (display (stringify (car entry)))
         (display ": ")
         (display (cdr entry))
         (newline))
       data))
     ;; Fallback: just print
     (else
      (display "  ")
      (write data)
      (newline)))
    (newline)))

;; ============================================================
;; scatter — display x,y pairs
;; ============================================================

(define (scatter data . args)
  (let ((title (if (null? args) "Scatter" (car args))))
    (display "--- ")
    (display title)
    (display " ---")
    (newline)
    (when (list? data)
      (for-each
       (lambda (pt)
         (cond
          ((pair? pt)
           (display "  (")
           (display (car pt))
           (display ", ")
           (display (cdr pt))
           (display ")")
           (newline))
          (else
           (display "  ")
           (write pt)
           (newline))))
       data))
    (newline)))

;; ============================================================
;; lineplot — same as scatter for text mode
;; ============================================================

(define (lineplot data . args)
  (apply scatter data (if (null? args) '("Lineplot") args)))

;; ============================================================
;; table — print rows
;; ============================================================

(define (table data . args)
  (let ((title (if (null? args) #f (car args))))
    (when title
      (display "--- ")
      (display title)
      (display " ---")
      (newline))
    (when (list? data)
      (for-each
       (lambda (row)
         (display "  ")
         (write row)
         (newline))
       data))
    (newline)))

;; ============================================================
;; Helpers
;; ============================================================

(define (string-pad-right s width)
  (if (>= (string-length s) width)
      s
      (string-append s (make-string (- width (string-length s)) #\space))))
