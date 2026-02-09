;;; church/erps.scm — Elementary Random Primitives for Church
;;;
;;; Each ERP provides:
;;;   - A sampling function
;;;   - A log-density/log-probability function (for MH scoring)
;;;   - A proposal function (for MH proposals)
;;;
;;; ERPs interact with the inference engine via the current-erp-handler parameter.
;;; Outside of inference, ERPs just sample directly.
;;; Inside inference, the handler intercepts ERP calls for trace management.

(import (srfi 27))  ; random-source, random-real, random-integer

;; Initialize the random source
(define church-random-source (make-random-source))
(random-source-randomize! church-random-source)
(define church-random-real (random-source-make-reals church-random-source))
(define church-random-integer (random-source-make-integers church-random-source))

;; ============================================================
;; Mathematical helpers for density functions
;; ============================================================

(define LOG-2-PI (log (* 2.0 (acos -1.0))))
(define PI (acos -1.0))

(define (log-gamma x)
  ;; Stirling's approximation for log(Gamma(x))
  ;; Uses Lanczos approximation for better accuracy
  (if (<= x 0)
      +inf.0
      (let* ((g 7)
             (c (vector 0.99999999999980993
                        676.5203681218851
                        -1259.1392167224028
                        771.32342877765313
                        -176.61502916214059
                        12.507343278686905
                        -0.13857109526572012
                        9.9843695780195716e-6
                        1.5056327351493116e-7)))
        (if (< x 0.5)
            ;; Reflection formula
            (- (log (/ PI (sin (* PI x))))
               (log-gamma (- 1.0 x)))
            (let* ((x (- x 1.0))
                   (t (+ x g 0.5))
                   (sum (let loop ((i (- (vector-length c) 1))
                                   (s (vector-ref c 0)))
                          (if (= i 0)
                              s
                              (loop (- i 1)
                                    (+ s (/ (vector-ref c i) (+ x i))))))))
              (+ (* 0.5 (log (* 2.0 PI)))
                 (* (+ x 0.5) (log t))
                 (- t)
                 (log sum)))))))

(define (log-beta a b)
  (- (+ (log-gamma a) (log-gamma b))
     (log-gamma (+ a b))))

;; ============================================================
;; ERP Handler — the hook for inference engines
;; ============================================================

;; The ERP handler is a procedure: (handler erp-name sampler log-density-fn params)
;; Default handler just samples directly.
(define current-erp-handler
  (make-parameter
   (lambda (erp-name sampler log-density-fn params)
     (apply sampler params))))

;; ============================================================
;; Box-Muller transform for Gaussian sampling
;; ============================================================

(define *box-muller-spare* (make-parameter #f))
(define *box-muller-has-spare* (make-parameter #f))

(define (sample-standard-normal)
  (if (*box-muller-has-spare*)
      (begin
        (*box-muller-has-spare* #f)
        (*box-muller-spare*))
      (let loop ()
        (let* ((u (- (* 2.0 (church-random-real)) 1.0))
               (v (- (* 2.0 (church-random-real)) 1.0))
               (s (+ (* u u) (* v v))))
          (if (or (>= s 1.0) (= s 0.0))
              (loop)
              (let ((mul (sqrt (/ (* -2.0 (log s)) s))))
                (*box-muller-has-spare* #t)
                (*box-muller-spare* (* v mul))
                (* u mul)))))))

;; ============================================================
;; Gamma sampling (Marsaglia and Tsang's method)
;; ============================================================

(define (sample-gamma-gt1 shape)
  ;; For shape >= 1
  (let* ((d (- shape (/ 1.0 3.0)))
         (c (/ 1.0 (sqrt (* 9.0 d)))))
    (let loop ()
      (let* ((x (sample-standard-normal))
             (v (expt (+ 1.0 (* c x)) 3)))
        (if (and (> v 0.0)
                 (< (log (church-random-real))
                    (+ (* 0.5 x x)
                       (* d (- 1.0 v (log v))))))
            (* d v)
            (loop))))))

(define (sample-gamma shape rate)
  (if (< shape 1.0)
      ;; For shape < 1, use the trick: Gamma(a) = Gamma(a+1) * U^(1/a)
      (* (sample-gamma-gt1 (+ shape 1.0))
         (expt (church-random-real) (/ 1.0 shape))
         (/ 1.0 rate))
      (/ (sample-gamma-gt1 shape) rate)))

;; ============================================================
;; flip — Bernoulli distribution
;; ============================================================

(define (flip-sampler p)
  (< (church-random-real) p))

(define (flip-log-density val p)
  (if val
      (log (max p 1e-300))
      (log (max (- 1.0 p) 1e-300))))

(define (flip . args)
  (let ((p (if (null? args) 0.5 (car args))))
    ((current-erp-handler)
     'flip
     flip-sampler
     flip-log-density
     (list p))))

;; ============================================================
;; gaussian — Normal distribution
;; ============================================================

(define (gaussian-sampler mu sigma)
  (+ mu (* sigma (sample-standard-normal))))

(define (gaussian-log-density val mu sigma)
  (if (= sigma 0)
      (if (= val mu) 0.0 -inf.0)
      (let ((z (/ (- val mu) sigma)))
        (* -0.5 (+ LOG-2-PI (* 2.0 (log sigma)) (* z z))))))

(define (gaussian . args)
  (let ((mu (if (null? args) 0.0 (car args)))
        (sigma (if (or (null? args) (null? (cdr args))) 1.0 (cadr args))))
    (if (= sigma 0)
        mu
        ((current-erp-handler)
         'gaussian
         gaussian-sampler
         gaussian-log-density
         (list mu sigma)))))

;; ============================================================
;; uniform — Continuous uniform distribution
;; ============================================================

(define (uniform-sampler a b)
  (+ a (* (church-random-real) (- b a))))

(define (uniform-log-density val a b)
  (if (and (>= val a) (<= val b))
      (- (log (- b a)))
      -inf.0))

(define (uniform a b)
  ((current-erp-handler)
   'uniform
   uniform-sampler
   uniform-log-density
   (list a b)))

;; ============================================================
;; beta — Beta distribution
;; ============================================================

(define (beta-sampler a b)
  (let ((x (sample-gamma a 1.0))
        (y (sample-gamma b 1.0)))
    (/ x (+ x y))))

(define (beta-log-density val a b)
  (if (or (<= val 0.0) (>= val 1.0))
      -inf.0
      (- (+ (* (- a 1.0) (log val))
            (* (- b 1.0) (log (- 1.0 val))))
         (log-beta a b))))

(define (beta a b)
  (if (<= a 0) (error "The a argument to beta must be greater than 0"))
  (if (<= b 0) (error "The b argument to beta must be greater than 0"))
  ((current-erp-handler)
   'beta
   beta-sampler
   beta-log-density
   (list a b)))

;; ============================================================
;; gamma — Gamma distribution
;; ============================================================

(define (gamma-sampler shape rate)
  (sample-gamma shape rate))

(define (gamma-log-density val shape rate)
  (if (<= val 0.0)
      -inf.0
      (- (+ (* (- shape 1.0) (log val))
            (* (- rate) val)
            (* shape (log rate)))
         (log-gamma shape))))

(define (gamma shape rate)
  ((current-erp-handler)
   'gamma
   gamma-sampler
   gamma-log-density
   (list shape rate)))

;; ============================================================
;; exponential — Exponential distribution
;; ============================================================

(define (exponential-sampler rate)
  (/ (- (log (church-random-real))) rate))

(define (exponential-log-density val rate)
  (if (< val 0.0)
      -inf.0
      (- (log rate) (* rate val))))

(define (exponential rate)
  ((current-erp-handler)
   'exponential
   exponential-sampler
   exponential-log-density
   (list rate)))

;; ============================================================
;; dirichlet — Dirichlet distribution
;; ============================================================

(define (dirichlet-sampler alpha)
  (let* ((samples (map (lambda (a) (sample-gamma a 1.0)) alpha))
         (total (apply + samples)))
    (map (lambda (s) (/ s total)) samples)))

(define (dirichlet-log-density val alpha)
  ;; val and alpha are both lists
  (let ((k (length alpha)))
    (- (apply + (map (lambda (v a) (* (- a 1.0) (log v))) val alpha))
       (apply + (map log-gamma alpha))
       (- (log-gamma (apply + alpha))))))

(define (dirichlet alpha)
  ((current-erp-handler)
   'dirichlet
   dirichlet-sampler
   dirichlet-log-density
   (list alpha)))

;; ============================================================
;; multinomial — Categorical distribution (draw from weighted list)
;; ============================================================

(define (multinomial-draw items probs)
  ;; Sample an item from items with given probabilities
  (let* ((total (apply + probs))
         (r (* (church-random-real) total)))
    (let loop ((items items)
               (probs probs)
               (cumulative 0.0))
      (if (null? (cdr items))
          (car items)
          (let ((new-cum (+ cumulative (car probs))))
            (if (< r new-cum)
                (car items)
                (loop (cdr items) (cdr probs) new-cum)))))))

(define (multinomial-sampler items probs)
  (multinomial-draw items probs))

(define (multinomial-log-density val items probs)
  (let ((total (apply + probs)))
    (let loop ((items items) (probs probs))
      (cond
       ((null? items) -inf.0)
       ((equal? val (car items))
        (log (/ (car probs) total)))
       (else (loop (cdr items) (cdr probs)))))))

(define (multinomial items probs)
  ((current-erp-handler)
   'multinomial
   multinomial-sampler
   multinomial-log-density
   (list items probs)))

;; ============================================================
;; uniform-draw — Uniform draw from a list
;; ============================================================

(define (uniform-draw-sampler items)
  (let ((n (length items)))
    (list-ref items (church-random-integer n))))

(define (uniform-draw-log-density val items)
  (if (member val items)
      (- (log (length items)))
      -inf.0))

(define (uniform-draw items)
  ((current-erp-handler)
   'uniform-draw
   uniform-draw-sampler
   uniform-draw-log-density
   (list items)))

;; ============================================================
;; random-integer — Uniform random integer from 0 to n-1
;; ============================================================

(define (random-integer-sampler n)
  (church-random-integer n))

(define (random-integer-log-density val n)
  (if (and (integer? val) (>= val 0) (< val n))
      (- (log n))
      -inf.0))

(define (random-integer n)
  ((current-erp-handler)
   'random-integer
   random-integer-sampler
   random-integer-log-density
   (list n)))
(define sample-integer random-integer)

;; ============================================================
;; sample-discrete — Sample index proportional to weights
;; ============================================================

(define (sample-discrete weights)
  (let ((items (iota (length weights))))
    (multinomial items weights)))

;; ============================================================
;; set-seed — Seed the PRNG for reproducibility
;; ============================================================

(define (set-seed seed)
  (let ((rs (make-random-source)))
    (random-source-pseudo-randomize! rs 0 seed)
    (set! church-random-real (random-source-make-reals rs))
    (set! church-random-integer (random-source-make-integers rs))))
