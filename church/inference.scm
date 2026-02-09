;;; church/inference.scm — Rejection sampling inference for Church
;;;
;;; Provides:
;;;   - condition — assert a constraint (rejection sampling aborts on false)
;;;   - factor — accumulate log-probability (soft conditioning)
;;;   - rejection-query — macro for rejection sampling

(import (chicken condition))

;; ============================================================
;; Condition failure exception
;; ============================================================

(define-record-type <condition-failure>
  (make-condition-failure)
  condition-failure?)

;; ============================================================
;; Dynamic variables for inference state
;; ============================================================

;; Whether we are currently inside an inference query
(define *in-query* (make-parameter #f))

;; Accumulated log-probability from factor statements
(define *log-score* (make-parameter 0.0))

;; ============================================================
;; condition — assert a boolean constraint
;; ============================================================

(define (condition b)
  (if (not b)
      (if (*in-query*)
          (abort (make-condition-failure))
          (error "condition failed outside of a query"))))

;; ============================================================
;; factor — soft conditioning (accumulate log-probability)
;; ============================================================

(define (factor log-prob)
  (if (*in-query*)
      (*log-score* (+ (*log-score*) log-prob))
      (error "factor used outside of a query")))

;; ============================================================
;; Rejection sampling
;; ============================================================

(define (rejection-sample thunk #!optional (max-attempts 1000000))
  (let loop ((attempts 0))
    (if (>= attempts max-attempts)
        (error "rejection-query: exceeded maximum attempts" max-attempts)
        (parameterize ((*in-query* #t)
                       (*log-score* 0.0))
          (let ((result
                 (handle-exceptions exn
                   (if (condition-failure? exn)
                       'rejected
                       (abort exn))
                   (thunk))))
            (if (eq? result 'rejected)
                (loop (+ attempts 1))
                ;; With factor, accept with probability exp(log-score)
                (let ((score (*log-score*)))
                  (if (or (= score 0.0)
                          (< (log (church-random-real)) score))
                      result
                      (loop (+ attempts 1))))))))))

;; ============================================================
;; rejection-query macro
;;
;; Usage: (rejection-query define-forms... query-expr condition-expr)
;;
;; The last two expressions are the query expression and condition.
;; Everything before them can be define forms or expressions.
;; ============================================================

(define-syntax rejection-query
  (syntax-rules ()
    ;; Two-body form: (rejection-query query-expr cond-expr)
    ((_ query-expr cond-expr)
     (rejection-sample
      (lambda ()
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Three-body form: (rejection-query body1 query-expr cond-expr)
    ((_ body1 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Four-body form
    ((_ body1 body2 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Five-body form
    ((_ body1 body2 body3 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Six-body form
    ((_ body1 body2 body3 body4 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        body4
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Seven-body form
    ((_ body1 body2 body3 body4 body5 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        body4
        body5
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Eight-body form
    ((_ body1 body2 body3 body4 body5 body6 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        body4
        body5
        body6
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Nine-body form
    ((_ body1 body2 body3 body4 body5 body6 body7 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        body4
        body5
        body6
        body7
        (let ((q query-expr))
          (condition cond-expr)
          q))))
    ;; Ten-body form
    ((_ body1 body2 body3 body4 body5 body6 body7 body8 query-expr cond-expr)
     (rejection-sample
      (lambda ()
        body1
        body2
        body3
        body4
        body5
        body6
        body7
        body8
        (let ((q query-expr))
          (condition cond-expr)
          q))))))
