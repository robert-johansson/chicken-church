;;; church/enumerate.scm — Exact enumeration using continuations
;;;
;;; When a discrete ERP is called during enumeration, we capture the
;;; current continuation and fork: for each possible value, resume
;;; the continuation with that value, tracking accumulated probability.
;;;
;;; Uses Chicken's native call/cc (via call-with-current-continuation).

;; ============================================================
;; Enumeration state
;; ============================================================

;; Whether we're currently in enumeration mode
(define *enumerating* (make-parameter #f))

;; The work queue: list of (thunk . log-probability) to explore
(define *enum-queue* (make-parameter '()))

;; Accumulated results: alist of (value . probability)
(define *enum-results* (make-parameter '()))

;; Current path log-probability
(define *enum-log-prob* (make-parameter 0.0))

;; ============================================================
;; ERP handler for enumeration
;; ============================================================

(define (enumeration-erp-handler erp-name sampler log-density-fn params)
  ;; Get the support (possible values) for this ERP
  (let ((support (erp-support erp-name params)))
    (if (not support)
        ;; Continuous ERP — can't enumerate, just sample
        (apply sampler params)
        ;; Discrete ERP — fork via continuation
        (call-with-current-continuation
         (lambda (k)
           ;; Save the rest of the support to the queue
           (for-each
            (lambda (val)
              (let ((lp (apply log-density-fn (cons val params))))
                (when (> lp -inf.0)
                  (*enum-queue*
                   (cons (cons (lambda () (k val))
                               (+ (*enum-log-prob*) lp))
                         (*enum-queue*))))))
            (cdr support))
           ;; Continue with the first value
           (let ((first-val (car support))
                 (first-lp (apply log-density-fn (cons (car support) params))))
             (*enum-log-prob* (+ (*enum-log-prob*) first-lp))
             first-val))))))

;; ============================================================
;; ERP support — returns list of possible values for discrete ERPs
;; ============================================================

(define (erp-support erp-name params)
  (case erp-name
    ((flip) (list #t #f))
    ((multinomial)
     ;; params = (items probs)
     (car params))
    ((uniform-draw)
     ;; params = (items)
     (car params))
    ((random-integer)
     ;; params = (n)
     (let ((n (car params)))
       (let loop ((i 0) (acc '()))
         (if (>= i n)
             (reverse acc)
             (loop (+ i 1) (cons i acc))))))
    (else #f)))  ; continuous — can't enumerate

;; ============================================================
;; Run enumeration
;; ============================================================

(define (run-enumeration thunk)
  (parameterize ((*enumerating* #t)
                 (*enum-queue* '())
                 (*enum-results* '())
                 (*enum-log-prob* 0.0)
                 (*in-query* #t)
                 (*log-score* 0.0)
                 (current-erp-handler enumeration-erp-handler))
    ;; Run the thunk, catching condition failures
    (let ((result
           (handle-exceptions exn
             (if (condition-failure? exn)
                 'rejected
                 (abort exn))
             (thunk))))
      (when (not (eq? result 'rejected))
        ;; Record this result with its probability
        (let ((total-lp (+ (*enum-log-prob*) (*log-score*))))
          (when (> total-lp -inf.0)
            (add-enum-result! result (exp total-lp)))))
      ;; Process remaining queue items
      (let loop ()
        (when (not (null? (*enum-queue*)))
          (let* ((item (car (*enum-queue*)))
                 (thunk (car item))
                 (lp (cdr item)))
            (*enum-queue* (cdr (*enum-queue*)))
            (*enum-log-prob* lp)
            (*log-score* 0.0)
            (let ((result
                   (handle-exceptions exn
                     (if (condition-failure? exn)
                         'rejected
                         (abort exn))
                     (thunk))))
              (when (not (eq? result 'rejected))
                (let ((total-lp (+ (*enum-log-prob*) (*log-score*))))
                  (when (> total-lp -inf.0)
                    (add-enum-result! result (exp total-lp))))))
            (loop))))
      ;; Return results
      (*enum-results*))))

(define (add-enum-result! value prob)
  (let ((existing (assoc value (*enum-results*))))
    (if existing
        (set-cdr! existing (+ (cdr existing) prob))
        (*enum-results* (cons (cons value prob) (*enum-results*))))))

;; ============================================================
;; enumeration-query macro
;;
;; Returns: (list values-list probs-list)
;; where values and probs are aligned lists
;; ============================================================

(define (format-enum-results results)
  ;; results is an alist of (value . unnormalized-prob)
  (let* ((total (apply + (map cdr results)))
         (values (map car results))
         (probs (map (lambda (r) (/ (cdr r) total)) results)))
    (list values probs)))

(define-syntax enumeration-query
  (syntax-rules ()
    ((_ query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 body4 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3 body4
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 body4 body5 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3 body4 body5
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 body4 body5 body6 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3 body4 body5 body6
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 body4 body5 body6 body7 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3 body4 body5 body6 body7
         (let ((q query-expr))
           (condition cond-expr)
           q)))))
    ((_ body1 body2 body3 body4 body5 body6 body7 body8 query-expr cond-expr)
     (format-enum-results
      (run-enumeration
       (lambda ()
         body1 body2 body3 body4 body5 body6 body7 body8
         (let ((q query-expr))
           (condition cond-expr)
           q)))))))
