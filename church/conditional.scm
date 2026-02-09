;;; church/conditional.scm — Generic query interface
;;;
;;; (conditional params body...) returns a thunk that when called
;;; returns a sample from the conditional distribution.
;;;
;;; params is a quoted list like:
;;;   '(enumerate) — use enumeration
;;;   '(rejection) — use rejection sampling
;;;   '(mh lag) — use MH with given lag

(define-syntax conditional
  (syntax-rules ()
    ;; Two bodies: query-expr and condition-expr
    ((_ params query-expr cond-expr)
     (let ((p params))
       (cond
        ((equal? (car p) 'enumerate)
         (let ((dist (enumeration-query query-expr cond-expr)))
           (lambda ()
             (multinomial (car dist) (cadr dist)))))
        ((equal? (car p) 'rejection)
         (lambda ()
           (rejection-query query-expr cond-expr)))
        ((equal? (car p) 'mh)
         (let ((mh-lag (cadr p)))
           (lambda ()
             (car (mh-query 1 mh-lag query-expr cond-expr)))))
        (else (error "conditional: unknown method" (car p))))))
    ;; Three bodies
    ((_ params body1 query-expr cond-expr)
     (let ((p params))
       (cond
        ((equal? (car p) 'enumerate)
         (let ((dist (enumeration-query body1 query-expr cond-expr)))
           (lambda ()
             (multinomial (car dist) (cadr dist)))))
        ((equal? (car p) 'rejection)
         (lambda ()
           (rejection-query body1 query-expr cond-expr)))
        ((equal? (car p) 'mh)
         (let ((mh-lag (cadr p)))
           (lambda ()
             (car (mh-query 1 mh-lag body1 query-expr cond-expr)))))
        (else (error "conditional: unknown method" (car p))))))
    ;; Four bodies
    ((_ params body1 body2 query-expr cond-expr)
     (let ((p params))
       (cond
        ((equal? (car p) 'enumerate)
         (let ((dist (enumeration-query body1 body2 query-expr cond-expr)))
           (lambda ()
             (multinomial (car dist) (cadr dist)))))
        ((equal? (car p) 'rejection)
         (lambda ()
           (rejection-query body1 body2 query-expr cond-expr)))
        ((equal? (car p) 'mh)
         (let ((mh-lag (cadr p)))
           (lambda ()
             (car (mh-query 1 mh-lag body1 body2 query-expr cond-expr)))))
        (else (error "conditional: unknown method" (car p))))))
    ;; Five bodies
    ((_ params body1 body2 body3 query-expr cond-expr)
     (let ((p params))
       (cond
        ((equal? (car p) 'enumerate)
         (let ((dist (enumeration-query body1 body2 body3 query-expr cond-expr)))
           (lambda ()
             (multinomial (car dist) (cadr dist)))))
        ((equal? (car p) 'rejection)
         (lambda ()
           (rejection-query body1 body2 body3 query-expr cond-expr)))
        ((equal? (car p) 'mh)
         (let ((mh-lag (cadr p)))
           (lambda ()
             (car (mh-query 1 mh-lag body1 body2 body3 query-expr cond-expr)))))
        (else (error "conditional: unknown method" (car p))))))))
