;;; church/mh.scm — Trace-based Metropolis-Hastings MCMC for Church
;;;
;;; Implements single-site MH (traceMH):
;;;   1. Forward run: execute program, each ERP records its choice in the trace
;;;   2. Propose: pick a random trace entry, propose new value
;;;   3. Replay: re-execute program with proposed value forced; others reuse old values
;;;   4. Accept/reject: MH acceptance ratio accounting for structural changes

;; ============================================================
;; MH ERP handler — records choices in trace, reuses old values
;; ============================================================

;; The current trace being built during execution
(define *current-trace* (make-parameter #f))

;; The old trace to reuse values from
(define *old-trace* (make-parameter #f))

;; The address being proposed (force a new value here)
(define *proposal-address* (make-parameter #f))

;; The proposed value to force
(define *proposal-value* (make-parameter #f))

;; Addresses touched during this execution
(define *touched-addresses* (make-parameter '()))

(define (mh-erp-handler erp-name sampler log-density-fn params)
  (let* ((addr (current-address))
         (old-trace (*old-trace*))
         (old-entry (and old-trace (trace-ref old-trace addr)))
         ;; Decide what value to use
         (value
          (cond
           ;; If this is the proposal site, use the proposed value
           ((and (*proposal-address*)
                 (equal? addr (*proposal-address*)))
            (*proposal-value*))
           ;; If we have an old value at this address with same ERP and params, reuse it
           ((and old-entry
                 (eq? (trace-entry-erp-name old-entry) erp-name)
                 (equal? (trace-entry-params old-entry) params))
            (trace-entry-value old-entry))
           ;; Otherwise, sample fresh
           (else
            (apply sampler params))))
         (log-score (apply log-density-fn (cons value params))))
    ;; Record in current trace
    (trace-set! (*current-trace*) addr
                (make-trace-entry erp-name params value log-score))
    ;; Track touched addresses
    (*touched-addresses* (cons addr (*touched-addresses*)))
    value))

;; ============================================================
;; Execute a thunk under MH tracing
;; ============================================================

(define (mh-execute thunk old-trace proposal-addr proposal-val)
  (let ((new-trace (make-trace)))
    (parameterize ((*current-trace* new-trace)
                   (*old-trace* old-trace)
                   (*proposal-address* proposal-addr)
                   (*proposal-value* proposal-val)
                   (*touched-addresses* '())
                   (*address-counters* (make-hash-table equal?))
                   (*address-stack* '())
                   (*in-query* #t)
                   (*log-score* 0.0)
                   (current-erp-handler mh-erp-handler))
      (let ((result
             (handle-exceptions exn
               (if (condition-failure? exn)
                   'rejected
                   (abort exn))
               (thunk))))
        (values result
                new-trace
                (*touched-addresses*)
                (*log-score*))))))

;; ============================================================
;; Compute total trace log-probability
;; ============================================================

(define (trace-log-prob trace)
  (let ((total 0.0))
    (hash-table-walk
     trace
     (lambda (addr entry)
       (set! total (+ total (trace-entry-log-score entry)))))
    total))

;; ============================================================
;; MH proposal: pick a random address, propose a new value
;; ============================================================

(define (propose-from-prior trace)
  ;; Pick a random address from the trace
  (let* ((addrs (trace-addresses trace))
         (n (length addrs))
         (idx (church-random-integer n))
         (addr (list-ref addrs idx))
         (entry (trace-ref trace addr))
         (erp-name (trace-entry-erp-name entry))
         (params (trace-entry-params entry))
         ;; Get the sampler and log-density for this ERP type
         (sampler/density (get-erp-functions erp-name))
         (sampler (car sampler/density))
         (log-density-fn (cadr sampler/density))
         ;; Sample a new value from the prior
         (new-value (apply sampler params))
         ;; Forward and reverse proposal scores (for prior proposal, these cancel)
         (fwd-score (apply log-density-fn (cons new-value params)))
         (rev-score (apply log-density-fn (cons (trace-entry-value entry) params))))
    (values addr new-value fwd-score rev-score)))

;; ============================================================
;; ERP function lookup table
;; ============================================================

(define (get-erp-functions erp-name)
  (case erp-name
    ((flip) (list flip-sampler flip-log-density))
    ((gaussian) (list gaussian-sampler gaussian-log-density))
    ((uniform) (list uniform-sampler uniform-log-density))
    ((beta) (list beta-sampler beta-log-density))
    ((gamma) (list gamma-sampler gamma-log-density))
    ((exponential) (list exponential-sampler exponential-log-density))
    ((dirichlet) (list dirichlet-sampler dirichlet-log-density))
    ((multinomial) (list multinomial-sampler multinomial-log-density))
    ((uniform-draw) (list uniform-draw-sampler uniform-draw-log-density))
    ((random-integer) (list random-integer-sampler random-integer-log-density))
    (else (error "Unknown ERP type" erp-name))))

;; ============================================================
;; Compute structural change scores
;; ============================================================

;; An address is "fresh" if it doesn't exist in old-trace, OR if
;; its ERP name or params changed (meaning we sampled a fresh value).
(define (address-reused? old-trace new-trace addr)
  ;; Returns #t if the address existed in old-trace with same ERP and params
  (let ((old-entry (trace-ref old-trace addr))
        (new-entry (trace-ref new-trace addr)))
    (and old-entry new-entry
         (eq? (trace-entry-erp-name old-entry) (trace-entry-erp-name new-entry))
         (equal? (trace-entry-params old-entry) (trace-entry-params new-entry)))))

;; Sum log-scores of addresses in new-trace that were freshly sampled
;; (not reused from old-trace), EXCLUDING the proposal address.
(define (fresh-score old-trace new-trace proposal-addr)
  (let ((total 0.0))
    (hash-table-walk
     new-trace
     (lambda (addr entry)
       (when (and (not (equal? addr proposal-addr))
                  (not (address-reused? old-trace new-trace addr)))
         (set! total (+ total (trace-entry-log-score entry))))))
    total))

;; Sum log-scores of addresses in old-trace that are "stale"
;; (not present in new-trace, or params changed), EXCLUDING proposal address.
(define (stale-score old-trace new-trace proposal-addr)
  (let ((total 0.0))
    (hash-table-walk
     old-trace
     (lambda (addr entry)
       (when (and (not (equal? addr proposal-addr))
                  (not (address-reused? old-trace new-trace addr)))
         (set! total (+ total (trace-entry-log-score entry))))))
    total))

;; ============================================================
;; traceMH — Main MH inference loop
;; ============================================================

(define (trace-mh thunk num-samples lag #!optional (return-scores #f))
  ;; Initial forward run
  (let-values (((init-result init-trace init-touched init-factor-score)
                (mh-execute thunk #f #f #f)))
    ;; If the initial run fails condition, keep retrying
    (let loop-init ((result init-result)
                    (trace init-trace)
                    (touched init-touched)
                    (factor-score init-factor-score)
                    (attempts 0))
      (if (eq? result 'rejected)
          (if (> attempts 100000)
              (error "mh-query: could not find initial satisfying sample")
              (let-values (((r t tt fs) (mh-execute thunk #f #f #f)))
                (loop-init r t tt fs (+ attempts 1))))
          ;; We have an initial valid sample — run MCMC
          (let ((samples '()))
            ;; Collect num-samples samples
            (let sample-loop ((collected 0)
                              (cur-result result)
                              (cur-trace trace)
                              (cur-factor factor-score))
              (if (>= collected num-samples)
                  (reverse samples)
                  ;; Run `lag` MH steps between samples
                  (let step-loop ((steps 0)
                                  (r cur-result)
                                  (t cur-trace)
                                  (f cur-factor))
                    (if (>= steps (max 1 lag))
                        ;; Record this sample
                        (begin
                          (set! samples
                                (cons (if return-scores
                                          (list r (+ (trace-log-prob t) f))
                                          r)
                                      samples))
                          (sample-loop (+ collected 1) r t f))
                        ;; Do one MH step
                        (if (= (trace-size t) 0)
                            ;; No random choices — always return same value
                            (step-loop (+ steps 1) r t f)
                            (let-values (((prop-addr prop-val fwd-score rev-score)
                                          (propose-from-prior t)))
                              ;; Re-run with proposal
                              (let-values (((new-result new-trace new-touched new-factor)
                                            (mh-execute thunk t prop-addr prop-val)))
                                (if (eq? new-result 'rejected)
                                    ;; Proposal rejected by condition — keep old
                                    (step-loop (+ steps 1) r t f)
                                    ;; Compute acceptance ratio using Wingate et al.'s formula:
                                    ;; α = (p_new/p_old) × (|τ|/|τ'|) × (p_stale/p_fresh)
                                    (let* ((old-size (trace-size t))
                                           (new-size (trace-size new-trace))
                                           (old-trace-lp (trace-log-prob t))
                                           (new-trace-lp (trace-log-prob new-trace))
                                           (f-lp (fresh-score t new-trace prop-addr))
                                           (s-lp (stale-score t new-trace prop-addr))
                                           (log-accept
                                            (+ (- new-trace-lp old-trace-lp)
                                               (- new-factor f)
                                               (- (log old-size) (log new-size))
                                               (- s-lp f-lp))))
                                      (if (< (log (church-random-real)) log-accept)
                                          ;; Accept
                                          (step-loop (+ steps 1) new-result new-trace new-factor)
                                          ;; Reject — keep old
                                          (step-loop (+ steps 1) r t f))))))))))))))))

;; ============================================================
;; mh-query macro
;;
;; Usage: (mh-query num-samples lag defines... query-expr condition-expr)
;; ============================================================

(define-syntax mh-query
  (syntax-rules ()
    ((_ num-samples lag query-expr cond-expr)
     (trace-mh
      (lambda ()
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 body4 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 body4 body5 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4 body5
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 body4 body5 body6 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4 body5 body6
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 body4 body5 body6 body7 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4 body5 body6 body7
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))
    ((_ num-samples lag body1 body2 body3 body4 body5 body6 body7 body8 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4 body5 body6 body7 body8
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag))))

;; ============================================================
;; mh-query-scored — returns (sample logprob) pairs
;; ============================================================

(define-syntax mh-query-scored
  (syntax-rules ()
    ((_ num-samples lag query-expr cond-expr)
     (trace-mh
      (lambda ()
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))
    ((_ num-samples lag body1 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))
    ((_ num-samples lag body1 body2 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))
    ((_ num-samples lag body1 body2 body3 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))
    ((_ num-samples lag body1 body2 body3 body4 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))
    ((_ num-samples lag body1 body2 body3 body4 body5 query-expr cond-expr)
     (trace-mh
      (lambda ()
        body1 body2 body3 body4 body5
        (let ((q query-expr))
          (condition cond-expr)
          q))
      num-samples lag #t))))
