;;; church/trace.scm — Trace data structure and address tracking for MH
;;;
;;; Provides:
;;;   - Address stack management (dynamic variable tracking call-site IDs)
;;;   - Trace: hash-table mapping address → trace-entry
;;;   - trace-entry record type

(import (srfi 69))  ; Hash tables

;; ============================================================
;; Trace entry record
;; ============================================================

(define-record-type <trace-entry>
  (make-trace-entry erp-name params value log-score)
  trace-entry?
  (erp-name trace-entry-erp-name)
  (params trace-entry-params)
  (value trace-entry-value)
  (log-score trace-entry-log-score))

;; ============================================================
;; Address stack — identifies each random choice uniquely
;; ============================================================

;; The address is a list of call-site IDs + a per-site counter.
;; Each ERP call increments a counter at the current address depth
;; to distinguish multiple calls at the same lexical position.

(define *address-stack* (make-parameter '()))

;; Per-address counters for disambiguation of multiple calls at same site
(define *address-counters* (make-parameter (make-hash-table)))

(define (current-address)
  ;; The address is the current stack plus a counter for this position
  (let* ((stack (*address-stack*))
         (key stack)
         (counters (*address-counters*))
         (count (hash-table-ref/default counters key 0)))
    (hash-table-set! counters key (+ count 1))
    (cons count stack)))

;; ============================================================
;; Trace — hash-table of address → trace-entry
;; ============================================================

(define (make-trace)
  (make-hash-table equal?))

(define (trace-ref trace addr)
  (hash-table-ref/default trace addr #f))

(define (trace-set! trace addr entry)
  (hash-table-set! trace addr entry))

(define (trace-delete! trace addr)
  (hash-table-delete! trace addr))

(define (trace-addresses trace)
  (hash-table-keys trace))

(define (trace-size trace)
  (hash-table-size trace))

(define (trace-copy trace)
  (let ((new-trace (make-trace)))
    (hash-table-walk trace
                     (lambda (k v) (trace-set! new-trace k v)))
    new-trace))
