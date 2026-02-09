;;; church/dpmem.scm — Dirichlet Process Memoization
;;;
;;; DPmem implements stochastic memoization using the Chinese Restaurant Process.
;;; Given concentration parameter alpha and function f, returns a function that:
;;;   - For each unique argument set, maintains a CRP table
;;;   - With probability proportional to alpha, samples a new value from f
;;;   - With probability proportional to count, returns an existing value

(define (DPmem alpha f)
  (let ((all-labels (make-hash-table equal?))  ; args-hash → list of labels
        (all-indices (make-hash-table equal?)) ; args-hash → list of indices
        (all-counts (make-hash-table equal?))) ; args-hash → list of counts
    (lambda args
      (let* ((args-key args)
             (labels (hash-table-ref/default all-labels args-key #f))
             (indices (hash-table-ref/default all-indices args-key #f))
             (counts (hash-table-ref/default all-counts args-key #f)))
        ;; Initialize tables for this argument set if needed
        (when (not labels)
          (set! labels '())
          (set! indices '(-1))      ; virtual index -1 for "new table"
          (set! counts (list alpha)) ; virtual count alpha for "new table"
          (hash-table-set! all-labels args-key labels)
          (hash-table-set! all-indices args-key indices)
          (hash-table-set! all-counts args-key counts))

        ;; Sample an index from the CRP
        (let ((sampled-index (multinomial indices counts)))
          (if (= sampled-index -1)
              ;; New table: sample from base distribution
              (let ((label (apply f args)))
                (set! labels (append labels (list label)))
                (set! indices (append indices (list (- (length indices) 1))))
                (set! counts (append counts (list 1)))
                (hash-table-set! all-labels args-key labels)
                (hash-table-set! all-indices args-key indices)
                (hash-table-set! all-counts args-key counts)
                label)
              ;; Existing table: return its label and increment count
              (let ((label (list-ref labels sampled-index)))
                ;; Increment the count for this table
                (let ((new-counts
                       (let loop ((c counts) (i 0) (acc '()))
                         (if (null? c)
                             (reverse acc)
                             (loop (cdr c) (+ i 1)
                                   (cons (if (= i (+ sampled-index 1))
                                             (+ (car c) 1)
                                             (car c))
                                         acc))))))
                  (hash-table-set! all-counts args-key new-counts))
                label)))))))
