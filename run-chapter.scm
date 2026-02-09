;;; run-chapter.scm — Run ProbMods chapter code blocks with Chicken Church
;;;
;;; Usage: csi -q -s run-chapter.scm <chapter.md>
;;;
;;; Extracts ~~~~ fenced code blocks from a ProbMods markdown file,
;;; skipping blocks annotated {.norun}, {.idealized}, or {.shouldfail},
;;; and evaluates each block in the Church runtime environment.

(import (chicken process-context))
(import (chicken condition))
(import (chicken io))
(import (chicken port))

;; Load Church modules directly (same as church.scm but without the
;; entry point that would try to load the .md file as Church code)
(include "church/erps.scm")
(include "church/builtins.scm")
(include "church/inference.scm")
(include "church/trace.scm")
(include "church/enumerate.scm")
(include "church/mh.scm")
(include "church/dpmem.scm")
(include "church/conditional.scm")

;; Church-specific aliases (from church.scm)
(set! iota church-iota)
(set! make-list church-make-list)
(set! take church-take)
(set! drop church-drop)
(set! sort church-sort)
(set! partition church-partition)
(set! fold church-fold)
(set! foldl church-foldl)
(set! foldr church-foldr)

(define true #t)
(define false #f)

;; Load visualization stubs
(include "church/viz.scm")

;; ============================================================
;; Markdown code block extraction
;; ============================================================

(define (read-file-text filename)
  (with-input-from-file filename
    (lambda ()
      (read-string))))

(define (string-starts-with? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))

(define (string-trim-whitespace s)
  ;; Trim leading whitespace
  (let loop ((i 0))
    (if (and (< i (string-length s))
             (let ((c (string-ref s i)))
               (or (char=? c #\space) (char=? c #\tab))))
        (loop (+ i 1))
        (substring s i (string-length s)))))

(define (skip-block? annotation)
  ;; Returns #t if the block should be skipped based on its annotation
  (and annotation
       (or (string-contains annotation ".norun")
           (string-contains annotation ".idealized")
           (string-contains annotation ".shouldfail")
           (string-contains annotation ".skip"))))

(define (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
       ((> (+ i nlen) hlen) #f)
       ((string=? (substring haystack i (+ i nlen)) needle) #t)
       (else (loop (+ i 1)))))))

(define (split-lines text)
  (let loop ((start 0) (i 0) (acc '()))
    (cond
     ((>= i (string-length text))
      (reverse (cons (substring text start i) acc)))
     ((char=? (string-ref text i) #\newline)
      (loop (+ i 1) (+ i 1) (cons (substring text start i) acc)))
     (else
      (loop start (+ i 1) acc)))))

(define (extract-code-blocks text)
  ;; Returns list of (annotation-or-#f . code-string) pairs
  (let ((lines (split-lines text)))
    (let loop ((lines lines) (blocks '()) (in-block #f) (annotation #f) (current-lines '()))
      (if (null? lines)
          (reverse blocks)
          (let* ((line (car lines))
                 (trimmed (string-trim-whitespace line)))
            (cond
             ;; Opening fence
             ((and (not in-block)
                   (string-starts-with? trimmed "~~~~"))
              (let ((rest (substring trimmed 4 (string-length trimmed))))
                (let ((ann (if (> (string-length rest) 0)
                               (string-trim-whitespace rest)
                               #f)))
                  (loop (cdr lines) blocks #t ann '()))))
             ;; Closing fence
             ((and in-block
                   (string-starts-with? trimmed "~~~~"))
              (let ((code (string-join-lines (reverse current-lines))))
                (loop (cdr lines)
                      (cons (cons annotation code) blocks)
                      #f #f '())))
             ;; Inside a block
             (in-block
              (loop (cdr lines) blocks #t annotation (cons line current-lines)))
             ;; Outside a block
             (else
              (loop (cdr lines) blocks #f #f '()))))))))

(define (string-join-lines lst)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc "\n" (car rest)))))))

;; ============================================================
;; Block evaluation
;; ============================================================

(define (eval-block code block-num)
  ;; Evaluate a code string as Church code. Returns #t on success, #f on error.
  (handle-exceptions exn
    (begin
      (display "  ERROR: ")
      (display (condition->string exn))
      (newline)
      #f)
    (let ((port (open-input-string code)))
      (let read-loop ()
        (let ((expr (read port)))
          (unless (eof-object? expr)
            (let ((result (eval expr)))
              (when (not (eq? result (void)))
                (write result)
                (newline)))
            (read-loop))))
      #t)))

(define (condition->string exn)
  (with-output-to-string
    (lambda ()
      (if (condition? exn)
          (let ((msg (get-condition-property exn 'exn 'message #f))
                (args (get-condition-property exn 'exn 'arguments #f)))
            (when msg (display msg))
            (when (and args (pair? args))
              (display " ")
              (write args)))
          (begin (display "exception: ") (write exn))))))

;; ============================================================
;; Main
;; ============================================================

(define (run-chapter filename)
  (display "=== Running: ")
  (display filename)
  (display " ===")
  (newline)
  (newline)

  (let* ((text (read-file-text filename))
         (blocks (extract-code-blocks text))
         (total 0)
         (passed 0)
         (skipped 0)
         (failed 0))
    (let loop ((blocks blocks) (num 1))
      (if (null? blocks)
          (begin
            (newline)
            (display "=== Summary: ")
            (display total) (display " blocks, ")
            (display passed) (display " passed, ")
            (display failed) (display " failed, ")
            (display skipped) (display " skipped ===")
            (newline)
            (if (> failed 0) 1 0))
          (let* ((block (car blocks))
                 (annotation (car block))
                 (code (cdr block)))
            (cond
             ;; Skip annotated blocks
             ((skip-block? annotation)
              (set! skipped (+ skipped 1))
              (loop (cdr blocks) (+ num 1)))
             ;; Skip empty blocks
             ((string=? (string-trim-whitespace code) "")
              (set! skipped (+ skipped 1))
              (loop (cdr blocks) (+ num 1)))
             ;; Run the block
             (else
              (set! total (+ total 1))
              (display "[Block ")
              (display num)
              (display "]")
              (when annotation
                (display " ")
                (display annotation))
              (newline)
              (let ((ok (eval-block code num)))
                (if ok
                    (set! passed (+ passed 1))
                    (set! failed (+ failed 1))))
              (loop (cdr blocks) (+ num 1)))))))))

;; Entry point
(when (pair? (command-line-arguments))
  (let ((exit-code (run-chapter (car (command-line-arguments)))))
    (when (> exit-code 0)
      (exit 1))))
