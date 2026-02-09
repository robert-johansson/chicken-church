;;; church/church.scm — Main Church runtime
;;;
;;; This file loads all Church modules and provides the complete
;;; Church language runtime for Chicken Scheme.
;;;
;;; Usage:
;;;   csi -q -s church/church.scm myfile.church  ; Run a Church program
;;;   csi -q -s church/church.scm                ; Interactive
;;;
;;; Or from Scheme:
;;;   (include "church/church.scm")
;;;   ... Church code here (using church-* prefixed names or aliases) ...

;; ============================================================
;; Chicken Scheme imports
;; ============================================================

(import (chicken process-context))  ; command-line-arguments
(import (chicken condition))        ; handle-exceptions

;; ============================================================
;; Load all Church modules in dependency order
;; ============================================================

(include "church/erps.scm")
(include "church/builtins.scm")
(include "church/inference.scm")
(include "church/trace.scm")
(include "church/enumerate.scm")
(include "church/mh.scm")
(include "church/dpmem.scm")
(include "church/conditional.scm")

;; ============================================================
;; Church-specific aliases and overrides
;;
;; These shadow Scheme builtins where Church has different semantics.
;; We set them AFTER all modules are loaded so internal code can
;; still use Scheme's built-in definitions during module loading.
;;
;; IMPORTANT: We can't redefine gensym, display, or print at the
;; top level because Chicken's macro expander uses them internally.
;; Instead we install these in the eval environment for Church programs.
;; ============================================================

;; Safe to override (not used by macro expander):
(set! iota church-iota)
(set! make-list church-make-list)
(set! take church-take)
(set! drop church-drop)
(set! sort church-sort)
(set! partition church-partition)
(set! fold church-fold)
(set! foldl church-foldl)
(set! foldr church-foldr)

;; Church's true/false
(define true #t)
(define false #f)

;; ============================================================
;; Entry point: load a .church file
;; ============================================================

(define (church-load filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ()
        (let ((expr (read)))
          (unless (eof-object? expr)
            (let ((result (eval expr)))
              (when (not (eq? result (void)))
                (write result)
                (newline)))
            (loop)))))))

;; If run as a script with arguments, load the file
;; Only run if the first argument looks like a file (doesn't start with -)
(when (and (pair? (command-line-arguments))
           (let ((arg (car (command-line-arguments))))
             (and (> (string-length arg) 0)
                  (not (char=? (string-ref arg 0) #\-)))))
  (church-load (car (command-line-arguments))))
