#!r6rs

(library
 (mquat ilp-measurement)
 (export run-test-one)
 (import (rnrs) (racr core) (srfi :19)
         (mquat utils) (mquat main) (mquat ilp) (mquat ast-generation) (mquat ui))
 
 (define (rw comp-name restype prop-name new-value ast)
   (debug comp-name prop-name new-value ast)
   (rewrite-terminal 'value (att-value 'provided-clause (find (lambda (pe) (eq? (ast-child 'name pe) comp-name))
                                                              (att-value 'every-pe ast)) prop-name restype)
                     (lambda _ new-value)))
 
 (define (rw* restype prop-name new-value? ast)
   (for-each
    (lambda (pe) (rewrite-terminal 'value (att-value 'provided-clause pe prop-name restype)
                                   (if new-value? (lambda _ new-value?) (rand 1 3 0))))
    (att-value 'every-pe ast)))
 
 (define (sit name ast) ; [s]ave-[i]lp-[t]imed
   (let ([result (time-it (lambda _ (save-ilp (string-append name ".lp") ast)))])
     (save-to-file (string-append name ".lp.time") (list (time-second (cdr result)) (time-nanosecond (cdr result))))))
 
 ;;; Test definitions
 
 (define (run-tests)
   (run-test-one))
 
 (define (run-test-one)
   (let* ([ast (create-system mquat-spec 10 0 1 1 2)]
          [rt (ast-child 1 (ast-child 'ResourceType* (ast-child 'HWRoot ast)))]
          [was-debugging debugging?])
     (debug "Running test one")
     (set!debugging #f)
     (sit "profiling/one/01-init" ast)
     (rw 'res-1 rt 'load 0.1 ast) (sit "profiling/one/02-comp1" ast)
     (rw 'res-1 rt 'load 0.5 ast) (sit "profiling/one/03-comp1" ast)
     (rw 'res-1 rt 'load 0.8 ast) (rw 'res-2 rt 'load 0.8 ast) (rw 'res-3 rt 'load 0.8 ast)
     (sit "profiling/one/04-three-comps" ast)
     (rw* rt 'load 0.1 ast) (sit "profiling/one/05-every-comp" ast)
     (rw* rt 'load #f ast) (sit "profiling/one/06-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit "profiling/one/07-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit "profiling/one/08-every-comp-rand" ast)
     (set!debugging was-debugging)))
 
 (define (test) (create-system mquat-spec 10 0 1 1 2))
 
 (when (find (lambda (arg) (string=? "all" arg)) (command-line))
   (run-tests)))
