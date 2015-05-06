#!r6rs

(library
 (mquat ilp-measurement)
 (export rewrite)
 (import (rnrs) (racr core) (racr testing) (srfi :19)
         (mquat constants) (mquat utils) (mquat main) (mquat ilp) (mquat ast-generation))
 
 (define (rewrite comp-name prop-name new-value ast)
   (rewrite-terminal 'value (att-value 'provided-clause (find (lambda (pe) (eq? (ast-child 'name pe) comp-name))
                                                              (att-value 'every-pe ast)) prop-name 'theType) (lambda _ new-value)))
 
 (define (save-ilp-timed name ast)
   (let ([result (time-it (lambda _ (save-ilp (string-append name ".txt") ast)))])
     (save-to-file (string-append name "-time.txt") (list (time-second (car result) (time-nanosecond (car result)))))))
 
 (define (run-test-one)
   (define ast
     (create-example-ast 10 0 1 1 2))
   (save-ilp-timed "test/one/01" ast))
 )