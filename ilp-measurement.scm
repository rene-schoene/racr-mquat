#!r6rs

(library
 (mquat ilp-measurement)
 (export run-tests)
 (import (rnrs) (racr core) (srfi :19)
         (mquat ast) (mquat basic-ag) (mquat utils) (mquat main) (mquat ilp) (mquat ast-generation) (mquat ui))
 
 (define params
   (list (list 1 "001" 10 0 1 1 2)
         (list 2 "002" 10 0 10 1 2)
         (list 3 "003" 10 0 10 10 2)
         (list 4 "004" 10 0 10 10 10)
         (list 5 "005" 20 0 1 1 2)
         (list 6 "006" 20 0 10 1 2)
         (list 7 "007" 20 0 10 10 2)
         (list 8 "008" 20 0 10 10 10)))
 
 (define (rw comp-name restype prop-name new-value ast)
   (debug comp-name prop-name new-value ast)
   (rewrite-terminal 'value (=provided-clause (find (lambda (pe) (eq? (->name pe) comp-name))
                                                              (=every-pe ast)) prop-name restype)
                     (lambda _ new-value)))
 
 (define (rw* restype prop-name new-value? ast)
   (for-each
    (lambda (pe) (rewrite-terminal 'value (=provided-clause pe prop-name restype)
                                   (if new-value? (lambda _ new-value?) (rand 1 3 0))))
    (=every-pe ast)))
 
 (define (sit id-s suffix ast) ; [s]ave-[i]lp-[t]imed
   (let* ([name (string-append "profiling/" id-s "/" suffix)]
          [result (time-it (lambda _ (save-ilp (string-append name ".lp") ast)))])
     (save-to-file (string-append name ".lp.time") (list (time-second (cdr result)) (time-nanosecond (cdr result))))))
 
 ;;; Test definitions
 
 (define (run-tests . ids) (for-each run-test ids))

 (define (run-simple-test id-s specs)
   (let* ([all-specs (append specs (list (list (lambda _ #t) #f #f #f)))]
          [ast (apply create-system all-specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))])
     (sit id-s "01-init" ast)
     (rw 'res-1 rt 'load 0.1 ast) (sit id-s "02-comp1" ast)
     (rw 'res-1 rt 'load 0.5 ast) (sit id-s "03-comp1" ast)
     (rw 'res-1 rt 'load 0.8 ast) (rw 'res-2 rt 'load 0.8 ast) (rw 'res-3 rt 'load 0.8 ast)
     (sit id-s "04-three-comps" ast)
     (rw* rt 'load 0.1 ast) (sit id-s "05-every-comp" ast)
     (rw* rt 'load #f ast) (sit id-s "06-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit id-s "07-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit id-s "08-every-comp-rand" ast)))

 (define (run-test id)
   (display ".")
   (let ([entry (assq id params)])
     (if entry (run-simple-test (cadr entry) (cddr entry))
         (error 'run-test "Invalid id" id))))

 (when (find (lambda (arg) (string=? "all" arg)) (command-line))
   (let ([was-debugging? debugging?])
     (set!debugging #f)
     (run-tests 1 2 3 4 5)
     (set!debugging was-debugging?)))

 (when (find (lambda (arg) (string=? "dirs" arg)) (command-line))
   (set!debugging #f)
   (for-each (lambda (entry) (display (cadr entry)) (display " ")) params)))
