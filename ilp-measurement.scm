#!r6rs

(library
 (mquat ilp-measurement)
 (export)
 (import (rnrs) (racr core) (srfi :19)
         (mquat ast) (mquat basic-ag) (mquat utils) (mquat main) (mquat ilp) (mquat ast-generation) (mquat ui))
 
 (define (dirname id)
   (let ([id-s (number->string id)])
     (string-append (make-string (- 3 (string-length id-s)) #\0) id-s)))
 
 (define params
   (map (lambda (l) (append (cons (dirname (car l)) (cdr l))
                            (list (list (lambda _ #t) #f #f #f))))
        (list (list  1 10 0 1 1 2)
              (list  2 10 0 10 1 2)
              (list  3 10 0 10 10 2)
              (list  4 10 0 10 10 10)
              (list  5 20 0 1 1 2)
              (list  6 20 0 10 1 2)
              (list  7 20 0 10 10 2)
              (list  8 20 0 10 10 10)
              (list  9 20 0 20 2 2)
              (list 10 25 0 25 2 2)
              (list 11 30 0 30 2 2)
              (list 12 40 0 40 2 2)
              (list 13 50 0 50 2 2))))
   
 (define (rw comp-name restype prop-name new-value ast)
;   (debug comp-name prop-name new-value ast)
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

 (define (cst id-s specs) ; [c]reate-[s]ystem-[t]imed
   (let* ([name (string-append "profiling/" id-s "/specs")]
          [result (time-it (lambda _ (apply create-system specs)))])
     (save-to-file name (cons* (time-second (cdr result)) (time-nanosecond (cdr result)) specs))
     (car result)))
 
 (define (run-test id-s specs)
   (let* ([ast (cst id-s specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))])
     (display id-s) (display " ") (flush-output-port (current-output-port))
     (sit id-s "01-init" ast)
     (rw 'res-1 rt 'load 0.1 ast) (sit id-s "02-comp1" ast)
     (rw 'res-1 rt 'load 0.5 ast) (sit id-s "03-comp1" ast)
     (rw 'res-1 rt 'load 0.8 ast) (rw 'res-2 rt 'load 0.8 ast) (rw 'res-3 rt 'load 0.8 ast)
     (sit id-s "04-three-comps" ast)
     (rw* rt 'load 0.1 ast) (sit id-s "05-every-comp" ast)
     (rw* rt 'load #f ast) (sit id-s "06-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit id-s "07-every-comp-rand" ast)
     (rw* rt 'load #f ast) (sit id-s "08-every-comp-rand" ast)))

 (cond
   [(find (lambda (arg) (string=? "all" arg)) (command-line))
    (let ([was-debugging? debugging?])
      (set!debugging #f)
      (for-each run-test (map car params) (map cdr params))
      (set!debugging was-debugging?))]
   [(find (lambda (arg) (string=? "dirs" arg)) (command-line))
    (set!debugging #f)
    (for-each (lambda (entry) (display (car entry)) (display " ")) params)]
   [(> (length (command-line)) 1)
    (let ([ids (map dirname (cdr command-line))]
          [was-debugging? debugging?])
      (set!debugging #f)
      (for-each run-test (ids) (map (lambda (id) (assq id params)) ids))
      (set!debugging was-debugging?))]))
