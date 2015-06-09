#!r6rs

(library
 (mquat ilp-measurement)
 (export measurement-cli-call)
 (import (rnrs) (racr core) (srfi :19)
         (mquat ast) (mquat basic-ag) (mquat utils) (mquat main) (mquat ilp)
         (mquat ast-generation) (mquat properties))
 
 (define (dirname id)
   (let ([id-s (if (number? id) (number->string id) id)])
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
              (list 13 50 0 50 2 2)
              (list 14 60 0 50 2 2)
              (list 15 70 0 50 2 2)
              (list 16 80 0 50 2 2)
              (list 17 80 0 60 2 2)
              (list 18 80 0 70 2 2)
              (list 19 80 0 80 2 2)
              (list 20 90 0 60 2 2)
              (list 21 90 0 70 2 2)
              (list 22 90 0 80 2 2)
              (list 23 90 0 90 2 2))))
 
 (define (valf val) (lambda _ val))
 
 (define (check-value-is-static ast)
   (let* ([first-res (car (->* (->SubResources (->HWRoot ast))))]
          [val-0 ((->value (car (->* (->ProvClause* first-res)))))]
          [val-1 ((->value (car (->* (->ProvClause* first-res)))))])
     (when (not (= val-0 val-1)) (error "check-value-is-static" "Value is not static" val-0 val-1))))

 (define (check-attribut-is-flushed-to val ast res-name restype prop-name)
   (let ([attr-val (=value-attr (=provided-clause (find (lambda (pe) (eq? (->name pe) res-name))
                                                              (=every-pe ast)) prop-name restype))])
     (when (not (= val attr-val))
       (error "check-attribut-is-flushed-to" "Value is not equal" val attr-val))))

 
 (define (rw comp-name restype prop-name new-value ast)
;   (debug comp-name prop-name new-value ast)
   (rewrite-terminal 'value (=provided-clause (find (lambda (pe) (eq? (->name pe) comp-name))
                                                              (=every-pe ast)) prop-name restype)
                     (lambda _ new-value)))
 
 (define (rw* restype prop-name new-value? ast)
   (for-each
    (lambda (pe)
      (let ([f (if new-value? (valf new-value?) (rand 1 3 0))])
       (rewrite-terminal 'value (=provided-clause pe prop-name restype) f)))
    (=every-pe ast)))
 
 (define (sit id-s suffix ast) ; [s]ave-[i]lp-[t]imed
   (debug "sit:" suffix)
   (let* ([name (string-append "profiling/" id-s "/" suffix ".lp")]
          [result (time-it (lambda _ (=to-ilp ast)))])
     (when write-ilp? (save-to-file name (car result)))
     (save-to-file (string-append name ".time") (list (car (command-line)) (time-second (cdr result))
                                                      (time-nanosecond (cdr result))))))

 (define (cst id-s specs) ; [c]reate-[s]ystem-[t]imed
   (let* ([name (string-append "profiling/" id-s "/specs")]
          [result (time-it (lambda _ (apply create-system specs)))])
     (save-to-file name (cons* (time-second (cdr result)) (time-nanosecond (cdr result)) specs))
     (car result)))
 
 (define (display+flush s) (display s) (flush-output-port (current-output-port)))
 
 (define (run-test id-s specs)
   (let* ([ast (cst id-s specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))])
     (display id-s) (display+flush " ")
     (sit id-s "01-init" ast)
     (rw 'res-1 rt 'load 0.1 ast) (sit id-s "02-comp1" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.1 ast 'res-1 rt 'load) ;<-larceny bug tracking
     (rw 'res-1 rt 'load 0.5 ast) (sit id-s "03-comp1" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.5 ast 'res-1 rt 'load) ;<-larceny bug tracking
     (rw 'res-1 rt 'load 0.8 ast) (rw 'res-2 rt 'load 0.8 ast) (rw 'res-3 rt 'load 0.8 ast)
     (sit id-s "04-three-comps" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.8 ast 'res-1 rt 'load) ;<-larceny bug tracking
     (rw* rt 'load 0.1 ast) (sit id-s "05-every-comp" ast) (display+flush ".")
;     (check-value-is-static ast) ;<-larceny bug tracking
     (rw* rt 'load #f ast) (sit id-s "06-every-comp-rand" ast) (display+flush ".")
     (rw* rt 'load #f ast) (sit id-s "07-every-comp-rand" ast) (display+flush ".")
     (rw* rt 'load #f ast) (sit id-s "08-every-comp-rand" ast) (display+flush ".")))

 (define (print-usage) (error "measurement-cli-call" "No valid arguments found, either use 'all', 'dirs' or a number of ids."))
 
 (define (measurement-cli-call command-line)
   (cond
     [(= 0 (length command-line)) (print-usage)]
     [(string=? "all" (car command-line)) (for-each run-test (map car params) (map cdr params))]
     [(string=? "dirs" (car command-line)) (for-each (lambda (entry) (display (car entry)) (display " ")) params)]
     [else (let ([ids (map dirname command-line)])
             (for-each run-test ids (map (lambda (id) (cdr (assoc id params))) ids)))])))
 