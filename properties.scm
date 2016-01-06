#!r6rs

(library
 (mquat properties)
 (export timing? profiling? log.debug? log.info? log.warn? write-ilp? measure-flush? measure-non-cached? preesleep)
 (import (rnrs) (only (srfi :13) string-tokenize string-index string-trim-both) (srfi :14))

 (define (file->char_list path)
   (call-with-input-file path
     (lambda (input-port)
       (let loop ((x (read-char input-port)))
         (if (eof-object? x) (list)
             (cons x (loop (read-char input-port))))))))

 (define (load-file f) (apply string (file->char_list f)))

 (define (read-properties f)
   (filter (lambda (line) (not (eq? (string-ref line 0) #\#)))
           (string-tokenize (load-file f) (char-set-union char-set:letter+digit char-set:blank char-set:punctuation))))

 (define (parse-properties f)
   (make-pairs (map string-trim-both (read-properties f))))

 (define (parse-value v)
   (cond
     [(find (lambda (x) (string=? x v)) (list "0" "false" "False")) #f]
     [(find (lambda (x) (string=? x v)) (list "1" "true" "True")) #t]
     [else v]))

 (define (make-pairs l)
   (if (or (null? l) (null? (cdr l))) (list)
       (cons (cons (car l) (parse-value (cadr l)))
             (make-pairs (cddr l)))))

 (define property-list (parse-properties "scheme.properties"))
 (define (get-value name default) (let ([entry (assp (lambda (x) (string=? name x)) property-list)])
                                    (if entry (cdr entry) default)))

 (define timing? (get-value "timing" #f))
 (define profiling? (get-value "measure.profiling" #f))
 (define log.debug? (get-value "log.debug" #f))
 (define log.info? (get-value "log.info" #t))
 (define log.warn? (get-value "log.warn" #t))
 (define write-ilp? (get-value "measure.lp.write" #f))
 (define measure-flush? (get-value "measure.flush" #f))
 (define measure-non-cached? (get-value "measure.non-cached" #f))
 (define preesleep (get-value "measure.preesleep" 0.0)))
