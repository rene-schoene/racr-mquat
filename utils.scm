#!r6rs

(library
 (mquat utils)
 (export add-to-al merge-al union intersect-b ; associate lists
         lonely? recur ; ast
         save-to-file time-it current-date-formatted date-file-name ; measurement
         debug cli-debugging-arg=? debugging?) ; debug
 (import (rnrs) (racr core) (racr testing) (srfi :19) (srfi :27)
         (mquat constants) (mquat properties))
 
 ; Either cons val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
 (define (add-to-al al key val) (add-to-al0 al key val cons))
 
 ; Either op val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
 (define (add-to-al0 al key val op)
   (if (null? al) (list (list key (op val (list)))) ; make new entry
       (let ([entry (car al)])
         (if (eq? (car entry) key)
             (cons (list key (op val (cadr entry))) (cdr al)) ; add to entry and return
             (cons entry (add-to-al0 (cdr al) key val op)))))) ; recur
 
 ; Merge two associate lists
 (define (merge-al al1 al2) (fold-left (lambda (big-al entry) (add-to-al0 big-al (car entry) (cadr entry) append)) al1 al2))
 
 ; Returns the union of set1 and set2
 (define union
   (lambda (set1 set2)
     (letrec ([U (lambda (set2)
                   (cond ((null? set2) set1)
                         ((member (car set2) set1) (U (cdr set2)))
                         (else (cons (car set2) (U (cdr set2))))))])
       (U set2))))
 
 ; If (eq? start set1) return set2, else return the intersection of set1 and set2
 (define intersect-b
   (lambda (start set1 set2)
     (letrec([I (lambda (set2)
                  (cond ((null? set2) set2)
                        ((member (car set2) set1) (cons (car set2) (I (cdr set2))))
                        (else (I (cdr set2)))))])
       (if (eq? start set1) set2 (I set2)))))
 
 ; Returns true, iff the node does not have any true siblings, i.e. if its parent does have more than one children
 (define lonely? (lambda (n) (= 1 (ast-num-children (ast-parent n)))))
 
 ; Apply the given attribute on each child and aggregate with op
 (define (recur n op att childs . args) (fold-left (lambda (result sub) (op (apply att sub args) result))
                                                   (list) (ast-children (childs n))))
 

 ;; Debugging
 
 (define (cli-debugging-arg=? arg) (or (string=? "-v" arg) (string=? "--verbose" arg)))
 (define debugging verbose?)
 (define (debugging?) debugging)

 ; Displays the given arguments
 (define (debug0 . args)
  (letrec
      ([D (lambda (loa) ; [l]ist [o]f [a]rgs
            (cond
              ((= (length loa) 0) "") ;no arguments given
              ((null? (car loa)) "") ;end of recursion
              (else ;recure with cdr
               (string-append (P (car loa)) " " (D (cdr loa))))))]
       [P (lambda (s)
             (cond
               ((string? s) s)
               ((boolean? s) (if s "#t" "#f"))
               ((symbol? s) (symbol->string s))
               ((number? s) (number->string s))
               ((list? s) (string-append "(" (D s) ")"))
               ((procedure? s) "<proc>")
               ((ast-node? s) (if (ast-has-child? 'name s) (P (ast-child 'name s))
                                  (string-append "<node " (symbol->string (ast-node-type s)) ">")))
               (else "?")))])
    (display (D args)) (display "\n") (when (not (null? args)) (car args))))
 
 (define-syntax debug
   (syntax-rules ()
     [(_ args ...) (and debugging (debug0 args ...))]))
 
 ;; Text save
 
 ; Prints each list on its own line
 (define (print-per-line x nl)
   (cond
     ((null? x) (if nl (newline)))
     ((list? x)
      (if (list? (car x))
          (begin
            (print-per-line (car x) #f)
            ;(newline)
            (print-per-line (cdr x) #f))
          (begin
            (print-per-line (car x) #f)
            (print-per-line (cdr x) #t))))
     (else (display x) (display " "))))
 
 ; Prints the values to the file with the given path
 (define (save-to-file path values)
   (if (file-exists? path) (delete-file path))
   (with-output-to-file path
     (lambda () (for-each (lambda (x) (print-per-line x #t)) values) (newline))))
 
 (define (with-append-output-to-file path thunk)
;   (let ([former-port (current-output-port)])
   (let ([port (open-file-output-port path (file-options no-create no-truncate))])
     (call-with-port port thunk)
     (close-output-port port)));)
   
 ;; Measurement
 
 ; Execute the given 0-arg procedure and returns (time-diff result)
 (define (time-it proc)
  (let ([before (current-time)])
    (let ([result (proc)])
      (cons result (time-difference (current-time) before)))))
 
 (define (current-date-formatted)
   (let ([now (current-date)])
     (string-append (number->string (date-year now)) "_"
                    (number->string (date-month now)) "_"
                    (number->string (date-day now)) "T"
                    (number->string (date-hour now)) "_"
                    (number->string (date-minute now)) "_"
                    (number->string (date-second now)) "_"
                    (number->string (date-nanosecond now)))))
 
 (define (date-file-name prefix ext) (string-append prefix (current-date-formatted) "." ext)))
