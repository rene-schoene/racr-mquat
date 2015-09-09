#!r6rs

(library
 (mquat utils)
 (export add-to-al merge-al union intersect-b ; associate lists
         lonely? recur ; ast
         save-to-file ; files
         time-it current-date-formatted date-file-name; measurement
         att-value-call att-value-compute print-counts get-counts ; profiling
         debug info warn log0) ; logging
 (import (rnrs) (rnrs mutable-pairs) (racr core) (racr testing) (srfi :19) (srfi :27)
         (only (srfi :13) string-pad string-pad-right string-take)
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

 ; Displays the given arguments
 (define (log0 . args)
   (map (lambda (x) (display (if (ast-node? x)
                                 (if (ast-has-child? 'name x) (ast-child 'name x)
                                     (string-append "<node " (cond [(ast-list-node? x)   "#List"]
                                                                   [(ast-bud-node? x)    "#Bud"]
                                                                   [else (symbol->string (ast-node-type x))]) ">")) x))
          (display " ")) args) (newline) (when (not (null? args)) (car args)))

 (define-syntax debug (syntax-rules () [(_ args ...) (and log.debug? (log0 args ...))]))
 (define-syntax info (syntax-rules () [(_ args ...) (and log.info? (log0 args ...))]))
 (define-syntax warn (syntax-rules () [(_ args ...) (and log.warn? (log0 args ...))]))

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
                    (string-take (number->string (date-nanosecond now)) 3))))

 (define (date-file-name prefix ext) (string-append prefix (if measure-non-chached? "-noncached" (if measure-flush? "-flush" ""))
                                                    "-" (current-date-formatted) "." ext))

 ; name -> call-count
 (define call-counts (list))
 ; name -> call-count
 (define compute-counts (list))

 (define (updated-al al entry update-f default-f)
   (if entry (map (lambda (e) (if (eq? e entry) (update-f) e)) al)
       (cons (default-f) al)))

 (define (att-value-call name n . args)
   (when profiling?
     (let ([entry (assq name call-counts)])
       (set! call-counts (updated-al call-counts entry
                                     (lambda _ (let ([count (cdr entry)])
                                                 (cons (car entry) (+ 1 count))))
                                     (lambda _ (cons name 1))))))
   (apply att-value (cons* name n args)))

 (define (att-value-compute name)
   (when profiling?
     (let ([entry (assq name compute-counts)])
       (set! compute-counts (updated-al compute-counts entry
                                        (lambda _ (let ([count (cdr entry)])
                                                    (cons (car entry) (+ 1 count))))
                                        (lambda _ (cons name 1)))))))

 (define (maybe entry default) (if entry (number->string (cdr entry)) default))

 (define (print-counts)
   (info "Counts")
   (let ([max-name (apply max (map (lambda (x) (string-length (symbol->string (car x)))) call-counts))])
     (map (lambda (entry) (let ([name (car entry)])
                            (info (string-pad-right (symbol->string name) max-name)
                                  (string-pad (maybe (assq name compute-counts) "?") 10)
                                  "/" (string-pad (number->string (cdr entry)) 10))))
          call-counts)))

  (define (get-counts)
    (map (lambda (entry)
      (let ([name (car entry)])
        (list (symbol->string name) "," (maybe (assq name compute-counts) "-1") "," (number->string (cdr entry)))))
         call-counts)))

; (define (print-compute-counts) (print-per-line compute-counts #t)))
