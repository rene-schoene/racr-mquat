#!r6rs

(library
 (mquat ilp-measurement)
 (export measurement-cli-call)
 (import (rnrs) (racr core) (srfi :19) (only (srfi :13) string-prefix? string-suffix?)
         (mquat ast) (mquat basic-ag) (mquat utils) (mquat join) (mquat ilp) (mquat constants)
         (mquat ast-generation) (mquat properties))

 (define (dirname kind id)
   (let ([id-s (if (number? id) (number->string id) id)])
     (string-append kind (if measure-non-chached? "noncached-" (if measure-flush? "flush-" ""))
                    (make-string (- 3 (string-length id-s)) #\0) id-s)))

 (define resource-test 'resource-test)
 (define sw-test 'sw-test)
 (define update-test 'update-test)
 (define raw-paper-params
   ; HW = number hw. S = number of sub-per-hw. #C[omp], #I[mpl], #M[ode]
   ;           ID  HW  S #C #I #M
   (list (list  1  10  0  2  1  2)
         (list  2  10  0 10  1  2)
         (list  3  10  0 10 10  2)
         (list  4  10  0 10 10 10)
         (list  5  20  0  1  1  2)
         (list  6  20  0 10  1  2)
         (list  7  20  0 10 10  2)
         (list  8  20  0 10 10 10)
         (list  9  20  0 20  2  2)
         (list 10  25  0 25  2  2)
         (list 11  30  0 30  2  2)
         (list 12  40  0 40  2  2)
         (list 13  50  0 50  2  2)
         (list 14  60  0 50  2  2)
         (list 15  70  0 50  2  2)
         (list 16  80  0 50  2  2)
         (list 17  80  0 60  2  2)
         (list 18  80  0 70  2  2)
         (list 19  80  0 80  2  2)
         (list 20  90  0 60  2  2)
         (list 21  90  0 70  2  2)
         (list 22  90  0 80  2  2)
         (list 23  90  0 90  2  2)
         (list 24 100 10 10  2  2)
         (list 25 200 10 10  2  2)
         (list 26 300 10 10  2  2)
         (list 27 400 10 10  2  2)
         (list 28  10  0 10  2  2)
         (list 29  10  0 20  2  2)
         (list 30  10  0 40  2  2)
         (list 31  10  0 80  2  2)))
 (define raw-params
   ; HW = number hw. S = number of sub-per-hw. #C[omp], #I[mpl], #M[ode]
   ;           ID  HW  S #C #I #M
   (list (list  1  10  0  1  1  2)
         (list  2  10  0 10  1  2)
         (list  3  10  0 10 10  2)
         (list  4  10  0 10 10 10)
         (list  5  20  0  1  1  2)
         (list  6  20  0 10  1  2)
         (list  7  20  0 10 10  2)
         (list  8  20  0 10 10 10)
         (list  9  20  0 20  2  2)
         (list 10  25  0 25  2  2)
         (list 11  30  0 30  2  2)
         (list 12  40  0 40  2  2)
         (list 13  50  0 50  2  2)
         (list 14  60  0 50  2  2)
         (list 15  70  0 50  2  2)
         (list 16  80  0 50  2  2)
         (list 17  80  0 60  2  2)
         (list 18  80  0 70  2  2)
         (list 19  80  0 80  2  2)
         (list 20  90  0 60  2  2)
         (list 21  90  0 70  2  2)
         (list 22  90  0 80  2  2)
         (list 23  90  0 90  2  2)
         (list 24 100 10 10  2  2)
         (list 25 200 10 10  2  2)
         (list 26 300 10 10  2  2)
         (list 27 400 10 10  2  2)
         (list 28  10  0 10  2  2)
         (list 29  10  0 20  2  2)
         (list 30  10  0 40  2  2)
         (list 31  10  0 80  2  2)))
 (define params
   (append
     ; paper params
     (reverse
      (fold-left
       (lambda (all l) (let ([rest (append (cdr l) (list (list (lambda _ #t) #f #f #f)))])
                         (cons* (cons* (dirname "res-" (car l)) resource-test rest)
                                (cons* (dirname "sw-" (car l))  sw-test rest)
                                all)))
       (list) raw-paper-params))
     ; normal params
     (map (lambda (l) (append (cons* (dirname "update-" (car l)) update-test (cdr l))
                              (list (list (lambda _ #t) #f #f #f))))
          raw-params)))

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
   (rewrite-terminal 'value (=provided-clause (find (lambda (pe) (string=? (->name pe) comp-name))
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
   (let* ([name (string-append "profiling/" id-s "/" suffix)]
          [result (time-it (lambda _ (=to-ilp ast)))]
          [ilp (if profiling? (caar result) (car result))])
     (when write-ilp? (save-to-file (string-append name ".lp") ilp))
     (when measure-flush? (display+flush "_") (touch-terminals ast))
     (when profiling? (save-to-file (string-append name "-att.csv") (cdar result)))
     (save-to-file (string-append name ".lp.time") (list (car (command-line)) (time-second (cdr result))
                                                      (time-nanosecond (cdr result))))))

 (define (cst id-s specs) ; [c]reate-[s]ystem-[t]imed
   (let* ([name (string-append "profiling/" id-s "/specs")]
          [result (time-it (lambda _ (apply create-system specs)))])
     (save-to-file name (cons* (time-second (cdr result)) (time-nanosecond (cdr result)) specs))
     (car result)))

 (define (display+flush s) (display s) (flush-output-port (current-output-port)))

 (define (touch-terminals node)
   (debug "touch-terminals" node)
   (ast-for-each-child (lambda (i child) (if (and (ast-node? child))
                                             (touch-terminals child)
                                             (begin (debug "rewrite-terminal" i node child) (rewrite-terminal i node child))))
                       node))

 (define (run-test id-s specs)
   (when profiling? (reset-counts))
   (cond
     [(eq? (car specs) resource-test) (run-resource-test id-s (cdr specs))]
     [(eq? (car specs) sw-test) (run-sw-test id-s (cdr specs))]
     [(eq? (car specs) update-test) (run-update-test id-s (cdr specs))]
     [else (error "Unknown kind" (car specs))]))

 (define (ret-odds lst)
   (if (null? lst) (list)
       (cons (car lst)
             (if (null? (cdr lst)) (list) (ret-odds (cdr (cdr lst)))))))

 (define (run-resource-test id-s specs)
   (let* ([ast (cst id-s specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))]
          [pe+parent (lambda (pe) (cons pe (<- pe)))]
          [first-pe (pe+parent (car (=every-container ast)))]
          [odd-pes (map pe+parent (ret-odds (=every-container ast)))])
     (display id-s)
     (rewrite-terminal 'config ast id-s)
     (sit id-s "01-init" ast) (display+flush ".")
     (let ([removed (rewrite-delete (car first-pe))])
       (sit id-s "02-del-pe1" ast) (display+flush ".")
       (rewrite-add (cdr first-pe) removed)
       (sit id-s "03-add-pe1" ast) (display+flush "."))
     ;pp = pe and parent
     (let ([removed (map (lambda (pp) (cons (rewrite-delete (car pp)) (cdr pp))) odd-pes)])
       (sit id-s "04-del-odd-pes" ast) (display+flush ".")
       ;rp = removed and parent
       (for-each (lambda (rp) (rewrite-add (cdr rp) (car rp))) (reverse removed))
       (rw* rt "load" #f ast) (sit id-s "05-add-odd-pes" ast) (display+flush "."))
     (let ([removed (map (lambda (pp) (cons (rewrite-delete (car pp)) (cdr pp))) odd-pes)])
       (rw* rt "load" #f ast)  (sit id-s "06-del-change-odd-pes" ast) (display+flush ".")
       ;rp = removed and parent
       (for-each (lambda (rp) (rewrite-add (cdr rp) (car rp))) (reverse removed))
       (rw* rt "load" #f ast) (sit id-s "07-add-odd-pes" ast) (display+flush "."))))

 (define (run-sw-test id-s specs)
   (let* ([ast (cst id-s specs)]
          [energy (ast-find-child (lambda (i child) (string=? (->name child) pn-energy)) (->RealProperty* (->SWRoot ast)))])
     (define (add-comp comp-nr)
       (debug "#create new comp" comp-nr)
       (let ([new (:Comp mquat-spec (node-name "c" (list comp-nr)) (list) #f
                         (list (:RealProperty mquat-spec (node-name "p" (list comp-nr))
                                              #f "runtime" 'increasing agg-sum)
                               (:PropertyRef mquat-spec energy)))])
         (rewrite-add (->Comp* (->SWRoot ast)) new) new))
     (define (find-create l prefix lon make-new)
       (let ([name (node-name prefix lon)])
         (or (ast-find-child (lambda (i child) (string=? (->name child) name)) l) (make-new))))
     (define (find-create-comp comp-nr) (find-create (->Comp* (->SWRoot ast)) "c" (list comp-nr) (lambda _ (add-comp comp-nr))))
     (define (add-impl comp-nr impl-nr reqcomps)
       (debug "#create new impl" comp-nr impl-nr reqcomps)
       (let ([new (:Impl mquat-spec (node-name "i" (list impl-nr comp-nr)) (list)
                         (map (lambda (nr) (find-create-comp nr)) reqcomps) #f #f)])
         (rewrite-add (->Impl* (find-create-comp comp-nr)) new) new))
     (define (find-create-impl comp-nr impl-nr reqcomps) (find-create (->Impl* (find-create-comp comp-nr)) "i"
                                                                      (list impl-nr comp-nr)
                                                                      (lambda _ (add-impl comp-nr impl-nr reqcomps))))
     (define (add-mode comp-nr impl-nr mode-nr req-comp-nr load-f energy-f prov-f prev-f)
       (debug "#create new mode" comp-nr impl-nr mode-nr req-comp-nr)
       (let* ([impl (find-create-impl comp-nr impl-nr (if req-comp-nr (list req-comp-nr) (list)))]
              [find-prop-hw (lambda (name) (ast-find-child (lambda (i child) (string=? (->name (=real child)) name))
                                                           (->Property* (car (->* (->ResourceType* (->HWRoot ast)))))))]
              [find-prop-sw (lambda (name comp) (ast-find-child (lambda (i child) (string=? (->name (=real child)) name))
                                                                (->Property* comp)))]
              [load (find-prop-hw load-name)]
              [energy (find-prop-sw pn-energy (find-create-comp comp-nr))]
              [prev-p (and req-comp-nr (find-prop-sw (node-name "p" (list req-comp-nr)) (find-create-comp req-comp-nr)))]
              [this-p (find-prop-sw (node-name "p" (list comp-nr)) (find-create-comp comp-nr))]
              [clauses (filter (lambda (c) c) (list (:ReqClause mquat-spec load comp-max-eq load-f)
                                                    (:ProvClause mquat-spec energy comp-max-eq energy-f)
                                                    (:ProvClause mquat-spec this-p comp-max-eq prov-f)
                                                    (and req-comp-nr (:ReqClause mquat-spec prev-p comp-max-eq prev-f))))]
              [new (:Mode mquat-spec (node-name "m" (list mode-nr impl-nr comp-nr)) clauses)])
         (rewrite-add (->Mode* impl) new) new))
     (define (comp-nr-of i) (ast-child-index (<=comp i)))
     (define (impl-nr-of i) (ast-child-index i))
     (define (make-new-modes)
       ; Add new mode to every impl
       (for-each (lambda (i) (let ([comp-nr (comp-nr-of i)])
                               (add-mode comp-nr (impl-nr-of i) (+ 1 (length (->* (->Mode* i))))
                                         (if (eq? comp-nr 1) #f (- comp-nr 1)) (lambda _ 0.8) ;req-comp-nr load-f
                                         (lambda _ 20) (lambda _ 2) (lambda _ 7)))) ;energy-f prov-f prev-f
                 (=every-impl ast)))
     (define (delete-odd-modes) (for-each (lambda (m) (rewrite-delete m)) (ret-odds (=every-mode ast))))

     (display id-s)
     (rewrite-terminal 'config ast id-s)
     (sit id-s "01-init" ast) (display+flush ".")
     (make-new-modes) (sit id-s "02-new-modes" ast) (display+flush ".")
     (make-new-modes) (sit id-s "03-new-modes" ast) (display+flush ".")
     (make-new-modes) (sit id-s "04-new-modes" ast) (display+flush ".")
     (delete-odd-modes) (sit id-s "05-del-modes" ast) (display+flush ".")
     (delete-odd-modes) (sit id-s "06-del-modes" ast) (display+flush ".")
     (delete-odd-modes) (sit id-s "07-del-modes" ast) (display+flush ".")))

 (define (run-update-test id-s specs)
   (let* ([ast (cst id-s specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))])
     (rewrite-terminal 'config ast id-s)
     (display id-s)
     (sit id-s "01-init" ast)
     (rw "r-1" rt "load" 0.1 ast) (sit id-s "02-comp1" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.1 ast 'r-1 rt "load") ;<-larceny bug tracking
     (rw "r-1" rt "load" 0.5 ast) (sit id-s "03-comp1" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.5 ast 'r-1 rt "load") ;<-larceny bug tracking
     (rw "r-1" rt "load" 0.8 ast) (rw "r-2" rt "load" 0.8 ast) (rw "r-3" rt "load" 0.8 ast)
     (sit id-s "04-three-comps" ast) (display+flush ".")
;     (check-attribut-is-flushed-to 0.8 ast 'r-1 rt "load") ;<-larceny bug tracking
     (rw* rt "load" 0.1 ast) (sit id-s "05-every-comp" ast) (display+flush ".")
;     (check-value-is-static ast) ;<-larceny bug tracking
     (rw* rt "load" #f ast) (sit id-s "06-every-comp-rand" ast) (display+flush ".")
     (rw* rt "load" #f ast) (sit id-s "07-every-comp-rand" ast) (display+flush ".")
     (rw* rt "load" #f ast) (sit id-s "08-every-comp-rand" ast) (display+flush ".")))

 (define (print-usage) (error "measurement-cli-call" "No valid arguments found, use 'all', 'dirs', 'prefix', 'suffix' or a number of ids."))

 (define (measurement-cli-call command-line)
   (let ([first (if (= 0 (length command-line)) #f (car command-line))])
       (cond
         [(= 0 (length command-line)) (print-usage)]
         [(string=? "all" first) (for-each run-test (map car params) (map cdr params))]
         [(string=? "dirs" first) (for-each (lambda (entry) (display (car entry)) (display " ")) params)]
         [(string=? "prefix" first)
            (let ([param_subset (filter (lambda (p) (string-prefix? (cadr command-line) (car p))) params)])
                (for-each run-test (map car param_subset) (map cdr param_subset)))]
         [(string=? "suffix" first)
            (let ([param_subset (filter (lambda (p) (string-suffix? (cadr command-line) (car p))) params)])
                (for-each run-test (map car param_subset) (map cdr param_subset)))]
         [else (for-each run-test command-line (map (lambda (id) (cdr (assoc id params))) command-line))]))))
