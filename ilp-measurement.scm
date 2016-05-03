#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

; Measurement definitions
; To add a new measurement FOO:
;   1) define a new scheme symbol (define foo-test 'foo-test)
;   2) reuse or define a new set of raw parameters
;   3) append raw parameters to params, ensure to use the symbol from (1)
;   4) define a method (define (run-foo-test id-s specs) to run the measurement, taking two input parameters (id and parameters)
;   5) append (cons foo-test run-foo-test) to tests

(library
 (mquat ilp-measurement)
 (export measurement-cli-call)
 (import (rnrs) (racr core) (srfi :19) (only (srfi :13) string-prefix? string-suffix? string-pad)
         (mquat ast) (mquat basic-ag) (mquat utils) (mquat join) (mquat ilp) (mquat constants)
         (mquat ast-generation) (mquat properties))

 (define (dirname kind id)
   (let ([id-s (if (number? id) (number->string id) id)])
     (string-append kind (if measure-non-cached? "noncached-" (if measure-flush? "flush-" ""))
                    (make-string (- 3 (string-length id-s)) #\0) id-s)))

 (define resource-test 'resource-test)
 (define sw-test 'sw-test)
 (define update-test 'update-test)
 (define complex-test 'complex-test)
 (define mixed-test 'mixed-test)
 (define scaling-test 'scaling-test)
 (define raw-mixed-params
   ; HW = number hw. S = number of sub-per-hw. #C[omp], #I[mpl], #M[ode]
   ;           ID  HW  S #C #I #M
   (list (list  1 300  0 10  2  2)
         (list  2 400  0 10  2  2)))
 (define raw-scaling-params
   ; HW = number hw. S = number of sub-per-hw. #C[omp], #I[mpl], #M[ode]
   ;            ID   HW    S #C #I #M
   (list (list   1     10  0 10  2  2)
         (list   2     50  0 10  2  2)
         (list   3    100  0 10  2  2)
         (list   4    500  0 10  2  2)
         (list   5   1000  0 10  2  2)
         (list   6   5000  0 10  2  2)
         (list   7  10000  0 10  2  2)
         (list   8  50000  0 10  2  2)
         (list   9 100000  0 10  2  2)
         (list  10 500000  0 10  2  2)))
 (define raw-short-params
   ; HW = number hw. S = number of sub-per-hw. #C[omp], #I[mpl], #M[ode]
   ;           ID  HW  S #C #I #M
   (list (list  1  10  0  2  1  2)
         (list  2  40  0 40  2  2)
         (list  3  50  0 50  2  2)
         (list  4  60  0 50  2  2)
         (list  5  70  0 50  2  2)
         (list  6  80  0 50  2  2)
         (list  7  90  0 60  2  2)
         (list  8 100 10 10  2  2)
         (list  9 200 10 10  2  2)
         (list 10 300 10 10  2  2)
         (list 11 400 10 10  2  2)))
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
     ; update, res and sw params
     (reverse
      (fold-left
       (lambda (all l) (let ([rest (append (cdr l) (list (list (lambda _ #t) #f #f #f)))])
                         (cons* (cons* (dirname "res-" (car l)) resource-test rest)
                                (cons* (dirname "sw-" (car l))  sw-test rest)
                                (cons* (dirname "update-" (car l))  update-test rest)
                                all)))
       (list) raw-params))
     ; mixed params
     (map (lambda (l) (append (cons* (dirname "mixed-" (car l)) mixed-test (cdr l))
                              (list (list (lambda _ #t) #f #f #f))))
          raw-mixed-params)
     ; scaling params
     (map (lambda (l) (append (cons* (dirname "scaling-" (car l)) scaling-test (cdr l))
                              (list (list (lambda _ #t) #f #f #f))))
          raw-scaling-params)
          ))

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

 (define (add-comp ast comp-nr)
   (debug "#create new comp" comp-nr)
   (let* ([energy (ast-find-child (lambda (i child) (string=? (->name child) pn-energy)) (->RealProperty* (->SWRoot ast)))]
          [new (:Comp mquat-spec (node-name "c" (list comp-nr)) (list) #f
                      (list (:RealProperty mquat-spec (node-name "p" (list comp-nr))
                                           #f "runtime" 'increasing agg-sum)
                            (:PropertyRef mquat-spec energy)))])
     (rewrite-add (->Comp* (->SWRoot ast)) new) new))
 (define (find-create ast l prefix lon make-new)
   (let ([name (node-name prefix lon)])
     (or (ast-find-child (lambda (i child) (string=? (->name child) name)) l) (make-new))))
 (define (find-create-comp ast comp-nr) (find-create ast (->Comp* (->SWRoot ast)) "c" (list comp-nr)
                                                     (lambda _ (add-comp ast comp-nr))))
 (define (add-impl ast comp-nr impl-nr reqcomps)
   (debug "#create new impl" comp-nr impl-nr reqcomps)
   (let ([new (:Impl mquat-spec (node-name "i" (list impl-nr comp-nr)) (list)
                     (map (lambda (nr) (find-create-comp ast nr)) reqcomps) #f #f)])
     (rewrite-add (->Impl* (find-create-comp ast comp-nr)) new) new))
 (define (find-create-impl ast comp-nr impl-nr reqcomps) (find-create ast (->Impl* (find-create-comp ast comp-nr)) "i"
                                                                  (list impl-nr comp-nr)
                                                                  (lambda _ (add-impl ast comp-nr impl-nr reqcomps))))
 (define (add-mode ast comp-nr impl-nr mode-nr req-comp-nr load-f energy-f prov-f prev-f)
   (debug "#create new mode" comp-nr impl-nr mode-nr req-comp-nr)
   (let* ([impl (find-create-impl ast comp-nr impl-nr (if req-comp-nr (list req-comp-nr) (list)))]
          [find-prop-hw (lambda (name) (ast-find-child (lambda (i child) (string=? (->name (=real child)) name))
                                                       (->Property* (car (->* (->ResourceType* (->HWRoot ast)))))))]
          [find-prop-sw (lambda (name comp) (ast-find-child (lambda (i child) (string=? (->name (=real child)) name))
                                                            (->Property* comp)))]
          [load (find-prop-hw pn-load)]
          [energy (find-prop-sw pn-energy (find-create-comp ast comp-nr))]
          [prev-p (and req-comp-nr (find-prop-sw (node-name "p" (list req-comp-nr)) (find-create-comp ast req-comp-nr)))]
          [this-p (find-prop-sw (node-name "p" (list comp-nr)) (find-create-comp ast comp-nr))]
          [clauses (filter (lambda (c) c) (list (:ReqClause mquat-spec (:PropertyRef mquat-spec pn-load) comp-max-eq load-f)
                                                (:ProvClause mquat-spec (:PropertyRef mquat-spec pn-energy) comp-max-eq energy-f)
                                                (:ProvClause mquat-spec (:PropertyRef mquat-spec (->name this-p)) comp-max-eq prov-f)
                                                (and req-comp-nr (:ReqClause mquat-spec (:PropertyRef mquat-spec (->name prev-p)) comp-max-eq prev-f))))]
          [new (:Mode mquat-spec (node-name "m" (list mode-nr impl-nr comp-nr)) clauses)])
     (rewrite-add (->Mode* impl) new) new))
 (define (comp-nr-of i) (ast-child-index (<=comp i)))
 (define (impl-nr-of i) (ast-child-index i))
 (define (make-new-modes ast)
   ; Add new mode to every impl
   (for-each (lambda (i) (let ([comp-nr (comp-nr-of i)])
                           (add-mode ast comp-nr (impl-nr-of i) (+ 1 (length (->* (->Mode* i))))
                                     (if (eq? comp-nr 1) #f (- comp-nr 1)) (lambda _ 0.8) ;req-comp-nr load-f
                                     (lambda _ 20) (lambda _ 2) (lambda _ 7)))) ;energy-f prov-f prev-f
             (=every-impl ast)))
 (define (delete-odd-modes ast) (for-each (lambda (m) (rewrite-delete m)) (ret-odds (=every-mode ast))))

 (define (run-sw-test id-s specs)
   (let ([ast (cst id-s specs)])
     (display id-s)
     (rewrite-terminal 'config ast id-s)
     (sit id-s "01-init" ast) (display+flush ".")
     (make-new-modes ast) (sit id-s "02-new-modes" ast) (display+flush ".")
     (make-new-modes ast) (sit id-s "03-new-modes" ast) (display+flush ".")
     (make-new-modes ast) (sit id-s "04-new-modes" ast) (display+flush ".")
     (delete-odd-modes ast) (sit id-s "05-del-modes" ast) (display+flush ".")
     (delete-odd-modes ast) (sit id-s "06-del-modes" ast) (display+flush ".")
     (delete-odd-modes ast) (sit id-s "07-del-modes" ast) (display+flush ".")))

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

 ; 01 init (4 sw component, N containers)
 ; 02 change values of containers
 ; 03 create new modes for each implementation
 ; 04 change values of containers
 ; 05 add new software component
 ; 06 change values of containers
 ; 07 remove some containers
 ; 08 change values of containers
 ; 09 (change nothing)
 ; 10 add previously removed containers (from 07)
 ; 11 remove some modes
 (define (run-complex-test id-s specs)
   (define (new-request-comp ast)
     (let ([req-comp-nr (ast-child-index (->target (<=request ast)))]
           [new-comp-nr (+ 1 (ast-num-children (->Comp* (->SWRoot ast))))])
       ;params of add-mode: ast comp-nr impl-nr mode-nr req-comp-nr load-f energy-f prov-f prev-f
       (add-mode ast new-comp-nr 1 1 ;comp-nr impl-nr mode-nr
                 req-comp-nr (lambda _ 0.8) ;req-comp-nr load-f
                 (lambda _ 20) (lambda _ 2) (lambda _ 7)) ;energy-f prov-f prev-f
       ;adjust request to target new comp
       (rewrite-terminal 'target (<=request ast) (find-create-comp ast new-comp-nr))))
   (let* ([ast (cst id-s specs)]
          [rt (ast-child 1 (->ResourceType* (->HWRoot ast)))]
          [pe+parent (lambda (pe) (cons pe (<- pe)))]
          [odd-pes (map pe+parent (ret-odds (=every-container ast)))])
     (rewrite-terminal 'config ast id-s)
     (display id-s)
     (sit id-s "01-init" ast)
     (rw* rt "load" #f ast) (sit id-s "02-every-comp-rand" ast) (display+flush ".")
     (make-new-modes ast) (sit id-s "03-new-modes" ast) (display+flush ".")
     (rw* rt "load" #f ast) (sit id-s "04-every-comp-rand" ast) (display+flush ".")
     (new-request-comp ast) (sit id-s "05-new-comp" ast) (display+flush ".")
     (rw* rt "load" #f ast) (sit id-s "06-every-comp-rand" ast) (display+flush ".")
     (let ([removed (map (lambda (pp) (cons (rewrite-delete (car pp)) (cdr pp))) odd-pes)])
       (sit id-s "07-del-odd-pes" ast) (display+flush ".")
       (rw* rt "load" #f ast) (sit id-s "08-every-comp-rand" ast) (display+flush ".")
       (sit id-s "09-no-change" ast) (display+flush ".")
       ;rp = removed and parent
       (for-each (lambda (rp) (rewrite-add (cdr rp) (car rp))) (reverse removed))
       (rw* rt "load" #f ast) (sit id-s "10-add-odd-pes" ast) (display+flush "."))
     (delete-odd-modes ast) (sit id-s "11-del-modes" ast) (display+flush ".")))

 ; make change kind frequency according to some measure, e.g. 80% update, 10% res, 5% sw, 5% no change
 ; = 5x (16 update, 2 res, 1 sw, 1 no)
 ; = 5x (3 update + res + 3 update + sw + 4 update + no + 3 update + res + 3 update)
 (define (run-mixed-test id-s specs)
   (define ast (cst id-s specs))
   (define rt (ast-child 1 (->ResourceType* (->HWRoot ast))))
   (define pe+parent (lambda (pe) (cons pe (<- pe))))
   (define odd-pes (map pe+parent (ret-odds (=every-container ast))))
   (define (make-step-name outer step suffix) (string-append (string-pad (number->string (+ (* 20 outer) step)) 2 #\0) suffix))
   (define (add-new-resource)
     (let* ([max-id (apply max (map (lambda (pe) (string->number (substring (->name pe) 2))) (=every-container ast)))]
            [first-clauses (->* (->ProvClause* (car (=every-container ast))))]
            [new-clauses (map (lambda (cl) (make-prov (:PropertyRef mquat-spec (ast-child 'refname (->ReturnType cl))) (->comparator cl) (rand 50 2 0))) first-clauses)] ;TODO
            [new-res (:Resource mquat-spec (string-append "r-" (number->string (+ 1 max-id))) rt online (list) new-clauses)])
        (rewrite-add (->SubResources (->HWRoot ast)) new-res)))
   (define (update-change outer steps)
     (for-each
       (lambda (step)
         (when (not (and (= outer 4) (= step 20)))
           (rw* rt "load" #f ast) (sit id-s (make-step-name outer step "-every-comp-rand") ast) (display+flush "."))) steps))
   (rewrite-terminal 'config ast id-s)
   (display+flush id-s)

;   (info (map (lambda (pe) (string->number (substring (->name pe) 2))) (=every-container ast)))
   (add-new-resource) ;; <- just for testing

   (sit id-s "00-init" ast)
   (for-each
    (lambda (outer)
      (update-change outer (list 1 2 3))
;      (let ([removed (map (lambda (pp) (cons (rewrite-delete (car pp)) (cdr pp))) odd-pes)])
;        (sit id-s (make-step-name outer 4 "-del-odd-pes") ast) (display+flush ".")
        (add-new-resource) (sit id-s (make-step-name outer 4 "-add-new-pes") ast) (display+flush ".")
        (update-change outer (list 5 6 7))
        (make-new-modes ast) (sit id-s (make-step-name outer 8 "-new-modes") ast) (display+flush ".")
        (update-change outer (list 9 10 11 12))
        (sit id-s (make-step-name outer 13 "-no-change") ast)
        (update-change outer (list 14 15 16))
;        (for-each (lambda (rp) (rewrite-add (cdr rp) (car rp))) (reverse removed))
;        (rw* rt "load" #f ast) (sit id-s (make-step-name outer 17 "-add-odd-pes") ast) (display+flush "."))
      (add-new-resource) (sit id-s (make-step-name outer 17 "-add-new-pes") ast) (display+flush ".")
      (update-change outer (list 18 19 20))) (list 0 1 2 3 4)))

 ; make change kind frequency according to some measure, e.g. 80% update, 10% res, 5% sw, 5% no change
 ; = 16 update, 2 res, 1 sw, 1 no
 (define (run-scaling-test id-s specs)
   (define ast (cst id-s specs))
   (define rt (ast-child 1 (->ResourceType* (->HWRoot ast))))
   (define pe+parent (lambda (pe) (cons pe (<- pe))))
   (define odd-pes (map pe+parent (ret-odds (=every-container ast))))
   (define (make-step-name step suffix) (string-append (string-pad (number->string step) 2 #\0) suffix))
   (define (add-new-resource)
     (let* ([max-id (apply max (map (lambda (pe) (string->number (substring (->name pe) 2))) (=every-container ast)))]
            [first-clauses (->* (->ProvClause* (car (=every-container ast))))]
            [new-clauses (map (lambda (cl) (make-prov (:PropertyRef mquat-spec (ast-child 'refname (->ReturnType cl))) (->comparator cl) (rand 50 2 0))) first-clauses)] ;TODO
            [new-res (:Resource mquat-spec (string-append "r-" (number->string (+ 1 max-id))) rt online (list) new-clauses)])
        (rewrite-add (->SubResources (->HWRoot ast)) new-res)))
   (define (update-change steps)
     (for-each
       (lambda (step)
         (when (not (= step 20))
           (rw* rt "load" #f ast) (sit id-s (make-step-name step "-every-comp-rand") ast) (display+flush "."))) steps))
   (rewrite-terminal 'config ast id-s)
   (display+flush id-s)

;   (info (map (lambda (pe) (string->number (substring (->name pe) 2))) (=every-container ast)))
   (add-new-resource) ;; <- just for testing

   (sit id-s "00-init" ast)
   (update-change (list 1 2 3))
;       (let ([removed (map (lambda (pp) (cons (rewrite-delete (car pp)) (cdr pp))) odd-pes)])
;         (sit id-s (make-step-name 4 "-del-odd-pes") ast) (display+flush ".")
     (add-new-resource) (sit id-s (make-step-name 4 "-add-new-pes") ast) (display+flush ".")
     (update-change (list 5 6 7))
     (make-new-modes ast) (sit id-s (make-step-name 8 "-new-modes") ast) (display+flush ".")
     (update-change (list 9 10 11 12))
     (sit id-s (make-step-name 13 "-no-change") ast)
     (update-change (list 14 15 16))
;        (for-each (lambda (rp) (rewrite-add (cdr rp) (car rp))) (reverse removed))
;        (rw* rt "load" #f ast) (sit id-s (make-step-name 17 "-add-odd-pes") ast) (display+flush "."))
   (add-new-resource) (sit id-s (make-step-name 17 "-add-new-pes") ast) (display+flush ".")
   (update-change (list 18 19 20)))

 (define tests (list (cons resource-test run-resource-test)
                     (cons sw-test run-sw-test)
                     (cons update-test run-update-test)
                     (cons complex-test run-complex-test)
                     (cons mixed-test run-mixed-test)
                     (cons scaling-test run-scaling-test)))

 (define (run-test id-s specs)
   (when profiling? (reset-counts))
   (let ([f (assoc (car specs) tests)])
     (if f ((cdr f) id-s (cdr specs)) (error "Unknown kind" (car specs)))))

 (define (print-usage) (error "measurement-cli-call"
                              "No valid arguments found, use 'all', 'dirs', 'prefix', 'suffix' or a number of ids."))

 (define (measurement-cli-call command-line)
   (if (= 0 (length command-line)) (print-usage)
       (let ([first (car command-line)])
           ;(sleep preesleep)
           (cond
             [(string=? "all" first) (for-each run-test (map car params) (map cdr params))]
             [(string=? "dirs" first) (for-each (lambda (entry) (display (car entry)) (display " ")) params)]
             [(string=? "prefix" first)
                (let ([param_subset (filter (lambda (p) (string-prefix? (cadr command-line) (car p))) params)])
                    (for-each run-test (map car param_subset) (map cdr param_subset)))]
             [(string=? "suffix" first)
                (let ([param_subset (filter (lambda (p) (string-suffix? (cadr command-line) (car p))) params)])
                    (for-each run-test (map car param_subset) (map cdr param_subset)))]
             [else (for-each run-test command-line (map (lambda (id) (cdr (assoc id params))) command-line))])))))
