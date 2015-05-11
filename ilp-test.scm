#!r6rs

(library
 (mquat ilp-test)
 (export run-test)
 (import (rnrs) (racr core)
         (mquat utils) (mquat ilp) (mquat main) (mquat basic-ag) (mquat ast)
         (mquat constants) (mquat ast-generation))
 ;; Testing generated ILP, whether correct results are computed for small models
 
 (define tmp-lp "test/tmp.lp")
 
 (define (change-hw-prov ast prop-name new-value . res-names)
   (let ([resources (if (null? res-names)
                        (=every-pe ast)
                        (filter (lambda (res) (find (lambda (name) (eq? (->name res) name)) res-names))
                                (=every-pe ast)))])
     (for-each (lambda (res)
                 (rewrite-terminal 'value (=provided-clause res prop-name (->type res))
                                               (lambda _ new-value))) resources)))
 
 (define (change-sw-hw-req ast prop-name comparator new-value . mode-names)
   (let ([modes (if (null? mode-names)
                    (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))])
     (for-each (lambda (mode) (let ([clause (=search-req-clause mode prop-name)])
                                (rewrite-terminal 'value clause (lambda _ new-value))
                                (rewrite-terminal 'comp clause comparator))) modes)))

 (define (change-sw-prov ast prop-name new-value . mode-names)
   (let ([modes (if (null? mode-names)
                    (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))])
     (for-each (lambda (mode) (rewrite-terminal 'value (=provided-clause mode prop-name)
                                                (lambda _ new-value))) modes)))
 
 (define (remove-req-constraints ast) (for-each (lambda (req) (rewrite-delete req)) (->* (->Constraints (<=request ast)))))
 
 (define (change-req-constraint ast name comparator new-value)
   (debug (->* (->Constraints (<=request ast))))
   (let ([clause (ast-find-child (lambda (i child) (eq? (->name (->return-type child)) name)) (->Constraints (<=request ast)))])
     (rewrite-terminal 'value clause (lambda _ new-value))
     (rewrite-terminal 'comp clause comparator)))
 
 (define (run-test id-string)
   (set!no-frequency) ; only use property load for system creation
   (let ([id (string->number id-string)])
     (cond
       [(< id 100) (two-modes id)]
       [(< id 200) (two-impls id)]
       [(< id 300) (two-comps id)]
       [else (display (string-append "unknown id " id-string)) 1])))
 
 (define (two-modes id)
   ; General description: 2 modes, first mode is better
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'comp-1-1-1)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'comp-1-1-2)
     (case id
       [(1) ; Description: no further constraints
        ; Expected outcome: first mode (comp-1-1-1) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(2) ; Description: comp-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'comp-1-1-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(3) ; Description: comp-1-1-1 does not meet its requirements (min-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'comp-1-1-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(4) ; Description: comp-1-1-1 does not meet request constraint (max-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'comp-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'comp-1-1-2)
        (save-ilp tmp-lp ast)]
 
       [(5) ; Description: comp-1-1-1 does not meet request constraint (min-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 3 'comp-1-1-1) ; min-req is *not* met
        (change-sw-prov ast 'prop-1 7 'comp-1-1-2)
        (save-ilp tmp-lp ast)]

       [(6) ; Description: Reqs only met on res-1
        ; Expected outcome: first mode (comp-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(7) ; Description: Reqs only met on res-2
        ; Expected outcome: first mode (comp-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [else (display (string-append "Unknown test case id '" id "'\n"))])))
 
 (define (two-impls id)
   ; General description: 2 impls with each 2 modes, first modes are better, first impl is better
   (let ([ast (create-system mquat-spec 3 0 1 2 2)])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'comp-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'comp-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'comp-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'comp-1-2-2)
     (case id
       [(100) ; Description: normal load constraints
        ; Expected outcome: comp-1-1-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(101) ; Description: comp-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: comp-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(102) ; Description: comp-1-1-1 and comp-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: comp-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-2) ; max-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(103) ; Description: comp-1-1-1 and comp-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: comp-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-2) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(104) ; Description: comp-1-1-1 and comp-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: comp-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-2) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(105) ; Description: comp-1-1-1 and comp-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: comp-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-2-1) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(106) ; Description: only comp-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: comp-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-2-1) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(107) ; Description: only comp-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: comp-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.7 'comp-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'comp-1-2-1) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(3) ; Description: 2 modes, first mode is better, but does not meet its requirements (min-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-min-eq 0.8 'comp-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'comp-1-1-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(4) ; Description: 2 modes, first mode is better, but does not meet request constraint (max-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'comp-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'comp-1-1-2)
        (save-ilp tmp-lp ast)]
 
       [(5) ; Description: 2 modes, first mode is better, but does not meet request constraint (min-eq)
        ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 3 'comp-1-1-1) ; min-req is *not* met
        (change-sw-prov ast 'prop-1 7 'comp-1-1-2)
        (save-ilp tmp-lp ast)]

       [(6) ; Description: 2 modes, reqs only met on res-1, first mode is better
        ; Expected outcome: first mode (comp-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(7) ; Description: 2 modes, reqs only met on res-2, first mode is better
        ; Expected outcome: first mode (comp-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [else (display (string-append "Unknown test case id '" id "'\n"))])))

 (define (two-comps id)
   (case (id)
     [(20) 1]
     [else (display (string-append "Unknown test case id '" id "'\n"))]))
 
 (define (read-solution table fname error)
   (when (not (file-exists? fname)) (error (string-append "File " fname " not found.")))
   (with-input-from-file fname
     (lambda ()
       (for-each (lambda (entry) (hashtable-set! table (car entry) (cdr entry))) (read)))))
 
 (define (check-test id-s obj fname) ; l = (cons objective-value vars=1)
   (call/cc
    (lambda (error)
      (let* ([table (make-hashtable string-hash string=?)]
             [val (lambda (name) (or (hashtable-ref table name #f) (error (string-append "var " name " not found\n"))))]
             [val=1? (lambda (name) (= (val name) 1))] [val=0? (lambda (name) (= (val name) 0))]
             [contains? (lambda (name) (hashtable-contains? table name))]
             [id (string->number id-s)]
             [test-obj (lambda (expected-base actual id) (if (not (= actual (+ expected-base (/ id 1e3))))
                                                             (error "Wrong objective value")))]
             [test-assert (lambda (msg expr) (if (not expr) (error msg)))])
        (read-solution table fname error)
        (for-each (lambda (key) (debug key ":" (hashtable-ref table key #f))) (vector->list (hashtable-keys table)))
        ; impl deployment
        (case id
          [(1 2 3 4 5 6 7) (test-assert "first impl not deployed" (val=1? "b#comp_1#"))]
          [(100 101 105) (test-assert "first impl not deployed"  (val=1? "b#comp_1#comp_1_1"))
                           (test-assert "second impl deployed"     (val=0? "b#comp_1#comp_1_2"))]
          [(102 103 104) (test-assert "first impl deployed"      (val=0? "b#comp_1#comp_1_1"))
                           (test-assert "second impl not deployed" (val=1? "b#comp_1#comp_1_2"))]
          [else (error (string-append "Unknown test case id '" id-s " for impls'\n"))])
        
        ; mode-deployment
        (case id
          [(1) (test-assert "first mode not deployed" (or (val=1? "b#comp_1##comp_1_1_1#res_1")
                                                          (val=1? "b#comp_1##comp_1_1_1#res_2")))]
          [(2 3 4 5)
           (test-assert "second mode not deployed" (or (val=1? "b#comp_1##comp_1_1_2#res_1")
                                                       (val=1? "b#comp_1##comp_1_1_2#res_2")))]
          [(6) (test-assert "first mode not deployed on res1" (val=1? "b#comp_1##comp_1_1_1#res_1"))]
          [(7) (test-assert "first mode not deployed on res2" (val=1? "b#comp_1##comp_1_1_1#res_2"))]
          [(100) (test-assert "first mode not deployed" (or (val=1? "b#comp_1##comp_1_1_1#res_1")
                                                            (val=1? "b#comp_1##comp_1_1_1#res_2")
                                                            (val=1? "b#comp_1##comp_1_1_1#res_3")))]
          [(101 105) (test-assert "second mode not deployed" (or (val=1? "b#comp_1##comp_1_1_2#res_1")
                                                                  (val=1? "b#comp_1##comp_1_1_2#res_2")
                                                                  (val=1? "b#comp_1##comp_1_1_2#res_3")))]
          [(102 103 104) (test-assert "third mode not deployed" (or (val=1? "b#comp_1##comp_1_1_3#res_1")
                                                                      (val=1? "b#comp_1##comp_1_1_3#res_2")
                                                                      (val=1? "b#comp_1##comp_1_1_3#res_3")))]
          [else (error (string-append "Unknown test case id '" id-s " for modes'\n"))])
        (case id
          [(1 6 7) (test-obj 10 obj id)]
          [(2 3 4 5) (test-obj 20 obj id)]
          [(100) (test-obj 10 obj id)]
          [(101 105) (test-obj 15 obj id)]
          [(102 103 104) (test-obj 20 obj id)]
          [else (error (string-append "Unknown test case id '" id-s " for objectives'\n"))])
        #f))))
 
 (if (< (length (command-line)) 2)
     (display "Usage: ilp-test.scm action cmds\n action = run → cmds = id\n action = check → cmds id objective-value file-name\n")
     (begin
       (when (string=? "run" (cadr (command-line))) ; expect "run" id
         (set!debugging #f)
         (let* ([cmds (cddr (command-line))]
                [id-s (car cmds)])
           (run-test id-s)))
       (when (string=? "check" (cadr (command-line))) ; expect "check" id obj fname
         (set!debugging #t)
         (let* ([cmds (cddr (command-line))]
                [id-s (car cmds)] [obj (string->number (cadr cmds))] [fname (caddr cmds)]
                [error (check-test id-s obj fname)])
           (when error (display error) (exit 1))
           (exit 0))))))
