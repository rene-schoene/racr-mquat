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
   (let ([ast (create-system 2 0 1 1 2)])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-1-2)
     (case id
       [(1) ; Description: no further constraints
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(2) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(3) ; Description: mode-1-1-1 does not meet its requirements (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(4) ; Description: mode-1-1-1 does not meet request constraint (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (save-ilp tmp-lp ast)]
 
       [(5) ; Description: mode-1-1-1 does not meet request constraint (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req is *not* met
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2)
        (save-ilp tmp-lp ast)]

       [(6) ; Description: Reqs only met on res-1
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(7) ; Description: Reqs only met on res-2
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(8) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met on both resources
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req is *not* met on res-1
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(9) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (min-eq and max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req is *not* met on both resources
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req is *not* met on res-1
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(10) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-hw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req is *not* met on both resources
        (change-sw-hw-req ast 'load comp-min-eq 0.8 'mode-1-1-2) ; min-req is *not* met on res-1
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(11) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet request constraint (only max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8) ; max-req is *not* met on res-1
        (save-ilp tmp-lp ast)]

       [(12) ; Description: Reqs only met on res-2, mode-1-1-2 does not meet request constraint (req:min-eq and res:max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8) ; max-req is *not* met on res-1
        (save-ilp tmp-lp ast)]

       [(13) ; Description: Reqs only met on res-1, mode-1-1-1 does not meet request constraint (req:max-eq and res:min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-1
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (change-sw-hw-req ast 'load comp-min-eq 0.8) ; max-req is *not* met on res-2
        (save-ilp tmp-lp ast)]

       [(14) ; Description: Reqs only met on res-1, mode-1-1-2 does not meet request constraint (only min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.8) ; min-req is *not* met on res-2
        (save-ilp tmp-lp ast)]

       [else (display (string-append "Unknown test case id '" id "'\n"))])))
 
 (define (two-impls id)
   ; General description: 2 impls with each 2 modes, first modes are better, first impl is better
   (let ([ast (create-system 3 0 1 2 2)])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'mode-1-2-2)
     (case id
       [(100) ; Description: normal load constraints
        ; Expected outcome: mode-1-1-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(101) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(102) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-2) ; max-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(103) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(104) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(105) ; Description: mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req is *not* met
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(106) ; Description: only mode-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(107) ; Description: only mode-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req is *not* met
        (change-sw-hw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(108) ; Description: Reqs only met on res-2 and res-3 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(109) ; Description: Reqs only met on res-2 and res-3 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.7)
        (change-hw-prov ast 'load 0.3 'res-1)
        (change-sw-hw-req ast 'load comp-min-eq 0.4)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(110) ; Description: Reqs only met on res-2 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(111) ; Description: Reqs only met on res-2 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.3)
        (change-hw-prov ast 'load 0.7 'res-2)
        (change-sw-hw-req ast 'load comp-min-eq 0.4)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(4) ; Description: 2 modes, first mode is better, but does not meet request constraint (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req is *not* met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (save-ilp tmp-lp ast)]
 
       [(5) ; Description: 2 modes, first mode is better, but does not meet request constraint (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req is *not* met
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2)
        (save-ilp tmp-lp ast)]

       [(6) ; Description: 2 modes, reqs only met on res-1, first mode is better
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-hw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)]

       [(7) ; Description: 2 modes, reqs only met on res-2, first mode is better
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
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
 
 (define (check-test id-s obj fname)
   (call/cc
    (lambda (error)
      (let* ([table (make-hashtable string-hash string=?)]
             [val (lambda (name) (or (hashtable-ref table name #f) (error (string-append "var " name " not found\n"))))]
             [contains? (lambda (name) (hashtable-contains? table name))]
             [id (string->number id-s)]
             [test-obj (lambda (expected-base actual id) (if (not (= actual (+ expected-base (/ id 1e3))))
                                                             (error "Wrong objective value")))]
             [test-assert (lambda (msg expr) (if (not expr) (error msg)))])
        (define (val=? base expected lres)
          (if (null? lres) (lambda (name) (= (val name) expected))
              (let ([=name (lambda (base res-nr) (string-append base "#res_" (number->string res-nr)))])
                (exists (lambda (res-nr) (= (val (=name base res-nr)) expected)) lres))))
        (define (val=1? base . lres) (val=? base 1 lres))
        (define (val=0? base . lres) (val=? base 0 lres))
        (read-solution table fname error)
        (for-each (lambda (key) (debug key ":" (hashtable-ref table key #f))) (vector->list (hashtable-keys table)))
        ; impl deployment
        (case id
          [(1 2 3 4 5 6 7 8 9 10 11 12 13 14) (test-assert "first impl not deployed"  (val=1? "b#comp_1#"))]
          [(100 101 105)                      (test-assert "first impl not deployed"  (val=1? "b#comp_1#impl_1_1"))
                                              (test-assert "second impl deployed"     (val=0? "b#comp_1#impl_1_2"))]
          [(102 103 104 106 107 108 109 110 111) (test-assert "first impl deployed"   (val=0? "b#comp_1#impl_1_1"))
                                              (test-assert "second impl not deployed" (val=1? "b#comp_1#impl_1_2"))]
          [else (error (string-append "Unknown test case id '" id-s " for impls'\n"))])
        
        ; mode-deployment
        (case id
          [(1)           (test-assert "first mode not deployed"          (val=1? "b#comp_1##mode_1_1_1" 1 2))]
          [(2 3 4 5)     (test-assert "second mode not deployed"         (val=1? "b#comp_1##mode_1_1_2" 1 2))]
          [(6 14)        (test-assert "first mode not deployed on res1"  (val=1? "b#comp_1##mode_1_1_1" 1))]
          [(7 12)        (test-assert "first mode not deployed on res2"  (val=1? "b#comp_1##mode_1_1_1" 2))]
          [(13)          (test-assert "second mode not deployed on res1" (val=1? "b#comp_1##mode_1_1_2" 1))]
          [(8 9 10 11)   (test-assert "second mode not deployed on res2" (val=1? "b#comp_1##mode_1_1_2" 2))]
          [(100)         (test-assert "first mode not deployed 123"      (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))]
          [(108 109)     (test-assert "first mode not deployed 23"       (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2 3))]
          [(110 111)     (test-assert "first mode not deployed 2"        (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2))]
          [(101 105)     (test-assert "second mode not deployed 123"     (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))]
          [(102 103 104) (test-assert "third mode not deployed 123"      (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
          [(106 107)     (test-assert "fourth mode not deployed 123"     (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [else (error (string-append "Unknown test case id '" id-s " for modes'\n"))])
        (case id
          [(1 6 7 12 14)          (test-obj 10 obj id)]
          [(2 3 4 5 8 9 10 11 13) (test-obj 20 obj id)]
          [(100 108 109 110 111)  (test-obj 10 obj id)]
          [(101 105)              (test-obj 15 obj id)]
          [(102 103 104)          (test-obj 20 obj id)]
          [(106 107)              (test-obj 25 obj id)]
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
