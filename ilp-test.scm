#!r6rs

(library
 (mquat ilp-test)
 (export run-test)
 (import (rnrs) (racr core) (srfi :64)
         (mquat utils) (mquat ilp) (mquat main)
         (mquat constants) (mquat ast-generation))
 ;; Testing generated ILP, whether correct results are computed for small models
 
 (define tmp-lp "test/tmp.lp")
 
 (define (change-hw-prov ast prop-name new-value . res-names)
   (let ([resources (if (null? res-names)
                        (att-value 'every-pe ast)
                        (filter (lambda (res) (find (lambda (name) (eq? (ast-child 'name res) name)) res-names))
                                (att-value 'every-pe ast)))])
     (for-each (lambda (res)
                 (rewrite-terminal 'value (att-value 'provided-clause res prop-name (ast-child 'type res))
                                               (lambda _ new-value))) resources)))
 
 (define (change-sw-hw-req ast prop-name comparator new-value . mode-names)
   (let ([modes (if (null? mode-names)
                    (att-value 'every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (ast-child 'name mode) name)) mode-names))
                            (att-value 'every-mode ast)))])
     (for-each (lambda (mode) (let ([clause (att-value 'search-clause mode prop-name 'ReqClause)])
                                (rewrite-terminal 'value clause (lambda _ new-value))
                                (rewrite-terminal 'comp clause comparator))) modes)))

 (define (change-sw-prov ast prop-name new-value . mode-names)
   (let ([modes (if (null? mode-names)
                    (att-value 'every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (ast-child 'name mode) name)) mode-names))
                            (att-value 'every-mode ast)))])
     (for-each (lambda (mode) (rewrite-terminal 'value (att-value 'provided-clause mode prop-name)
                                                (lambda _ new-value))) modes)))
 
 (define (remove-req-constraints ast)
   (for-each (lambda (req) (rewrite-delete req)) (ast-children (ast-child 'Constraints (ast-child 'Request ast)))))
 
 (define (change-req-constraint ast name comparator new-value)
   (debug (ast-children (ast-child 'Constraints (ast-child 'Request ast))))
   (let ([clause (ast-find-child (lambda (i child) (eq? (ast-child 'name (ast-child 'returntype child)) name))
                                 (ast-child 'Constraints (ast-child 'Request ast)))])
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
 
 (define (read-solution fname)
   (cons "vars" "obj"))
 (define (check-test id vars obj . cmds)
   (call/cc
    (lambda (error)
      (define (val name) (let ([entry (assq name vars)]) (if entry (cdr entry) (error (string-append "var " name " not found")))))
      (test-begin id)
      (test-equal "First implementation is not deployed!" (val "b#comp_1#") 1) ; First component has to be always deployed
      (case (string->number id)
        [(1 2) (test-equal "first mode not deployed" (+ (val "b#comp_1##comp_1_1_1#res_1") (val "b#comp_1##comp_1_1_1#res_2")) 1)]
        
        [else (display (string-append "Unknown test case id '" id "'\n"))])
      (case (string->number id)
        [(1) (test-equal "Wrong objective value" obj 10.01)]
        [else (display (string-append "Unknown test case id '" id "'\n"))])
      (test-end id)
      #f)))
 
 (when (member "run" (command-line)) ; expect "run" "$id"
   (set!debugging #f)
   (run-test (caddr (command-line)))) ; take 2nd real argument, i.e. id
 (when (member "check" (command-line)) ; expect "run" "$id" "$fname"
   (set!debugging #f)
   (let* ([os (read-solution (cadddr (command-line)))]
          [error (check-test (caddr (command-line)) (car os) (cdr os))])
     (when error
       (display error)
       (exit 1))
     (exit 0))))
