#!r6rs

(library
 (mquat ilp-test)
 (export run-test)
 (import (rnrs) (racr core)
         (mquat utils) (mquat ilp) (mquat main)
         (mquat constants) (mquat ast-generation))
 ;; Testing generated ILP, whether correct results are computed for small models
 
 (define tmp-lp "test/tmp.lp")
 
 (define (run-test id)
   (case (string->number id)
     [(1) (run-1)] [(2) (run-2)] [(3) (run-3)] [(4) (run-4)]
     [else (display (string-append "Unknown test case id '" id "'"))]))
 
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
 
 ; Description: 2 modes, first mode is better
 ; Expected outcome: first mode (comp-1-1-1) is deployed on either res-1 or res-2
 (define (run-1)
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-hw-prov ast 'load 0.5)
     (change-sw-hw-req ast 'load comp-max-eq 0.8)
     (remove-req-constraints ast)
     (change-sw-prov ast pn-energy 10 'comp-1-1-1)
     (change-sw-prov ast pn-energy 20 'comp-1-1-2)
     (save-ilp tmp-lp ast)))

 ; Description: 2 modes, first mode is better, but does not meet its requirements (max-eq)
 ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
 (define (run-2)
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-hw-prov ast 'load 0.5)
     (change-sw-hw-req ast 'load comp-max-eq 0.2 'comp-1-1-1) ; max-req is *not* met
     (change-sw-hw-req ast 'load comp-max-eq 0.8 'comp-1-1-2)
     (remove-req-constraints ast)
     (change-sw-prov ast pn-energy 10 'comp-1-1-1)
     (change-sw-prov ast pn-energy 20 'comp-1-1-2)
     (save-ilp tmp-lp ast)))

 ; Description: 2 modes, first mode is better, but does not meet its requirements (min-eq)
 ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
 (define (run-3)
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-hw-prov ast 'load 0.5)
     (change-sw-hw-req ast 'load comp-min-eq 0.8 'comp-1-1-1) ; min-req is *not* met
     (change-sw-hw-req ast 'load comp-max-eq 0.8 'comp-1-1-2)
     (remove-req-constraints ast)
     (change-sw-prov ast pn-energy 10 'comp-1-1-1)
     (change-sw-prov ast pn-energy 30 'comp-1-1-2)
     (save-ilp tmp-lp ast)))
 
 ; Description: 2 modes, first mode is better, but does not meet request constraint (max-eq)
 ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
 (define (run-4)
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-hw-prov ast 'load 0.5)
     (change-sw-hw-req ast 'load comp-max-eq 0.8)
     (change-sw-prov ast pn-energy 10 'comp-1-1-1)
     (change-sw-prov ast pn-energy 40 'comp-1-1-2)
     (change-req-constraint ast 'prop-1 comp-max-eq 4)
     (change-sw-prov ast 'prop-1 7 'comp-1-1-1) ; max-req is *not* met
     (change-sw-prov ast 'prop-1 3 'comp-1-1-2)
     (save-ilp tmp-lp ast)))
 
 ; Description: 2 modes, first mode is better, but does not meet request constraint (min-eq)
 ; Expected outcome: second mode (comp-1-1-2) is deployed on either res-1 or res-2
 (define (run-5)
   (let ([ast (create-system mquat-spec 2 0 1 1 2)])
     (change-hw-prov ast 'load 0.5)
     (change-sw-hw-req ast 'load comp-max-eq 0.8)
     (change-sw-prov ast pn-energy 10 'comp-1-1-1)
     (change-sw-prov ast pn-energy 40 'comp-1-1-2)
     (change-req-constraint ast 'prop-1 comp-min-eq 4)
     (change-sw-prov ast 'prop-1 3 'comp-1-1-1) ; min-req is *not* met
     (change-sw-prov ast 'prop-1 7 'comp-1-1-2)
     (save-ilp tmp-lp ast)))
 
 (set!no-frequency) ; only property load upon system creation
 
 (when (member "run" (command-line)) ; expect "run" "$id"
   (set!debugging #f)
   (run-test (caddr (command-line))))) ; take 2nd real argument, i.e. id
