#!r6rs

(library
 (mquat ilp-test)
 (export run-test)
 (import (rnrs) (racr core) (racr testing)
         (mquat utils) (mquat ilp) (mquat main) (mquat basic-ag) (mquat ast)
         (mquat constants) (mquat ast-generation) (mquat ui))
 ;; Testing generated ILP, whether correct results are computed for small models
 
 (define tmp-lp "test/tmp.lp")
 
 (define (change-hw-prov ast prop-name new-value . res-names)
   (let ([resources (if (null? res-names) (=every-pe ast)
                        (filter (lambda (res) (find (lambda (name) (eq? (->name res) name)) res-names))
                                (=every-pe ast)))])
     (for-each (lambda (res)
                 (rewrite-terminal 'value (=provided-clause res prop-name (->type res))
                                               (lambda _ new-value))) resources)))
 
 (define (change-sw-req ast prop-name comparator new-value . mode-names)
   (let ([modes (if (null? mode-names) (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))])
     (for-each (lambda (mode) (let ([clause (=search-req-clause mode prop-name)])
                                (when (not clause) (error change-sw-req "No clause found" prop-name (->name mode)))
                                (rewrite-terminal 'value clause (lambda _ new-value))
                                (rewrite-terminal 'comp clause comparator))) modes)))

 (define (change-sw-prov ast prop-name new-value . mode-names)
   (let ([modes (if (null? mode-names) (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))])
     (for-each (lambda (mode) (rewrite-terminal 'value (=provided-clause mode prop-name)
                                                (lambda _ new-value))) modes)))
 
 (define (remove-request-constraints ast) (for-each (lambda (req) (rewrite-delete req)) (->* (->Constraints (<=request ast)))))
 
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
       [(< id 400) (two-comps-reqc id)]
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
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(2) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(3) ; Description: mode-1-1-1 does not meet its requirements (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(4) ; Description: mode-1-1-1 does not meet request constraint (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (save-ilp tmp-lp ast)]
 
       [(5) ; Description: mode-1-1-1 does not meet request constraint (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2)
        (save-ilp tmp-lp ast)]

       [(6) ; Description: Reqs only met on res-1
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(7) ; Description: Reqs only met on res-2
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(8) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met on both resources
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req not met on res-1
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(9) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (min-eq and max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met on both resources
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req not met on res-1
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(10) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.5 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met on both resources
        (change-sw-req ast 'load comp-min-eq 0.8 'mode-1-1-2) ; min-req not met on res-1
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(11) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet request constraint (only max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (change-sw-req ast 'load comp-max-eq 0.8) ; max-req not met on res-1
        (save-ilp tmp-lp ast)]

       [(12) ; Description: Reqs only met on res-2, mode-1-1-2 does not meet request constraint (req:min-eq and res:max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8) ; max-req not met on res-1
        (save-ilp tmp-lp ast)]

       [(13) ; Description: Reqs only met on res-1, mode-1-1-1 does not meet request constraint (req:max-eq and res:min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on res-1
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
        (change-sw-req ast 'load comp-min-eq 0.8) ; max-req not met on res-2
        (save-ilp tmp-lp ast)]

       [(14) ; Description: Reqs only met on res-1, mode-1-1-2 does not meet request constraint (only min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.8) ; min-req not met on res-2
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
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(101) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(102) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-2) ; max-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(103) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(104) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(105) ; Description: mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(106) ; Description: only mode-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(107) ; Description: only mode-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(108) ; Description: Reqs only met on res-2 and res-3 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(109) ; Description: Reqs only met on res-2 and res-3 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.7)
        (change-hw-prov ast 'load 0.3 'res-1)
        (change-sw-req ast 'load comp-min-eq 0.4)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(110) ; Description: Reqs only met on res-2 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9)
        (change-hw-prov ast 'load 0.5 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(111) ; Description: Reqs only met on res-2 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.3)
        (change-hw-prov ast 'load 0.7 'res-2)
        (change-sw-req ast 'load comp-min-eq 0.4)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(112) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: mode-1-1-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(113) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-2) ; max-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(114) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 & mode-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(115) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(116) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: mode-1-1-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(117) ; Description: Reqs only met on res-2 and res-3
        ; only mode-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(118) ; Description: Reqs only met on res-2 and res-3
        ; only mode-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]
 
       [(119) ; Description: Reqs only met on res-1 and res-3 (max-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.4 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req not met
        (change-sw-prov ast 'prop-1 2 'mode-1-2-1) ; min-req not met
        (save-ilp tmp-lp ast)]
 
       [(120) ; Description: Reqs only met on res-1 and res-3 (min-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.8 'res-3)
        (change-sw-req ast 'load comp-min-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req not met
        (change-sw-prov ast 'prop-1 2 'mode-1-2-1) ; min-req not met
        (save-ilp tmp-lp ast)]
 
       [(121) ; Description: Reqs only met on res-1 and res-3 (max-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.4 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 1)
        (change-sw-prov ast 'prop-1 8 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-2-1) ; max-req not met
        (save-ilp tmp-lp ast)]
 
       [(122) ; Description: Reqs only met on res-1 and res-3 (min-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.8 'res-3)
        (change-sw-req ast 'load comp-min-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 1)
        (change-sw-prov ast 'prop-1 8 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-2-1) ; max-req not met
        (save-ilp tmp-lp ast)]
 
       [(123) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (max-eq)
        ; mode-1-1-1 and mode-1-1-2 do not meet request constraints (max-eq)
        ; Expected outcome: fourth mode (mode-1-2-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.8 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-2-1) ; max-req not met
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 1)
        (change-sw-prov ast 'prop-1 8 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2) ; max-req not met
        (save-ilp tmp-lp ast)]
 
       [(124) ; Description: Reqs only met on res-2
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (max-eq)
        ; mode-1-1-1 and mode-1-1-2 do not meet request constraints (max-eq)
        ; Expected outcome: fourth mode (mode-1-2-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.9 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-2-1) ; max-req not met
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 1)
        (change-sw-prov ast 'prop-1 8 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2) ; max-req not met
        (save-ilp tmp-lp ast)]
 
       [(125) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq)
        ; mode-1-1-1 and mode-1-1-2 do not meet request constraints (min-eq)
        ; Expected outcome: fourth mode (mode-1-2-2) is deployed on either res-1 or res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.8 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 4)
        (change-sw-prov ast 'prop-1 1 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 2 'mode-1-1-2) ; max-req not met
        (save-ilp tmp-lp ast)]
 
       [(126) ; Description: Reqs only met on res-2
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq)
        ; mode-1-1-1 and mode-1-1-2 do not meet request constraints (min-eq)
        ; Expected outcome: fourth mode (mode-1-2-2) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-hw-prov ast 'load 0.9 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 4)
        (change-sw-prov ast 'prop-1 1 'mode-1-1-1) ; min-req not met
        (change-sw-prov ast 'prop-1 2 'mode-1-1-2) ; min-req not met
        (save-ilp tmp-lp ast)]

       [else (display (string-append "Unknown test case id '" id "'\n"))])))

 (define (two-comps id)
   ; General description: 2 comps with each 2 impls with each 2 modes, first comps, first impls and first modes are better
   ; All modes of comp-1 require prop-2 <= 20, where by default all modes provide sufficiently prop-2 = 10
   (let ([ast (create-system 3 0 2 2 2 (lambda (mode-name) #t))])
     (change-sw-prov ast pn-energy 10 'mode-1-1-1)
     (change-sw-prov ast pn-energy 15 'mode-1-1-2)
     (change-sw-prov ast pn-energy 20 'mode-1-2-1)
     (change-sw-prov ast pn-energy 25 'mode-1-2-2)
     (change-sw-prov ast pn-energy (+ 30 (/ id 1e3)) 'mode-2-1-1)
     (change-sw-prov ast pn-energy (+ 35 (/ id 1e3)) 'mode-2-1-2)
     (change-sw-prov ast pn-energy (+ 40 (/ id 1e3)) 'mode-2-2-1)
     (change-sw-prov ast pn-energy (+ 45 (/ id 1e3)) 'mode-2-2-2)
     (change-sw-req ast 'prop-2 comp-max-eq 20 'mode-1-1-1 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
     (change-sw-prov ast 'prop-2 10 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
     (case id
       [(200) ; Description: normal load constraints
        ; Expected outcome: both, mode-1-1-1 and mode-2-1-1 are deployed on either res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]

       [(201) ; Description: only impl-2-2 meets requirement of comp-1
        ; Expected outcome: both, mode-1-1-1 and mode-2-2-1 are deployed on either res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (change-sw-prov ast 'prop-2 40 'mode-2-1-1 'mode-2-1-2) ; max-req not met
        (save-ilp tmp-lp ast)]

       [else (display (string-append "Unknown test case id '" id "'\n"))])))

 (define (two-comps-reqc id)
   ; General description: 2 comps with each 2 impls with each 2 modes, first comps, first impls and first modes are better
   ; Only the first impl of comp-1 require prop-2 <= 20, where by default all modes provide sufficiently prop-2 = 10
   (let ([ast (create-system 3 0 2 2 2 (lambda (impl-name) (eq? impl-name 'impl-1-1)))])
     (change-sw-prov ast pn-energy 10 'mode-1-1-1)
     (change-sw-prov ast pn-energy 15 'mode-1-1-2)
     (change-sw-prov ast pn-energy 20 'mode-1-2-1)
     (change-sw-prov ast pn-energy 25 'mode-1-2-2)
     (change-sw-prov ast pn-energy (+ 30 (/ id 1e3)) 'mode-2-1-1)
     (change-sw-prov ast pn-energy (+ 35 (/ id 1e3)) 'mode-2-1-2)
     (change-sw-prov ast pn-energy (+ 40 (/ id 1e3)) 'mode-2-2-1)
     (change-sw-prov ast pn-energy (+ 45 (/ id 1e3)) 'mode-2-2-2)
     (change-sw-req ast 'prop-2 comp-max-eq 20 'mode-1-1-1 'mode-1-1-2)
     (change-sw-prov ast 'prop-2 10 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
     (case id
       [(300) ; Description: normal load constraints
        ; Expected outcome: only mode-1-2-1 is deployed on either res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]
       
       [(301) ; Description: modes of impl-1-2 do not meet their requirements
        ; Expected outcome: both, mode-1-1-1 and mode-2-1-1 are deployed on either res-1, res-2 or res-3
        (change-hw-prov ast 'load 0.5)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-2-1 'mode-1-2-2) ; max-req not met
        (remove-request-constraints ast)
        (save-ilp tmp-lp ast)]
       
       [else (display (string-append "Unknown test case id '" id "'\n"))])))
 
 (define (read-solution table fname error)
   (when (not (file-exists? fname)) (error (string-append "File " fname " not found.")))
   (with-input-from-file fname
     (lambda ()
       (for-each (lambda (entry) (hashtable-set! table (car entry) (cdr entry))) (read)))))
 
 (define (check-test id-s obj fname)
   (call/cc
    (lambda (error)
      (let* ([table (make-hashtable string-hash string=?)]
             [val (lambda (name) (or (hashtable-ref table name #f) (error (string-append name " not found\n"))))]
             [contains? (lambda (name) (hashtable-contains? table name))]
             [id (string->number id-s)]
             [test-obj (lambda (expected-base actual id) (if (not (= actual (+ expected-base (/ id 1e3))))
                                                             (error "Wrong objective value")))]
             [test-assert (lambda (msg expr) (if (not expr) (error msg)))])
        (define (val=? base expected op lres)
          (if (null? lres) (lambda (name) (= (val name) expected))
              (let ([=name (lambda (base res-nr) (string-append base "#res_" (number->string res-nr)))])
                (op (lambda (res-nr) (= (val (=name base res-nr)) expected)) lres))))
        (define (val=1? base . lres) (val=? base 1 exists  lres))
        (define (val=0? base . lres) (val=? base 0 for-all lres))
        (read-solution table fname error)
        (for-each (lambda (key) (debug key ":" (hashtable-ref table key #f))) (vector->list (hashtable-keys table)))
        ; impl deployment
        (case id
          [(1 2 3 4 5 6 7 8 9 10 11 12 13 14)
           (test-assert "first impl not deployed"  (val=1? "b#comp_1#"))]
          [(100 101 105 112 116 119 120 121 122)
           (test-assert "first impl not deployed"  (val=1? "b#comp_1#impl_1_1"))
           (test-assert "second impl deployed"     (val=0? "b#comp_1#impl_1_2"))]
          [(102 103 104 106 107 108 109 110 111 113 114 115 117 118 123 124 125 126)
           (test-assert "first impl deployed"      (val=0? "b#comp_1#impl_1_1"))
           (test-assert "second impl not deployed" (val=1? "b#comp_1#impl_1_2"))]
          [(200 301)
           (test-assert "impl-1-1 not deployed"  (val=1? "b#comp_1#impl_1_1"))
           (test-assert "impl-2-1 not deployed"  (val=1? "b#comp_2#impl_2_1"))
           (test-assert "impl-1-2 deployed"      (val=0? "b#comp_1#impl_1_2"))
           (test-assert "impl-2-2 deployed"      (val=0? "b#comp_2#impl_2_2"))]
          [(201)
           (test-assert "impl-1-1 not deployed"  (val=1? "b#comp_1#impl_1_1"))
           (test-assert "impl-2-2 not deployed"  (val=1? "b#comp_2#impl_2_2"))
           (test-assert "impl-1-2 deployed"      (val=0? "b#comp_1#impl_1_2"))
           (test-assert "impl-2-1 deployed"      (val=0? "b#comp_2#impl_2_1"))]
          [(300)
           (test-assert "impl-1-2 not deployed"  (val=1? "b#comp_1#impl_1_2"))
           (test-assert "impl-1-1 deployed"      (val=0? "b#comp_1#impl_1_1"))
           (test-assert "impl-2-1 deployed"      (val=0? "b#comp_2#impl_2_1"))
           (test-assert "impl-2-2 deployed"      (val=0? "b#comp_2#impl_2_2"))]
          [else (error (string-append "Unknown test case id '" id-s " for impls'\n"))])
        
        ; mode-deployment
        (case id
          [(1)           (test-assert "first mode not deployed on 12"   (val=1? "b#comp_1##mode_1_1_1" 1 2))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
          [(2 3 4 5)     (test-assert "second mode not deployed on 12"  (val=1? "b#comp_1##mode_1_1_2" 1 2))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1##mode_1_1_1" 1 2))]
          [(6 14)        (test-assert "first mode not deployed on 1"    (val=1? "b#comp_1##mode_1_1_1" 1))
                         (test-assert "first mode deployed on 2"        (val=0? "b#comp_1##mode_1_1_1" 2))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
          [(7 12)        (test-assert "first mode not deployed on 2"    (val=1? "b#comp_1##mode_1_1_1" 2))
                         (test-assert "first mode deployed on 1"        (val=0? "b#comp_1##mode_1_1_1" 1))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
          [(13)          (test-assert "second mode not deployed on 1"   (val=1? "b#comp_1##mode_1_1_2" 1))
                         (test-assert "second mode deployed on 2"       (val=0? "b#comp_1##mode_1_1_2" 2))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1##mode_1_1_1" 1 2))]
          [(8 9 10 11)   (test-assert "second mode not deployed on 2"   (val=1? "b#comp_1##mode_1_1_2" 2))
                         (test-assert "second mode deployed on 1"       (val=0? "b#comp_1##mode_1_1_2" 1))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1##mode_1_1_1" 1 2))]
          [(100)         (test-assert "first mode not deployed 123"     (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(108 109)     (test-assert "first mode not deployed 23"      (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2 3))
                         (test-assert "first mode deployed on 1"        (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(110 111)     (test-assert "first mode not deployed 2"       (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2))
                         (test-assert "first mode deployed on 1"        (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(101 105)     (test-assert "second mode not deployed 123"    (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(112 116)     (test-assert "second mode not deployed 23"     (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 2 3))
                         (test-assert "second mode deployed on 1"       (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(119 120 121 122) (test-assert "second mode not deployed 13" (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 3))
                             (test-assert "second mode deployed on 2"   (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 2))
                             (test-assert "first mode deployed"         (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                             (test-assert "third mode deployed"         (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                             (test-assert "fourth mode deployed"        (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(102 103 104) (test-assert "third mode not deployed 123"     (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(113 114 115) (test-assert "third mode not deployed 23"      (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 2 3))
                         (test-assert "third mode deployed on 1"        (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "fourth mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
          [(106 107)     (test-assert "fourth mode not deployed 123"    (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
          [(117 118)     (test-assert "fourth mode not deployed 23"     (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2 3))
                         (test-assert "fourth mode deployed on 1"       (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
          [(123 125)     (test-assert "fourth mode not deployed 12"     (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 1 2))
                         (test-assert "fourth mode deployed on 3"       (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 3))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
          [(124 126)     (test-assert "fourth mode not deployed 2"      (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2))
                         (test-assert "fourth mode deployed on 13"      (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 3))
                         (test-assert "first mode deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "second mode deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_1_2" 1 2 3))
                         (test-assert "third mode deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
          [(200 301)     (test-assert "mode-1-1-1 not deployed 123"     (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "mode-2-1-1 not deployed 123"     (val=1? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                         (test-assert "mode-1-1-2 deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "mode-1-2-1 deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "mode-1-2-2 deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                         (test-assert "mode-2-1-2 deployed"             (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                         (test-assert "mode-2-2-1 deployed"             (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                         (test-assert "mode-2-2-2 deployed"             (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
          [(201)         (test-assert "mode-1-1-1 not deployed 123"     (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "mode-2-2-1 not deployed 123"     (val=1? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                         (test-assert "mode-1-1-2 deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "mode-1-2-1 deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "mode-1-2-2 deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                         (test-assert "mode-2-1-1 deployed"             (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                         (test-assert "mode-2-1-2 deployed"             (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                         (test-assert "mode-2-2-2 deployed"             (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
          [(300)         (test-assert "mode-1-2-1 not deployed 123"     (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                         (test-assert "mode-1-1-1 deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                         (test-assert "mode-1-1-2 deployed"             (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                         (test-assert "mode-1-2-2 deployed"             (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                         (test-assert "mode-2-1-1 deployed"             (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                         (test-assert "mode-2-1-2 deployed"             (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                         (test-assert "mode-2-2-1 deployed"             (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                         (test-assert "mode-2-2-2 deployed"             (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
          [else (error (string-append "Unknown test case id '" id-s " for modes'\n"))])
        (case id
          [(1 6 7 12 14)                      (test-obj 10 obj id)]
          [(2 3 4 5 8 9 10 11 13)             (test-obj 20 obj id)]
          [(100 108 109 110 111)              (test-obj 10 obj id)]
          [(101 105 112 116 119 120 121 122)  (test-obj 15 obj id)]
          [(102 103 104 113 114 115)          (test-obj 20 obj id)]
          [(106 107 117 118 123 124 125 126)  (test-obj 25 obj id)]
          [(200 301)                          (test-obj 40 obj id)] ;10+30
          [(201)                              (test-obj 50 obj id)] ;10+40
          [(300)                              (test-obj 20 obj id)]
          [else (error (string-append "Unknown test case id '" id-s " for objectives'\n"))])
        #f))))
 
 (define (display-ranges)
;   (display "1 13 100 122 200 201 300 301"))
   (display "300 301"))
   
 (if (< (length (command-line)) 2)
     (begin
       (display "Usage: ilp-test.scm action cmds")
       (display " action = ranges → no cmds, outputs valid inclusive intervals for test case ids")
       (display " action = run → cmds = id, outputs nothing")
       (display " action = check → cmds id objective-value file-name, outputs error messages on failures\n"))
     (begin
       (when (string=? "ranges" (cadr (command-line))) ; expect just "ranges"
         (display-ranges))
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
