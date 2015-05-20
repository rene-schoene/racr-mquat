#!r6rs

(library
 (mquat ilp-test)
 (export run-test)
 (import (rnrs) (racr core) (racr testing)
         (mquat utils) (mquat ilp) (mquat main) (mquat basic-ag) (mquat ast)
         (mquat constants) (mquat ast-generation) (mquat ui))
 ;; Testing generated ILP, whether correct results are computed for small models
 
 (define tmp-lp "test/tmp.lp")
 
 (define (wrong-id who id) (error who (string-append "Unknown test case id '" (number->string id) "'\n")))
 
 (define (change-hw-prov ast prop-name new-value . res-names)
   (let ([resources (if (null? res-names) (=every-pe ast)
                        (filter (lambda (res) (find (lambda (name) (eq? (->name res) name)) res-names))
                                (=every-pe ast)))]
         [f (if (procedure? new-value) new-value (lambda _ new-value))])
     (for-each (lambda (res) (rewrite-terminal 'value (=provided-clause res prop-name (->type res)) f)) resources)))
 
 (define (change-sw-req ast prop-name comparator new-value . mode-names)
   (let ([modes (if (null? mode-names) (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))]
         [f (if (procedure? new-value) new-value (lambda _ new-value))])
     (for-each (lambda (mode) (let ([clause (=search-req-clause mode prop-name)])
                                (when (not clause) (error change-sw-req "No clause found" prop-name (->name mode)))
                                (rewrite-terminal 'value clause f)
                                (rewrite-terminal 'comp clause comparator))) modes)))

 (define (change-sw-prov ast prop-name new-value . mode-names)
   (let ([modes (if (null? mode-names) (=every-mode ast)
                    (filter (lambda (mode) (find (lambda (name) (eq? (->name mode) name)) mode-names))
                            (=every-mode ast)))]
         [f (if (procedure? new-value) new-value (lambda _ new-value))])
     (for-each (lambda (mode) (rewrite-terminal 'value (=provided-clause mode prop-name) f)) modes)))
 
 (define (remove-req-constraints ast) (for-each (lambda (req) (rewrite-delete req)) (->* (->Constraints (<=request ast)))))
 
 (define (change-req-constraint ast name comparator new-value)
   (debug (->* (->Constraints (<=request ast))))
   (let ([clause (ast-find-child (lambda (i child) (eq? (->name (->return-type child)) name)) (->Constraints (<=request ast)))])
     (rewrite-terminal 'value clause (lambda _ new-value))
     (rewrite-terminal 'comp clause comparator)))
 
 (define no-freq-hw-clauses (lambda _ (lambda (p) (if (eq? freq-name p) #f (list make-prov comp-eq (lambda _ 0.5))))))
 (define no-freq-sw-clauses (lambda _ (lambda (p comp-nr) (if (eq? freq-name p) #f #t))))
 
 (define (run-test id-string)
;   (set!no-frequency) ; only use property load for system creation
   (let ([id (string->number id-string)])
     (cond
       [(< id 100)  (two-modes id)]
       [(< id 200)  (two-impls id)]
       [(< id 300)  (two-comps id)]
       [(< id 400)  (two-comps-reqc id)]
       [(< id 500)  (two-resource-types id)]
       [(< id 600)  (new-resources id)]
       [(< id 700)  (new-software id)]
       [(>= id 900) (unsolvable id)]
       [else (wrong-id id) 1])))
 
 (define (two-modes id)
   (cond
     ((< id 15)
      ; General description: 2 modes, first mode is better
      (let* ([ast (create-system 2 0 1 1 2 (list #f no-freq-sw-clauses no-freq-hw-clauses #f))])
        (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
        (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-1-2)
        (case id
          [(1) ; Description: no further constraints
           ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-1 or res-2
           (change-sw-req ast 'load comp-max-eq 0.8)
           (remove-req-constraints ast)]
          
          [(2) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
           (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
           (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
           (remove-req-constraints ast)]
          
          [(3) ; Description: mode-1-1-1 does not meet its requirements (min-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
           (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
           (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2)
           (remove-req-constraints ast)]
          
          [(4) ; Description: mode-1-1-1 does not meet request constraint (max-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
           (change-sw-req ast 'load comp-max-eq 0.8)
           (change-req-constraint ast 'prop-1 comp-max-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
           (change-sw-prov ast 'prop-1 3 'mode-1-1-2)]
          
          [(5) ; Description: mode-1-1-1 does not meet request constraint (min-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-2
           (change-sw-req ast 'load comp-max-eq 0.8)
           (change-req-constraint ast 'prop-1 comp-min-eq 4)
           (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req not met
           (change-sw-prov ast 'prop-1 7 'mode-1-1-2)]
          
          [(6) ; Description: Reqs only met on res-1
           ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
           (change-hw-prov ast 'load 0.9 'res-2)
           (change-sw-req ast 'load comp-max-eq 0.8)
           (remove-req-constraints ast)]
          
          [(7) ; Description: Reqs only met on res-2
           ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-sw-req ast 'load comp-max-eq 0.8)
           (remove-req-constraints ast)]
          
          [(8) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only max-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met on both resources
           (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req not met on res-1
           (remove-req-constraints ast)]
          
          [(9) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (min-eq and max-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met on both resources
           (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-1-2) ; max-req not met on res-1
           (remove-req-constraints ast)]
          
          [(10) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet its requirements at all (only min-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-2)
           (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met on both resources
           (change-sw-req ast 'load comp-min-eq 0.8 'mode-1-1-2) ; min-req not met on res-1
           (remove-req-constraints ast)]
          
          [(11) ; Description: Reqs only met on res-2, mode-1-1-1 does not meet request constraint (only max-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-max-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
           (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
           (change-sw-req ast 'load comp-max-eq 0.8)] ; max-req not met on res-1
          
          [(12) ; Description: Reqs only met on res-2, mode-1-1-2 does not meet request constraint (req:min-eq and res:max-eq)
           ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-min-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req not met
           (change-sw-req ast 'load comp-max-eq 0.8)] ; max-req not met on res-1
          
          [(13) ; Description: Reqs only met on res-1, mode-1-1-1 does not meet request constraint (req:max-eq and res:min-eq)
           ; Expected outcome: second mode (mode-1-1-2) is deployed on res-1
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-max-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
           (change-sw-prov ast 'prop-1 3 'mode-1-1-2)
           (change-sw-req ast 'load comp-min-eq 0.8)] ; max-req not met on res-2
          
          [(14) ; Description: Reqs only met on res-1, mode-1-1-2 does not meet request constraint (only min-eq)
           ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-min-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 3 'mode-1-1-2) ; min-req not met
           (change-sw-req ast 'load comp-min-eq 0.8)]) ; min-req not met on res-2
        (save-ilp tmp-lp ast)))
     (else
      ; General description: 2 impls with each 1 mode, first impl is better
      (let ([ast (create-system 2 0 1 2 1 (list #f no-freq-sw-clauses no-freq-hw-clauses #f))])
        (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
        (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (case id
          [(15) ; No further constraints
           ; Expected outcome: first mode (mode-1-1-1) is deployed on res-1 or res-2
           (remove-req-constraints ast)]

          [(16) ; mode-1-1-1 does not meet its requirements (max-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-1 or res-2
           (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1)
           (remove-req-constraints ast)]

          [(17) ; mode-1-1-1 does not meet its requirements (min-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-1 or res-2
           (change-sw-req ast 'load comp-min-eq 0.9 'mode-1-1-1)
           (remove-req-constraints ast)]

          [(18) ; mode-1-1-1 does not meet request constraint (max-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-1 or res-2
           (change-req-constraint ast 'prop-1 comp-max-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 3 'mode-1-2-1)]

          [(19) ; mode-1-1-1 does not meet request constraint (min-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-1 or res-2
           (change-req-constraint ast 'prop-1 comp-min-eq 4)
           (change-sw-prov ast 'prop-1 3 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 7 'mode-1-2-1)]

          [(20) ; Reqs only met on res-2, mode-1-1-1 does not meet request constraint (max-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-max-eq 4)
           (change-sw-prov ast 'prop-1 7 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 3 'mode-1-2-1)]

          [(21) ; Reqs only met on res-2, mode-1-1-1 does not meet request constraint (min-eq)
           ; Expected outcome: second mode (mode-1-2-1) is deployed on res-2
           (change-hw-prov ast 'load 0.9 'res-1)
           (change-req-constraint ast 'prop-1 comp-min-eq 4)
           (change-sw-prov ast 'prop-1 3 'mode-1-1-1)
           (change-sw-prov ast 'prop-1 7 'mode-1-2-1)]

           [else (wrong-id two-modes id)])
        (save-ilp tmp-lp ast)))))
   
 
 (define (two-impls id)
   ; General description: 2 impls with each 2 modes, first modes are better, first impl is better
   (let ([ast (create-system 3 0 1 2 2 (list #f no-freq-sw-clauses no-freq-hw-clauses #f))])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'mode-1-2-2)
     (case id
       [(100) ; Description: normal load constraints
        ; Expected outcome: mode-1-1-1 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)]

       [(101) ; Description: mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (remove-req-constraints ast)]

       [(102) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-2) ; max-req not met
        (remove-req-constraints ast)]

       [(103) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (remove-req-constraints ast)]

       [(104) ; Description: mode-1-1-1 and mode-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: mode-1-2-1 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (remove-req-constraints ast)]

       [(105) ; Description: mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: mode-1-1-2 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (remove-req-constraints ast)]

       [(106) ; Description: only mode-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)]

       [(107) ; Description: only mode-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-1, res-2 or res-3
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 0.7 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)]

       [(108) ; Description: Reqs only met on res-2 and res-3 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)]

       [(109) ; Description: Reqs only met on res-2 and res-3 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on either res-2 or res-3
        (change-hw-prov ast 'load 0.7)
        (change-hw-prov ast 'load 0.3 'res-1)
        (change-sw-req ast 'load comp-min-eq 0.4)
        (remove-req-constraints ast)]

       [(110) ; Description: Reqs only met on res-2 (max-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.9 'res-1 'res-3)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)]

       [(111) ; Description: Reqs only met on res-2 (min-eq)
        ; Expected outcome: first mode (mode-1-1-1) is deployed on res-2
        (change-hw-prov ast 'load 0.3)
        (change-hw-prov ast 'load 0.7 'res-2)
        (change-sw-req ast 'load comp-min-eq 0.4)
        (remove-req-constraints ast)]
 
       [(112) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 does not meet its requirements (max-eq)
        ; Expected outcome: mode-1-1-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (remove-req-constraints ast)]

       [(113) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-1-2 do not meet their requirements (both max-eq)
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-2) ; max-req not met
        (remove-req-constraints ast)]

       [(114) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 & mode-1-1-2 do not meet their requirements (min-eq and max-eq)
        ; Interesting: complete impl not meeting reqs
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (remove-req-constraints ast)]

       [(115) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-1-2 do not meet their requirements (both min-eq)
        ; Interesting: one mode per impl not meeting reqs (only min reqs)
        ; Expected outcome: mode-1-2-1 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (remove-req-constraints ast)]

       [(116) ; Description: Reqs only met on res-2 and res-3
        ; mode-1-1-1 and mode-1-2-1 do not meet their requirements (min-eq and max-eq)
        ; Interesting: one mode per impl not meeting reqs
        ; Expected outcome: mode-1-1-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (remove-req-constraints ast)]

       [(117) ; Description: Reqs only met on res-2 and res-3
        ; only mode-1-2-2 meet its requirements (others mix of max-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-1-1) ; max-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)]

       [(118) ; Description: Reqs only met on res-2 and res-3
        ; only mode-1-2-2 meet its requirements (all min-eq)
        ; Expected outcome: mode-1-2-2 is deployed on res-2 or res-3
        (change-hw-prov ast 'load 0.9 'res-1)
        (change-hw-prov ast 'load 0.4 'res-2)
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-1) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-1-2) ; min-req not met
        (change-sw-req ast 'load comp-min-eq 1.0 'mode-1-2-1) ; min-req not met
        (change-sw-req ast 'load comp-max-eq 0.8 'mode-1-2-2)
        (remove-req-constraints ast)]
 
       [(119) ; Description: Reqs only met on res-1 and res-3 (max-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (min-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.4 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-min-eq 4)
        (change-sw-prov ast 'prop-1 7)
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1) ; min-req not met
        (change-sw-prov ast 'prop-1 2 'mode-1-2-1)] ; min-req not met
 
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
        (change-sw-prov ast 'prop-1 2 'mode-1-2-1)] ; min-req not met
 
       [(121) ; Description: Reqs only met on res-1 and res-3 (max-eq),
        ; mode-1-1-1 and mode 1-2-1 do not meet request constraints (max-eq)
        ; Expected outcome: second mode (mode-1-1-2) is deployed on either res-1 or res-3
        (change-hw-prov ast 'load 0.4 'res-1)
        (change-hw-prov ast 'load 0.9 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 1)
        (change-sw-prov ast 'prop-1 8 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 7 'mode-1-2-1)] ; max-req not met
 
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
        (change-sw-prov ast 'prop-1 7 'mode-1-2-1)] ; max-req not met
 
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
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2)] ; max-req not met
 
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
        (change-sw-prov ast 'prop-1 7 'mode-1-1-2)] ; max-req not met
 
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
        (change-sw-prov ast 'prop-1 2 'mode-1-1-2)] ; max-req not met
 
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
        (change-sw-prov ast 'prop-1 2 'mode-1-1-2)] ; min-req not met

       [else (wrong-id two-impls id)])
     (save-ilp tmp-lp ast)))

 (define (two-comps id)
   ; General description: 2 comps with each 2 impls with each 2 modes, first comps, first impls and first modes are better
   ; All modes of comp-1 require prop-2 <= 20, where by default all modes provide sufficiently prop-2 = 10
   (let ([ast (create-system 3 0 2 2 2 (list (lambda (impl) #t) no-freq-sw-clauses no-freq-hw-clauses #f))])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'mode-1-2-2)
     (change-sw-prov ast pn-energy 30 'mode-2-1-1)
     (change-sw-prov ast pn-energy 35 'mode-2-1-2)
     (change-sw-prov ast pn-energy 40 'mode-2-2-1)
     (change-sw-prov ast pn-energy 45 'mode-2-2-2)
     (change-sw-req ast 'prop-2 comp-max-eq 20 'mode-1-1-1 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
     (change-sw-prov ast 'prop-2 10 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
     (case id
       [(200) ; Description: normal load constraints
        ; Expected outcome: both, mode-1-1-1 and mode-2-1-1 are deployed on either res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)]

       [(201) ; Description: only impl-2-2 meets requirement of comp-1
        ; Expected outcome: both, mode-1-1-1 and mode-2-2-1 are deployed on either res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)
        (change-sw-prov ast 'prop-2 40 'mode-2-1-1 'mode-2-1-2)] ; max-req not met

       [(202) ; Description: mode-2-1-1 not meet hw-reqs (max-eq), mode-1-1-1 not meet request constraint (max-eq)
        ; mode-2-2-1 and mode-2-2-2 not meet comp-1 reqs (max-eq)
        ; Expected outcome: both, mode-1-1-2 and mode-2-1-2 are deployed on either res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-2-1-1)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 4 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
        (change-sw-prov ast 'prop-2 40 'mode-2-2-1 'mode-2-2-2)] ; max-req not met

       [(203) ; Description: mode-2-1-1 not meet hw-reqs (max-eq), mode-1-1-1 not meet request constraint (max-eq)
        ; mode-2-2-1 and mode-2-2-2 not meet comp-1 reqs (max-eq)
        ; Reqs only met at res-3 at all
        ; Expected outcome: both, mode-1-1-2 and mode-2-1-2 are deployed on res-3
        (change-hw-prov ast 'load 1.0 'res-1 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-2-1-1)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (change-sw-prov ast 'prop-1 7 'mode-1-1-1) ; max-req not met
        (change-sw-prov ast 'prop-1 4 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
        (change-sw-prov ast 'prop-2 40 'mode-2-2-1 'mode-2-2-2)] ; max-req not met

       [(204) ; Description: mode-2-2-2 meet hw-reqs at res-1 (max-eq), mode-1-2-2 meet hw-req at res-2 (min-eq)
        ; mode-2-1-*, mode-2-2-1, mode-1-1-* and mode-1-2-1 not meeting reqs
        ; Reqs not met at res-3
        ; Expected outcome: mode-1-2-2 at res-2 and mode-2-2-2 on res-1
        (change-hw-prov ast 'load 0.3 'res-1)
        (change-hw-prov ast 'load 0.5 'res-3)
        (change-hw-prov ast 'load 0.7 'res-2)
        (change-sw-req ast 'load comp-max-eq 0.2)
        (change-sw-req ast 'load comp-max-eq 0.4 'mode-2-2-2)
        (change-sw-req ast 'load comp-min-eq 0.6 'mode-1-2-2)
        (remove-req-constraints ast)]

       [else (wrong-id two-comps id)])
     (save-ilp tmp-lp ast)))

 (define (two-comps-reqc id)
   ; General description: 2 comps with each 2 impls with each 2 modes, first comps, first impls and first modes are better
   ; Only the first impl of comp-1 require prop-2 <= 20, where by default all modes provide sufficiently prop-2 = 10
   (let ([ast (create-system 3 0 2 2 2 (list (lambda (impl) (eq? impl 'impl-1-1))
                                             no-freq-sw-clauses no-freq-hw-clauses #f) )])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'mode-1-2-2)
     (change-sw-prov ast pn-energy 30 'mode-2-1-1)
     (change-sw-prov ast pn-energy 35 'mode-2-1-2)
     (change-sw-prov ast pn-energy 40 'mode-2-2-1)
     (change-sw-prov ast pn-energy 45 'mode-2-2-2)
     (change-sw-req ast 'prop-2 comp-max-eq 20 'mode-1-1-1 'mode-1-1-2)
     (change-sw-prov ast 'prop-2 10 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
     (case id
       [(300) ; Description: normal load constraints
        ; Expected outcome: only mode-1-2-1 is deployed on either res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (remove-req-constraints ast)]
       
       [(301) ; Description: modes of impl-1-2 do not meet their requirements
        ; Expected outcome: both, mode-1-1-1 and mode-2-1-1 are deployed on either res-1, res-2 or res-3
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-req ast 'load comp-max-eq 0.2 'mode-1-2-1 'mode-1-2-2) ; max-req not met
        (remove-req-constraints ast)]
       
       [else (wrong-id two-comps-reqc id)])
     (save-ilp tmp-lp ast)))
 
 ; XXX Currently not working due to PropertyRefs bug XXX
 (define (two-resource-types id)
   ; General description: 1 comp, 2 impls with 2 modes each. Two different resources types and, thus,
   ; dynamical value functions within modes (type-0:res-2 → max load 0.7, type-1:res-1,res-3 → max load 0.3)
   (let* ([hw-types (lambda (res-name) (if (or (eq? res-name 'res-1) (eq? res-name 'res-3)) 1 #f))]
          [load-f (lambda (lomp target) (if (eq? (->name (->type target)) 'type-1) 0.3 0.7))]
          [sw-clauses (lambda _ (lambda (p comp-nr) (if (eq? load-name p) (list make-req comp-max-eq load-f)
                                                        ((no-freq-sw-clauses) p comp-nr))))]
          [ast (create-system 3 0 1 2 2 (list #f sw-clauses no-freq-hw-clauses hw-types))])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-prov ast pn-energy (+ 20 (/ id 1e3)) 'mode-1-2-1)
     (change-sw-prov ast pn-energy (+ 25 (/ id 1e3)) 'mode-1-2-2)
     (change-sw-req ast 'load comp-max-eq 0.8)
     
     (case id
       [(400) ; No further constraints
        ; Expected outcome: mode-1-1-1 on res-2
        (remove-req-constraints ast)]
       
       [(401) ; Impl-1-1 does not meet request constraint
        ; Expected outcome: mode-1-2-1 on either res-2
        (change-sw-prov ast 'prop-1 3 'mode-1-1-1 'mode-1-1-2)
        (change-sw-prov ast 'prop-1 7 'mode-1-2-1 'mode-1-2-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 5)]
       
       [(402) ; Dynamic provision value for requested property
        ; Expected outcome: No solution
        (change-sw-prov ast 'prop-1 (lambda (lomp target) (if (eq? (->name (->type target)) 'type-1) 3 7)))
        (change-req-constraint ast 'prop-1 comp-min-eq 5)]
       
       [(403) ; Dynamic provision value for requested property only for first three modes
        ; Expected outcome: mode-1-2-2 on res-2
        (change-sw-prov ast 'prop-1 (lambda (lomp target) (if (eq? (->name (->type target)) 'type-1) 3 7))
                        'mode-1-1-1 'mode-1-1-2 'mode-1-2-1)
        (change-sw-prov ast 'prop-1 (lambda (lomp target) 7) 'mode-1-2-2)
        (change-req-constraint ast 'prop-1 comp-min-eq 5)]
       
       [(404) ; Low load of 0.2
        ; Expected outcome: mode-1-1-1 on any res
        (change-hw-prov ast 'load 0.2)
        (remove-req-constraints ast)]

       [else (wrong-id two-resource-types id)])
     (save-ilp tmp-lp ast)))

 (define (new-resources id)
   (define (add-resource name prototype parent)
     (let* ([type (->type prototype)]
            [cs (->* (->ProvClause* prototype))]
            [new-cs (map (lambda (c) (:ProvClause mquat-spec (->return-type c) (->comparator c) (->value c))) cs)])
       (rewrite-add (->SubResources parent) (:Resource mquat-spec name type (list) new-cs))))
   ; General description: New resources entering the system, enabling new configurations
   (let ([ast (create-system 2 0 1 1 2 (list #f no-freq-sw-clauses no-freq-hw-clauses #f))])
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-req ast 'load comp-max-eq 0.8)
     (case id
       [(500) ; No further constraints
        ; Expected outcome: mode-1-1-1 on either res-1 or res-2
        (remove-req-constraints ast)]

       [(501) ; High load
        ; Expected outcome: No solution
        (change-hw-prov ast 'load 0.9)
        (remove-req-constraints ast)]

       [(502) ; High load, but a fresh resource available
        ; Expected outcome: mode-1-1-1 on (new) res-3
        (change-hw-prov ast 'load 0.9)
        (save-ilp tmp-lp ast)
        (add-resource 'res-3 (car (->* (->SubResources (->HWRoot ast)))) (->HWRoot ast))
        (change-hw-prov ast 'load 0.5 'res-3)
        (remove-req-constraints ast)]

       [(503) ; High load, new resource available, but still no viable solution
        ; Expected outcome: No solution
        (change-hw-prov ast 'load 0.9)
        (save-ilp tmp-lp ast)
        (add-resource 'res-3 (car (->* (->SubResources (->HWRoot ast)))) (->HWRoot ast))
        (change-hw-prov ast 'load 0.9 'res-3)
        (remove-req-constraints ast)]

       [else (wrong-id new-resources id)])
     (save-ilp tmp-lp ast)))

 (define (new-software id)
   ; General description: New software (comp,impl,mode) entering the system, enabling new configurations
   (let ([ast (create-system 2 0 1 1 2 (list #f no-freq-sw-clauses no-freq-hw-clauses #f))])
     (define (add-comp comp-nr)
       (debug "#create new comp" comp-nr)
       (let ([new (:Comp mquat-spec (node-name 'comp (list comp-nr)) (list) #f 
                                         (list (:Property mquat-spec (node-name 'prop (list comp-nr))
                                                          '1 'runtime 'increasing 'sum)
                                               (:Property mquat-spec pn-energy 'J 'runtime 'decreasing 'sum)))])
         (rewrite-add (->Comp* (->SWRoot ast)) new) new))
     (define (find-create l prefix lon make-new)
       (let ([name (node-name prefix lon)])
         (or (ast-find-child (lambda (i child) (eq? (->name child) name)) l) (make-new))))
     (define (find-create-comp comp-nr) (find-create (->Comp* (->SWRoot ast)) 'comp (list comp-nr) (lambda _ (add-comp comp-nr))))
     (define (add-impl comp-nr impl-nr reqcomps)
       (debug "#create new impl" comp-nr impl-nr reqcomps)
       (let ([new (:Impl mquat-spec (node-name 'impl (list impl-nr comp-nr)) (list)
                         (map (lambda (nr) (find-create-comp nr)) reqcomps) #f #f)])
         (rewrite-add (->Impl* (find-create-comp comp-nr)) new) new))
     (define (find-create-impl comp-nr impl-nr reqcomps) (find-create (->Impl* (find-create-comp comp-nr)) 'impl
                                                                      (list impl-nr comp-nr)
                                                                      (lambda _ (add-impl comp-nr impl-nr reqcomps))))
     (define (add-mode comp-nr impl-nr mode-nr req-comp-nr load-f energy-f prov-f prev-f)
       (debug "#create new mode" comp-nr impl-nr mode-nr req-comp-nr)
       (let* ([impl (find-create-impl comp-nr impl-nr (if req-comp-nr (list req-comp-nr) (list)))]
              [find-prop-hw (lambda (name) (ast-find-child (lambda (i child) (eq? (->name child) name))
                                                            (->Property* (car (->* (->ResourceType* (->HWRoot ast)))))))]
              [find-prop-sw (lambda (name comp) (ast-find-child (lambda (i child) (eq? (->name child) name))
                                                                 (->Property* comp)))]
              [load (find-prop-hw load-name)]
              [energy (find-prop-sw pn-energy (find-create-comp comp-nr))]
              [prev-p (and req-comp-nr (find-prop-sw (node-name 'prop (list req-comp-nr)) (find-create-comp req-comp-nr)))]
              [this-p (find-prop-sw (node-name 'prop (list comp-nr)) (find-create-comp comp-nr))]
              [clauses (filter (lambda (c) c) (list (:ReqClause mquat-spec load comp-max-eq load-f)
                                                    (:ProvClause mquat-spec energy comp-max-eq energy-f)
                                                    (:ProvClause mquat-spec this-p comp-max-eq prov-f)
                                                    (and req-comp-nr (:ReqClause mquat-spec prev-p comp-max-eq prev-f))))]
              [new (:Mode mquat-spec (node-name 'mode (list mode-nr impl-nr comp-nr)) clauses)])
         (rewrite-add (->Mode* impl) new) new))
     (define (prov-obj val id) (+ val (/ id 1e3)))
     (change-sw-prov ast pn-energy (+ 10 (/ id 1e3)) 'mode-1-1-1)
     (change-sw-prov ast pn-energy (+ 15 (/ id 1e3)) 'mode-1-1-2)
     (change-sw-req ast 'load comp-max-eq 0.2)
     (case id
       [(600) ; No further constraints
        ; Expected outcome: No solution
        (remove-req-constraints ast)]

       [(601) ; New mode of first impl requiring less
        ; Expected outcome: (new) mode-1-1-3 on either res-1 or res-2
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)
        (add-mode 1 1 3 #f (lambda _ 0.8) (lambda _ (prov-obj 20 id)) (lambda _ 2) #f)]

       [(602) ; New mode of a new second impl requiring less
        ; Expected outcome: (new) mode-1-2-1 on either res-1 or res-2
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)
        (add-mode 1 2 1 #f (lambda _ 0.8) (lambda _ (prov-obj 20 id)) (lambda _ 2) #f)]

       [(603) ; New component using the existing comp, with one impl and one mode
        ; Expected outcome: mode-1-1-1 and (new) mode-2-1-1, both on either res-1 or res-2
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-1 4)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)
        (add-mode 2 1 1 1 (lambda _ 0.8) (lambda _ 20) (lambda _ 2) (lambda _ 7))
        (rewrite-terminal 'target (<=request ast) (find-create-comp 2))]

       [(604) ; New component using the existing comp, with one impl and one mode
        ; High load on res-1, high load required for comp-1-modes, low load required for new comp/mode
        ; Expected outcome: mode-1-1-1 on res-1 and (new) mode-2-1-1 on res-2
        (change-sw-req ast 'load comp-min-eq 0.9)
        (change-hw-prov ast 'load 0.91 'res-1)
        (change-sw-prov ast 'prop-1 4)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)
        (add-mode 2 1 1 1 (lambda _ 0.8) (lambda _ 20) (lambda _ 2) (lambda _ 7))
        (rewrite-terminal 'target (<=request ast) (find-create-comp 2))]

       [(605) ; New component using the existing comp, with one impl and one mode. Request still targets old component.
        ; Expected outcome: mode-1-1-1 on either res-1 or res-2
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-1 4)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast)
        (add-mode 2 1 1 1 (lambda _ 0.8) (lambda _ 20) (lambda _ 2) (lambda _ 7))]

       [else (wrong-id new-software id)])
     (save-ilp tmp-lp ast) ast))
 
 (define (unsolvable id)
   ; General description: Systems without a solution, thus, no optimal solution
   (case id
     [(900) ; All hw-reqs not met (min-eq)
      (let [(ast (create-system 3 0 1 1 2 (list #f no-freq-sw-clauses no-freq-hw-clauses #f)))]
        (change-hw-prov ast 'load 0.4)
        (change-sw-req ast 'load comp-min-eq 0.7)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast))]

     [(901) ; All hw-reqs not met (max-eq)
      (let [(ast (create-system 3 0 1 1 2))]
        (change-hw-prov ast 'load 0.8)
        (change-sw-req ast 'load comp-max-eq 0.3)
        (remove-req-constraints ast)
        (save-ilp tmp-lp ast))]

     [(902) ; Request constraint not met for any mode of the target component (min-eq)
      (let [(ast (create-system 3 0 1 2 3))]
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-1 2)
        (change-req-constraint ast 'prop-1 comp-min-eq 15)
        (save-ilp tmp-lp ast))]

     [(903) ; Request constraint not met for any mode of the target component (max-eq)
      (let [(ast (create-system 3 0 1 2 3))]
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-1 7)
        (change-req-constraint ast 'prop-1 comp-max-eq 4)
        (save-ilp tmp-lp ast))]

     [(904) ; Requirement on second component not met by any of its modes (max-eq)
      (let [(ast (create-system 3 0 2 2 2 (list (lambda _ #t) #f #f #f)))]
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-2 7 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
        (change-sw-req ast 'prop-2 comp-max-eq 4 'mode-1-1-1 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
        (save-ilp tmp-lp ast))]

     [(905) ; Requirement on second component not met by any of its modes (min-eq)
      (let [(ast (create-system 3 0 2 2 2 (list (lambda _ #t) #f #f #f)))]
        (change-hw-prov ast 'load 0.8)
        (change-sw-req ast 'load comp-max-eq 0.3)
        (change-sw-prov ast 'prop-2 2 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
        (change-sw-req ast 'prop-2 comp-min-eq 15 'mode-1-1-1 'mode-1-1-2 'mode-1-2-1 'mode-1-2-2)
        (save-ilp tmp-lp ast))]

     [(906) ; Requirement on third component not met by any of its modes (max-eq)
      (let [(ast (create-system 3 0 3 2 2 (list (lambda _ #t) #f #f #f)))]
        (change-sw-req ast 'load comp-max-eq 0.8)
        (change-sw-prov ast 'prop-3 7 'mode-3-1-1 'mode-3-1-2 'mode-3-2-1 'mode-3-2-2)
        (change-sw-req ast 'prop-3 comp-max-eq 4 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
        (save-ilp tmp-lp ast))]

     [(907) ; Requirement on third component not met by any of its modes (min-eq)
      (let [(ast (create-system 3 0 3 2 2 (list (lambda _ #t) #f #f #f)))]
        (change-hw-prov ast 'load 0.8)
        (change-sw-req ast 'load comp-max-eq 0.3)
        (change-sw-prov ast 'prop-3 2 'mode-3-1-1 'mode-3-1-2 'mode-3-2-1 'mode-3-2-2)
        (change-sw-req ast 'prop-3 comp-min-eq 15 'mode-2-1-1 'mode-2-1-2 'mode-2-2-1 'mode-2-2-2)
        (save-ilp tmp-lp ast))]

     [else (wrong-id unsolvable id)]))
 
 (define (read-solution table fname)
   (when (not (file-exists? fname)) (error 'read-solution "File not found." fname))
   (with-input-from-file fname
     (lambda ()
       (for-each (lambda (entry) (hashtable-set! table (car entry) (cdr entry))) (read)))))
 
 (define (check-test id-s obj fname)
   (let* ([table (make-hashtable string-hash string=?)]
          [val (lambda (name) (or (hashtable-ref table name #f) (error #f "Var not found" name)))]
          [contains? (lambda (name) (hashtable-contains? table name))]
          [id (string->number id-s)]
          [test-obj (lambda (expected-base actual id) (let ([expected (+ expected-base (/ id 1e3))])
                                                        (if (not (= actual expected))
                                                            (error #f "Wrong objective value, expected first, given last"
                                                                   expected actual))))]
          [test-assert (lambda (msg expr) (if (not expr) (error #f msg)))])
     (define (val=? base expected op lres)
       (if (null? lres) (lambda (name) (= (val name) expected))
           (let ([=name (lambda (base res-nr) (string-append base "#res_" (number->string res-nr)))])
             (op (lambda (res-nr) (= (val (=name base res-nr)) expected)) lres))))
     (define (val=1? base . lres) (val=? base 1 exists  lres))
     (define (val=0? base . lres) (val=? base 0 for-all lres))
     (read-solution table fname)
     (for-each (lambda (key) (debug key ":" (hashtable-ref table key #f))) (vector->list (hashtable-keys table)))
     ; impl deployment
     (case id
       [(1 2 3 4 5 6 7 8 9 10 11 12 13 14 500 502 601)
        (test-assert "impl not deployed"     (val=1? "b#comp_1#"))]
       [(15 100 101 105 112 116 119 120 121 122 400 401 403 404 602)
        (test-assert "impl-1 not deployed"   (val=1? "b#comp_1#impl_1_1"))
        (test-assert "impl-2 deployed"       (val=0? "b#comp_1#impl_1_2"))]
       [(16 17 18 19 20 21 102 103 104 106 107 108 109 110 111 113 114 115 117 118 123 124 125 126)
        (test-assert "impl-1 deployed"       (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-2 not deployed"   (val=1? "b#comp_1#impl_1_2"))]
       [(200 202 203 301)
        (test-assert "impl-1-1 not deployed" (val=1? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-1 not deployed" (val=1? "b#comp_2#impl_2_1"))
        (test-assert "impl-1-2 deployed"     (val=0? "b#comp_1#impl_1_2"))
        (test-assert "impl-2-2 deployed"     (val=0? "b#comp_2#impl_2_2"))]
       [(201)
        (test-assert "impl-1-1 not deployed" (val=1? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-2 not deployed" (val=1? "b#comp_2#impl_2_2"))
        (test-assert "impl-1-2 deployed"     (val=0? "b#comp_1#impl_1_2"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))]
       [(204)
        (test-assert "impl-1-2 not deployed" (val=1? "b#comp_1#impl_1_2"))
        (test-assert "impl-2-2 not deployed" (val=1? "b#comp_2#impl_2_2"))
        (test-assert "impl-1-1 deployed"     (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))]
       [(300)
        (test-assert "impl-1-2 not deployed" (val=1? "b#comp_1#impl_1_2"))
        (test-assert "impl-1-1 deployed"     (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))
        (test-assert "impl-2-2 deployed"     (val=0? "b#comp_2#impl_2_2"))]
       [(402 902 903)
        (test-assert "impl-1 deployed"       (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-2 deployed"       (val=0? "b#comp_1#impl_1_2"))]
       [(501 503 600 900 901)
        (test-assert "impl-1 deployed"       (val=0? "b#comp_1#impl_1_1"))]
       [(603 604)
        (test-assert "impl-1-1 not deployed" (val=1? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-1 not deployed" (val=1? "b#comp_2#impl_2_1"))]
       [(605)
        (test-assert "impl-1-1 not deployed" (val=1? "b#comp_1#impl_1_1"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))]
       [(904 905)
        (test-assert "impl-1-1 deployed"     (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-1-2 deployed"     (val=0? "b#comp_1#impl_1_2"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))
        (test-assert "impl-2-2 deployed"     (val=0? "b#comp_2#impl_2_2"))]
       [(906 907)
        (test-assert "impl-1-1 deployed"     (val=0? "b#comp_1#impl_1_1"))
        (test-assert "impl-1-2 deployed"     (val=0? "b#comp_1#impl_1_2"))
        (test-assert "impl-2-1 deployed"     (val=0? "b#comp_2#impl_2_1"))
        (test-assert "impl-2-2 deployed"     (val=0? "b#comp_2#impl_2_2"))
        (test-assert "impl-3-1 deployed"     (val=0? "b#comp_3#impl_3_1"))
        (test-assert "impl-3-2 deployed"     (val=0? "b#comp_3#impl_3_2"))]
       [else (error #f "Unknown test case id for impls" id)])
     
     ; mode-deployment
     (case id
       [(1 500)       (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1##mode_1_1_1" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(2 3 4 5)     (test-assert "mode-1-1-2 not deployed"        (val=1? "b#comp_1##mode_1_1_2" 1 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1##mode_1_1_1" 1 2))]
       [(6 14)        (test-assert "mode-1-1-1 not deployed on 1"   (val=1? "b#comp_1##mode_1_1_1" 1))
                      (test-assert "mode-1-1-1 deployed on 2"       (val=0? "b#comp_1##mode_1_1_1" 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(7 12)        (test-assert "mode-1-1-1 not deployed on 2"   (val=1? "b#comp_1##mode_1_1_1" 2))
                      (test-assert "mode-1-1-1 deployed on 1"       (val=0? "b#comp_1##mode_1_1_1" 1))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(13)          (test-assert "mode-1-1-2 not deployed on 1"   (val=1? "b#comp_1##mode_1_1_2" 1))
                      (test-assert "mode-1-1-2 deployed on 2"       (val=0? "b#comp_1##mode_1_1_2" 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1##mode_1_1_1" 1 2))]
       [(8 9 10 11)   (test-assert "mode-1-1-2 not deployed on 2"   (val=1? "b#comp_1##mode_1_1_2" 2))
                      (test-assert "mode-1-1-2 deployed on 1"       (val=0? "b#comp_1##mode_1_1_2" 1))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1##mode_1_1_1" 1 2))]
       [(15)          (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1#impl_1_1#" 1 2))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#" 1 2))]
       [(16 17 18 19) (test-assert "mode-1-2-1 not deployed"        (val=1? "b#comp_1#impl_1_2#" 1 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#" 1 2))]
       [(20 21)       (test-assert "mode-1-2-1 not deployed on 2"   (val=1? "b#comp_1#impl_1_2#" 2))
                      (test-assert "mode-1-2-1 deployed on 1"       (val=0? "b#comp_1#impl_1_2#" 1))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#" 1 2))]
       [(100)         (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(101 105)     (test-assert "mode-1-1-2 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(102 103 104) (test-assert "mode-1-2-1 not deployed"        (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(106 107)     (test-assert "mode-1-2-2 not deployed"        (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
       [(108 109)     (test-assert "mode-1-1-1 not deployed on 23"  (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2 3))
                      (test-assert "mode-1-1-1 deployed on 1"       (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(110 111)     (test-assert "mode-1-1-1 not deployed on 2"   (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2))
                      (test-assert "mode-1-1-1 deployed on 1"       (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(112 116)     (test-assert "mode-1-1-2 not deployed on 23"  (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 2 3))
                      (test-assert "mode-1-1-2 deployed on 1"       (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(113 114 115) (test-assert "mode-1-2-1 not deployed on 23"  (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 2 3))
                      (test-assert "mode-1-2-1 deployed on 1"       (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(117 118)     (test-assert "mode-1-2-2 not deployed on 23"  (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2 3))
                      (test-assert "mode-1-2-2 deployed on 1"       (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
       [(119 120 121 122) (test-assert "mode-1-1-2 not deployed on 13"  (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 3))
                          (test-assert "mode-1-1-2 deployed on 2"       (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 2))
                          (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                          (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                          (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(123 125)     (test-assert "mode-1-2-2 not deployed on 12"  (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 1 2))
                      (test-assert "mode-1-2-2 deployed on 3"       (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
       [(124 126)     (test-assert "mode-1-2-2 not deployed on 2"   (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2))
                      (test-assert "mode-1-2-2 deployed on 13"      (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
       [(200 301)     (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-2-1-1 not deployed"        (val=1? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
       [(201)         (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-2-2-1 not deployed"        (val=1? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
       [(202)         (test-assert "mode-1-1-2 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-2-1-2 not deployed"        (val=1? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
       [(203)         (test-assert "mode-1-1-2 not deployed on 3"   (val=1? "b#comp_1#impl_1_1#mode_1_1_2" 3))
                      (test-assert "mode-2-1-2 not deployed on 3"   (val=1? "b#comp_2#impl_2_1#mode_2_1_2" 3))
                      (test-assert "mode-1-1-2 deployed on 12"      (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2))
                      (test-assert "mode-2-1-2 deployed on 12"      (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
       [(204)         (test-assert "mode-1-2-2 not deployed on 2"   (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2))
                      (test-assert "mode-2-2-2 not deployed on 1"   (val=1? "b#comp_2#impl_2_2#mode_2_2_2" 1))
                      (test-assert "mode-1-2-2 deployed on 13"      (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 3))
                      (test-assert "mode-2-2-2 deployed on 23"      (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))]
       [(300)         (test-assert "mode-1-2-1 not deployed"        (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_2" 1 2 3))]
       [(400)         (test-assert "mode-1-1-1 not deployed on 2"   (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 2))
                      (test-assert "mode-1-1-1 deployed on 13"      (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(401)         (test-assert "mode-1-2-1 not deployed on 2"   (val=1? "b#comp_1#impl_1_2#mode_1_2_1" 2))
                      (test-assert "mode-1-2-1 deployed on 13"      (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(403)         (test-assert "mode-1-2-2 not deployed on 2"   (val=1? "b#comp_1#impl_1_2#mode_1_2_2" 2))
                      (test-assert "mode-1-2-2 deployed on 13"      (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 3))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))]
       [(404)         (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))]
       [(502)         (test-assert "mode-1-1-1 not deployed on 3"   (val=1? "b#comp_1##mode_1_1_1" 3))
                      (test-assert "mode-1-1-1 deployed on 12"      (val=0? "b#comp_1##mode_1_1_1" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2 3))]
       [(601)         (test-assert "mode-1-1-3 not deployed"        (val=1? "b#comp_1##mode_1_1_3" 1 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1##mode_1_1_1" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(602)         (test-assert "mode-1-2-1 not deployed"        (val=1? "b#comp_1#impl_1_2#" 1 2))
                      (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2))]
       [(603)         (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1##mode_1_1_1" 1 2))
                      (test-assert "mode-2-1-1 not deployed"        (val=1? "b#comp_2##" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(604)         (test-assert "mode-1-1-1 not deployed on 1"   (val=1? "b#comp_1##mode_1_1_1" 1))
                      (test-assert "mode-2-1-1 not deployed on 2"   (val=1? "b#comp_2##" 2))
                      (test-assert "mode-1-1-1 deployed on 2"       (val=0? "b#comp_1##mode_1_1_1" 2))
                      (test-assert "mode-2-1-1 deployed on 1"       (val=0? "b#comp_2##" 1))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(605)         (test-assert "mode-1-1-1 not deployed"        (val=1? "b#comp_1##mode_1_1_1" 1 2))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2##" 1 2))]
       [(900 901 501 503 600) (test-assert "mode-1-1-1 deployed"    (val=0? "b#comp_1##mode_1_1_1" 1 2))
                              (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1##mode_1_1_2" 1 2))]
       [(902 903)     (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-1-3 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_3" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-1-2-3 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_3" 1 2 3))]
       [(904 905)     (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))]
       [(906 907)     (test-assert "mode-1-1-1 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_1" 1 2 3))
                      (test-assert "mode-1-1-2 deployed"            (val=0? "b#comp_1#impl_1_1#mode_1_1_2" 1 2 3))
                      (test-assert "mode-1-2-1 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_1" 1 2 3))
                      (test-assert "mode-1-2-2 deployed"            (val=0? "b#comp_1#impl_1_2#mode_1_2_2" 1 2 3))
                      (test-assert "mode-2-1-1 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_1" 1 2 3))
                      (test-assert "mode-2-1-2 deployed"            (val=0? "b#comp_2#impl_2_1#mode_2_1_2" 1 2 3))
                      (test-assert "mode-2-2-1 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-2-2-2 deployed"            (val=0? "b#comp_2#impl_2_2#mode_2_2_1" 1 2 3))
                      (test-assert "mode-3-1-1 deployed"            (val=0? "b#comp_3#impl_3_1#mode_3_1_1" 1 2 3))
                      (test-assert "mode-3-1-2 deployed"            (val=0? "b#comp_3#impl_3_1#mode_3_1_2" 1 2 3))
                      (test-assert "mode-3-2-1 deployed"            (val=0? "b#comp_3#impl_3_2#mode_3_2_1" 1 2 3))
                      (test-assert "mode-3-2-2 deployed"            (val=0? "b#comp_3#impl_3_2#mode_3_2_1" 1 2 3))]
       [else (error #f "Unknown test case id for modes" id)])

     (case id
       [(1 6 7 12 14 15 100 108 109 110 111 400 404 500 502 605)
        (test-obj 10 obj id)]
       [(101 105 112 116 119 120 121 122)
        (test-obj 15 obj id)]
       [(2 3 4 5 8 9 10 11 13 16 17 18 19 20 21 102 103 104 113 114 115 300 401 601 602)
        (test-obj 20 obj id)]
       [(106 107 117 118 123 124 125 126 403)
        (test-obj 25 obj id)]
       [(603 604) (test-obj 30 obj id)]
       [(200 301) (test-obj 40 obj id)] ;10+30
       [(201)     (test-obj 50 obj id)] ;10+40
       [(202 203) (test-obj 50 obj id)] ;15+35
       [(204)     (test-obj 70 obj id)] ;25+45
       [(402 501 503 600 900 901 902 903 904 905 906 907) (test-assert "Objective not zero." (eq? 0.0 obj))]
       [else (error #f "Unknown test case id for objectives" id)])))
 
 (define (display-ranges)
   (display "1 21 100 126 200 204 300 301 400 404 500 503 600 605 900 907"))
 
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
                [id-s (car cmds)] [obj (string->number (cadr cmds))] [fname (caddr cmds)])
           (check-test id-s obj fname))))))
