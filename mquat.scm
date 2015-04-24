#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define debugging #f)
(define comp-names (list))
(define pn-energy 'energy-consumption) ; Property-Name of energy-consumption used for all properties and the default objective function

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Property*-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-Comp*<ReqComps-selectedimpl)
   (ast-rule 'Impl->name-Contract-deployedon-selectedmode)
   (ast-rule 'Contract->Mode*)
   (ast-rule 'Mode->name-Clause*)
   ;value is a lambda expecting an AST-List-Node with MetaParameters, returning the value
   ;comp is a lambda expecting two values, required and actual, returning #t or #f
   (ast-rule 'Clause->returntype-comp-value)
   ; target is either a Comp or a ResourceType
   (ast-rule 'ReqClause:Clause->target)
   (ast-rule 'ProvClause:Clause->)
   (ast-rule 'HWRoot->ResourceType*-Resource*)
   (ast-rule 'ResourceType->name)
   ; type is a ResourceType
   (ast-rule 'Resource->name-type-Resource*<SubResources-ProvClause*)
   (ast-rule 'Request->MetaParameter*-ReqClause*<Constraints-objective)
   (ast-rule 'MetaParameter->name-value)
   ; kind=static|runtime|derived. direction=decreasing|increasing
   (ast-rule 'Property->name-unit-kind-direction)
   
   (define comp-min-eq (lambda (req act) (<= req act)))
   (define comp-max-eq (lambda (req act) (>= req act)))
   (define comp-eq (lambda (req act) (= req act)))
   (define f-comp-max-diff-eq (lambda (diff) (lambda (req act) (<= (- req act) diff))))
   
   (set! comp-names
         (list
          (list comp-eq '=)
          (list comp-min-eq '<=)
          (list comp-max-eq '>=)))
   
   (compile-ast-specifications 'Root)
   
   ;; AG rules
   (ag-rule
    objective-value
    (Root ; sum of objective value of all software components (skipping SWRoot)
     (lambda (n)
       (fold-left
        ; call the same attribute on all childs
        (lambda (totalValue comp) (+ totalValue (att-value 'objective-value comp)))
        0 (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp ; sum of objective value of selected impl and all objective value of required components
     (lambda (n)
       (fold-left (lambda (totalValue reqComp) (+ totalValue (att-value 'objective-value reqComp)))
                  (att-value 'objective-value (ast-child 'selectedimpl n))
                  (ast-children (ast-child 'ReqComps n)))))
    (Impl ; call the same attribute on the mode to use
     (lambda (n)
       (att-value 'objective-value (att-value 'mode-to-use n))))
    (Mode ; find and evaluate the energy-consumption provClause
     (lambda (n)
       (att-value 'eval (att-value 'provided-clause n pn-energy)))))
   
   (ag-rule
    clauses-met?
    (Root ; clauses-met for all software components and for request?
     (lambda (n)
       (fold-left (lambda (result comp) (and result (att-value 'clauses-met? comp)))
                  (att-value 'objective-value (ast-child 'Request n))
                  (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp ; clauses-met for selected impl and for all required components?
     (lambda (n)
       (fold-left (lambda (result reqComp) (and result (att-value 'clauses-met? reqComp)))
                  (att-value 'clauses-met? (ast-child 'selectedimpl n))
                  (ast-children (ast-child 'ReqComps n)))))
    (Impl ; clauses-met for mode to use?
     (lambda (n)
       (att-value 'clauses-met? (att-value 'mode-to-use n))))
    (Mode ; clauses-met for all clauses?
     (lambda (n)
       (fold-left (lambda (result clause) (and result (att-value 'clauses-met? clause)))
                  #t (ast-children (ast-child 'Clause* n)))))
    (ReqClause ; comparator function returns true?
     (lambda (n)
       ((ast-child 'comp n) (att-value 'eval n) (att-value 'actual-value n))))
    (ProvClause ; Provision clauses are always fulfilled
     (lambda (n) #t))
    (Request ; clauses-met for all constraints
     (lambda (n)
       (fold-left (lambda (result clause) (and result (att-value 'clauses-met? clause)))
                  #t (ast-children (ast-child 'Constraints n))))))
   
   (ag-rule
    actual-value
    (ReqClause
     (lambda (n)
       (let
           ([propName (ast-child 'name (ast-child 'returntype n))]
            [target (ast-child 'target n)])
         ((ast-child
           'value (if (ast-subtype? target 'ResourceType)
                      (att-value 'provided-clause (ast-child 'deployedon  (att-value 'get-impl n)) propName target) ; hw -> search in deployedon for name and type
                      (att-value 'provided-clause (att-value 'mode-to-use (ast-child 'selectedimpl target)) propName))) ; sw -> search in target-component
          (ast-child 'MetaParameter* (att-value 'get-request n)))))) ; Params from request, applied to the value function
    (ProvClause
     (lambda (n)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n))))))
   
   ; Given the name of a property (and optionally the type of the resource), get ProvisionClause for this property
   (ag-rule
    provided-clause
    (Resource ; Search through ProvClauses of this resource and its subresources to find a clause with matching name of a resource with matching type
     (lambda (n name type)
       (let
           ([search-subresources
             (lambda ()
               (ast-find-child
                (lambda (index subres)
                  (att-value 'provided-clause subres))
                (ast-child 'SubResources n)))])
         (if (eq? (ast-child 'type n) type) ; if n has correct type ...
             (let
                 ([found-clause
                   (ast-find-child ; (1) ... then try to find a child in n ...
                    (lambda (index clause)
                      (eq? (ast-child 'name (ast-child 'returntype clause)) name))
                    (ast-child 'ProvClause* n))])
               (if found-clause ; (1.q) if a child was found ...
                   found-clause ; (1.1) ... return it
                   (search-subresources))) ; (1.2) ... else search in subresources
             ; (2) ... if not correct type or no child was found, search subresources of n
             (search-subresources)))))
    (Mode ; Search through Clauses to find a ProvClause with matching name
     (lambda (n name)
       (att-value 'search-clause n name 'ProvClause))))
   
   (ag-rule
    search-clause
    (Mode
     (lambda (n name subtype)
       (debug name " in " (ast-child 'name n))
       (ast-find-child
        (lambda (index clause)
          (and (ast-subtype? clause subtype) (eq? (ast-child 'name (ast-child 'returntype clause)) name)))
        (ast-child 'Clause* n)))))
   
   (ag-rule get-request (Root (lambda (n) (ast-child 'Request n)))) ; Get request from every node
   
   (ag-rule get-impl (Impl (lambda (n) n))) ; Get Impl in subtree of the Impl
   
   (ag-rule get-comp (Comp (lambda (n) n))) ; Get Comp in subtree of the Comp
   
   ; Call the function of a Clause with the MetaParams-AST-node of the request
   (ag-rule
    eval
    (Clause
     (lambda (n)
       ; If inside a mode and impl of mode is selected, or outside of a mode ...
       (if (or (not (ast-subtype? (ast-parent (ast-parent n)) 'Mode)) (att-value 'is-selected? (att-value 'get-impl n)))
           (att-value 'eval-unsafe n) ; ... apply value function with metaparams ...
           #f)))) ; ... else don't evaluate and return false
   
   (ag-rule
    eval-unsafe
    (Clause
     (lambda (n)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n))))))
   
   ; Given a list-node n, search for a MetaParameter with the given name. If none found, return the default value
   (define get-val
     (lambda (n name default)
       (letrec
           ([G
             (lambda (lomp)
               (cond
                 ((null? lomp) default)
                 ((eq? (ast-child 'name (car lomp)) name) (ast-child 'value (car lomp)))
                 (else (G (cdr lomp)))))])
         (G (ast-children n)))))
   
   ; Given a metaparameter name, return the value of the according metaparameter
   (ag-rule value-of (Request (lambda (n name) (get-val (ast-child 'MetaParameter* n) name #f))))
   
   (ag-rule is-selected? (Impl (lambda (n) (eq? (ast-child 'selectedimpl (ast-parent (ast-parent n))) n))))
   
   (ag-rule is-deployed? (Impl (lambda (n) (ast-node? (ast-child 'deployedon n)))))
   
   ; Return either the selected-mode or the first mode
   (ag-rule
    mode-to-use
    (Impl
     (lambda (n)
       (or (ast-child 'selectedmode n) (ast-child 1 (ast-child 'Mode* (ast-child 'Contract n)))))))
   
   ;;; ILP-Creation rules

   ; Return a list of 1 objective, some constraints, some bounds and some generals, everything
   ; packed inside a list, e.g. one constraint is also a list.
   (ag-rule
    to-ilp
    (Root
     (lambda (n)
       (let
           ([binary-vars (att-value 'ilp-binary-vars n)])
         (list
          (list "Minimize")
          (att-value 'ilp-objective n)
          (list "Subject To")
          (append
           (att-value 'to-ilp (ast-child 'Request n)) ; request-constraints
           (att-value 'to-ilp (ast-child 'SWRoot n)) ; archtitecture-constraints
           (att-value 'ilp-nego n)) ; NFP-negotiation
          (list "Bounds")
          (append
           (att-value 'to-ilp (ast-child 'HWRoot n))
           (map (lambda (var) (list 0 "<=" var "<=" 1)) binary-vars))
          (list "Generals")
          binary-vars
         (list "End")))))
    (Request
     (lambda (n)
       (fold-left
        (lambda (result c)
          (cons (list
                 (att-value 'eval c)
                 (comp->string (ast-child 'comp c))
                 (ast-child 'name (ast-child 'returntype c)))
                result))
        (list)
        (ast-children (ast-child 'Constraints n)))))
    (SWRoot
     (lambda (n)
       (fold-left
        (lambda (result comp) (append (att-value 'to-ilp comp) result))
        (list)
        (ast-children (ast-child 'Comp* n)))))
    (Comp
     (lambda (n)
       (debug "Comp:" (ast-child 'name n))
       (fold-left
        (lambda (result impl) (cons
                               (list "1" "=" "+" (att-value 'ilp-varname impl))
                               (append (att-value 'to-ilp impl) result)))
        (list)
        (ast-children (ast-child 'Impl* n)))))
    (Impl
     (lambda (n)
       (debug "Impl:" (ast-child 'name n))
       (cons
        (fold-left ; deploy the same impl only on one pe
         (lambda (result pe) (cons* "+" (att-value 'ilp-varname-deployed n pe) result))
         (list "-" (att-value 'ilp-varname n) "=" 0)
         (att-value 'every-pe n))
        (fold-left
         (lambda (result reqC)
           (cons (fold-left ; if use this impl, also use one of the impls per required component
                  (lambda (result rci) (cons* "+" (att-value 'ilp-varname rci) result))
                  (list "-" (att-value 'ilp-varname n) "=" 0)
                  (ast-children (ast-child 'Impl* reqC)))
                 (append (att-value 'to-ilp reqC) result)))
         (list)
         (att-value 'reqComps n)))))
    (HWRoot
     (lambda (n)
       (fold-left
        (lambda (result res) (append (att-value 'to-ilp res) result))
        (list)
        (ast-children (ast-child 'Resource* n)))))
    (Resource
     (lambda (n)
       (fold-left
        (lambda (result c)
          (cons (list 0 "<=" (att-value 'ilp-propname c) "<=" (att-value 'eval c)) result))
        (list)
        (ast-children (ast-child 'ProvClause* n))))))
   
   (ag-rule
    ilp-objective
    (Root
     (lambda (n)
       (fold-left
        (lambda (result mode)
          (cons* "+" (att-value 'ilp-objective mode) (att-value 'ilp-varname mode) result))
        (list)
        (att-value 'every-mode n))))
    (Mode (lambda (n) (att-value 'actual-value (att-value 'provided-clause n pn-energy)))))
   
   (ag-rule reqComps (Comp (lambda (n) (ast-children (ast-child 'ReqComps n)))))
   
   ; Creates a list of NFP-negotiation constraints
   (ag-rule
    ilp-nego
    (Root
     (lambda (n)
       (remove (list) ; remove empty constraints
               (fold-left
                (lambda (result prop) (append (att-value 'ilp-nego prop) result))
                (list)
                (ast-children (ast-child 'Property* n))))))
    (Property
     (lambda (n)
       (let*
           ([pname (att-value 'ilp-varname n)]
            [append-if-constrained (lambda (comp loc) (debug loc) (if (null? loc) (list) (cons* pname comp loc)))]) ; add property name and "=", ">=" and "<=" resp.
         (map append-if-constrained (list "=" "<=" ">=")
              (fold-left
               (lambda (result mode) (merge-list (att-value 'ilp-nego mode n) result))
               (list (list) (list) (list))
               (att-value 'every-mode n))))))
    (Mode
     (lambda (n prop)
       (debug "ilp-nego:" (ast-child 'name n) ",prop:" (ast-child 'name prop))
       (let*
           ([found (att-value 'search-clause n (ast-child 'name prop) 'Clause)]
            [value (and found (att-value 'eval-unsafe found))] ; TODO: clean up to not call unsafe rules
            [name (att-value 'ilp-varname n)]
            [comp (and found (ast-child 'comp found))])
         (cond ; Did not work with 'case' :|
           ((eq? comp comp-eq) (list (list "+" value name) (list) (list))) ; eq = 1st
           ((eq? comp comp-min-eq) (list (list) (list "+" value name) (list))) ; min-eq = 2nd
           ((eq? comp comp-max-eq) (list (list) (list) (list "+" value name))) ; max-eq = 3rd
           (else (list (list) (list) (list)))))))) ; not found = three empty lists
   
   (define ilp-build-list
     (lambda (prop var-list comp)
       (if (null? var-list) (list)
           (append (list (ast-child 'name prop) comp) var-list))))
   
   ; (merge-list ((eq1 eq2) (min1 min2) (max1 max2)) ((eqA) (minA) (maxA)) = ((eq1 eq2 eqA) (min1 min2 minA) (max1 max2 maxA))
   (define (merge-list loc1 loc2) (debug loc1) (debug loc2) (map append loc1 loc2)) ; [l]ist [o]f [c]onstraints
   
;   props: (("prop-A" ((eq-c1,eq-c2) (min-c1,min-c2,…) (max-c1))) ("prop-B" (…)))
;   eq-c1: (0 "=" "+" var "+" var2)
   
   (ag-rule
    ilp-binary-vars
    (Root
     (lambda (n)
       (map (lambda (x) (att-value 'ilp-varname x))
            (append (att-value 'every-impl n) (att-value 'every-mode n))))))
   
   ; TODO make bidirectional mapping: {_ - +} -> {_0 _1 _2}
   (define subs (list (list #\- #\_) (list #\+ #\_)))
   (define (ilp-conform-name name)
     (list->string
      (map
       (lambda (c)
         (let ([entry (assq c subs)])
           (if entry (cadr entry) c)))
       (string->list name))))
   
   (ag-rule
    ilp-varname
    (Property (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
    (Impl (lambda (n) (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name n))))))
    (Mode (lambda (n) (ilp-conform-name
                       (string-append (att-value 'ilp-varname (att-value 'get-impl n))
                                      "#" (symbol->string (ast-child 'name n)))))))

   (ag-rule
    ilp-varname-deployed
    (Impl (lambda (n pe) (ilp-conform-name
                          (string-append "b#" (symbol->string (ast-child 'name n))
                                         "#" (symbol->string (ast-child 'name pe)))))))
   
   (ag-rule
    ilp-propname
    (Clause
     (lambda (n)
       (let ([pp (ast-parent (ast-parent n))]
             [rpname (symbol->string (ast-child 'name (ast-child 'returntype n)))])
         (ilp-conform-name (if (ast-subtype? pp 'Resource)
                               (string-append (symbol->string (ast-child 'name pp)) "#" rpname) ; Resource
                               (string-append (symbol->string (ast-child 'name (att-value 'get-impl pp)))
                                              "#" rpname))))))) ; Mode
           
   (ag-rule every-pe (Root (lambda (n) (att-value 'res* (ast-child 'HWRoot n)))))
   
   (ag-rule
    res*
    (HWRoot
     (lambda (n)
       (fold-left
        (lambda (result res) (append (att-value 'res* res) result))
        (list)
        (ast-children (ast-child 'Resource* n)))))
    (Resource
     (lambda (n)
       (fold-left
        (lambda (result sub) (append (att-value 'res* sub) result))
        (list n)
        (ast-children (ast-child 'SubResources n))))))

   (ag-rule every-mode (Root (lambda (n) (att-value 'sw* (ast-child 'SWRoot n) 0))))
   (ag-rule every-impl (Root (lambda (n) (att-value 'sw* (ast-child 'SWRoot n) 1))))
   
   ; Search through AST and find either Impls (what = 0) or Modes (what = 1)
   (ag-rule
    sw*
    (SWRoot
     (lambda (n what)
       (fold-left
        (lambda (result comp) (append (att-value 'sw* comp what) result))
        (list)
        (ast-children (ast-child 'Comp* n)))))
    (Comp
     (lambda (n what)
       (fold-left
        (lambda (result impl) (append (att-value 'sw* impl what) result))
        (fold-left
          (lambda (result reqC) (append (att-value 'sw* reqC what) result))
          (list)
          (ast-children (ast-child 'ReqComps n)))
        (ast-children (ast-child 'Impl* n)))))
    (Impl
     (lambda (n what)
       (case what
         [(0) (ast-children (ast-child 'Mode* (ast-child 'Contract n)))]
         [(1) (list n)]))))


   ;; Misc functions
   
   (define (debug . args)
     (letrec
         ([D
           (lambda (loa) ; [l]ist [o]f [a]rgs
             (cond
               ((= (length loa) 0) "\n") ;no arguments given
               ((null? (car loa)) "\n") ;end of recursion
               (else ;recure with cdr
                (let ([s (car loa)])
                  (string-append
                   (cond
                     ((string? s) s)
                     ((symbol? s) (symbol->string s))
                     ((number? s) (number->string s))
                     ((list? s) (string-append "(" (D s) ")"))
                     ((procedure? s) "proc")
                     (else "?"))
                   (D (cdr loa)))))))])
       (when debugging (display (D args)))))
   
   (compile-ag-specifications)
   
   ;; Concrete AST
   (let*
       ([make-simple-prop ; kind=runtime, direction=decreasing
         (lambda (name unit) (create-ast 'Property (list name unit 'runtime 'decreasing)))]
        [load (make-simple-prop 'server-load '%)]
        [energy (make-simple-prop pn-energy 'Joule)]
        [rt-C1 (make-simple-prop 'response-time-C1 'ms)]
        [rt-C2 (make-simple-prop 'response-time-C2 'ms)]
        [Cubieboard (create-ast 'ResourceType (list 'Cubieboard))]
        [make-cubie
         (lambda (name f-load)
           (create-ast
            'Resource
            (list name Cubieboard ;type
                  (create-ast-list (list)) ;Subresources
                  (create-ast-list (list (create-ast 'ProvClause (list load comp-eq f-load)))))))] ;ProvClause*
        [cubie1 (make-cubie 'Cubie1 (lambda (lomp) 0.7))]
        [cubie2 (make-cubie 'Cubie2 (lambda (lomp) 0.4))]
        [make-mp-size (lambda (value) (create-ast 'MetaParameter (list 'size value)))]
        [make-simple-mode
         (lambda (req-f other-reqs prov-e-f rt prov-rt-f mode-name)
           (create-ast
            'Mode
            (list
             mode-name
             (create-ast-list ;Clause*
              (append
               other-reqs
               (list
                (create-ast 'ReqClause (list load comp-max-eq req-f Cubieboard))
                (create-ast 'ProvClause (list energy comp-eq prov-e-f))
                (create-ast 'ProvClause (list rt comp-eq prov-rt-f))))))))]
        [make-simple-contract (lambda (mode) (create-ast 'Contract (list (create-ast-list (list mode)))))]
        [part-impl2a
         (let
             [(mode2a
               (make-simple-mode
                (lambda (lomp) ;static value of 0.5 for prop-load
                  0.5)
                (list) ; other-reqs
                (lambda (lomp) ;dynamic value for energy
                  (let
                      ([mp-size (att-value 'value-of lomp 'size)]
;                       [deployed-kind (ast-child 'type (ast-child 'deployedon impl2a))]
                       )
;                    (if (eq? deployed-kind Cubieboard)
                        (* 3 (log mp-size))
;                        (* 1.5 mp-size))
                    ))
                rt-C2 (lambda (lomp) 0.5) ;always return 0.5 for response-time
                'dynamic-mode-2a))] ;name of Mode
           (create-ast
            'Impl (list 'Part-Impl2a (make-simple-contract mode2a)
                        cubie1 ;deployedon
                        mode2a)))] ;selectedmode
        [comp2
         (create-ast
          'Comp
          (list
           'Depth2-Component
           (create-ast-list (list part-impl2a)) ;Impl*
           (create-ast-list (list)) ;ReqComps
           part-impl2a ;selectedimpl of Depth2-Component
           ))]
        [sample-impl1a
         (let
             [(mode1a (make-simple-mode
                     (lambda (lomp) 0.5) ;always return 0.5 for prop-load
                     (list
                      (create-ast
                       'ReqClause
                       (list rt-C2 comp-max-eq (lambda (lomp) (att-value 'value-of lomp 'size)) comp2)))
                     (lambda (lomp) 20) ;always return 20 for energy
                     rt-C1 (lambda (lomp) 0.2) ;always return 0.2 for response-time
                     'static-mode-1a))] ;name of Mode
           (create-ast
            'Impl
            (list 'Sample-Impl1a (make-simple-contract mode1a)
             cubie1 ;deployedon
             mode1a)))] ;selectedmode
        [sample-impl1b
         (create-ast
          'Impl ; impl-1b is not deployed, default selected mode
          (list 'The-Sample-Impl1b
                (make-simple-contract
                 (make-simple-mode
                  (lambda (lomp) ;dynamic value for prop-load
                    (let
                        ([mp-size (att-value 'value-of lomp 'size)])
                      (if (>= mp-size 100)
                          0.2
                          0.8)))
                  (list)
                  (lambda (lomp) ;dynamic value for energy
                    (let
                        ([mp-size (att-value 'value-of lomp 'size)]
;                       [deployed-kind (ast-child 'type (ast-child 'deployedon impl1b))]
                         )
;                    (if (eq? deployed-kind Cubieboard)
                      (* 10 (log mp-size))
;                        (* 2 mp-size))
                      ))
                  rt-C1 (lambda (lomp) 0.4) ;always return 0.4 for response-time
                  'dynamic-mode-1b)) #f #f))]
        [comp1
         (create-ast
          'Comp
          (list
           'Example-Component ;name of Comp
           (create-ast-list (list sample-impl1a sample-impl1b)) ;Impl*
           (create-ast-list (list comp2)) ;ReqComps
           sample-impl1a ;selectedimpl of Example-Component
           ))])
     (create-ast
      'Root
      (list
       (create-ast 'HWRoot (list
                            (create-ast-list (list Cubieboard))
                            (create-ast-list (list cubie1 cubie2))))
       (create-ast 'SWRoot (list (create-ast-list (list comp1))))
       (create-ast-list (list load energy rt-C1 rt-C2)) ;Property*
       (create-ast
        'Request
        (list
         (create-ast-list (list (make-mp-size 50))) ;MetaParameter*
         (create-ast-list (list (create-ast 'ReqClause (list rt-C1 comp-max-eq (lambda (n) 0.3) comp1))))
         #f))))))) ;default objective

;;; Misc and UI ;;;

(define comp1 (ast-child 1 (ast-child 'Comp* (ast-child 'SWRoot ast))))
(define impl1a (ast-child 1 (ast-child 'Impl* comp1)))
(define impl1b (ast-child 2 (ast-child 'Impl* comp1)))
(define comp2 (ast-child 1 (ast-child 'ReqComps comp1)))
(define impl2a (ast-child 1 (ast-child 'Impl* comp2)))
(define cb1 (ast-child 1 (ast-child 'Resource* (ast-child 'HWRoot ast))))
(define cb2 (ast-child 2 (ast-child 'Resource* (ast-child 'HWRoot ast))))

;; Change impls, selecting and/or mapping

; Given a component (or an impl) and a resource, change deployed-on of the selected impl
; of the given component (or the given impl) to the given resource, returning the old resource
(define deploy-on
  (lambda (x new-pe)
    (rewrite-terminal 'deployedon (if (ast-subtype? x 'Comp) (ast-child 'selectedimpl x) x) new-pe)))

(define use-next-impl
  (lambda (comp)
    (let*
        ([former-impl (ast-child 'selectedimpl comp)]
         [former-index (ast-child-index former-impl)]
         [num-impls (ast-num-children (ast-child 'Impl* comp))]
         [former-deployed (ast-child 'deployedon former-impl)]
         [new-index (+ (mod former-index num-impls) 1)]
         [new-impl (ast-sibling new-index former-impl)]
         [first-new-mode (car (ast-children (ast-child 'Mode* (ast-child 'Contract new-impl))))])
      (rewrite-terminal 'deployedon former-impl #f)
      (rewrite-terminal 'selectedmode former-impl #f)
      (rewrite-terminal 'selectedimpl comp new-impl)
      (rewrite-terminal 'deployedon new-impl former-deployed)
      (rewrite-terminal 'selectedmode new-impl first-new-mode) ; use first mode
      new-impl)))

;; Display (parts of) AST

(define (display-part node)
  (define (print name) (cons name (lambda (v) v)))
  (define printer (list (print 'eval)))
  (print-ast node printer (current-output-port)))

(define display-ast (lambda () (display-part ast)))

(define comp->string
  (lambda (comp)
    (let ([entry (assq comp comp-names)])
      (if entry (cadr entry) '?~))))

(define clauses-to-list
  (lambda (loc)
    (fold-left
     (lambda (result clause)
       (let
           ([returnType (ast-child 'name (ast-child 'returntype clause))]
            [evalValue (att-value 'eval clause)]
            [compName (comp->string (ast-child 'comp clause))])
         (cons
          (if (ast-subtype? clause 'ProvClause)
              (list returnType compName evalValue)
              (list
               returnType
               'on
               (ast-child 'name (ast-child 'target clause))
               evalValue
               compName
               (att-value 'actual-value clause)))
          result)))
     (list) loc)))

; [Debugging] returns a list of the components, implementations and modes
; Form: (compI ((implI1 deployedon-I1 (mode-to-use-I1 ((propName min|max actual-value) ... ))) ...) ...)
(define cim
  (letrec
      ([C
        (lambda (comp)
          (list
           (ast-child 'name comp)
           (I (ast-children (ast-child 'Impl* comp)))
           (fold-left
            (lambda (result reqComp)
              (cons (C reqComp) result))
            (list)
            (ast-children (ast-child 'ReqComps comp)))))]
       [M
        (lambda (mode)
          (list
           (ast-child 'name mode)
           (clauses-to-list
            (ast-children (ast-child 'Clause* mode)))))]
       [I
        (lambda (loi) ; [l]ist [o]f [i]mpls
          (if (null? loi)
              (list)
              (let*
                  ([impl (car loi)]
                   [name (ast-child 'name impl)])
                (cons
                 (list
                  (if (att-value 'is-selected? (car loi))
                      (string-append "*" (symbol->string name))
                      name)
                  (if (att-value 'is-deployed? impl)
                      (ast-child 'name (ast-child 'deployedon impl))
                      #f)
                  (if (att-value 'is-selected? impl)
                      (M (att-value 'mode-to-use impl))
                      #f))
                 (I (cdr loi))))))])
    (lambda ()
      (fold-left
       (lambda (result comp) (cons (C comp) result))
       (list)
       (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast)))))))

; [Debugging] Returns a list of hardware resources along with their provided properties
; Form: (res1-type res1-name ((provClause1a-name -comp->string -actualValue) ... (res1-subresources ... )) ... )
(define hw
  (lambda ()
    (letrec
        ([R
          (lambda (lor) ; [l]ist [o]f [r]esources
            (if (null? lor)
                (list)
                (let
                    ([subs (R (ast-children (ast-child 'SubResources (car lor))))]
                     [rest (R (cdr lor))])
                  (cons
                   (list
                    (ast-child 'name (ast-child 'type (car lor))) ; resource type name
                    (ast-child 'name (car lor)) ; resource name
                    (clauses-to-list
                     (ast-children (ast-child 'ProvClause* (car lor))))) ; list of clauses
                   (if (null? subs)
                       rest
                       (cons subs rest))))))])
      (R (ast-children (ast-child 'Resource* (ast-child 'HWRoot ast)))))))

; [Debugging] Returns a list of the request
; Form: (((metaparam1-name -value) ... ) ((constraint1-name -comp->string -requiredValue) ... ) objective)
(define req
  (lambda ()
    (letrec
        ([r (att-value 'get-request ast)]
         [MP
          (lambda (lomp) ; [l]ist [o]f [m]eta[p]arameter
            (if (null? lomp)
                (list)
                (cons
                 (list
                  (ast-child 'name (car lomp))
                  (ast-child 'value (car lomp)))
                 (MP (cdr lomp)))))])
      (list
       (MP (ast-children (ast-child 'MetaParameter* r))) ; metaparams
       (clauses-to-list (ast-children (ast-child 'Constraints r))) ; constraints
       (ast-child 'name (ast-child 'objective r)))))) ; objective

;; Shortcuts

(define clauses-met? (lambda () (att-value 'clauses-met? ast)))
(define obj (lambda () (att-value 'objective-value ast)))
(define comp1-next-impl (lambda () (use-next-impl comp1)))

;; Text save
(define (print-per-line x)
  (cond
    ((null? x) (newline))
    ((list? x)
     (if (list? (car x))
         (begin
           (print-per-line (car x))
           ;(newline)
           (print-per-line (cdr x)))
         (begin
           (print-per-line (car x))
           (print-per-line (cdr x)))))
    (else (display x) (display " "))))

(define (save-to-file path values)
  (if (file-exists? path) (delete-file path))
  (with-output-to-file path
    (lambda () (for-each print-per-line values) (newline))))

(define (save-ilp path)
  (save-to-file path (att-value 'to-ilp ast)))