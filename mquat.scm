#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define debugging #t)
(define comp-names (list)) (define rev-comp-names (list))
(define pn-energy 'energy-consumption) ; Property-Name of energy-consumption used for all properties and the default objective function

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-selectedimpl-Property*)
   (ast-rule 'Impl->name-Mode*-reqcomps-deployedon-selectedmode)
   (ast-rule 'Mode->name-Clause*)
   ;value is a lambda expecting an AST-List-Node with MetaParameters, returning the value
   ;comp is a lambda expecting two values, required and actual, returning #t or #f
   (ast-rule 'Clause->returntype-comp-value)
   ; target is either a Comp or a ResourceType
   (ast-rule 'ReqClause:Clause->target)
   (ast-rule 'ProvClause:Clause->)
   (ast-rule 'HWRoot->ResourceType*-Resource*)
   (ast-rule 'ResourceType->name-Property*)
   ; type is a ResourceType
   (ast-rule 'Resource->name-type-Resource*<SubResources-ProvClause*)
   (ast-rule 'Request->MetaParameter*-target-ReqClause*<Constraints-objective)
   (ast-rule 'MetaParameter->name-value)
   ; kind=static|runtime|derived. direction=decreasing|increasing. agg = sum|max.
   (ast-rule 'Property->name-unit-kind-direction-agg)
   
   (define comp-min-eq (lambda (req act) (<= req act)))
   (define comp-max-eq (lambda (req act) (>= req act)))
   (define comp-eq (lambda (req act) (= req act)))
   (define f-comp-max-diff-eq (lambda (diff) (lambda (req act) (<= (- req act) diff))))
   
   (set! comp-names
         (list
          (list comp-eq '=)         ; property =  forumla
          (list comp-min-eq '<=)    ; property <= formula
          (list comp-max-eq '>=)))  ; property >= formula
   (set! rev-comp-names
         (list
          (list comp-eq '=)         ; forumla =  property
          (list comp-min-eq '>=)    ; forumla <= property
          (list comp-max-eq '<=)))  ; forumla >= property
   
   (define agg-max 1) (define agg-sum 2)
   
   (compile-ast-specifications 'Root)
   
   ;; AG rules
   (ag-rule
    req-comp-map
    (Comp ; { (requiredComponent { impl-requiring-this-component ... }) ... }
     (lambda (n)
       (debug "c: " (ast-child 'name n))
       (fold-left
        (lambda (result-for-comp impl)
          (debug "out: impl=" (ast-child 'name impl) ",result=" result-for-comp)
          (fold-left (lambda (result-for-impl req) (debug "inner: impl=" (ast-child 'name impl) ",req=" (ast-child 'name req)) (add-to-al result-for-impl req impl))
                     result-for-comp (att-value 'req-comp-map impl))) ;fold over reqs of impl
        (list) (ast-children (ast-child 'Impl* n))))) ;fold over impls
    (Impl
     (lambda (n)
       (ast-child 'reqcomps n))))
   
   ; Either cons val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
   (define (add-to-al al key val) (add-to-al0 al key val cons))

   ; Either op val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
   (define (add-to-al0 al key val op)
     (if (null? al) (list (list key (op val (list)))) ; make new entry
         (let ([entry (car al)])
;           (debug entry)
;           (debug (cadr entry))
           (if (eq? (car entry) key)
               (cons (list key (op val (cadr entry))) (cdr al)) ; add to entry and return
               (cons entry (add-to-al0 (cdr al) key val op)))))) ; recur
   
   (ag-rule
    req-comp-min
    (Comp
     (lambda (n)
       (fold-left
        (lambda (result impl) (intersect-b #f result (ast-child 'reqcomps impl)))
        #f (ast-children (ast-child 'Impl* n))))))
   
   (define intersect-b
     (lambda (start set1 set2)
       (debug "set1=" set1 ",set2=" set2)
       (letrec([I (lambda (set2)
                    (cond ((null? set2) set2)
                          ((memq (car set2) set1) (cons (car set2) (I (cdr set2))))
                          (else (I (cdr set2)))))])
         (if (eq? start set1) set2 (I set2)))))
    
   (ag-rule
    req-comp-all
    (Comp
     (lambda (n)
       (fold-left
        (lambda (result impl)
          (union result (ast-child 'reqcomps impl)))
        (list) (ast-children (ast-child 'Impl* n))))))
   
   (define union
     (lambda (set1 set2)
       (letrec ([U (lambda (set2)
                     (cond ((null? set2) set1)
                           ((memq (car set2) set1) (U (cdr set2)))
                           (else (cons (car set2) (U (cdr set2))))))])
         (U set2))))
   
   (ag-rule
    objective-value
    (Root ; sum of objective value of all software components (skipping SWRoot)
     (lambda (n)
       (fold-left
        (lambda (totalValue comp) (+ totalValue (att-value 'objective-value comp)))
        0 (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp ; objective value of selected impl
     (lambda (n)
       (att-value 'objective-value (ast-child 'selectedimpl n))))
    (Impl ; objective value of the mode to use
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
    (Comp ; clauses-met for the selected impl
     (lambda (n)
       (att-value 'clauses-met? (ast-child 'selectedimpl n))))
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
       (or (ast-child 'selectedmode n) (ast-child 1 (ast-child 'Mode* n))))))
   
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
                 (string-append " req" (number->string (ast-child-index c)) ":")
                 (att-value 'ilp-varname (ast-child 'returntype c))
                 (comp->rev-string (ast-child 'comp c))
                 (att-value 'eval c))
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
       (let
           ([result (fold-left
                     (lambda (result impl)
                       (cons
                        (cons* "+" (att-value 'ilp-varname impl) (car result))
                        (append (att-value 'to-ilp impl) (cdr result))))
                     (list (list "=" "1") (list))
                     (ast-children (ast-child 'Impl* n)))])
         (if (att-value 'request-target? n) result (cdr result)))))
    (Impl
     (lambda (n)
       (debug "Impl:" (ast-child 'name n))
       (cons
        (fold-left ; deploy the same mode only on one pe
         (lambda (result pe)
           (append (fold-left (lambda (inner mode) (cons* "+" (att-value 'ilp-varname-deployed mode pe) inner))
                              (list)
                              (ast-children (ast-child 'Mode* n)))
                   result))
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
         (ast-child 'reqcomps n)))))
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
   
   (ag-rule request-target? (Comp (lambda (n) (eq? (ast-child 'target (att-value 'get-request n)) n))))
   (define prepend-sign (lambda (val) (if (< val 0) val (string-append "+ " (number->string val))))) ;TODO check if space need between +/- and value
   (define prepend-neg-sign (lambda (val) (if (> val 0) (- val) (string-append "+ " (number->string (- val)))))) ;TODO check if space need between +/- and value
   
   (ag-rule
    ilp-objective
    (Root
     (lambda (n)
       (fold-left
        (lambda (result mode)
          (cons* (prepend-sign (att-value 'ilp-objective mode)) (att-value 'ilp-varname mode) result))
        (list)
        (att-value 'every-mode n))))
    (Mode (lambda (n) (att-value 'actual-value (att-value 'provided-clause n pn-energy)))))
   
   ; Creates a list of NFP-negotiation constraints
   (ag-rule
    ilp-nego
    (Root
     (lambda (n)
       (remove (list) ; remove empty constraints
               (fold-left
                (lambda (result prop) (append (att-value 'ilp-nego prop) result))
                (list)
                (ast-children (ast-child 'Property* n)))))) ;TODO -- wip: implement ag-rule 'all-properties
    (Property
     (lambda (n)
       (let*
           ([pname (att-value 'ilp-varname n)]
            [append-if-constrained (lambda (comp loc) (debug loc) (if (null? loc) (list) (append loc (list "-" pname comp 0))))])
         (map append-if-constrained (assq-values comp-names) ; add "-" property name, ("=", ">=" or "<=" resp.) and "0"
              (fold-left
               (lambda (result mode) (merge-list (att-value 'ilp-nego mode n) result))
               (list (list) (list) (list))
               (att-value 'every-mode n)))))) ;TODO: fold stepwise over components, impls, modes
    (Mode
     (lambda (n prop)
       (debug "ilp-nego:" (ast-child 'name n) ",prop:" (ast-child 'name prop))
       (let*
           ([found (att-value 'search-clause n (ast-child 'name prop) 'Clause)]
            [value (and found (att-value 'eval-unsafe found))] ; TODO: clean up to not call unsafe rules
            [name (att-value 'ilp-varname n)]
            [comp (and found (ast-child 'comp found))])
         (cond ; Did not work with 'case' :|
           ((eq? comp comp-eq) (list (list (prepend-sign value) name) (list) (list))) ; eq = 1st
           ((eq? comp comp-min-eq) (list (list) (list (prepend-sign value) name) (list))) ; min-eq = 2nd
           ((eq? comp comp-max-eq) (list (list) (list) (list (prepend-sign value) name))) ; max-eq = 3rd
           (else (list (list) (list) (list)))))))) ; not found = three empty lists
   
   (define (assq-values loe)
     (if (null? loe) (list) (cons (cadar loe) (assq-values (cdr loe)))))
   
   ; (merge-list ((eq1 eq2) (min1 min2) (max1 max2)) ((eqA) (minA) (maxA)) = ((eq1 eq2 eqA) (min1 min2 minA) (max1 max2 maxA))
   (define (merge-list loc1 loc2) (debug loc1) (debug loc2) (map append loc1 loc2)) ; [l]ist [o]f [c]onstraints
   
   (ag-rule
    ilp-nego-sw
    (Comp
     (lambda (n prop)
       (fold-left ; fold over req-comp-map
        (lambda (result entry)
          (append
           (fold-left ; fold over Impl* results in "left-hand-side"
            (lambda (constraint impl)
              (cons* (prepend-neg-sign (att-value 'eval-unsafe (att-value 'get-provided-clause impl prop)))
                     (att-value 'ilp-varname impl) constraint))
            ("0" ">=") ; always ">=" ?
            (ast-children (ast-child 'Impl* n)))
           
           )))
        (list)
        (att-value 'req-comp-map n))))
   
   (ag-rule
    ilp-nego-reqc
    (Comp ; return a list of (prop ((prop-value impl-name) ... ))-pairs for each ProvClause in each mode of each impl
     (lambda (n)
       (fold-left
        (lambda (result impl)
          (merge-al result (att-value 'ilp-nego-reqc impl)))
        (list)
        (ast-children (ast-child 'Impl* n)))))
    (Impl ; return a list of (prop ((prop-value impl-name) ... ))-pairs for each ProvClause in each mode
     (lambda (n)
       (fold-left
        (lambda (result mode)
          (merge-al result (att-value 'ilp-nego-reqc mode)))
        (list) (ast-children (ast-child 'Mode* n)))))
    (Mode ; return a list of (prop ((prop-value impl-name) ... ))-pairs for each ProvClause
     (lambda (n)
       (fold-left
        (lambda (result clause)
          (if (ast-subtype? clause 'ProvClause)
              (add-to-al result (ast-child 'returntype clause) (list (att-value 'eval-unsafe clause) (att-value 'ilp-varname n)))
              result))
        (list) (ast-children (ast-child 'Clause* n))))))
   
   (define (merge-al al1 al2) (fold-left (lambda (big-al entry) (add-to-al0 big-al (car entry) (cadr entry) append-d)) al1 al2))
   (define (append-d a b) (debug a) (debug b) (append a b))
   
   (ag-rule
    ilp-binary-vars
    (Root
     (lambda (n)
       (append
        (map (lambda (impl) (att-value 'ilp-varname impl)) (att-value 'every-impl n))
        (fold-left (lambda (result mode) (append (map (lambda (pe) (att-value 'ilp-varname-deployed mode pe))
                                                      (att-value 'every-pe n)) result))
                   (list) (att-value 'every-mode n))))))
   
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
    (Impl (lambda (n) (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name n)))))) ;TODO prepend name of Comp
    (Mode (lambda (n) (ilp-conform-name
                       (string-append (att-value 'ilp-varname (att-value 'get-impl n))
                                      "#" (symbol->string (ast-child 'name n)))))))

   (ag-rule
    ilp-varname-deployed
;    (Impl (lambda (n pe) (ilp-conform-name
;                          (string-append "b#" (symbol->string (ast-child 'name n))
;                                         "#" (symbol->string (ast-child 'name pe))))))
    (Mode (lambda (n pe) (ilp-conform-name ;TODO: Prepend Comp-Name?
                          (string-append "b#" (symbol->string (ast-child 'name (att-value 'get-impl n)))
                                         "#" (symbol->string (ast-child 'name n))
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
        (list)
        (ast-children (ast-child 'Impl* n)))))
    (Impl
     (lambda (n what)
       (case what
         [(0) (ast-children (ast-child 'Mode* n))]
         [(1) (list n)]))))

   (compile-ag-specifications)
   
   ;; Concrete AST
   (let*
       ([make-simple-prop ; kind=runtime, direction=decreasing
         (lambda (name unit agg) (create-ast 'Property (list name unit 'runtime 'decreasing agg)))]
        [load (make-simple-prop 'server-load '% agg-sum)]
        [freq (make-simple-prop 'cpu-frequency 'Mhz agg-max)] ; TODO add some clauses referencing this
        [energy-c1 (make-simple-prop pn-energy 'Joule agg-sum)]
        [energy-c2 (make-simple-prop pn-energy 'Joule agg-sum)]
        [rt-C1 (make-simple-prop 'response-time-C1 'ms agg-sum)]
        [rt-C2 (make-simple-prop 'response-time-C2 'ms agg-sum)]
        [Cubieboard (create-ast 'ResourceType (list 'Cubieboard (create-ast-list (list load freq))))]
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
         (lambda (req-f other-reqs c-energy prov-e-f rt prov-rt-f mode-name)
           (create-ast
            'Mode (list mode-name
                        (create-ast-list ;Clause*
                         (append other-reqs
                                 (list (create-ast 'ReqClause (list load comp-max-eq req-f Cubieboard))
                                       (create-ast 'ProvClause (list c-energy comp-eq prov-e-f))
                                       (create-ast 'ProvClause (list rt comp-eq prov-rt-f))))))))]
        [part-impl2a
         (let
             [(mode2a
               (make-simple-mode
                (lambda (lomp) ;static value of 0.5 for prop-load
                  0.5)
                (list) ; other-reqs
                energy-c2
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
            'Impl (list 'Part-Impl2a (create-ast-list (list mode2a))
                        (list) ;reqcomps
                        cubie1 ;deployedon
                        mode2a)))] ;selectedmode
        [comp2
         (create-ast
          'Comp
          (list
           'Depth2-Component
           (create-ast-list (list part-impl2a)) ;Impl*
           part-impl2a (create-ast-list (list rt-C2 energy-c2))))] ;selectedimpl and Property*
        [c1-impl1a
         (let
             [(mode1a (make-simple-mode
                     (lambda (lomp) 0.5) ;always return 0.5 for prop-load
                     (list
                      (create-ast
                       'ReqClause
                       (list rt-C2 comp-max-eq (lambda (lomp) (att-value 'value-of lomp 'size)) comp2)))
                     energy-c1
                     (lambda (lomp) 20) ;always return 20 for energy
                     rt-C1 (lambda (lomp) 0.2) ;always return 0.2 for response-time
                     'static-mode-1a))] ;name of Mode
           (create-ast
            'Impl
            (list 'Sample-Impl1a (create-ast-list (list mode1a))
                  (list comp2) ;reqcomps
                  cubie1 ;deployedon
                  mode1a)))] ;selectedmode
        [c1-impl1b
         (create-ast
          'Impl ; impl-1b is not deployed, default selected mode
          (list 'The-Sample-Impl1b
                (create-ast-list
                 (list
                  (make-simple-mode
                   (lambda (lomp) ;dynamic value for prop-load
                     (let ([mp-size (att-value 'value-of lomp 'size)])
                       (if (>= mp-size 100) 0.2 0.8)))
                   (list)
                   energy-c1
                   (lambda (lomp) ;dynamic value for energy
                     (let ([mp-size (att-value 'value-of lomp 'size)])
;                           [deployed-kind (ast-child 'type (ast-child 'deployedon impl1b))])
;                       (if (eq? deployed-kind Cubieboard)
                       (* 10 (log mp-size))))
;                         (* 2 mp-size))))
                   rt-C1 (lambda (lomp) 0.4) ;always return 0.4 for response-time
                   'dynamic-mode-1b)))
                (list) ;reqcomps
                #f #f))] ;deployedon + selectedmode
        [c1-impl1c
         (create-ast
          'Impl
          (list 'Useless-Impl1c
                (create-ast-list
                 (list
                  (make-simple-mode
                   (lambda (lomp) 0) ;propload
                   (list (create-ast 'ReqClause (list rt-C2 comp-max-eq (lambda (lomp) -1) comp2)))
                   energy-c1
                   (lambda (lomp) 100) ;energy
                   rt-C1 (lambda (lomp) 0.2) ;response-time
                   'default-mode-1c)))
                (list comp2) #f #f))]
        [comp1
         (create-ast
          'Comp
          (list
           'Example-Component ;name of Comp
           (create-ast-list (list c1-impl1a c1-impl1b c1-impl1c)) ;Impl*
           c1-impl1a (create-ast-list (list rt-C1 energy-c1))))]) ;selectedimpl Property*
     (create-ast
      'Root
      (list
       (create-ast 'HWRoot (list
                            (create-ast-list (list Cubieboard))
                            (create-ast-list (list cubie1 cubie2))))
       (create-ast 'SWRoot (list (create-ast-list (list comp1 comp2))))
       (create-ast
        'Request
        (list
         (create-ast-list (list (make-mp-size 50))) ;MetaParameter*
         comp1
         (create-ast-list (list (create-ast 'ReqClause (list rt-C1 comp-max-eq (lambda (n) 0.3) comp1))))
         #f))))))) ;default objective

;;; Misc and UI ;;;
   
(define (debug . args)
  (letrec
      ([D (lambda (loa) ; [l]ist [o]f [a]rgs
            (cond
              ((= (length loa) 0) "") ;no arguments given
              ((null? (car loa)) "") ;end of recursion
              (else ;recure with cdr
               (let ([s (car loa)])
                 (string-append
                  (cond
                    ((string? s) s)
                    ((symbol? s) (symbol->string s))
                    ((number? s) (number->string s))
                    ((list? s) (string-append "(" (D s) ")"))
                    ((procedure? s) "<proc>")
                    ((ast-node? s) (if (ast-has-child? 'name s) (symbol->string (ast-child 'name s)) "<node>"))
                    (else "?")) " "
                  (D (cdr loa)))))))])
    (when debugging (display (D args)) (display "\n"))))

(define (print . args)
  (let* ([old-d debugging])
    (set! debugging #t)
    (debug args)
    (set! debugging old-d)))

(define comp1 (ast-child 1 (ast-child 'Comp* (ast-child 'SWRoot ast))))
(define impl1a (ast-child 1 (ast-child 'Impl* comp1)))
(define impl1b (ast-child 2 (ast-child 'Impl* comp1)))
(define impl1c (ast-child 3 (ast-child 'Impl* comp1)))
(define comp2 (car (ast-child 'reqcomps impl1a)))
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
    (let* ([former-impl (ast-child 'selectedimpl comp)]
           [former-index (ast-child-index former-impl)]
           [num-impls (ast-num-children (ast-child 'Impl* comp))]
           [former-deployed (ast-child 'deployedon former-impl)]
           [new-index (+ (mod former-index num-impls) 1)]
           [new-impl (ast-sibling new-index former-impl)]
           [first-new-mode (car (ast-children (ast-child 'Mode* new-impl)))])
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
(define comp->string (lambda (comp) (let ([entry (assq comp comp-names)]) (if entry (cadr entry) '?~))))
(define comp->rev-string (lambda (comp) (let ([entry (assq comp rev-comp-names)]) (if entry (cadr entry) '?~))))

(define clauses-to-list
  (lambda (loc)
    (fold-left
     (lambda (result clause)
       (let ([returnType (ast-child 'name (ast-child 'returntype clause))]
             [evalValue (att-value 'eval clause)]
             [compName (comp->string (ast-child 'comp clause))])
         (cons
          (if (ast-subtype? clause 'ProvClause) (list returnType compName evalValue)
              (list returnType 'on (ast-child 'name (ast-child 'target clause))
                    evalValue compName (att-value 'actual-value clause)))
          result)))
     (list) loc)))

; [Debugging] returns a list of the components, implementations and modes
; Form: (compI ((implI1 deployedon-I1 (mode-to-use-I1 ((propName min|max actual-value) ... ))) ...) ...)
(define (cim)
  (letrec
      ([C (lambda (comp)
            (list (ast-child 'name comp) (I (ast-children (ast-child 'Impl* comp)))))]
       [M (lambda (mode)
            (list
             (ast-child 'name mode)
             (clauses-to-list (ast-children (ast-child 'Clause* mode)))))]
       [I (lambda (loi) ; [l]ist [o]f [i]mpls
            (if (null? loi) (list)
                (let* ([impl (car loi)]
                       [name (ast-child 'name impl)])
                  (cons
                   (list
                    (map (lambda (c) (ast-child 'name c)) (ast-child 'reqcomps impl))
                    (if (att-value 'is-selected? impl) (string-append "*" (symbol->string name)) name)
                    (if (att-value 'is-deployed? impl) (ast-child 'name (ast-child 'deployedon impl)) #f)
                    (if (att-value 'is-selected? impl) (M (att-value 'mode-to-use impl)) #f))
                   (I (cdr loi))))))])
    (fold-left
     (lambda (result comp) (cons (C comp) result))
     (list)
     (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast))))))

; [Debugging] Returns a list of hardware resources along with their provided properties
; Form: (res1-type res1-name ((provClause1a-name -comp->string -actualValue) ... (res1-subresources ... )) ... )
(define (hw)
  (letrec
      ([R (lambda (lor) ; [l]ist [o]f [r]esources
            (if (null? lor) (list)
                (let ([subs (R (ast-children (ast-child 'SubResources (car lor))))]
                      [rest (R (cdr lor))])
                  (cons
                   (list
                    (ast-child 'name (ast-child 'type (car lor))) ; resource type name
                    (ast-child 'name (car lor)) ; resource name
                    (clauses-to-list (ast-children (ast-child 'ProvClause* (car lor))))) ; list of clauses
                   (if (null? subs) rest (cons subs rest))))))])
    (R (ast-children (ast-child 'Resource* (ast-child 'HWRoot ast))))))

; [Debugging] Returns a list of the request
; Form: (((metaparam1-name -value) ... ) ((constraint1-name -comp->string -requiredValue) ... ) objective)
(define (req)
  (letrec
      ([MP
        (lambda (lomp) ; [l]ist [o]f [m]eta[p]arameter
          (if (null? lomp) (list)
              (cons
               (list
                (ast-child 'name (car lomp))
                (ast-child 'value (car lomp)))
               (MP (cdr lomp)))))])
    (let* ([r (att-value 'get-request ast)]
           [o (ast-child 'objective r)])
      (list
       (MP (ast-children (ast-child 'MetaParameter* r))) ; metaparams
       (clauses-to-list (ast-children (ast-child 'Constraints r))) ; constraints
       (if o (ast-child 'name o) "default"))))) ; objective

;; Shortcuts

(define (clauses-met?) (att-value 'clauses-met? ast))
(define (obj) (att-value 'objective-value ast))
(define (comp1-next-impl) (use-next-impl comp1))

;; Text save
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

(define (save-to-file path values)
  (if (file-exists? path) (delete-file path))
  (with-output-to-file path
    (lambda () (for-each (lambda (x) (print-per-line x #t)) values) (newline))))

(define (save-ilp path) (save-to-file path (att-value 'to-ilp ast)))
(define (make) (save-ilp "gen/ilp.txt"))
