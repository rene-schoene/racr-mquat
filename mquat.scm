#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define debugging #t)
(define comp-names (list))
(define pn-energy 'energy-consumption) ; Name of the property energy-consumption and as default objective function property name

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-selectedimpl-Property*)
   (ast-rule 'Impl->name-Mode*-reqcomps-deployedon-selectedmode)
   (ast-rule 'Mode->name-Clause*)
   ;value is a lambda expecting two values, an AST-List-Node with MetaParameters and the target resource, returning the value
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
   
   ; 1st = op: property op forumla
   ; 2nd = rev-op: formula rev-op property
   ; 3rd = name as string
   (set! comp-names (list (list comp-eq '= '= "=")
                          (list comp-min-eq '<= '>= "min")
                          (list comp-max-eq '>= '<= "max")))
   
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
    (Impl (lambda (n) (ast-child 'reqcomps n))))
   
   ; Either cons val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
   (define (add-to-al al key val) (add-to-al0 al key val cons))

   ; Either op val to an entry in the [a]ssociate [l]ist, or make a new entry (key (val))
   (define (add-to-al0 al key val op)
     (if (null? al) (list (list key (op val (list)))) ; make new entry
         (let ([entry (car al)])
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
                          ((member (car set2) set1) (cons (car set2) (I (cdr set2))))
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
                           ((member (car set2) set1) (U (cdr set2)))
                           (else (cons (car set2) (U (cdr set2))))))])
         (U set2))))
   
   (ag-rule
    objective-value
    (Root (lambda (n) (att-value 'objective-value (ast-child 'SWRoot n))))
    (SWRoot ; sum of objective value of all software components
     (lambda (n) (fold-left
                  (lambda (totalValue comp) (+ totalValue (att-value 'objective-value comp)))
                  0 (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp (lambda (n) (att-value 'objective-value (ast-child 'selectedimpl n))))
    (Impl (lambda (n) (att-value 'objective-value (att-value 'mode-to-use n))))
    (Mode (lambda (n) (att-value 'eval (att-value 'provided-clause n pn-energy)))))
   
   (ag-rule
    clauses-met?
    (Root ; clauses-met for all software components and for request?
     (lambda (n) (fold-left (lambda (result comp) (and result (att-value 'clauses-met? comp)))
                            (att-value 'objective-value (ast-child 'Request n))
                            (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp (lambda (n) (att-value 'clauses-met? (ast-child 'selectedimpl n))))
    (Impl (lambda (n) (att-value 'clauses-met? (att-value 'mode-to-use n))))
    (Mode ; clauses-met for all clauses?
     (lambda (n) (fold-left (lambda (result clause) (and result (att-value 'clauses-met? clause)))
                            #t (ast-children (ast-child 'Clause* n)))))
    (ReqClause (lambda (n) ((ast-child 'comp n) (att-value 'eval n) (att-value 'actual-value n))))
    (ProvClause (lambda (n) #t)) ; Provision clauses are always fulfilled
    (Request ; clauses-met for all constraints
     (lambda (n) (fold-left (lambda (result clause) (and result (att-value 'clauses-met? clause)))
                            #t (ast-children (ast-child 'Constraints n))))))
   
   (ag-rule
    actual-value
    (ReqClause
     (lambda (n)
       (let ([propName (ast-child 'name (ast-child 'returntype n))]
             [target (ast-child 'target n)])
         ((ast-child
           'value (if (ast-subtype? target 'ResourceType)
                      ; hw → search in deployedon for name and type
                      (att-value 'provided-clause (ast-child 'deployedon  (att-value 'get-impl n)) propName target)
                      ; sw → search in target-component
                      (att-value 'provided-clause (att-value 'mode-to-use (ast-child 'selectedimpl target)) propName)))
          (ast-child 'MetaParameter* (att-value 'get-request n)))))) ; Params from request, applied to the value function
    (ProvClause (lambda (n) (att-value 'eval n))))
;       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n))))))
   
   ; Given the name of a property (and optionally the type of the resource), get ProvisionClause for this property
   (ag-rule
    provided-clause
    (Resource
     ; Search through ProvClauses of this resource and its subresources to find a clause
     ; 1) returning a property with the given name and
     ; 2) having a target-resource with the given type
     (lambda (n name type)
       (let ([search-subresources
              (lambda ()
                (ast-find-child
                 (lambda (index subres) (att-value 'provided-clause subres))
                 (ast-child 'SubResources n)))])
         (if (eq? (ast-child 'type n) type) ; if n has correct type ...
             (let ([found-clause
                    (ast-find-child ; (1) ... then try to find a child in n ...
                     (lambda (index clause) (eq? (ast-child 'name (ast-child 'returntype clause)) name))
                     (ast-child 'ProvClause* n))])
               (if found-clause ; (1.q) if a child was found ...
                   found-clause ; (1.1) ... return it
                   (search-subresources))) ; (1.2) ... else search in subresources
             ; (2) ... if not correct type or no child was found, search subresources of n
             (search-subresources)))))
    ; Search through Clauses to find a ProvClause with matching name
    (Mode (lambda (n name) (att-value 'search-clause n name 'ProvClause))))
   
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
       (att-value 'eval-on n (if (and (ast-subtype? (ast-parent (ast-parent n)) 'Mode)
                                      (att-value 'is-selected? (att-value 'get-impl n)))
                                 ; use the resource deployed-on...
                                 (ast-child 'deployedon (att-value 'get-impl n))
                                 #f))))) ; ... else evaluate it with #f as target
   
   (ag-rule
    eval-on
    (Clause
     (lambda (n target)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n)) target))))
   
   ; Given a list-node n, search for a MetaParameter with the given name. If none found, return the default value
   (define get-val
     (lambda (n name default)
       (letrec ([G (lambda (lomp)
                     (cond
                       ((null? lomp) default)
                       ((eq? (ast-child 'name (car lomp)) name) (ast-child 'value (car lomp)))
                       (else (G (cdr lomp)))))]) (G (ast-children n)))))
   
   ; Given a metaparameter name, return the value of the according metaparameter
   (ag-rule value-of (Request (lambda (n name) (get-val (ast-child 'MetaParameter* n) name #f))))
   
   (ag-rule is-selected? (Impl (lambda (n) (eq? (ast-child 'selectedimpl (ast-parent (ast-parent n))) n))))
   
   (ag-rule is-deployed? (Impl (lambda (n) (ast-node? (ast-child 'deployedon n)))))
   
   ; Return either the selected-mode or the first mode
   (ag-rule mode-to-use (Impl (lambda (n) (or (ast-child 'selectedmode n) (ast-child 1 (ast-child 'Mode* n))))))
   
   ;;; ILP-Creation rules

   ; Return a list of 1 objective, some constraints, some bounds and some generals, everything
   ; packed inside a list, e.g. one constraint is also a list.
   (ag-rule
    to-ilp
    (Root
     (lambda (n)
       (let ([binary-vars (att-value 'ilp-binary-vars n)])
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
    (Request (lambda (n) (att-value 'ilp-nego-sw n)))
    (SWRoot (lambda (n) (recur append n 'to-ilp 'Comp*)))
    (Comp
     (lambda (n)
       (debug "Comp:" (ast-child 'name n))
       (let ([ics (recur cons n 'to-ilp 'Impl*)])
         (cons
          (fold-left
           (lambda (result entry)
             (cons
              (append
               (list (string-append (att-value 'ilp-name n) "_requires_" (att-value 'ilp-name (car entry)) ": "))
               (fold-left (lambda (inner impl) (cons* "-" (att-value 'ilp-binvar impl) inner)) (list) (cadr entry))
               (fold-left (lambda (inner impl) (cons* "+" (att-value 'ilp-binvar impl) inner)) (list)
                          (ast-children (ast-child 'Impl* (car entry))))
               (list "=" 0)) result))
           (list) (att-value 'req-comp-map n))
          (if (att-value 'request-target? n)
              (cons
               (cons (string-append "request_target_" (att-value 'ilp-name n) ": ")
                     (fold-left (lambda (inner impl) (cons* "+" (att-value 'ilp-binvar impl) inner))
                          (list "=" 1) (ast-children (ast-child 'Impl* n))))
               ics) ics))))) ; list of cons, and else-branch
    (Impl
     (lambda (n)
       (debug "Impl:" (ast-child 'name n))
       (cons (string-append "single(" (att-value 'ilp-name n) "): ")
             (fold-left ; deploy one combination of mode and pe for this impl
              (lambda (result pe)
                (append (fold-left (lambda (inner mode) (cons* "+" (att-value 'ilp-binvar-deployed mode pe) inner))
                                   (list) (ast-children (ast-child 'Mode* n))) result))
              (list "-" (att-value 'ilp-binvar n) "=" 0)
              (att-value 'every-pe n)))))
    (HWRoot (lambda (n) (recur append n 'to-ilp 'Resource*)))
    (Resource
     (lambda (n)
       (fold-left
        (lambda (result c)
          (cons (list 0 "<=" (att-value 'ilp-propname c) "<=" (att-value 'eval-on c n)) result))
        (list) (ast-children (ast-child 'ProvClause* n))))))
   
   (ag-rule request-target? (Comp (lambda (n) (eq? (ast-child 'target (att-value 'get-request n)) n))))
   
   (define prepend-sign (lambda (val) (if (< val 0) val (string-append "+ " (number->string val)))))
   
   (ag-rule
    ilp-objective
    (Root
     (lambda (n)
       (fold-left
        (lambda (result mode)
          (cons*
           (fold-left (lambda (inner pe) (cons* (prepend-sign (att-value 'ilp-objective mode pe))
                                                (att-value 'ilp-binvar-deployed mode pe) inner))
            (list) (att-value 'every-pe n))
           result))
        (list) (att-value 'every-mode n))))
    (Mode (lambda (n pe) (att-value 'eval-on (att-value 'provided-clause n pn-energy) pe))))
   
   ; Creates a list of NFP-negotiation constraints
   (ag-rule
    ilp-nego
    (Root (lambda (n) (remove (list) (att-value 'ilp-nego (ast-child 'SWRoot n))))) ; remove empty constraints
    (SWRoot (lambda (n) (recur append n 'ilp-nego 'Comp*)))
    (Comp (lambda (n) (append (att-value 'ilp-nego-sw n)
                              (att-value 'ilp-nego-hw n)))))
   
   (define (assq-values loe) (map cadar loe))
   (define (merge-list loc1 loc2) (map append loc1 loc2)) ; [l]ist [o]f [c]onstraints
   
   (ag-rule
    ilp-nego-sw
    (Comp
     (lambda (n)
       (fold-left ; fold over req-comp-map
        (lambda (result entry)
          (append
           (make-constraints
            (att-value 'ilp-nego-reqc (car entry) 'ProvClause comp-eq) ;provs
            (att-value 'ilp-nego-reqc n 'ReqClause comp-max-eq) ;max-reqs
            (att-value 'ilp-nego-reqc n 'ReqClause comp-min-eq) #f) ;min-reqs,request?
           result)) (list) (att-value 'req-comp-map n))))
    (Request
     (lambda (n)
       (make-constraints
        (att-value 'ilp-nego-reqc (ast-child 'target n) 'ProvClause comp-eq) ;provs
        (att-value 'ilp-nego-reqc n comp-max-eq) ;max-reqs
        (att-value 'ilp-nego-reqc n comp-min-eq) #t)))) ;min-reqs,request?
   
   (define (make-constraints provs max-reqs min-reqs request?)
     (fold-left ; fold over provisions
      (lambda (constraints prov-entry)
        (let ([max-req-entry (assq (car prov-entry) max-reqs)]
              [min-req-entry (assq (car prov-entry) min-reqs)])
          (cons-if (and max-req-entry (make-constraint (car prov-entry) (cadr prov-entry)
                                                       (cadr max-req-entry) comp-max-eq request?))
                   (cons-if (and min-req-entry (make-constraint (car prov-entry) (cadr prov-entry)
                                                                (cadr min-req-entry) comp-min-eq request?))
                            constraints)))) (list) provs))
     
   (define (make-constraint prov prov-entry req-entry comp request?)
     (let* ([maximum (+ 1 (fold-left (lambda (max-val pair) (max (car pair) max-val)) 0 (append prov-entry req-entry)))]
            [f-prov (if (eq? comp comp-max-eq)
                       ; prov for max: (maximum - val)
                       (lambda (constraint val name) (cons* (prepend-sign (- maximum val)) name constraint))
                       (lambda (constraint val name) (cons* (prepend-sign val) name constraint)))] ; prov for other: val
            [f-req (if (eq? comp comp-max-eq)
                        ; req for max: - (maximum - val) = val - maximum
                        (lambda (constraint val name) (cons* (prepend-sign (- val maximum)) name constraint))
                        (lambda (constraint val name) (cons* (prepend-sign (- val)) name constraint)))]) ; req for other: -val
       (debug "mc: prov-entry:" prov-entry ",req-entry:" req-entry ",maximum:" maximum ",name:" prov "request?" request?)
       (if request?
           (append
            (list (string-append "request("(att-value 'ilp-name prov) "_" (comp->name comp) "): "))
            (fold-left (lambda (constraint pair) (f-prov constraint (car pair) (cadr pair))) (list) prov-entry)
            (cons* ">=" (f-req (list) (caar req-entry) "")))
           (append
            (list (string-append (att-value 'ilp-name prov) "_" (comp->name comp) ": "))
            (fold-left (lambda (constraint pair) (f-prov constraint (car pair) (cadr pair))) (list) prov-entry)
            (fold-left (lambda (constraint pair) (f-req constraint (car pair) (cadr pair))) (list) req-entry)
            (list ">= 0")))))
   
   (define (cons-if x y) (if x (cons x y) y))
   
   (ag-rule
    ilp-nego-reqc
    (Comp ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode of each impl
     (lambda (n clausetype comparator) (recur2 merge-al n 'ilp-nego-reqc 'Impl* clausetype comparator)))
    (Impl ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode
     (lambda (n clausetype comparator) (recur2 merge-al n 'ilp-nego-reqc 'Mode* clausetype comparator)))
    (Mode ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type
     (lambda (n clausetype comparator)
       (fold-left
        (lambda (result clause)
          (if (and (ast-subtype? clause clausetype) (eq? (ast-child 'comp clause) comparator))
              (fold-left ; fold over pe
               (lambda (inner pe)
                 (add-to-al inner (ast-child 'returntype clause)
                            (list (att-value 'eval-on clause pe) (att-value 'ilp-binvar-deployed n pe))))
               result (att-value 'every-pe n))
              result))
        (list) (att-value 'combined-reqs n))))
    (Request
     (lambda (n comparator)
       (fold-left
        (lambda (result clause)
          (if (eq? (ast-child 'comp clause) comparator)
              (add-to-al result (ast-child 'returntype clause) (list (att-value 'eval-on clause #f) "")) ;use arbitrary target #f
              result))
        (list) (ast-children (ast-child 'Constraints n))))))
   
   (ag-rule combined-reqs (Mode (lambda (n) (append (ast-children (ast-child 'Clause* n))
                                                    (ast-children (ast-child 'Constraints (att-value 'get-request n)))))))
   
   (define (merge-al al1 al2) (fold-left (lambda (big-al entry) (add-to-al0 big-al (car entry) (cadr entry) append)) al1 al2))
   
   (ag-rule
    ilp-nego-hw
    (Comp
     (lambda (n)
       (fold-left
        (lambda (result cp) ;[c]omparator+[p]roperty
          (append
           (fold-left
            (lambda (inner pe) (cons (att-value 'ilp-nego-hw0 n (car cp) (cadr cp) pe) inner))
            (list) (att-value 'every-pe n))
           result))
        (list) (att-value 'required-hw-properties n)))))

   (ag-rule
    ilp-nego-hw0
    (Comp
     (lambda (n comp prop pe)
       (let* ([lop (fold-left ;here we have a [l]ist [o]f [p]airs (evaled-prop mode-on-pe-name)
                    (lambda (result impl) (append (att-value 'ilp-nego-hw0 impl comp prop pe) result))
                    (list) (ast-children (ast-child 'Impl* n)))]
              [maximum (+ 1 (fold-left (lambda (max-val pair) (max (car pair) max-val)) 0 lop))]
              [f (if (eq? comp comp-max-eq)
                       (lambda (val)
                         (prepend-sign (- maximum val))) ; for max: (maximum - val)
                       (lambda (val)
                         (prepend-sign val)))]) ; for other: val
         (append (list (string-append (att-value 'ilp-name n) "_" (att-value 'ilp-name prop) "_"
                                      (att-value 'ilp-name pe) "_" (comp->name comp) ": "))
                 (fold-left (lambda (result p) (cons* (f (car p)) (cadr p) result)) (list) lop)
                 (list "<=" (f (att-value 'eval-on (att-value 'provided-clause pe (ast-child 'name prop)
                                                              (ast-child 'type pe)) pe)))))))
    (Impl (lambda (n comp prop pe) (recur3 append n 'ilp-nego-hw0 'Mode* comp prop pe)))
    (Mode (lambda (n comp prop pe) (recur3 append n 'ilp-nego-hw0 'Clause* comp prop pe)))
    (Clause
     (lambda (n comp prop pe)
       (if (and (eq? prop (ast-child 'returntype n))
                (eq? comp (ast-child 'comp n))
                (eq? (ast-child 'type pe) (ast-child 'target n)))
           (list (list (att-value 'eval-on n pe) (att-value 'ilp-binvar-deployed (ast-pp n) pe)))
           (list))))) ;empty pair if not a suitable clause
   
   (define (ast-pp n) (ast-parent (ast-parent n)))
   
   (ag-rule
    required-hw-properties
    (Comp (lambda (n) (recur union n 'required-hw-properties 'Impl*)))
    (Impl (lambda (n) (recur union n 'required-hw-properties 'Mode*)))
    (Mode (lambda (n) (recur union n 'required-hw-properties 'Clause*)))
    (Clause
     (lambda (n)
       (let ([prop (ast-child 'returntype n)])
         (if (and (ast-subtype? n 'ReqClause) (att-value 'is-hw prop))
             (list (list (ast-child 'comp n) prop)) (list))))))
   
   (define (recur op n att-name child-name)
     (fold-left (lambda (result sub)
                  (debug "rec" n att-name child-name (att-value att-name sub))
                  (op (att-value att-name sub) result))
                (list) (ast-children (ast-child child-name n))))
   
   (define (recur2 op n att-name child-name arg1 arg2)
     (fold-left (lambda (result sub)
                  (debug "rec2" n att-name child-name "arg1:" arg1 "arg2:" arg2 "result:" (att-value att-name sub arg1 arg2))
                  (op (att-value att-name sub arg1 arg2) result))
                (list) (ast-children (ast-child child-name n))))

   (define (recur3 op n att-name child-name arg1 arg2 arg3)
     (fold-left (lambda (result sub)
                  (debug "rec2" n att-name child-name "arg1:" arg1 "arg2:" arg2 "arg3:" arg3 "result:" (att-value att-name sub arg1 arg2 arg3))
                  (op result (att-value att-name sub arg1 arg2 arg3)))
                (list) (ast-children (ast-child child-name n))))
   
   (ag-rule is-hw (Property (lambda (n) (ast-subtype? (ast-parent (ast-parent n)) 'ResourceType))))
   
   (ag-rule
    ilp-binary-vars
    (Root
     (lambda (n)
       (append
        (map (lambda (impl) (att-value 'ilp-binvar impl)) (att-value 'every-impl n))
        (fold-left (lambda (result mode) (append (map (lambda (pe) (att-value 'ilp-binvar-deployed mode pe))
                                                      (att-value 'every-pe n)) result))
                   (list) (att-value 'every-mode n))))))
   
   ; TODO make bidirectional mapping: {_ - +} -> {_0 _1 _2}
   (define subs (list (list #\- #\_) (list #\+ #\_)))
   (define (ilp-conform-name name)
     (list->string (map (lambda (c) (let ([entry (assq c subs)]) (if entry (cadr entry) c)))
                        (string->list name))))
   
   (ag-rule
    ilp-name
    (Property (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
    (Comp (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
    (Impl (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
    (Mode (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
    (Resource (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n))))))
   
   (ag-rule
    ilp-binvar
    (Impl (lambda (n) (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name (att-value 'get-comp n)))
                                                       "#"  (if (lonely? n) "" (symbol->string (ast-child 'name n)))))))
    (Mode (lambda (n) (error "Should not be called for Modes"))))

   (ag-rule
    ilp-binvar-deployed
    (Mode (lambda (n pe)
            (let ([impl (att-value 'get-impl n)])
              (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name (att-value 'get-comp impl)))
                                               "#"  (if (lonely? impl) "" (symbol->string (ast-child 'name (att-value 'get-impl n))))
                                               "#"  (if (lonely? n) "" (symbol->string (ast-child 'name n)))
                                               "#"  (symbol->string (ast-child 'name pe))))))))
   
   (define lonely? (lambda (n) (= 1 (ast-num-children (ast-parent n))))) ;lonely iff no siblings
   
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
        [cubie1 (make-cubie 'Cubie1 (lambda _ 0.7))]
        [cubie2 (make-cubie 'Cubie2 (lambda _ 0.4))]
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
                (lambda _ 0.5) ;prop-load
                (list) ;other-reqs
                energy-c2
                (lambda (lomp target) ;dynamic value for energy
                  (let ([mp-size (att-value 'value-of lomp 'size)]
                        [deployed-kind (ast-child 'type target)])
                    (if (eq? deployed-kind Cubieboard)
                        (* 3 (log mp-size))
                        (* 1.5 mp-size))))
                rt-C2 (lambda _ 0.5) ;response-time
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
                     (lambda _ 0.5) ;prop-load
                     (list
                      (create-ast
                       'ReqClause
                       (list rt-C2 comp-max-eq (lambda (lomp target) (att-value 'value-of lomp 'size)) comp2)))
                     energy-c1
                     (lambda _ 20) ;energy
                     rt-C1 (lambda _ 0.2) ;response-time
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
                   (lambda (lomp target) ;prop-load
                     (let ([mp-size (att-value 'value-of lomp 'size)])
                       (if (>= mp-size 100) 0.2 0.8)))
                   (list)
                   energy-c1
                   (lambda (lomp target) ;energy
                     (let ([mp-size (att-value 'value-of lomp 'size)]
                           [deployed-kind (ast-child 'type target)])
                       (if (eq? deployed-kind Cubieboard)
                           (* 10 (log mp-size))
                           (* 2 mp-size))))
                   rt-C1 (lambda _ 0.4) ;response-time
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
                   (lambda _ 0) ;propload
                   (list (create-ast 'ReqClause (list rt-C2 comp-max-eq (lambda _ -1) comp2)))
                   energy-c1
                   (lambda _ 100) ;energy
                   rt-C1 (lambda _ 0.2) ;response-time
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
         (create-ast-list (list (create-ast 'ReqClause (list rt-C1 comp-max-eq (lambda _ 0.3) comp1))))
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
                    ((boolean? s) (if s "#t" "#f"))
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
(define comp->X (lambda (comp picker default) (let ([entry (assq comp comp-names)]) (if entry (picker entry) default))))
(define comp->string (lambda (comp) (comp->X comp cadr '?~)))
(define comp->rev-string (lambda (comp) (comp->X comp caddr '?~)))
(define comp->name (lambda (comp) (comp->X comp cadddr "error")))

(define (clauses-to-list loc)
  (fold-left
   (lambda (result clause)
     (let ([returnType (ast-child 'name (ast-child 'returntype clause))]
           [evalValue (att-value 'eval clause)]
           [compName (comp->rev-string (ast-child 'comp clause))])
       (cons
        (if (ast-subtype? clause 'ProvClause) (list returnType compName evalValue)
            (list returnType 'on (ast-child 'name (ast-child 'target clause))
                  compName evalValue 'currently: (att-value 'actual-value clause)))
        result)))
   (list) loc))

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
