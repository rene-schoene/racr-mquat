#!r6rs

(library
 (mquat basic-ag)
 (export add-basic-ags
         =objective-val =objective-name =clauses-met?
         =mode-to-use =selected? =deployed? =hw?
         =req-comp-map =req-comp-min =req-comp-all =real
         =eval =eval-on =value-of =actual-value =value-attr =maximum
         <=request <=impl <=comp
         =search-prov-clause =search-req-clause =search-pe =provided-clause
         =every-pe =every-res-type =every-container =every-hw-clause
         =every-comp =every-impl =every-mode =every-req-clause =every-prov-clause =every-sw-clause)
 (import (rnrs) (racr core)
         (mquat constants) (mquat utils) (mquat ast))

 (define (=actual-value n)     (att-value 'actual-value n))
 (define (=clauses-met? n)     (att-value 'clauses-met? n))
 (define (<=comp n)            (att-value 'get-comp n))
 (define (=deployed? n)        (att-value 'deployed? n))
 (define (=eval n)             (att-value 'eval n))
 (define (=eval-on n pe)       (att-value 'eval-on n pe))
 (define (=hw? n)              (att-value 'hw? n))
 (define (<=impl n)            (att-value 'get-impl n))
 (define (=lookup-clause n prop)      (att-value 'lookup-clause n prop))
 (define (=maximum n)          (att-value 'maximum n))
 (define (=max-help n arg0)    (att-value 'max-help n arg0))
 (define (=mode-to-use n)      (att-value 'mode-to-use n))
 (define (=objective-name n)   (att-value 'objective-name n))
 (define (=objective-val n)    (att-value 'objective-value n))
 (define =provided-clause
   (case-lambda ((n name type) (att-value 'provided-clause n name type))
                ((n name)      (att-value 'provided-clause n name))))
 (define (=req-comp-map n)     (att-value 'req-comp-map n))
 (define (=req-comp-min n)     (att-value 'req-comp-min n))
 (define (=req-comp-all n)     (att-value 'req-comp-all n))
 (define (<=request n)         (att-value 'get-request n))
 (define (=real n)             (att-value 'real n))
 (define (=selected? n)        (att-value 'selected? n))
 (define (=every-pe n)         (att-value 'every-pe n))
 (define (=every-container n)  (att-value 'every-container n))
 (define (=every-res-type n)   (att-value 'every-res-type n))
 (define (=resources-of n)     (att-value 'resources-of n))
 (define (=every-comp n)       (att-value 'every-comp n))
 (define (=every-impl n)       (att-value 'every-impl n))
 (define (=every-mode n)       (att-value 'every-mode n))
 (define (=every-req-clause n comp)
                               (att-value 'every-req-clause n comp))
 (define (=every-prov-clause n comp)
                               (att-value 'every-prov-clause n comp))
 (define (=every-sw-clause n)  (att-value 'every-sw-clause n))
 (define (=every-hw-clause n)  (att-value 'every-hw-clause n))
 (define (=search-prov-clause n name) (att-value 'search-clause n name 'ProvClause))
 (define (=search-req-clause n name)  (att-value 'search-clause n name 'ReqClause))
 (define (=search-pe n name)   (att-value 'search-pe n name))
 (define (=value-attr n)       (att-value 'value-attr n))
 (define (=value-of n name)    (att-value 'value-of n name))

 (define (add-basic-ags mquat-spec)
   (with-specification
    mquat-spec

    ; =actual-value: Returns the actual value of the property used in this clause in the current configuration
    (ag-rule
     actual-value
     (ReqClause
      (lambda (n)
        (let ([propName (->name (=real (->return-type n)))]
              [target (<<- (=real (->return-type n)))])
          ((->value (if (ast-subtype? target 'ResourceType)
                        ; hw → search in deployedon for name and type
                        (=provided-clause (->deployed-on (<=impl n)) propName target)
                        ; sw → search in target-component
                        (=provided-clause (=mode-to-use (->selected-impl target)) propName)))
           (->MetaParameter* (<=request n)))))) ; Params from request, applied to the value function
     (ProvClause (lambda (n) (=eval n))))

    ; <=comp: Get Comp in subtree of the Comp
    (ag-rule get-comp (Comp (lambda (n) n)))

    ; =clauses-met?: Returns #t, iff all clauses (HW, SW, Request) are met in the current configuration
    (ag-rule
     clauses-met?
     (Root       (lambda (n) (and (=clauses-met? (<=request n)) (for-all =clauses-met? (->* (->Comp* (->SWRoot n)))))))
     (Comp       (lambda (n) (=clauses-met? (->selected-impl n))))
     (Impl       (lambda (n) (=clauses-met? (=mode-to-use n))))
     (Mode       (lambda (n) (for-all =clauses-met? (->* (->Clause* n)))))
     (ReqClause  (lambda (n) ((comp->f (->comparator n)) (=eval n) (=actual-value n))))
     (ProvClause (lambda (n) #t)) ; Provision clauses are always fulfilled
     (Request    (lambda (n) (for-all =clauses-met? (->* (->Constraints n))))))

    ; =deployed?: Returns #t, if the Implementation is deployed somewhere
    (ag-rule deployed? (Impl (lambda (n) (ast-node? (->deployed-on n)))))

    ; =eval: Call the function of a Clause with the MetaParams-AST-node of the request and on the current deployed resource type
    (ag-rule
     eval
     (Clause
      (lambda (n)
        ; If inside a mode and impl of mode is selected, or outside of a mode ...
        (att-value 'eval-on n (if (and (ast-subtype? (<<- n) 'Mode) (=selected? (<=impl n)))
                                  ; use the resource deployed-on...
                                  (->deployed-on (<=impl n))
                                  #f))))) ; ... else evaluate it with #f as target

    ; =eval: Call the function of a Clause with the MetaParams-AST-node of the request and on the given resource type
    (ag-rule eval-on (Clause (lambda (n target) ((->value n) (->MetaParameter* (<=request n)) target))))

    ; =every-req-clause: Return every requirement clauses referencing this property and using the given comparator
    (ag-rule
     every-req-clause
     (Property
      (lambda (n comparator)
        (fold-left (lambda (result cl) (if (and (eq? comparator (->comparator cl)) (ast-subtype? n 'ReqClause)
                                                (eq? n (=real (->return-type cl)))) (cons cl result) result))
                   (list) (=every-sw-clause n)))))

    ; =every-prov-clause: Return every provision clauses referencing this property and using the given comparator
    (ag-rule
     every-prov-clause
     (Property
      (lambda (n comparator)
        (fold-left (lambda (result cl) (if (and (eq? comparator (->comparator cl)) (ast-subtype? n 'ProvClause)
                                                (eq? n (=real (->return-type cl)))) (cons cl result) result))
                   (list) (append (=every-sw-clause n) (=every-hw-clause n))))))

    ; =every-container: Returns a list of every pe that can run software on it
    (ag-rule every-container (Root (lambda (n) (filter (lambda (pe) (->container? (->type pe))) (=every-pe n)))))

    ; =every-pe: Returns a list containing every resource
    (ag-rule
     every-pe
     (Root     (lambda (n) (recur n append =every-pe (lambda (m) (->SubResources (->HWRoot m))))))
     (Resource (lambda (n) (cons n (recur n append =every-pe ->SubResources)))))

    ; =every-res-type: Returns a list containing every resource type
    (ag-rule every-res-type (Root (lambda (n) (->* (->ResourceType* (->HWRoot n))))))

    ; =every-comp: Returns a list containing every component, that may be needed for the request
    (ag-rule
     every-comp
     (Root (lambda (n) (=every-comp (->target (<=request n)))))
     (Comp (lambda (n) (cons n (fold-left (lambda (result c) (append (=every-comp c) result))
                                          (list) (=req-comp-all n))))))

    ; =every-impl: Returns a list containing every implementation, that may be needed for the request
    (ag-rule
     every-impl
     (Root (lambda (n) (=every-impl (->target (<=request n)))))
     (Comp (lambda (n) (append (->* (->Impl* n)) (fold-left (lambda (result c) (append (=every-impl c) result))
                                                            (list) (=req-comp-all n))))))

    ; =every-mode: Returns a list containing every mode, that may be needed for the request
    (ag-rule every-mode (Root (lambda (n) (fold-left (lambda (result impl) (append (->* (->Mode* impl)) result))
                                                     (list) (=every-impl n)))))

    ; =every-sw-clause: Returns a list containing every SW clause
    (ag-rule
     every-sw-clause
     (Root (lambda (n) (fold-left (lambda (result mode) (append (->* (->Clause* mode)) result)) (list) (=every-mode n)))))

    ; =every-hw-clause: Returns a list containing every HW clause
    (ag-rule
     every-hw-clause
     (Root (lambda (n) (fold-left (lambda (result pe) (append (->* (->ProvClause* pe)) result)) (list) (=every-pe n)))))

    ; =hw?: Returns #t, if the property belongs to a ResourceType or to the HWRoot (i.e. a general hardware property)
    (ag-rule hw? (Property (lambda (n) (let ([parent (<<- (=real n))])
                                         (or (ast-subtype? parent 'ResourceType) (ast-subtype? parent 'HWRoot))))))

    ; =lookup-clause: Given a property, return the first clause referencing this property at this resource/mode
    (ag-rule
     lookup-clause
     (Resource (lambda (n prop) (ast-find-child (lambda (i cl) (eq? (=real prop) (=real (->return-type cl)))) (->ProvClause* n))))
     (Mode (lambda (n prop) (ast-find-child (lambda (i cl) (eq? (=real prop) (=real (->return-type cl)))) (->Clause* n)))))

    ; <=impl: Get Impl in subtree of the Impl
    (ag-rule get-impl (Impl (lambda (n) n)))

    ; =maximum: Returns the maximum value for this property for clauses with comp-max-eq as comparator
    (ag-rule
     maximum (Property (lambda (n) (+ 1 (apply max (map (lambda (type) (att-value 'max-help n type)) (=every-res-type n)))))))

    (ag-rule ; Helper-Attribute for maximum
     max-help
     (Property (lambda (n type) (apply max (map (lambda (cl) (if cl (=eval-on cl type) 0))
                                                (append (=max-help type n) (=every-req-clause n comp-max-eq))))))
     (ResourceType (lambda (n prop) (map (lambda (pe) (=lookup-clause pe prop)) (=resources-of n)))))

    ; =mode-to-use: Return either the selected-mode or the first mode
    (ag-rule mode-to-use (Impl (lambda (n) (or (->selected-mode n) (ast-child 1 (->Mode* n))))))

    ; =objective-name: Get the name of the objective, defaults to pn-energy
    (ag-rule objective-name (Root (lambda (n) (or (->objective (<=request n)) pn-energy))))

    ;=objective-val:  Returns the objective value for the current configuration
    (ag-rule
     objective-value
     (Root   (lambda (n)   (=objective-val (->SWRoot n))))
     (SWRoot (lambda (n)   (fold-left (lambda (total comp) (+ total (=objective-val comp))) 0 (->* (->Comp* (->SWRoot n))))))
     (Comp   (lambda (n)   (=objective-val (->selected-impl n))))
     (Impl   (lambda (n)   (=objective-val (=mode-to-use n))))
     (Mode   (lambda (n)   (=eval (=provided-clause n (=objective-name n))))))

    ; =provided-clause: Given the name of a property (and maybe the type of the resource), get the ProvClause for this property
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
                  (lambda (index subres) (=provided-clause subres))
                  (->SubResources n)))])
          (if (eq? (->type n) type) ; if n has correct type ...
              (let ([found-clause
                     (ast-find-child ; (1) ... then try to find a child in n ...
                      (lambda (index clause) (string=? (->name (=real (->return-type clause))) name))
                      (->ProvClause* n))])
                (if found-clause ; (1.q) if a child was found ...
                    found-clause ; (1.1) ... return it
                    (search-subresources))) ; (1.2) ... else search in subresources
              ; (2) ... if not correct type or no child was found, search subresources of n
              (search-subresources)))))
     ; Search through Clauses to find a ProvClause with matching name
     (Mode (lambda (n name) (=search-prov-clause n name))))

    ; =real: Returns the real property the property-ref references, or the real property itself
    (ag-rule
     real
     (RealProperty (lambda (n) n))
     (PropertyRef  (lambda (n) (=real (ast-child 'ref n)))))

    ; =req-comp-map: Returns a associate list, mapping required components to a list of implementations requiring that component
    (ag-rule
     req-comp-map
     (Comp ; { (requiredComponent { impl-requiring-this-component ... }) ... }
      (lambda (n)
        (debug "c: " (->name n))
        (fold-left
         (lambda (result-for-comp impl)
           (debug "out: impl=" (->name impl) ",result=" result-for-comp)
           (fold-left (lambda (result-for-impl req)
                        (debug "inner: impl=" (->name impl) ",req=" (->name req))
                        (add-to-al result-for-impl req impl))
                      result-for-comp (=req-comp-map impl))) ;fold over reqs of impl
         (list) (->* (->Impl* n))))) ;fold over impls
     (Impl (lambda (n) (ast-child 'reqcomps n))))

    ; =req-comp-min: Returns a minimal list of required components
    (ag-rule
     req-comp-min
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result impl) (intersect-b #f result (ast-child 'reqcomps impl)))
         #f (->* (->Impl* n))))))

    ; =req-comp-all: Returns a list list of all possible required components
    (ag-rule
     req-comp-all
     (Comp (lambda (n) (fold-left (lambda (result impl) (union result (ast-child 'reqcomps impl))) (list) (->* (->Impl* n))))))

    ; <=request: Get request from every node
    (ag-rule get-request (Root (lambda (n) (ast-child 'Request n))))

    ; Returns all resources of the type
    (ag-rule resources-of (ResourceType (lambda (n) (filter (lambda (pe) (eq? n (->type pe))) (=every-pe n)))))

    ; =search-{prov|req}-clause: Returns a clause, matching property-name and clause-node-subtype (ReqClause or ProvClause)
    (ag-rule
     search-clause
     (Mode
      (lambda (n name subtype)
        (debug "search" name "in" (->name n))
        (ast-find-child
         (lambda (index clause)
           (and (ast-subtype? clause subtype) (string=? (->name (=real (->return-type clause))) name)))
         (->Clause* n)))))

    ; =search-pe: Search for a resource with the given name
    (ag-rule
     search-pe
     (Root (lambda (n name) (ast-find-child (lambda (i pe) (=search-pe pe name)) (->SubResources (->HWRoot n)))))
     (Resource (lambda (n name) (or (string=? (->name n) name) (ast-find-child (lambda (i pe) (=search-pe pe name))
                                                                               (->SubResources n))))))

    ; =selected?: Returns #t, if the Implementation is selected by its component
    (ag-rule selected? (Impl (lambda (n) (eq? (->selected-impl (<<- n)) n))))

    ; =value-of: Given a metaparameter name, return the value of the according metaparameter
    (ag-rule value-of (Request (lambda (n name)
                                 (ast-find-child* (lambda (i child) (and (string=? (->name child) name) (->value child)))
                                                  (->MetaParameter* n)))))

    ; =value-attr: Cached evaluation clause value
    (ag-rule value-attr (ProvClause (lambda (n) ((->value n))))))))
