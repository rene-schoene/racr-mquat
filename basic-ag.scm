#!r6rs

(library
 (mquat basic-ag)
 (export add-basic-ags
         =req-comp-map =req-comp-min =req-comp-all =objective-val =clauses-met?
         =eval =eval-on =value-of =actual-value =provided-clause =mode-to-use =selected? =deployed? =hw?
         <=request <=impl <=comp =every-pe =every-mode =every-impl =search-prov-clause =search-req-clause)
 (import (rnrs) (racr core)
         (mquat constants) (mquat utils) (mquat ast))

 (define (=req-comp-map n)     (att-value 'req-comp-map n))
 (define (=req-comp-min n)     (att-value 'req-comp-min n))
 (define (=req-comp-all n)     (att-value 'req-comp-all n))
 (define (=objective-val n)    (att-value 'objective-value n))
 (define (=clauses-met? n)     (att-value 'clauses-met? n))
 (define (=eval n)             (att-value 'eval n))
 (define (=eval-on n pe)       (att-value 'eval-on n pe))
 (define (=value-of n name)    (att-value 'value-of n name))
 (define (=actual-value n)     (att-value 'actual-value n))
 (define =provided-clause
   (case-lambda ((n name type) (att-value 'provided-clause n name type))
                ((n name)      (att-value 'provided-clause n name))))
 (define (=mode-to-use n)      (att-value 'mode-to-use n))
 (define (<=request n)         (att-value 'get-request n))
 (define (<=impl n)            (att-value 'get-impl n))
 (define (<=comp n)            (att-value 'get-comp n))
 (define (=selected? n)        (att-value 'selected? n))
 (define (=deployed? n)        (att-value 'deployed? n))
 (define (=hw? n)              (att-value 'hw? n))
 (define (=every-pe n)         (att-value 'every-pe n))
 (define (=every-mode n)       (att-value 'every-mode n))
 (define (=every-impl n)       (att-value 'every-impl n))
 (define (=search-prov-clause n name) (att-value 'search-clause n name 'ProvClause))
 (define (=search-req-clause n name)  (att-value 'search-clause n name 'ReqClause))

 ;; only used internally
 (define (=res* n)      (att-value 'res* n))
 (define (=sw* n what)  (att-value 'sw* n what))

 (define (add-basic-ags mquat-spec)
   (with-specification
    mquat-spec
    
    ;; Basic AG rules

    ; Returns a associate list, mapping required components to a list of implementations requiring that component
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
    
    ; Returns a minimal list of required components
    (ag-rule
     req-comp-min
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result impl) (intersect-b #f result (ast-child 'reqcomps impl)))
         #f (->* (->Impl* n))))))
    
    ; Returns a list list of all possible required components
    (ag-rule
     req-comp-all
     (Comp (lambda (n) (fold-left (lambda (result impl) (union result (ast-child 'reqcomps impl))) (list) (->* (->Impl* n))))))
    
    ; Returns the objective value for the current configuration
    (ag-rule
     objective-value
     (Root   (lambda (n)   (=objective-val (->SWRoot n))))
     (SWRoot (lambda (n)   (fold-left (lambda (total comp) (+ total (=objective-val comp))) 0 (->* (->Comp* (->SWRoot n))))))
     (Comp   (lambda (n)   (=objective-val (->selected-impl n))))
     (Impl   (lambda (n)   (=objective-val (=mode-to-use n))))
     (Mode   (lambda (n)   (=eval (=provided-clause n pn-energy)))))
    
    ; Returns #t, iff all clauses (HW, SW, Request) are met in the current configuration
    (ag-rule
     clauses-met?
     (Root       (lambda (n) (and (=clauses-met? (<=request n)) (for-all =clauses-met? (->* (->Comp* (->SWRoot n)))))))
     (Comp       (lambda (n) (=clauses-met? (->selected-impl n))))
     (Impl       (lambda (n) (=clauses-met? (=mode-to-use n))))
     (Mode       (lambda (n) (for-all =clauses-met? (->* (->Clause* n)))))
     (ReqClause  (lambda (n) ((->comparator n) (=eval n) (=actual-value n))))
     (ProvClause (lambda (n) #t)) ; Provision clauses are always fulfilled
     (Request    (lambda (n) (for-all =clauses-met? (->* (->Constraints n))))))
    
    ; Returns the actual value of the property used in this clause in the current configuration
    (ag-rule
     actual-value
     (ReqClause
      (lambda (n)
        (let ([propName (->name (->return-type n))]
              [target (<<- (->return-type n))])
          ((->value (if (ast-subtype? target 'ResourceType)
                        ; hw → search in deployedon for name and type
                        (=provided-clause (->deployed-on (<=impl n)) propName target)
                        ; sw → search in target-component
                        (=provided-clause (=mode-to-use (->selected-impl target)) propName)))
           (->MetaParameter* (<=request n)))))) ; Params from request, applied to the value function
     (ProvClause (lambda (n) (=eval n))))
    
    ; Given the name of a property (and optionally the type of the resource), get the ProvClause for this property
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
                      (lambda (index clause) (eq? (->name (->return-type clause)) name))
                      (->ProvClause* n))])
                (if found-clause ; (1.q) if a child was found ...
                    found-clause ; (1.1) ... return it
                    (search-subresources))) ; (1.2) ... else search in subresources
              ; (2) ... if not correct type or no child was found, search subresources of n
              (search-subresources)))))
     ; Search through Clauses to find a ProvClause with matching name
     (Mode (lambda (n name) (=search-prov-clause n name))))
    
    ; Returns a clause, both for a property with the given name and having the given subtype (ReqClause or ProvClause)
    (ag-rule
     search-clause
     (Mode
      (lambda (n name subtype)
        (debug "search" name "in" (->name n))
        (ast-find-child
         (lambda (index clause)
           (and (ast-subtype? clause subtype) (eq? (->name (->return-type clause)) name)))
         (->Clause* n)))))
    
    ; Get request from every node
    (ag-rule get-request (Root (lambda (n) (ast-child 'Request n))))

    ; Get Impl in subtree of the Impl
    (ag-rule get-impl (Impl (lambda (n) n)))
    
    ; Get Comp in subtree of the Comp
    (ag-rule get-comp (Comp (lambda (n) n)))
    
    ; Call the function of a Clause with the MetaParams-AST-node of the request
    (ag-rule
     eval
     (Clause
      (lambda (n)
        ; If inside a mode and impl of mode is selected, or outside of a mode ...
        (att-value 'eval-on n (if (and (ast-subtype? (<<- n) 'Mode) (=selected? (<=impl n)))
                                  ; use the resource deployed-on...
                                  (->deployed-on (<=impl n))
                                  #f))))) ; ... else evaluate it with #f as target
    
    (ag-rule eval-on (Clause (lambda (n target) ((->value n) (->MetaParameter* (<=request n)) target))))
    
    ; Given a metaparameter name, return the value of the according metaparameter
    (ag-rule value-of (Request (lambda (n name)
                                 (ast-find-child* (lambda (i child) (and (eq? (->name child) name) (->value child)))
                                                  (->MetaParameter* n)))))
    
    ; Returns #t, if the Implementation is selected by its component
    (ag-rule selected? (Impl (lambda (n) (eq? (->selected-impl (<<- n)) n))))
    
    ; Returns #t, if the Implementation is deployed somewhere
    (ag-rule deployed? (Impl (lambda (n) (ast-node? (->deployed-on n)))))
    
    ; Return either the selected-mode or the first mode
    (ag-rule mode-to-use (Impl (lambda (n) (or (->selected-mode n) (ast-child 1 (->Mode* n))))))
    
    ; Returns #t, if the property belongs to a ResourceType
    (ag-rule hw? (Property (lambda (n) (ast-subtype? (<<- n) 'ResourceType))))

    ; Returns a list containing every resource
    (ag-rule every-pe (Root (lambda (n) (=res* (->HWRoot n)))))
    
    (ag-rule ; Search through AST and find resources recursively
     res*
     (HWRoot    (lambda (n) (fold-left (lambda (result res) (append (=res* res) result)) (list) (->* (->SubResources n)))))
     (Resource  (lambda (n) (fold-left (lambda (result res) (append (=res* res) result)) (list n) (->* (->SubResources n))))))
    
    ; Returns a list containing every mode
    (ag-rule every-mode (Root (lambda (n) (=sw* (->SWRoot n) 0))))
    
    ; Returns a list containing every implementation
    (ag-rule every-impl (Root (lambda (n) (=sw* (->SWRoot n) 1))))
    
    (ag-rule ; Search through AST and find either Impls (what = 0) or Modes (what = 1)
     sw*
     (SWRoot (lambda (n what) (fold-left (lambda (result comp) (append (=sw* comp what) result)) (list) (->* (->Comp* n)))))
     (Comp   (lambda (n what) (fold-left (lambda (result impl) (append (=sw* impl what) result)) (list) (->* (->Impl* n)))))
     (Impl   (lambda (n what) (case what
                                [(0) (->* (->Mode* n))]
                                [(1) (list n)])))))))
