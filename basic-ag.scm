#!r6rs

(library
 (mquat basic-ag)
 (export add-basic-ags)
 (import (rnrs) (racr core)
         (mquat constants) (mquat utils))
 
 (define (add-basic-ags mquat-spec)
   (with-specification
    mquat-spec
    
    ;; Basic AG rules

    ; Returns a associate list, mapping required components to a list of implementations requiring that component
    (ag-rule
     req-comp-map
     (Comp ; { (requiredComponent { impl-requiring-this-component ... }) ... }
      (lambda (n)
        (debug "c: " (ast-child 'name n))
        (fold-left
         (lambda (result-for-comp impl)
           (debug "out: impl=" (ast-child 'name impl) ",result=" result-for-comp)
           (fold-left (lambda (result-for-impl req)
                        (debug "inner: impl=" (ast-child 'name impl) ",req=" (ast-child 'name req))
                        (add-to-al result-for-impl req impl))
                      result-for-comp (att-value 'req-comp-map impl))) ;fold over reqs of impl
         (list) (ast-children (ast-child 'Impl* n))))) ;fold over impls
     (Impl (lambda (n) (ast-child 'reqcomps n))))
    
    ; Returns a minimal list of required components
    (ag-rule
     req-comp-min
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result impl) (intersect-b #f result (ast-child 'reqcomps impl)))
         #f (ast-children (ast-child 'Impl* n))))))
    
    ; Returns a list list of all possible required components
    (ag-rule
     req-comp-all
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result impl)
           (union result (ast-child 'reqcomps impl)))
         (list) (ast-children (ast-child 'Impl* n))))))
    
    ; Returns the objective value for the current configuration
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
    
    ; Returns #t, iff all clauses (HW, SW, Request) are met in the current configuration
    (ag-rule
     clauses-met?
     (Root ; clauses-met for all software components and for request?
      (lambda (n) (fold-left (lambda (result comp) (and result (att-value 'clauses-met? comp)))
                             (att-value 'clauses-met? (ast-child 'Request n))
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
    
    ; Returns the actual value of the property used in this clause in the current configuration
    (ag-rule
     actual-value
     (ReqClause
      (lambda (n)
        (let ([propName (ast-child 'name (ast-child 'returntype n))]
              [target (ast-pp (ast-child 'returntype n))])
          ((ast-child
            'value (if (ast-subtype? target 'ResourceType)
                       ; hw → search in deployedon for name and type
                       (att-value 'provided-clause (ast-child 'deployedon  (att-value 'get-impl n)) propName target)
                       ; sw → search in target-component
                       (att-value 'provided-clause (att-value 'mode-to-use (ast-child 'selectedimpl target)) propName)))
           (ast-child 'MetaParameter* (att-value 'get-request n)))))) ; Params from request, applied to the value function
     (ProvClause (lambda (n) (att-value 'eval n))))
    
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
    
    ; Returns a clause, both for a property with the given name and having the given subtype (ReqClause or ProvClause)
    (ag-rule
     search-clause
     (Mode
      (lambda (n name subtype)
        (debug name " in " (ast-child 'name n))
        (ast-find-child
         (lambda (index clause)
           (and (ast-subtype? clause subtype) (eq? (ast-child 'name (ast-child 'returntype clause)) name)))
         (ast-child 'Clause* n)))))
    
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
    
    ; Given a metaparameter name, return the value of the according metaparameter
    (ag-rule value-of (Request (lambda (n name)
                                 (ast-find-child* (lambda (i child)
                                                    (and (eq? (ast-child 'name child) name) (ast-child 'value child)))
                                                  (ast-child 'MetaParameter* n)))))
    
    ; Returns #t, if the Implementation is selected by its component
    (ag-rule is-selected? (Impl (lambda (n) (eq? (ast-child 'selectedimpl (ast-parent (ast-parent n))) n))))
    
    ; Returns #t, if the Implementation is deployed somewhere
    (ag-rule is-deployed? (Impl (lambda (n) (ast-node? (ast-child 'deployedon n)))))
    
    ; Return either the selected-mode or the first mode
    (ag-rule mode-to-use (Impl (lambda (n) (or (ast-child 'selectedmode n) (ast-child 1 (ast-child 'Mode* n))))))
    
    ; Returns #t, if the property belongs to a ResourceType
    (ag-rule is-hw (Property (lambda (n) (ast-subtype? (ast-parent (ast-parent n)) 'ResourceType))))

    ; Returns a list containing every resource
    (ag-rule every-pe (Root (lambda (n) (att-value 'res* (ast-child 'HWRoot n)))))
    
    (ag-rule ; Search through AST and find resources recursively
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
    
    ; Returns a list containing every mode
    (ag-rule every-mode (Root (lambda (n) (att-value 'sw* (ast-child 'SWRoot n) 0))))
    
    ; Returns a list containing every implementation
    (ag-rule every-impl (Root (lambda (n) (att-value 'sw* (ast-child 'SWRoot n) 1))))
    
    (ag-rule ; Search through AST and find either Impls (what = 0) or Modes (what = 1)
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
    
    
    )))
