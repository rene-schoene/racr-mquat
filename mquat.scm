#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define impl1 0)
(define impl2 0)
(define comp1 0)
(define comp-names (list))

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-Comp*<ReqComp-selectedimpl)
   (ast-rule 'Impl->name-Contract-deployedon-selectedmode)
   (ast-rule 'Contract->Mode*)
   (ast-rule 'Mode->name-Clause*)
   ;value is a lambda expecting an AST-List-Node with MetaParameters, returning the value
   ;comp is a lambda expecting two values, required and actual, returning #t or #f
   (ast-rule 'Clause->Property<ReturnType-comp-value)
   ; target is either a Comp or a ResourceType
   (ast-rule 'ReqClause:Clause->target)
   (ast-rule 'ProvClause:Clause->)
   (ast-rule 'HWRoot->ResourceType*-Resource*)
   (ast-rule 'ResourceType->name)
   ; type is a ResourceType
   (ast-rule 'Resource->name-type-Resource*<SubResources-ProvClause*)
   (ast-rule 'Request->MetaParameter*-ReqClause*<Constraints-Property<Objective)
   (ast-rule 'MetaParameter->name-value)
   (ast-rule 'Property->name) ;tbd
   
   (define comp-min-eq (lambda (req act) (<= req act)))
   (define comp-max-eq (lambda (req act) (>= req act)))
   (define comp-eq (lambda (req act) (= req act)))
   (define f-comp-max-diff-eq
     (lambda (diff)
       (lambda (req act) (<= (- req act) diff))))
   
   (set! comp-names
         (list
          (list comp-min-eq '<=)
          (list comp-max-eq '>=)
          (list comp-eq '=)))
   
   (compile-ast-specifications 'Root)
   
   ;; AG rules
   (ag-rule
    get-objective-function-value
    (Root
     (lambda (n)
       (fold-left
        ; call the same attribute on all childs
        (lambda (totalValue child) (+ totalValue (att-value 'get-objective-function-value child)))
        0
        ; childs are components of sw-root (skipping call to SWRoot)
        (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp
     (lambda (n)
       (fold-left (lambda (totalValue child) (+ totalValue (att-value 'get-objective-function-value child)))
                  (att-value 'get-objective-function-value
                             (ast-child 'selectedimpl n))
                  (ast-children (ast-child 'ReqComp n)))))
    (Impl
     (lambda (n)
       (cond
         ((att-value 'is-selected n) (string-length (symbol->string (ast-child 'name n)))) ;TODO: calculate objective-function-value
         (else 0))))
    )
   
   (ag-rule
    clauses-met
    (Root
     (lambda (n)
       (fold-left (lambda (result comp) (and result (att-value 'clauses-met comp)))
                  #t
                  (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp
     (lambda (n)
       (att-value 'clauses-met (ast-child 'selectedimpl n))))
    (Impl
     (lambda (n)
       (att-value 'clauses-met (ast-child 'Contract n))))
    (Contract
     (lambda (n)
       (fold-left (lambda (result mode) (and result (att-value 'clauses-met mode)))
                  #t
                  (ast-children (ast-child 'Mode* n)))))
    (Mode
     (lambda (n)
       (fold-left (lambda (result clause) (and result (att-value 'clauses-met clause)))
                  #t
                  (ast-children (ast-child 'Clause* n)))))
    (ReqClause
     (lambda (n)
       (let*
           ([comp (ast-child 'comp n)]
            [required (att-value 'eval n)]
            [actual (att-value 'get-actual-value n)]
            )
         (comp required actual))
       ))
    (ProvClause
     (lambda (n) #t)) ;Provision clauses are always fulfilled
    )
   
   (ag-rule
    get-actual-value
    (ReqClause
     (lambda (n)
       (let
           ([propName (ast-child 'name (ast-child 'ReturnType n))]
            [target (ast-child 'target n)]
            )
         ((ast-child
           'value (if (ast-subtype? target 'ResourceType)
                         (att-value 'get-provided-clause (ast-child 'deployedon   (att-value 'get-impl n)) propName target) ;hw -> search in deployedon for name and type
                         (att-value 'get-provided-clause (ast-child 'selectedimpl (ast-child 'selectedmode target)) propName) ;sw -> search in reqComps
                         ))
          ;; Params from request, applied to the function
          (ast-child 'MetaParameter* (att-value 'get-request n))
        ))))
    (ProvClause
     (lambda (n)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n)))))
    )
   
   ; Given the name of a property (and optionally the type of the resource), get ProvisionClause for this property
   (ag-rule
    get-provided-clause
    (Resource ;; Search through ProvClauses of this resource and its subresources to find a clause with matching name of a resource with matching type
     (lambda (n name type)
       (let
           ([search-subresources
             (lambda ()
               (ast-find-child
                (lambda (index subres)
                  (att-value 'get-provided-clause subres))
                (ast-child 'SubResources n)
                ))
             ])
         (if (eq? (ast-child 'type n) type) ;; if n has correct type ...
             (let
                 ([found-clause
                   (ast-find-child ;; (1) ... then try to find a child in n ...
                    (lambda (index clause)
                      (eq? (ast-child 'name (ast-child 'ReturnType clause)) name))
                    (ast-child 'ProvClause* n))])
               (if found-clause ;; (1.q) if a child was found ...
                   found-clause ;; (1.1) ... return it
                   (search-subresources))) ;; (1.2) ... else search in subresources
             ;; (2) ... if not correct type or no child was found, search subresources of n
             (search-subresources)))
       ))
    (Mode ;;Search through Clauses to find a ProvClause with matching name
     (lambda (n name)
       (ast-find-child
        (lambda (index clause)
          (and (ast-subtype? clause 'ProvClause) (eq? (ast-child 'name) name)))
        (ast-child 'Clause* n))
       ))
    )
    
   (ag-rule
    get-request
    (Root
     (lambda (n) (ast-child 'Request n))))

   (ag-rule
    get-impl
    (Impl
     (lambda (n) n)))

   (ag-rule
    get-comp
    (Comp
     (lambda (n) n)))

   ; primary use case = Clause: Call the function with the Params-AST-node
   ; secondary use cases (for debugging clauses):
   ; Mode: Return list of form (property-name value)
   ; Contract/Impl: Return list of form (mode-name (eval clauses))
   (ag-rule
    eval
    (Clause
     (lambda (n)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n)))
       ))
    (Mode
     (lambda (n)
       (fold-left
        (lambda (result clause)
          (cons (list (ast-child 'name (ast-child 'ReturnType clause)) (att-value 'eval clause)) result))
        '()
        (ast-children (ast-child 'Clause* n)))))
    (Contract
     (lambda (n)
       (fold-left
        (lambda (result mode)
          (cons (list (ast-child 'name mode) (att-value 'eval mode)) result))
        '()
        (ast-children (ast-child 'Mode* n)))))
    (Impl
     (lambda (n)
       (att-value 'eval (ast-child 'Contract n))))
    )
   
   ; Given a list-node n, search for a MetaParameter with the given name.
   ; If none found, return the default value
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
   
   (ag-rule
    value-of
    (Request
     (lambda (n name)
       (get-val (ast-child 'MetaParameter* n) name #f))))
   
   (ag-rule
    is-selected
    (Impl
     (lambda (n)
       (eq? (ast-child 'selectedimpl (ast-parent (ast-parent n))) n)))
    )
   
   (ag-rule
    is-deployed
    (Impl
     (lambda (n)
       (ast-node? (ast-child 'deployedon n))))
    )
   
   
   (compile-ag-specifications)
   
   ;; Concrete AST
   (let*
       ([make-prop-load
         (lambda ()
           (create-ast
            'Property
            (list
             'server-load ;name
             )))]
        [Cubieboard
         (create-ast
          'ResourceType
          (list
           'Cubieboard
           ))]
        [cubie1
         (create-ast
          'Resource
          (list
           'Cubie1 ;name
           Cubieboard ;type
           (create-ast-list (list)) ;"subresources"
           (create-ast-list
            (list
             (create-ast
              'ProvClause
              (list
               (make-prop-load) ; ReturnType
               comp-eq ; comp
               (lambda (lomp) 0.4) ; value
               )) ;end-of:ProvClause
             )) ;"provClauses"
           ))]
        [make-mp-size
         (lambda (value)
           (create-ast
            'MetaParameter
            (list
             'size ;name
             value ;value
             )))]
        [make-simple-mode
         (lambda (f mode-name)
           (create-ast
            'Mode
            (list
             mode-name
             (create-ast-list ;Clause*
              (list
               (create-ast
                'ReqClause
                (list
                 (make-prop-load)
                 comp-max-eq
                 f ;function -> not a valid terminal
                 Cubieboard ;target
                 )) ;end-of:Clause
               )) ;end-of:Clause* in Mode
             )) ;end-of:Mode
           )]
        [make-simple-contract
         (lambda (mode)
           (create-ast
            'Contract
            (list
             (create-ast-list ;Mode*
              (list
               mode
               )) ;end-of:Mode* in Contract
             )) ;end-of:Contract
           )]
        [sample-impl1
         (let
             [(mode (make-simple-mode
                     (lambda (lomp)
                       0.5)
                     'static-mode-1 ;name of Mode
                     ))]
           (create-ast
            'Impl
            (list
             'Sample-Implementation1 ;name of Impl
             (make-simple-contract mode) ;Contract = static value of 0.5
             cubie1 ;deployedon
             mode
             )))]
        [sample-impl2
         (let
             [(mode
               (make-simple-mode
                (lambda (lomp)
                  (let
                      ([mp-size (att-value 'value-of lomp 'size)])
                    (if (> mp-size 100)
                        0.8
                        0.2)))
                'dynamic-mode-2))] ;name of Mode
           (create-ast
            'Impl
            (list
             'Another-Sample-Implementation2 ;name of Impl
             (make-simple-contract mode) ;Contract = dynamic value, either 0.2 or 0.8
             #f ;deployedon
             #f ;selectedmode
             )))])
     (set! impl1 sample-impl1)
     (set! impl2 sample-impl2)
     (create-ast
      'Root
      (list
       (create-ast
        'HWRoot
        (list
         (create-ast-list
          (list
           Cubieboard
           )) ;end-of:ResourceType* from HWRoot
         (create-ast-list
          (list
           cubie1
           )) ;end-of:Resource* from HWRoot
         )) ;end-of:HWRoot
       (create-ast
        'SWRoot
        (list
         (create-ast-list ;Comp*
          (list
           (create-ast
            'Comp
            (list
             'Example-Component ;name of Comp
             (create-ast-list ;Impl*
              (list
               sample-impl1
               sample-impl2
               )
              ) ;end-of:Impl* from ExampleComponent
             (create-ast-list ;ReqComp
              '()
              )
             sample-impl1 ;selectedimpl
             )) ;end-of:Comp (ExampleComponent)
           )) ;end-of:Comp* from SWRoot
         )) ;end-of:SWRoot
       (create-ast
        'Request
        (list
         (create-ast-list ;MetaParameter*
          (list
           (make-mp-size 50)
           ))
         (create-ast-list ;Constraints
          (list
           ;TODO: define a constraint on prop-load
           ))
         (create-ast
          'Property
          (list
           'Requested-property
           ))
         )) ;end-of:Request
       )) ;end-of:Root
     ))
  )

(set! comp1 (ast-parent (ast-parent impl1)))

; Copied from racr-tune
(define display-part
  (lambda (node)
    (print-ast
     node
     (list)
     (current-output-port))))

(define display-ast
  (lambda ()
    (display-part ast)))

(define select-impl
  (lambda (new-impl new-pe)
    (let*
        ((comp (ast-parent (ast-parent new-impl)))
         (former-impl (ast-child 'selectedimpl comp)))
      (rewrite-terminal 'deployedon former-impl #f)
      (rewrite-terminal 'selectedimpl comp new-impl)
      (rewrite-terminal 'deployedon new-impl new-pe)
      new-impl)))

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

(define clauses-to-list
  (letrec
      ([comp->string
        (lambda (comp table)
          (cond
            ((null? table) '?~)
            ((eq? comp (caar table)) (cadar table))
            (else (comp->string comp (cdr table)))))])
    (lambda (loc)
      (fold-left
       (lambda (result clause)
         (let
             ([returnType (ast-child 'name (ast-child 'ReturnType clause))]
              [evalValue (att-value 'eval clause)]
              [compName (comp->string (ast-child 'comp clause) comp-names)])
         (cons
          (if (ast-subtype? clause 'ProvClause)
              (list returnType compName evalValue)
              (list
               returnType
               'on
               (ast-child 'name (ast-child 'target clause))
               evalValue
               compName
               (att-value 'get-actual-value clause)))
          result)))
       (list) ; nil of fold-left
       loc)) ; list of fold-left
    ))

; [Debugging] returns a list of the components, implementations and modes
; Form: (compI ((implI1 deployedon-I1 (selectedmode-I1 ((propName min|max actual-value) ... ))) ...) ...)
(define cim
  (letrec
      ([M
        (lambda (mode)
          (list
           (ast-child 'name mode)
           (clauses-to-list
            (ast-children (ast-child 'Clause* mode)))))]
       [I
        (lambda (loi) ; [l]ist [o]f [i]mpls
          (if (null? loi)
              (list)
              (let
                  ([deployed-on (ast-child 'deployedon (car loi))]
                   [selected-mode (ast-child 'selectedmode (car loi))]
                   [name (ast-child 'name (car loi))])
                (cons
                 (list
                  (if (att-value 'is-selected (car loi))
                      (string-append "*" (symbol->string name))
                      name)
                  (if deployed-on
                      (ast-child 'name deployed-on)
                      #f)
                  (if selected-mode
                      (M selected-mode)
                      #f))
                  (I (cdr loi))))))])
  (lambda ()
    (fold-left
     (lambda (result comp)
       (cons (ast-child 'name comp) (cons (I (ast-children (ast-child 'Impl* comp))) result)))
     (list)
     (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast)))))))

; [Debuggin] Returns a list of hardware resources along with their provided properties
; Form: (res1 ((provClause1a-name -comp->string -actualValue) ... (res1-subresources ... )) ... )
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
                    (ast-child 'name (car lor)) ; resource name
                    (ast-child 'name (ast-child 'type (car lor))) ; resource type name
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
       (ast-child 'name (ast-child 'Objective r)) ; objective
       ))))
