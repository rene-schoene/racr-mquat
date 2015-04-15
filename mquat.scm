#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define impl1 0)
(define impl2 0)
(define comp1 0)

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
   ;function is lambda expecting an AST-List-Node
   ;kind is one of {kind-max, kind-min} (defined below)
   (ast-rule 'Clause->Property<ReturnType-kind-function)
   ; target is either a Comp or a ResourceType
   (ast-rule 'ReqClause:Clause->target)
   (ast-rule 'ProvClause:Clause->)
   (ast-rule 'HWRoot->ResourceType*-Resource*)
   ; type is a ResourceType
   (ast-rule 'ResourceType->name)
   (ast-rule 'Resource->name-type-Resource*<SubResources-ProvClause*)
   (ast-rule 'Request->MetaParameter*-ReqClause*<Constraints-Property<Objective)
   (ast-rule 'MetaParameter->name-value)
   (ast-rule 'Property->name) ;tbd
   
   (define kind-min 'min) ;requires MIN value
   (define kind-max 'max) ;requires MAX value
   
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
           ([kind (ast-child 'kind n)]
            [propName (ast-child 'name (ast-child 'ReturnType n))]
            [expected (att-value 'eval n)]
            [target (ast-child 'target n)]
            [actual ((ast-child
                      'function (if (ast-subtype? target 'ResourceType)
                                    (att-value 'get-provided-clause (ast-child 'deployedon   (att-value 'get-impl n)) propName target) ;hw -> search in deployedon for name and type
                                    (att-value 'get-provided-clause (ast-child 'selectedimpl (ast-child 'selectedmode target)) propName) ;sw -> search in reqComps
                                    ))
                     ;; Params from request, applied to the function
                     (ast-child 'MetaParameter* (att-value 'get-request n))
                     )]
            )
         (cond
           ((eq? kind kind-min) (<= expected actual))
           ((eq? kind kind-max) (>= expected actual))
           (else #f)))
       ))
    (ProvClause
     (lambda (n) #t)) ;Provision clauses are always fulfilled
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
       ((ast-child 'function n) (ast-child 'MetaParameter* (att-value 'get-request n)))
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
               (make-prop-load) ;ReturnType
               (create-ast-list (list)) ;Params
               (lambda (lomp) 0.4) ;function
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
                 kind-max
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

(define comp-names
  (lambda ()
    (fold-left
     (lambda (l child)
       (cons (ast-child 'name child) l))
     '()
     (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast))))))

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
         [new-impl (ast-sibling new-index former-impl)])
      (rewrite-terminal 'deployedon former-impl #f)
      (rewrite-terminal 'selectedimpl comp new-impl)
      (rewrite-terminal 'deployedon new-impl former-deployed)
      new-impl)))

; returns a list of the form
; (comp1 (impl1.a impl1.b (impl1.c cubie1)) comp2 ((impl2.a cubie2)) comp3 ())
; selected-impls are encapsulated inside a list along with their deployed-on-PE
(define comp-and-impls
  (letrec
      ((I
        (lambda (loi)
          (if (null? loi)
              '()
              (cons 
               (if (att-value 'is-selected (car loi))
                   (cons (ast-child 'name (car loi))
                         (cons (ast-child 'name (ast-child 'deployedon (car loi)))
                               '()))
                   (ast-child 'name (car loi)))
               (I (cdr loi)))))))
  (lambda ()
    (fold-left
     (lambda (result comp)
       (cons (ast-child 'name comp) (cons (I (ast-children (ast-child 'Impl* comp))) result)))
     '()
     (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast)))))))