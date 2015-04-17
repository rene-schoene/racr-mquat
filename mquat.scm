#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define comp-names (list))
(define energy 'energy-consumption) ; The name of the property energy-consumption, to be used for all properties. Used for the objective function

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-Comp*<ReqComps-selectedimpl)
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
    (Root ; sum of objective value of all software components (skipping SWRoot)
     (lambda (n)
       (fold-left
        ; call the same attribute on all childs
        (lambda (totalValue comp) (+ totalValue (att-value 'get-objective-function-value comp)))
        0
        (ast-children (ast-child 'Comp* (ast-child 'SWRoot n))))))
    (Comp ; sum of objective value of selected impl and all objective value of required components
     (lambda (n)
       (fold-left (lambda (totalValue reqComp) (+ totalValue (att-value 'get-objective-function-value reqComp)))
                  (att-value 'get-objective-function-value
                             (ast-child 'selectedimpl n))
                  (ast-children (ast-child 'ReqComps n)))))
    (Impl ; call the same attribute on the mode to use
     (lambda (n)
       (att-value 'get-objective-function-value (att-value 'mode-to-use n))))
    (Mode ; find and evaluate the energy-consumption provClause
     (lambda (n)
       (att-value 'eval (att-value 'get-provided-clause n energy)))))
   
   (ag-rule
    clauses-met?
    (Root ; clauses-met for all software components?
     (lambda (n)
       (fold-left (lambda (result comp) (and result (att-value 'clauses-met? comp)))
                  #t
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
                  #t
                  (ast-children (ast-child 'Clause* n)))))
    (ReqClause ; comparator function returns true?
     (lambda (n)
       (let*
           ([comp (ast-child 'comp n)]
            [required (att-value 'eval n)]
            [actual (att-value 'get-actual-value n)])
         (comp required actual))))
    (ProvClause ; Provision clauses are always fulfilled
     (lambda (n) #t)))
   
   (ag-rule
    get-actual-value
    (ReqClause
     (lambda (n)
       (let
           ([propName (ast-child 'name (ast-child 'ReturnType n))]
            [target (ast-child 'target n)])
         ((ast-child
           'value (if (ast-subtype? target 'ResourceType)
                      (att-value 'get-provided-clause (ast-child 'deployedon  (att-value 'get-impl n)) propName target) ; hw -> search in deployedon for name and type
                      (att-value 'get-provided-clause (att-value 'mode-to-use (ast-child 'selectedimpl target)) propName))) ; sw -> search in target-component
          (ast-child 'MetaParameter* (att-value 'get-request n)))))) ; Params from request, applied to the value function
    (ProvClause
     (lambda (n)
       ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n))))))
   
   ; Given the name of a property (and optionally the type of the resource), get ProvisionClause for this property
   (ag-rule
    get-provided-clause
    (Resource ; Search through ProvClauses of this resource and its subresources to find a clause with matching name of a resource with matching type
     (lambda (n name type)
       (let
           ([search-subresources
             (lambda ()
               (ast-find-child
                (lambda (index subres)
                  (att-value 'get-provided-clause subres))
                (ast-child 'SubResources n)))])
         (if (eq? (ast-child 'type n) type) ; if n has correct type ...
             (let
                 ([found-clause
                   (ast-find-child ; (1) ... then try to find a child in n ...
                    (lambda (index clause)
                      (eq? (ast-child 'name (ast-child 'ReturnType clause)) name))
                    (ast-child 'ProvClause* n))])
               (if found-clause ; (1.q) if a child was found ...
                   found-clause ; (1.1) ... return it
                   (search-subresources))) ; (1.2) ... else search in subresources
             ; (2) ... if not correct type or no child was found, search subresources of n
             (search-subresources)))))
    (Mode ; Search through Clauses to find a ProvClause with matching name
     (lambda (n name)
       (ast-find-child
        (lambda (index clause)
          (and (ast-subtype? clause 'ProvClause) (eq? (ast-child 'name (ast-child 'ReturnType clause)) name)))
        (ast-child 'Clause* n)))))
   
   (ag-rule get-request (Root (lambda (n) (ast-child 'Request n)))) ; Get request from every node
   
   (ag-rule get-impl (Impl (lambda (n) n))) ; Get Impl in subtree of the Impl
   
   (ag-rule get-comp (Comp (lambda (n) n))) ; Get Comp in subtree of the Comp
   
   ; Call the function of a Clause with the MetaParams-AST-node of the request
   (ag-rule
    eval
    (Clause
     (lambda (n)
       (if (or (not (ast-subtype? (ast-parent (ast-parent n)) 'Mode)) (att-value 'is-selected? (att-value 'get-impl n)))
           ((ast-child 'value n) (ast-child 'MetaParameter* (att-value 'get-request n)))
           #f))))
   
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
    is-selected?
    (Impl
     (lambda (n)
       (eq? (ast-child 'selectedimpl (ast-parent (ast-parent n))) n))))
   
   (ag-rule
    is-deployed?
    (Impl
     (lambda (n)
       (ast-node? (ast-child 'deployedon n))))
    )
   
   ; Return either the selected-mode or the first mode
   (ag-rule
    mode-to-use
    (Impl
     (lambda (n)
       (or (ast-child 'selectedmode n) (ast-child 1 (ast-child 'Mode* (ast-child 'Contract n)))))))
   
   (compile-ag-specifications)
   
   ;; Concrete AST
   (let*
       ([load 'server-load]
        [make-prop
         (lambda (name)
           (create-ast
            'Property
            (list name)))]
        [Cubieboard
         (create-ast
          'ResourceType
          (list 'Cubieboard))]
        [make-cubie
         (lambda (name f-load)
           (create-ast
            'Resource
            (list
             name
             Cubieboard ;type
             (create-ast-list (list)) ;Subresources
             (create-ast-list
              (list
               (create-ast
                'ProvClause
                (list
                 (make-prop load) ; ReturnType
                 comp-eq ; comp
                 f-load)))))))] ; value
        [cubie1 (make-cubie 'Cubie1 (lambda (lomp) 0.7))]
        [cubie2 (make-cubie 'Cubie2 (lambda (lomp) 0.4))]
        [make-mp-size
         (lambda (value)
           (create-ast
            'MetaParameter
            (list
             'size ;name
             value)))] ;value
        [make-simple-mode
         (lambda (req-f prov-f mode-name)
           (create-ast
            'Mode
            (list
             mode-name
             (create-ast-list ;Clause*
              (list
               (create-ast
                'ReqClause
                (list
                 (make-prop load)
                 comp-max-eq ;comp
                 req-f ;function
                 Cubieboard)) ;target
               (create-ast
                'ProvClause
                (list
                 (make-prop energy)
                 comp-eq ;comp
                 prov-f)))))))] ;function
        [make-simple-contract
         (lambda (mode)
           (create-ast
            'Contract
            (list
             (create-ast-list ;Mode*
              (list mode)))))]
        [sample-impl1a
         (let
             [(mode (make-simple-mode
                     (lambda (lomp)
                       0.5) ;always return 0.5 for prop-load
                     (lambda (lomp)
                       20) ;always return 20 for energy
                     'static-mode-1a))] ;name of Mode
           (create-ast
            'Impl
            (list
             'Sample-Implementation1a ;name of Impl
             (make-simple-contract mode) ;Contract = static value of 0.5
             cubie1 ;deployedon
             mode)))]
        [sample-impl1b
         (let
             [(mode
               (make-simple-mode
                (lambda (lomp) ;dynamic value for prop-load
                  (let
                      ([mp-size (att-value 'value-of lomp 'size)])
                    (if (>= mp-size 100)
                        0.2
                        0.8)))
                (lambda (lomp) ;dynamic value for energy
                  (let
                      ([mp-size (att-value 'value-of lomp 'size)]
                       [deployed-kind (ast-child 'type (ast-child 'deployedon impl1b))])
                    (if (eq? deployed-kind Cubieboard)
                        (* 10 (log mp-size))
                        (* 2 mp-size))))
                'dynamic-mode-1b))] ;name of Mode
           (create-ast
            'Impl
            (list
             'Another-Sample-Implementation1b ;name of Impl
             (make-simple-contract mode) ;Contract = dynamic value, either 0.2 or 0.8
             #f ;deployedon
             #f)))] ;selectedmode
        [part-impl2a
         (let
             [(mode
               (make-simple-mode
                (lambda (lomp) ;static value of 0.5 for prop-load
                  0.5)
                (lambda (lomp) ;dynamic value for energy
                  (let
                      ([mp-size (att-value 'value-of lomp 'size)]
                       [deployed-kind (ast-child 'type (ast-child 'deployedon impl2a))])
                    (if (eq? deployed-kind Cubieboard)
                        (* 3 (log mp-size))
                        (* 1.5 mp-size))))
                'dynamic-mode-2a))] ;name of Mode
           (create-ast
            'Impl
            (list
             'Part-Implementation2a ;name of Impl
             (make-simple-contract mode) ;Contract = dynamic value, either 0.2 or 0.8
             cubie1 ;deployedon
             mode)))]) ;selectedmode
     (create-ast
      'Root
      (list
       (create-ast
        'HWRoot
        (list
         (create-ast-list
          (list Cubieboard))
         (create-ast-list
          (list cubie1 cubie2))))
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
              (list sample-impl1a sample-impl1b))
             (create-ast-list ;ReqComps
              (list
               (create-ast
                'Comp
                (list
                 'Depth2-Component
                 (create-ast-list ;Impl*
                  (list part-impl2a))
                 (create-ast-list (list)) ;ReqComps
                 part-impl2a ;selectedimpl of Depth2-Component
                 ))))
             sample-impl1a ;selectedimpl of Example-Component
             ))))))
       (create-ast
        'Request
        (list
         (create-ast-list ;MetaParameter*
          (list
           (make-mp-size 50)))
         (create-ast-list ;Constraints
          (list
           ;TODO: define a constraint on prop-load
           ))
         (create-ast
          'Property
          (list
           'Requested-property
           )))))))))

;; Misc and UI

(define comp1 (ast-child 1 (ast-child 'Comp* (ast-child 'SWRoot ast))))
(define impl1a (ast-child 1 (ast-child 'Impl* comp1)))
(define impl1b (ast-child 2 (ast-child 'Impl* comp1)))
(define comp2 (ast-child 1 (ast-child 'ReqComps comp1)))
(define impl2a (ast-child 1 (ast-child 'Impl* comp1)))
(define cb1 (ast-child 1 (ast-child 'Resource* (ast-child 'HWRoot ast))))
(define cb2 (ast-child 2 (ast-child 'Resource* (ast-child 'HWRoot ast))))

;; Change impls, selecting and/or mapping

; Given a component and a resource, change deployed-on of the selected impl
; of the given component to the given resource, returning the old resource
(define deploy-on
  (lambda (comp new-pe)
    (rewrite-terminal 'deployedon (ast-child 'selectedimpl comp) new-pe)))

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
  (define printer
    (list (print 'eval)))
  (print-ast
   node
   printer
   (current-output-port)))

(define display-ast (lambda () (display-part ast)))

(define clauses-to-list
  (let
      ([comp->string
        (lambda (comp)
          (let ([entry (assq comp comp-names)])
            (if entry
                (cadr entry)
                '?~)))])
    (lambda (loc)
      (fold-left
       (lambda (result clause)
         (let
             ([returnType (ast-child 'name (ast-child 'ReturnType clause))]
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
                 (att-value 'get-actual-value clause)))
            result)))
       (list) ; nil of fold-left
       loc)))) ; list of fold-left

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
       (lambda (result comp)
         (cons (C comp) result))
       (list)
       (ast-children (ast-child 'Comp* (ast-child 'SWRoot ast)))))))

; [Debuggin] Returns a list of hardware resources along with their provided properties
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
       (ast-child 'name (ast-child 'Objective r)))))) ; objective

;; Shortcuts

(define clauses-met? (lambda () (att-value 'clauses-met? ast)))
(define obj (lambda () (att-value 'get-objective-function-value ast)))
(define comp1-next-impl (lambda () (use-next-impl comp1)))
