#!r6rs

(import (rnrs) (racr core) (racr testing))

(define spec (create-specification))

(define ast
  (with-specification
   spec
   ;; AST rules
   (ast-rule 'Root->HWRoot-SWRoot-Request)
   (ast-rule 'SWRoot->Comp*)
   (ast-rule 'Comp->name-Impl*-Comp*<ReqComp-selectedimpl)
   ; (ast-rule 'ReqComp:Comp->)
   (ast-rule 'Impl->name-Contract-deployedon)
   (ast-rule 'Contract->Mode*)
   (ast-rule 'Mode->Clause*)
   (ast-rule 'Clause->function) ;references MetaParameter
   (ast-rule 'ReqClause:Clause->)
   (ast-rule 'ProvClause:Clause->)
   (ast-rule 'HWRoot->Resource*)
   (ast-rule 'Resource->name-Resource*-ProvClause*)
   (ast-rule 'Request->MetaParameter*-ReqClause*<Constraints-Property<Objective)
   (ast-rule 'MetaParameter->name-value)
   (ast-rule 'Property->name) ;tbd
   
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
                  (ast-child 'ReqComp n))))
    (Impl
     (lambda (n)
       (cond
         ((att-value 'is-selected n) 1) ;TODO: calculate objective-function-value
         (else 0))))
    )
   
   ; TODO: fold-left durch funktion ersetzen, die eine funktion auf listen-werte anwendet ("Map-Reduce")
   (ag-rule
    clauses-met
    (Root
     (lambda (n)
       0));(fold-left and #t (ast-child 'Comp* (ast-child 'SWRoot n)))))
    (Comp
     (lambda (n)
       (att-value 'selectedimpl n)))
    (Impl
     (lambda (n)
       (att-value 'selectedimpl n)))
    )
   
   (ag-rule
    is-selected
    (Impl
     (lambda (n)
       (eq? (ast-child 'selectedimpl (ast-parent n)) n)))
    )
   
   (ag-rule
    is-deployed
    (Impl
     (lambda (n)
       (null? (ast-child 'deployedon n))))
    )
   
   
   (compile-ag-specifications)
   
   ;; Concrete AST
   (create-ast
    'Root
    (list
     (create-ast
      'HWRoot
      (list
       (create-ast-list
        (list
         (create-ast
          'Resource
          (list
           'Cubie1 ;name
           (create-ast-list (list)) ;"subresources"
           (create-ast-list (list)) ;"provClauses"
           )) ;end-of:Resource
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
           'Example-Component ;name
           (create-ast-list ;Impl*
            (list
             (create-ast
              'Impl
              (list
               'Sample-Implementation ;name
               (create-ast-bud) ;Contract
               (create-ast-bud) ;deployedon
               )) ;end-of:Impl (Sample-Implementation)
             )
            ) ;end-of:Impl* from ExampleComponent
           (create-ast-list ;ReqComp
            '()
            )
           (create-ast-bud) ;selectedimpl
           )) ;end-of:Comp (ExampleComponent)
         )) ;end-of:Comp* from SWRoot
       )) ;end-of:SWRoot
     (create-ast
      'Request
      (list
       (create-ast-list ;MetaParameter*
        '())
       (create-ast-list ;Constraints
        '())
       (create-ast
        'Property
        (list
         'Requested-property
         ))
       )) ;end-of:Request
     )) ;end-of:Root
   )
)

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

;(define set-first-impl
;  (lambda ()
;    )) ;TODO