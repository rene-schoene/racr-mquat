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
                  (ast-children (ast-child 'ReqComp n)))))
    (Impl
     (lambda (n)
       (cond
         ((att-value 'is-selected n) (string-length (symbol->string (ast-child 'name n)))) ;TODO: calculate objective-function-value
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
       ((cubie1
         (create-ast
          'Resource
          (list
           'Cubie1 ;name
           (create-ast-list (list)) ;"subresources"
           (create-ast-list (list)) ;"provClauses"
           )))
        (sample-impl1
         (create-ast
          'Impl
          (list
           'Sample-Implementation1 ;name
           (create-ast-bud) ;Contract
           cubie1 ;deployedon
           )))
        (sample-impl2
         (create-ast
          'Impl
          (list
           'Another-Sample-Implementation2 ;name
           (create-ast-bud) ;Contract
           #f ;deployedon
           ))))
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
             'Example-Component ;name
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