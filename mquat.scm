#!r6rs

(import (rnrs) (racr core))

(define spec (create-specification))

(with-specification
 spec
 ;; AST rules
 (ast-rule 'Root->HWRoot-SWRoot-Request)
 (ast-rule 'SWRoot->Comp*)
 (ast-rule 'Comp->Impl*-Comp*<ReqComp-name-selectedimpl)
; (ast-rule 'ReqComp:Comp->)
 (ast-rule 'Impl->Contract-name-deployedon)
 (ast-rule 'Contract->Mode*)
 (ast-rule 'Mode->Clause*)
 (ast-rule 'Clause->function) ;references MetaParameter
 (ast-rule 'ReqClause:Clause->)
 (ast-rule 'ProvClause:Clause->)
 (ast-rule 'HWRoot->Resource*)
 (ast-rule 'Resource->Resource*-ProvClause*)
 (ast-rule 'Request->MetaParameter*-ReqClause*<Constraints-Property<Objective)
 (ast-rule 'MetaParameter->name-value)
 (ast-rule 'Property->name) ;tbd

 (compile-ast-specifications 'Root)

 ; TODO: fold-left durch funktion ersetzen, die eine funktion auf listen-werte anwendet ("Map-Reduce")
 
 ;; AG rules
 (ag-rule
  get-objective-function-value
  (Root
   (lambda (n)
     (fold-left + 0 (ast-child 'Comp* (ast-child 'SWRoot n)))))

  (Impl
   (lambda (n)
     (cond
       ((att-value 'is-selected n) 1) ;TODO: calculate objective-function-value
       (else 0))))
  (Comp
   (lambda (n)
     (fold-left +
                (att-value 'get-objective-function-value
                           (ast-child 'selectedimpl n))
                (ast-child 'ReqComp n))))
  )
 
 (ag-rule
  clauses-met
  (Root
   (lambda (n)
     (fold-left and #t (ast-child 'Comp* (ast-child 'SWRoot n)))))
  (Comp
   (lambda (n)
     (att-value 'selectedimpl n)))
  (Impl
   (lambda (n)
     (att-value 'selectedimpl n)))
  

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
; (create-ast
;  'Root
;  (list
;   'schedule-robin
;   'one-two
;   ;'schedule-batman
;   (create-ast
;    'Config
;    (list 0 'RUNNING 0 (create-ast-list (list))))))
 )
