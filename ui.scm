#!r6rs

(library
 (mquat ui)
 (export cim hw req obj clauses-met? deploy-on use-next-impl
         display-part display-ast)
 (import (rnrs) (racr core) (racr testing)
         (mquat constants))

 (define (clauses-to-list loc)
   (fold-left
    (lambda (result clause)
      (let ([returnType (ast-child 'name (ast-child 'returntype clause))]
            [evalValue (att-value 'eval clause)]
            [compName (comp->rev-string (ast-child 'comp clause))])
        (cons
         (if (ast-subtype? clause 'ProvClause) (list returnType compName evalValue)
             (list returnType 'on (ast-child 'name (ast-parent (ast-parent (ast-child 'returntype clause))))
                   compName evalValue 'currently: (att-value 'actual-value clause)))
         result)))
    (list) loc))
 
 ; [Debugging] returns a list of the components, implementations and modes
 ; Form: (compI ((implI1 deployedon-I1 (mode-to-use-I1 ((propName min|max actual-value) ... ))) ...) ...)
 (define (cim ast)
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
 (define (hw ast)
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
 (define (req ast)
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
 
 (define (clauses-met? ast) (att-value 'clauses-met? ast))
 (define (obj ast) (att-value 'objective-value ast))
 
 ; Given a component (or an impl) and a resource, change deployed-on of the selected impl
 ; of the given component (or the given impl) to the given resource, returning the old resource
 (define (deploy-on x new-pe ast)
   (rewrite-terminal 'deployedon (if (ast-subtype? x 'Comp) (ast-child 'selectedimpl x) x) new-pe))
 
 (define (use-next-impl comp ast)
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
     new-impl))
 
 (define (display-part node)
   (define (print name) (cons name (lambda (v) v)))
   (define printer (list)); (print 'eval)))
   (print-ast node printer (current-output-port)))
 
 (define (display-ast ast) (display-part ast)))
