#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat ui)
 (export cim hw req obj clauses-met? deploy-on use-next-impl
         display-part display-ast)
 (import (rnrs) (racr core) (racr testing)
         (mquat constants) (mquat ast) (mquat basic-ag))

 (define (name-or-type n) (if (ast-has-child? 'name n) (->name n) (ast-node-type n)))
 
 (define (clauses-to-list loc)
   (fold-left
    (lambda (result clause)
      (let ([returnType (->name (=real (->ReturnType clause)))]
            [evalValue (=eval clause)]
            [compName (comp->rev-string (->comparator clause))])
        (cons
         (if (ast-subtype? clause 'ProvClause) (list returnType compName evalValue)
             (list returnType 'on (name-or-type (<<- (=real (->ReturnType clause))))
                   compName evalValue 'currently: (=actual-value clause)))
         result)))
    (list) loc))
 
 ; [Debugging] returns a list of the components, implementations and modes
 ; Form: (compI ((implI1 deployedon-I1 (mode-to-use-I1 ((propName min|max actual-value) ... ))) ...) ...)
 (define (cim ast)
   (letrec
       ([C (lambda (comp) (list (->name comp) (I (->* (->Impl* comp)))))]
        [M (lambda (mode) (list (->name mode) (clauses-to-list (->* (->Clause* mode)))))]
        [I (lambda (loi) ; [l]ist [o]f [i]mpls
             (if (null? loi) (list)
                 (let* ([impl (car loi)]
                        [name (->name impl)])
                   (cons
                    (list
                     (=req-comp-map impl)
                     (if (=selected? impl) (string-append "*" name) name)
                     (if (=deployed-on impl) (->name (=deployed-on impl)) #f)
                     (if (=selected? impl) (M (=mode-to-use impl)) #f))
                    (I (cdr loi))))))])
     (fold-left
      (lambda (result comp) (cons (C comp) result))
      (list)
      (->* (->Comp* (->SWRoot ast))))))
 
 ; [Debugging] Returns a list of hardware resources along with their provided properties
 ; Form: (res1-type res1-name ((provClause1a-name -comp->string -actualValue) ... (res1-subresources ... )) ... )
 (define (hw ast)
   (letrec
       ([R (lambda (lor) ; [l]ist [o]f [r]esources
             (if (null? lor) (list)
                 (let ([subs (R (->* (->SubResources (car lor))))]
                       [rest (R (cdr lor))])
                   (cons
                    (list
                     (->name (=type (car lor))) ; resource type name
                     (->name (car lor)) ; resource name
                     (clauses-to-list (->* (->ProvClause* (car lor))))) ; list of clauses
                    (if (null? subs) rest (cons subs rest))))))])
     (R (->* (->SubResources (->HWRoot ast))))))
 
 ; [Debugging] Returns a list of the request
 ; Form: (((metaparam1-name -value) ... ) ((constraint1-name -comp->string -requiredValue) ... ) objective)
 (define (req ast)
   (letrec
       ([MP
         (lambda (lomp) ; [l]ist [o]f [m]eta[p]arameter
           (if (null? lomp) (list)
               (cons (list (->name (car lomp)) (->value (car lomp)))
                     (MP (cdr lomp)))))])
     (let* ([r (<=request ast)]
            [o (->objective r)])
       (list
        (MP (->* (->MetaParameter* r))) ; metaparams
        (clauses-to-list (->* (->Constraints r))) ; constraints
        (if o (->name o) "default"))))) ; objective
 
 ;; Shortcuts
 
 (define (clauses-met? ast) (att-value 'clauses-met? ast))
 (define (obj ast) (=objective-val ast))
 
 ; Given a component (or an impl) and a resource, change deployed-on of the selected impl
 ; of the given component (or the given impl) to the given resource, returning the old resource
 (define (deploy-on x new-pe) (rewrite-terminal 'deployedon (if (ast-subtype? x 'Comp) (=selected-impl x) x) (->name new-pe)))
 
 (define (use-next-impl comp)
   (let* ([former-impl (=selected-impl comp)]
          [former-index (ast-child-index former-impl)]
          [num-impls (ast-num-children (->Impl* comp))]
          [former-deployed (=deployed-on former-impl)]
          [new-index (+ (mod former-index num-impls) 1)]
          [new-impl (ast-sibling new-index former-impl)]
          [first-new-mode (car (->* (->Mode* new-impl)))])
     (rewrite-terminal 'deployedon former-impl #f)
     (rewrite-terminal 'selectedmode former-impl #f)
     (rewrite-terminal 'selectedimpl comp (->name new-impl))
     (rewrite-terminal 'deployedon new-impl (->name former-deployed))
     (rewrite-terminal 'selectedmode new-impl (->name first-new-mode)) ; use first mode
     new-impl))
 
 (define (display-part node . attributes)
   (define (print name) (cons name (lambda (v) v)))
   ; (define printer (list)); (print 'eval)))
   (define printer (map (lambda (att) (print att)) (car attributes)))
   ; (display (car attributes)) (newline)
   ; (display (list 'remote-unit 'remote-container)) (newline)
   (print-ast node printer (current-output-port)))
 
 (define (display-ast ast . attributes) (display-part ast attributes)))
