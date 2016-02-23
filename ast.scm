#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat ast)
 (export specify&compile-ast
         ->name ->Property* ->* <- <<- ->SubResources ->value ; common
         ->HWRoot ->SWRoot ; Root
         ->Comp* ->RealProperty* ; SWRoot
         ->Impl* ->selected-impl ; Comp
         ->Mode* ->deployed-on ->selected-mode ; Impl
         ->Clause* ; Mode
         ->return-type ->comparator ; Clause
         ->ResourceType* ; HWRoot
         ->container? ; ResourceType
         ->type ->status ->ProvClause* ; Resource
         ->MetaParameter* ->target ->Constraints ->objective ; Request
         ->unit ->kind ->direction ->agg ; Property
         :Root :RealProperty :PropertyRef
         :SWRoot :Comp :Impl :Mode :ReqClause :ProvClause
         :HWRoot :ResourceType :Resource
         :Request :MetaParameter)
 (import (rnrs) (racr core))

 (define (->HWRoot n)         (ast-child 'HWRoot n))
 (define (->SWRoot n)         (ast-child 'SWRoot n))
 (define (->Comp* n)          (ast-child 'Comp* n))
 (define (->RealProperty* n)  (ast-child 'RealProperty* n))
 (define (->name n)           (ast-child 'name n))
 (define (->Impl* n)          (ast-child 'Impl* n))
 (define (->selected-impl n)  (ast-child 'selectedimpl n))
 (define (->Property* n)      (ast-child 'Property* n))
 (define (->deployed-on n)    (ast-child 'deployedon n))
 (define (->selected-mode n)  (ast-child 'selectedmode n))
 (define (->Mode* n)          (ast-child 'Mode* n))
 (define (->Clause* n)        (ast-child 'Clause* n))
 (define (->ResourceType* n)  (ast-child 'ResourceType* n))
 (define (->SubResources n)   (ast-child 'SubResources n))
 (define (->type n)           (ast-child 'type n))
 (define (->status n)         (ast-child 'status n))
 (define (->container? n)     (ast-child 'container n))
 (define (->ProvClause* n)    (ast-child 'ProvClause* n))
 (define (->MetaParameter* n) (ast-child 'MetaParameter* n))
 (define (->target n)         (ast-child 'target n))
 (define (->Constraints n)    (ast-child 'Constraints n))
 (define (->objective n)      (ast-child 'objective n))
 (define (->return-type n)    (ast-child 'returntype n))
 (define (->comparator n)     (ast-child 'comp n))
 (define (->value n)          (ast-child 'value n))
 (define (->unit n)           (ast-child 'unit n))
 (define (->kind n)           (ast-child 'kind n))
 (define (->direction n)      (ast-child 'direction n))
 (define (->agg n)            (ast-child 'agg n))
 (define (->* n)              (ast-children n))
 (define (<- n)               (ast-parent n))
 (define (<<- n)              (ast-parent (ast-parent n)))

 (define (:Root spec hw sw r c)     (create-ast spec 'Root (list hw sw r c)))
 (define (:SWRoot spec c p)         (create-ast spec 'SWRoot (list (create-ast-list c) (create-ast-list p))))
 (define (:Comp spec n i s p)       (create-ast spec 'Comp (list n (create-ast-list i) s (create-ast-list p))))
 (define (:Impl spec n m r d s)     (create-ast spec 'Impl (list n (create-ast-list m) r d s)))
 (define (:Mode spec n c)           (create-ast spec 'Mode (list n (create-ast-list c))))
 (define (:ReqClause spec r c v)    (create-ast spec 'ReqClause (list r c v)))
 (define (:ProvClause spec r c v)   (create-ast spec 'ProvClause (list r c v)))
 (define (:HWRoot spec t r p)       (create-ast spec 'HWRoot (list (create-ast-list t) (create-ast-list r) (create-ast-list p))))
 (define (:ResourceType spec n c p) (create-ast spec 'ResourceType (list n c (create-ast-list p))))
 (define (:Resource spec n t s subs p) (create-ast spec 'Resource (list n t s (create-ast-list subs) (create-ast-list p))))
 (define (:Request spec m t c o)    (create-ast spec 'Request (list (create-ast-list m) t (create-ast-list c) o)))
 (define (:MetaParameter spec n v)  (create-ast spec 'MetaParameter (list n v)))
 (define (:RealProperty spec n u k d a) (create-ast spec 'RealProperty (list n u k d a)))
 (define (:PropertyRef spec r)      (create-ast spec 'PropertyRef (list r)))

 (define (specify&compile-ast mquat-spec)
   (with-specification
    mquat-spec

    (ast-rule 'Root->HWRoot-SWRoot-Request-config)
    (ast-rule 'SWRoot->Comp*-RealProperty*)
    (ast-rule 'Comp->name-Impl*-selectedimpl-Property*)
    (ast-rule 'Impl->name-Mode*-reqcomps-deployedon-selectedmode)
    (ast-rule 'Mode->name-Clause*)
    ;value is a function, expecting an AST-List-Node with MetaParameters and the target resource type, returning the value
    ;comp is a lambda expecting two values, required and actual, returning #t or #f
    (ast-rule 'Clause->returntype-comp-value)
    ; target is either a Comp or a ResourceType
    (ast-rule 'ReqClause:Clause->)
    (ast-rule 'ProvClause:Clause->)
    (ast-rule 'HWRoot->ResourceType*-Resource*<SubResources-RealProperty*)
    (ast-rule 'ResourceType->name-container-Property*)
    ; type is a ResourceType
    (ast-rule 'Resource->name-type-status-Resource*<SubResources-ProvClause*)
    (ast-rule 'Request->MetaParameter*-target-ReqClause*<Constraints-objective)
    (ast-rule 'MetaParameter->name-value)
    (ast-rule 'Property->)
    ; kind=static|runtime|derived. direction=decreasing|increasing. agg = sum|max.
    (ast-rule 'RealProperty:Property->name-unit-kind-direction-agg)
    (ast-rule 'PropertyRef:Property->ref)

    (compile-ast-specifications 'Root))))
