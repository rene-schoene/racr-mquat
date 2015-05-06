#!r6rs

(library
 (mquat ast)
 (export specify&compile-ast)
 (import (rnrs) (racr core) (racr testing) (srfi :19) (srfi :27))
 
 (define (specify&compile-ast mquat-spec)
   (with-specification
    mquat-spec
    ;; AST rules
    (ast-rule 'Root->HWRoot-SWRoot-Request)
    (ast-rule 'SWRoot->Comp*)
    (ast-rule 'Comp->name-Impl*-selectedimpl-Property*)
    (ast-rule 'Impl->name-Mode*-reqcomps-deployedon-selectedmode)
    (ast-rule 'Mode->name-Clause*)
    ;value is a lambda expecting two values, an AST-List-Node with MetaParameters and the target resource, returning the value
    ;comp is a lambda expecting two values, required and actual, returning #t or #f
    (ast-rule 'Clause->returntype-comp-value)
    ; target is either a Comp or a ResourceType
    (ast-rule 'ReqClause:Clause->)
    (ast-rule 'ProvClause:Clause->)
    (ast-rule 'HWRoot->ResourceType*-Resource*)
    (ast-rule 'ResourceType->name-Property*)
    ; type is a ResourceType
    (ast-rule 'Resource->name-type-Resource*<SubResources-ProvClause*)
    (ast-rule 'Request->MetaParameter*-target-ReqClause*<Constraints-objective)
    (ast-rule 'MetaParameter->name-value)
    ; kind=static|runtime|derived. direction=decreasing|increasing. agg = sum|max.
    (ast-rule 'Property->name-unit-kind-direction-agg)
    
    (compile-ast-specifications 'Root))))
