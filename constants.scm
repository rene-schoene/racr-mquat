#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat constants)
 (export pn-energy agg-max agg-sum offline online booting halting error-state
         comp-min-eq comp-max-eq comp-eq f-comp-max-diff-eq
         comp->f comp->string comp->rev-string comp->name)
 (import (rnrs base) (rnrs lists))
 
 (define pn-energy "energy-consumption") ; Name of the property energy-consumption and as default objective function property name
 (define agg-max 1) (define agg-sum 2)  ; Used in agg of property to describe how to aggregate the property
 ; Status of resources
 (define offline 'offline) (define online 'online) (define booting 'booting) (define halting 'halting) (define error-state 'error)
 (define comp-min-eq 'min-eq)
 (define comp-max-eq 'max-eq)
 (define comp-eq 'eq)
 (define f-comp-max-diff-eq (lambda (diff) (lambda (req act) (<= (- req act) diff))))
 
 ; 1st = lookup-symbol
 ; 2nd = op: property op forumla
 ; 3rd = rev-op: formula rev-op property
 ; 4th = name as string
 (define comparators (list (list comp-eq     (lambda (req act) (= req act))  '=  '=)
                           (list comp-min-eq (lambda (req act) (<= req act)) '<= '>=)
                           (list comp-max-eq (lambda (req act) (>= req act)) '>= '<=)))

 (define comp->X (lambda (comp picker default) (let ([entry (assq comp comparators)]) (if entry (picker entry) default))))
 (define comp->f (lambda (comp) (comp->X comp cadr (lambda _ (error 'comparators "No function found" comp)))))
 (define comp->string (lambda (comp) (comp->X comp caddr '?~)))
 (define comp->rev-string (lambda (comp) (comp->X comp cadddr '~?)))
 (define comp->name (lambda (comp) (symbol->string comp))))
