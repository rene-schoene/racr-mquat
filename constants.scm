#!r6rs

(library
 (mquat constants)
 (export debugging
         pn-energy agg-max agg-sum
         comp-min-eq comp-max-eq comp-eq f-comp-max-diff-eq
         comp-names comp->string comp->rev-string comp->name)
 (import (rnrs) (racr core) (racr testing) (srfi :19) (srfi :27))
 
 (define debugging #t)
 (define pn-energy 'energy-consumption) ; Name of the property energy-consumption and as default objective function property name
 (define agg-max 1) (define agg-sum 2)  ; Used in agg of property to describe how to aggregate the property
 (define comp-min-eq (lambda (req act) (<= req act)))
 (define comp-max-eq (lambda (req act) (>= req act)))
 (define comp-eq (lambda (req act) (= req act)))
 (define f-comp-max-diff-eq (lambda (diff) (lambda (req act) (<= (- req act) diff))))
 
 ; 1st = op: property op forumla
 ; 2nd = rev-op: formula rev-op property
 ; 3rd = name as string
 (define comp-names (list (list comp-eq '= '= "=")
                        (list comp-min-eq '<= '>= "min")
                        (list comp-max-eq '>= '<= "max")))
 
 (define comp->X (lambda (comp picker default) (let ([entry (assq comp comp-names)]) (if entry (picker entry) default))))
 (define comp->string (lambda (comp) (comp->X comp cadr '?~)))
 (define comp->rev-string (lambda (comp) (comp->X comp caddr '?~)))
 (define comp->name (lambda (comp) (comp->X comp cadddr "error"))))
