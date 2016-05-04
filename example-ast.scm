#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat example-ast)
 (export print-the-example-ast the-example-ast comp1 comp2 impl1a impl1b impl1c cb1 cb2)
 (import (rnrs) (racr core)
         (mquat ast) (mquat basic-ag) (mquat constants) (mquat join) (mquat ui) (mquat utils) (mquat properties))

 (define the-example-ast
   (let* ([make-simple-prop ; kind=runtime, direction=decreasing
           (lambda (name unit agg) (:RealProperty mquat-spec name unit 'runtime 'decreasing agg))]
          [load (make-simple-prop "server-load" "%" agg-sum)]
          [freq (make-simple-prop "cpu-frequency" "Mhz" agg-max)] ; TODO add some clauses referencing this
          [make-ref (lambda (property) (:PropertyRef mquat-spec (->name property)))]
          [energy (make-simple-prop pn-energy "Joule" agg-sum)]
          [Cubieboard (:ResourceType mquat-spec "Cubieboard" #t (list load freq))]
          [make-cubie
           (lambda (name status f-load)
             (:Resource mquat-spec name (->name Cubieboard) status (list) (list (:ProvClause mquat-spec (make-ref load) comp-eq f-load))))]
          [cubie1 (make-cubie "Cubie1" online (lambda _ 0.7))]
          [cubie2 (make-cubie "Cubie2" online (lambda _ 0.4))]
          [size "size"]
          [make-mp-size (lambda (value) (:MetaParameter mquat-spec size value))]
          [make-simple-mode
           (lambda (req-f other-reqs prov-e-f rt prov-rt-f mode-name)
             (:Mode
              mquat-spec mode-name
              (cons* (:ReqClause mquat-spec (make-ref load) comp-max-eq req-f)
                     (:ProvClause mquat-spec (make-ref energy) comp-eq prov-e-f)
                     (:ProvClause mquat-spec (make-ref rt) comp-eq prov-rt-f)
                     other-reqs)))]
          [rt-C2 (make-simple-prop "response-time-C2" "ms" agg-sum)]
          [part-impl2a
           (let
               [(mode2a
                 (make-simple-mode
                  (lambda _ 0.5) ;prop-load
                  (list) ;other-reqs
                  (lambda (lomp target) ;dynamic value for energy
                    (let ([mp-size (=value-of lomp size)])
                      (if (eq? target Cubieboard)
                          (* 3 (log mp-size))
                          (* 1.5 mp-size))))
                  rt-C2 (lambda _ 0.5) ;response-time
                  "dynamic-mode-2a"))] ;name of Mode
             (:Impl mquat-spec "Part-Impl2a" (list mode2a) (list) cubie1 mode2a))]
          [comp2 (:Comp mquat-spec "Depth2-Component" (list part-impl2a) part-impl2a (list rt-C2))]
          [rt-C1 (make-simple-prop "response-time-C1" "ms" agg-sum)]
          [c1-impl1a
           (let
               [(mode1a (make-simple-mode
                         (lambda _ 0.5) ;prop-load
                         (list (:ReqClause mquat-spec (make-ref rt-C2) comp-max-eq (lambda (lomp target) (=value-of lomp size))))
                         (lambda _ 20) ;energy
                         rt-C1 (lambda _ 0.2) ;response-time
                         "static-mode-1a"))] ;name of Mode
             (:Impl mquat-spec "Sample-Impl1a" (list mode1a) (list comp2) cubie1 mode1a))]
          [c1-impl1b ; impl-1b is not deployed, default selected mode
           (:Impl
            mquat-spec "The-Sample-Impl1b"
            (list
             (make-simple-mode
              (lambda (lomp target) ;prop-load
                (let ([mp-size (=value-of lomp size)])
                  (if (>= mp-size 100) 0.2 0.8)))
              (list)
              (lambda (lomp target) ;energy
                (let ([mp-size (=value-of lomp size)])
                  (if (eq? target Cubieboard)
                      (* 10 (log mp-size))
                      (* 2 mp-size))))
              rt-C1 (lambda _ 0.4) ;response-time
              "dynamic-mode-1b"))
            (list) ;reqcomps
            #f #f)] ;deployedon + selectedmode
          [c1-impl1c
           (:Impl
            mquat-spec "Useless-Impl1c"
            (list
             (make-simple-mode
              (lambda _ 0) ;propload
              (list (:ReqClause mquat-spec (make-ref rt-C2) comp-max-eq (lambda _ -1)))
              (lambda _ 100) ;energy
              rt-C1 (lambda _ 0.2) ;response-time
              "default-mode-1c"))
            (list comp2) #f #f)]
          [comp1 (:Comp mquat-spec "Example-Component" (list c1-impl1a c1-impl1b c1-impl1c) c1-impl1a (list rt-C1))])
     (:Root mquat-spec
            (:HWRoot mquat-spec (list Cubieboard) (list cubie1 cubie2) (list))
            (:SWRoot mquat-spec (list comp1 comp2) (list energy))
            (:Request mquat-spec (list (make-mp-size 50)) (->name comp1)
                      (list (:ReqClause mquat-spec (make-ref rt-C1) comp-max-eq (lambda _ 0.3))) #f) #f)))

 (define comp1 (ast-child 1 (->Comp* (->SWRoot the-example-ast))))
 (define impl1a (ast-child 1 (->Impl* comp1)))
 (define impl1b (ast-child 2 (->Impl* comp1)))
 (define impl1c (ast-child 3 (->Impl* comp1)))
 (define comp2  (ast-child 2 (->Comp* (->SWRoot the-example-ast))))
 (define impl2a (ast-child 1 (->Impl* comp2)))
 (define cb1 (ast-child 1 (->SubResources (->HWRoot the-example-ast))))
 (define cb2 (ast-child 2 (->SubResources (->HWRoot the-example-ast))))

 (define (print-the-example-ast) (display-ast the-example-ast 'remote-unit 'remote-container)))
