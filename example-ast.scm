#!r6rs

(library
 (mquat example-ast)
 (export example-ast comp1 comp2 impl1a impl1b impl1c cb1 cb2)
 (import (rnrs) (racr core) (racr testing) (srfi :19) (srfi :27)
         (mquat ast) (mquat constants) (mquat main) (mquat ui))
 
 (define example-ast
   (with-specification
    mquat-spec
    (let*
        ([make-simple-prop ; kind=runtime, direction=decreasing
          (lambda (name unit agg) (create-ast 'Property (list name unit 'runtime 'decreasing agg)))]
         [load (make-simple-prop 'server-load '% agg-sum)]
         [freq (make-simple-prop 'cpu-frequency 'Mhz agg-max)] ; TODO add some clauses referencing this
         [energy-c1 (make-simple-prop pn-energy 'Joule agg-sum)]
         [energy-c2 (make-simple-prop pn-energy 'Joule agg-sum)]
         [rt-C1 (make-simple-prop 'response-time-C1 'ms agg-sum)]
         [rt-C2 (make-simple-prop 'response-time-C2 'ms agg-sum)]
         [Cubieboard (create-ast 'ResourceType (list 'Cubieboard (create-ast-list (list load freq))))]
         [make-cubie
          (lambda (name f-load)
            (create-ast
             'Resource
             (list name Cubieboard ;type
                   (create-ast-list (list)) ;Subresources
                   (create-ast-list (list (create-ast 'ProvClause (list load comp-eq f-load)))))))] ;ProvClause*
         [cubie1 (make-cubie 'Cubie1 (lambda _ 0.7))]
         [cubie2 (make-cubie 'Cubie2 (lambda _ 0.4))]
         [make-mp-size (lambda (value) (create-ast 'MetaParameter (list 'size value)))]
         [make-simple-mode
          (lambda (req-f other-reqs c-energy prov-e-f rt prov-rt-f mode-name)
            (create-ast
             'Mode (list mode-name
                         (create-ast-list ;Clause*
                          (append other-reqs
                                  (list (create-ast 'ReqClause (list load comp-max-eq req-f))
                                        (create-ast 'ProvClause (list c-energy comp-eq prov-e-f))
                                        (create-ast 'ProvClause (list rt comp-eq prov-rt-f))))))))]
         [part-impl2a
          (let
              [(mode2a
                (make-simple-mode
                 (lambda _ 0.5) ;prop-load
                 (list) ;other-reqs
                 energy-c2
                 (lambda (lomp target) ;dynamic value for energy
                   (let ([mp-size (att-value 'value-of lomp 'size)]
                         [deployed-kind (ast-child 'type target)])
                     (if (eq? deployed-kind Cubieboard)
                         (* 3 (log mp-size))
                         (* 1.5 mp-size))))
                 rt-C2 (lambda _ 0.5) ;response-time
                 'dynamic-mode-2a))] ;name of Mode
            (create-ast
             'Impl (list 'Part-Impl2a (create-ast-list (list mode2a))
                         (list) ;reqcomps
                         cubie1 ;deployedon
                         mode2a)))] ;selectedmode
         [comp2
          (create-ast
           'Comp
           (list
            'Depth2-Component
            (create-ast-list (list part-impl2a)) ;Impl*
            part-impl2a (create-ast-list (list rt-C2 energy-c2))))] ;selectedimpl and Property*
         [c1-impl1a
          (let
              [(mode1a (make-simple-mode
                        (lambda _ 0.5) ;prop-load
                        (list
                         (create-ast
                          'ReqClause
                          (list rt-C2 comp-max-eq (lambda (lomp target) (att-value 'value-of lomp 'size)))))
                        energy-c1
                        (lambda _ 20) ;energy
                        rt-C1 (lambda _ 0.2) ;response-time
                        'static-mode-1a))] ;name of Mode
            (create-ast
             'Impl
             (list 'Sample-Impl1a (create-ast-list (list mode1a))
                   (list comp2) ;reqcomps
                   cubie1 ;deployedon
                   mode1a)))] ;selectedmode
         [c1-impl1b
          (create-ast
           'Impl ; impl-1b is not deployed, default selected mode
           (list 'The-Sample-Impl1b
                 (create-ast-list
                  (list
                   (make-simple-mode
                    (lambda (lomp target) ;prop-load
                      (let ([mp-size (att-value 'value-of lomp 'size)])
                        (if (>= mp-size 100) 0.2 0.8)))
                    (list)
                    energy-c1
                    (lambda (lomp target) ;energy
                      (let ([mp-size (att-value 'value-of lomp 'size)]
                            [deployed-kind (ast-child 'type target)])
                        (if (eq? deployed-kind Cubieboard)
                            (* 10 (log mp-size))
                            (* 2 mp-size))))
                    rt-C1 (lambda _ 0.4) ;response-time
                    'dynamic-mode-1b)))
                 (list) ;reqcomps
                 #f #f))] ;deployedon + selectedmode
         [c1-impl1c
          (create-ast
           'Impl
           (list 'Useless-Impl1c
                 (create-ast-list
                  (list
                   (make-simple-mode
                    (lambda _ 0) ;propload
                    (list (create-ast 'ReqClause (list rt-C2 comp-max-eq (lambda _ -1))))
                    energy-c1 (lambda _ 100) ;energy
                    rt-C1 (lambda _ 0.2) ;response-time
                    'default-mode-1c)))
                 (list comp2) #f #f))]
         [comp1
          (create-ast
           'Comp
           (list
            'Example-Component ;name of Comp
            (create-ast-list (list c1-impl1a c1-impl1b c1-impl1c)) ;Impl*
            c1-impl1a (create-ast-list (list rt-C1 energy-c1))))]) ;selectedimpl Property*
      (create-ast
       'Root
       (list
        (create-ast 'HWRoot (list
                             (create-ast-list (list Cubieboard))
                             (create-ast-list (list cubie1 cubie2))))
        (create-ast 'SWRoot (list (create-ast-list (list comp1 comp2))))
        (create-ast
         'Request
         (list
          (create-ast-list (list (make-mp-size 50))) ;MetaParameter*
          comp1
          (create-ast-list (list (create-ast 'ReqClause (list rt-C1 comp-max-eq (lambda _ 0.3)))))
          #f)))))))
 
 (define comp1 (ast-child 1 (ast-child 'Comp* (ast-child 'SWRoot example-ast))))
 (define impl1a (ast-child 1 (ast-child 'Impl* comp1)))
 (define impl1b (ast-child 2 (ast-child 'Impl* comp1)))
 (define impl1c (ast-child 3 (ast-child 'Impl* comp1)))
 (define comp2 (car (ast-child 'reqcomps impl1a)))
 (define impl2a (ast-child 1 (ast-child 'Impl* comp2)))
 (define cb1 (ast-child 1 (ast-child 'Resource* (ast-child 'HWRoot example-ast))))
 (define cb2 (ast-child 2 (ast-child 'Resource* (ast-child 'HWRoot example-ast)))))
