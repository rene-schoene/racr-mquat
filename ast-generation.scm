#!r6rs

(library
 (mquat ast-generation)
 (export create-hw create-sw create-example-ast rand)
 (import (rnrs) (racr core) (srfi :27)
         (mquat constants))
 
 ;;; AST-Generation
 
 (define (call-n-times proc n)
   (letrec ([cnt (lambda (n) (if (>= 0 n) (list) (cons (proc n) (cnt (- n 1)))))])
     (create-ast-list (cnt n))))
 (define (random n) (random-integer n)) ;find a suitable impl
 (define (rand max digits offset) (let ([factor (expt 10 digits)])
                                    (lambda _ (inexact (+ offset (/ (random (* factor max)) factor))))))
 
 ; Returns the (load freq HWRoot)
 (define (create-hw mquat-spec num-pe num-subs)
   (with-specification
    mquat-spec
    (let* ([load (create-ast 'Property (list 'load '% 'runtime 'decreasing agg-sum))]
           [freq (create-ast 'Property (list 'frequency 'MHz 'runtime 'increasing agg-max))]
           [only-type (create-ast 'ResourceType (list 'theType (create-ast-list (list load freq))))]
           [res-name (lambda (outer-id n) (string->symbol (string-append (symbol->string outer-id) "-" (number->string n))))]
           [make-prov (lambda (p max digits offset)
                        (create-ast 'ProvClause (list p comp-eq (rand max digits offset))))])
      (letrec ([make-subs (lambda (outer-id total subs)
                            ;(debug '~make-subs total subs)
                            (call-n-times (lambda (sub-n)
                                            (make-res (res-name outer-id sub-n)
                                                      (floor (/ (- total 1) subs)) subs)) (min total subs)))]
               [make-res
                (lambda (id total subs)
                  ;(debug id total subs)
                  (create-ast 'Resource
                              (list id only-type (make-subs id total subs)
                                    (create-ast-list (list (make-prov load 1 3 0) ; load = 0.001 - 1.000
                                                           (make-prov freq 500 2 500))))))]) ; freq = 500.01 - 1000.00
        (list
         load freq
         (create-ast
          'HWRoot
          (list
           (create-ast-list (list only-type))
           (make-subs 'res num-pe (if (= 0 num-subs) num-pe num-subs)))))))))
 
 ; returns (mp-names prop-first-comp SWRoot)
 (define (create-sw mquat-spec load freq num-comp impl-per-comp mode-per-impl)
   (define prop-al (list)) (define last-comp-nr #f) (define last-comp #f) (define first-comp #f) (define mp-name 'size)
   (define (new-comp comp comp-nr) (set! last-comp-nr comp-nr) (set! last-comp comp)
     (unless first-comp (set! first-comp comp-nr)))
   (with-specification
    mquat-spec
    (let* ([node-name (lambda (outer-id n) (string->symbol (string-append (symbol->string outer-id) "-" (number->string n))))]
           [make-sw-prop (lambda (n) (list (create-ast 'Property (list (node-name 'prop- n) 'u 'runtime 'increasing 'max))
                                           (create-ast 'Property (list pn-energy 'J 'runtime 'decreasing 'sum))))]
           [prop (lambda (n) (let ([entry (assq n prop-al)])
                               (if entry (cdr entry) ;entry found, return it
                                   (let ([new (make-sw-prop n)]) (set! prop-al (cons (cons n new) prop-al)) new))))] ;new entry
           [make-req (lambda (p max digits offset) (create-ast 'ReqClause (list p comp-min-eq (rand max digits offset))))]
           [make-prov (lambda (p max digits offset) (create-ast 'ProvClause (list p comp-eq (rand max digits offset))))]
           [make-mode (lambda (comp impl mode) (let* ([ps (prop comp)]
                                                      [cls (list (make-req load 1 2 0)
                                                                 (make-req freq 480 1 510)
                                                                 (make-prov (car ps) comp 3 0) ;sw-property
                                                                 (make-prov (cadr ps) 100 3 0))]) ;energy
                                                 (create-ast
                                                  'Mode
                                                  (list
                                                   (node-name impl mode)
                                                   (create-ast-list
                                                    (if (or (eq? last-comp-nr #f)
                                                            (> (random 2) 1))
                                                        cls
                                                        (cons (make-req (car (prop (- last-comp-nr 1))) last-comp-nr 2 0) cls)))))))]
           [make-impl (lambda (comp comp-name impl) (let ([name (node-name comp-name impl)])
                                                      (create-ast
                                                       'Impl
                                                       (list
                                                        name
                                                        (call-n-times (lambda (mode) (make-mode comp name mode)) mode-per-impl) ; Mode*
                                                        (if (eq? last-comp-nr #f) (list) (list last-comp)) ; reqcomps
                                                        #f #f))))]
           [make-comp (lambda (comp) (let ([name (node-name 'comp comp)])
                                       (create-ast
                                        'Comp
                                        (list
                                         name
                                         (call-n-times (lambda (impl) (make-impl comp name impl)) impl-per-comp) #f
                                         (create-ast-list (prop comp))))))]
           [sw-root (create-ast
                     'SWRoot
                     (list (call-n-times (lambda (n) (let ([comp (make-comp n)]) (new-comp comp n) comp)) num-comp)))])
      (list (list mp-name) (car (prop first-comp)) sw-root))))
 
 (define (create-request mquat-spec mp-names prop)
   (with-specification
    mquat-spec
    (let ([make-req (lambda (p max digits offset) (create-ast 'ReqClause (list p comp-min-eq (rand max digits offset))))]
          [target (ast-parent (ast-parent prop))])
      (create-ast
       'Request
       (list
        (create-ast-list (map (lambda (mp-name) (create-ast 'MetaParameter (list mp-name (rand 100 2 0)))) mp-names))
        target (create-ast-list (list (make-req prop 1 2 0))) #f)))))
 
 (define (create-example-ast mquat-spec num-pe num-pe-subs num-comp impl-per-comp mode-per-impl)
   (let* ([hw-result (create-hw mquat-spec num-pe num-pe-subs)]
          [load (car hw-result)]
          [freq (cadr hw-result)]
          [hw-root (caddr hw-result)]
          [sw-result (create-sw mquat-spec load freq num-comp impl-per-comp mode-per-impl)]
          [mp-names (car sw-result)]
          [prop-c1 (cadr sw-result)]
          [sw-root (caddr sw-result)])
     (create-ast
      mquat-spec
      'Root
      (list hw-root
            sw-root
            (create-request mquat-spec mp-names prop-c1))))))
