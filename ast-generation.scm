#!r6rs

(library
 (mquat ast-generation)
 (export create-hw create-sw create-system rand
         set!no-frequency)
 (import (rnrs) (racr core) (srfi :27)
         (mquat constants) (mquat ast) (mquat main) (mquat utils))
 
 ;;; AST-Generation
 
 (define (call-n-times proc n) (letrec ([cnt (lambda (n) (if (>= 0 n) (list) (cons (proc n) (cnt (- n 1)))))]) (cnt n)))
 (define (random n) (random-integer n)) ;find a suitable impl
 (define (rand max digits offset) (let ([factor (expt 10 digits)])
                                    (lambda _ (inexact (+ offset (/ (random (* factor max)) factor))))))
 
 (define frequency? #t)
 (define (set!no-frequency) (set! frequency? #f))
 
 (define (f? clauses) (if frequency? clauses
                          (filter (lambda (clause) (not (eq? 'frequency (->name (->return-type clause))))) clauses)))
 ; Returns the (load freq HWRoot)
 (define (create-hw num-pe num-subs)
   (with-specification
    mquat-spec
    (let* ([load (:Property mquat-spec 'load '% 'runtime 'decreasing agg-sum)]
           [freq (:Property mquat-spec 'frequency 'MHz 'runtime 'increasing agg-max)]
           [only-type (:ResourceType mquat-spec 'theType (list load freq))]
           [res-name (lambda (outer-id n) (string->symbol (string-append (symbol->string outer-id) "-" (number->string n))))]
           [make-prov (lambda (p max digits offset)
                        (:ProvClause mquat-spec p comp-eq (rand max digits offset)))])
      (letrec ([make-subs (lambda (outer-id total subs)
                            ;(debug '~make-subs total subs)
                            (call-n-times (lambda (sub-n)
                                            (make-res (res-name outer-id sub-n)
                                                      (floor (/ (- total 1) subs)) subs)) (min total subs)))]
               [make-res
                (lambda (id total subs)
                  ;(debug id total subs)
                  (:Resource mquat-spec
                             id only-type (make-subs id total subs)
                             (f? (list (make-prov load 1 3 0) ; load = 0.001 - 1.000
                                       (make-prov freq 500 2 500)))))]) ; freq = 500.01 - 1000.00
        (list load freq (:HWRoot mquat-spec (list only-type) (make-subs 'res num-pe (if (= 0 num-subs) num-pe num-subs))))))))
 
 ; returns (mp-names prop-first-comp SWRoot)
 (define (create-sw load freq num-comp impl-per-comp mode-per-impl reqc)
   (define prop-al (list)) (define last-comp-nr #f) (define last-comp #f) (define mp-name 'size)
   (define (new-comp comp comp-nr) (set! last-comp-nr comp-nr) (set! last-comp comp))
   (let* ([node-name (lambda (ident lon) (string->symbol (fold-right (lambda (n s) (string-append s "-" (number->string n)))
                                                                     (symbol->string ident) lon)))]
          [make-sw-prop (lambda (n) (list (:Property mquat-spec (node-name 'prop (list n)) 'u 'runtime 'increasing 'max)
                                          (:Property mquat-spec pn-energy 'J 'runtime 'decreasing 'sum)))]
          [prop (lambda (n) (let ([entry (assq n prop-al)])
                              (if entry (cdr entry) ;entry found, return it
                                  (let ([new (make-sw-prop n)]) (set! prop-al (cons (cons n new) prop-al)) new))))] ;new entry
          [make-req (lambda (p max digits offset) (:ReqClause mquat-spec p comp-max-eq (rand max digits offset)))]
          [make-prov (lambda (p max digits offset) (:ProvClause mquat-spec p comp-eq (rand max digits offset)))]
          [make-mode (lambda (comp impl-lon mode req?)
                       (let* ([ps (prop comp)]
                              [name (node-name 'mode (cons mode impl-lon))]
                              [cls (f? (list (make-req load 1 2 0)
                                             (make-req freq 480 1 510)
                                             (make-prov (car ps) comp 3 0) ;sw-property
                                             (make-prov (cadr ps) 100 3 0)))]) ;energy
                         (:Mode mquat-spec name (if req? (cons (make-req (car (prop last-comp-nr)) last-comp-nr 2 0) cls) cls))))]
          [make-impl (lambda (comp impl)
                       (let* ([lon (list impl comp)]
                              [name (node-name 'impl lon)]
                              [req? (and last-comp-nr (if reqc (reqc name) (> (random 2) 1)))])
                         (:Impl
                          mquat-spec name
                          (call-n-times (lambda (mode) (make-mode comp lon mode req?)) mode-per-impl) ; Mode*
                          (if req? (list last-comp) (list)) ; reqcomps
                          #f #f)))]
          [make-comp (lambda (comp) (let ([lon (list comp)])
                                      (:Comp
                                       mquat-spec (node-name 'comp lon)
                                       (call-n-times (lambda (impl) (make-impl comp impl)) impl-per-comp) #f
                                       (prop comp))))]
          [sw-root (:SWRoot
                    mquat-spec (call-n-times (lambda (n) (let ([comp (make-comp n)]) (new-comp comp n) comp)) num-comp))])
     (list (list mp-name) (car (prop last-comp-nr)) sw-root)))
 
 (define (create-request mp-names prop)
   (let ([make-req (lambda (p max digits offset) (:ReqClause mquat-spec p comp-min-eq (rand max digits offset)))]
         [target (<<- prop)])
     (:Request
      mquat-spec
      (map (lambda (mp-name) (:MetaParameter mquat-spec mp-name (rand 100 2 0))) mp-names)
      target (list (make-req prop 1 2 0)) #f)))
 
 ; Creates a new system.
 ; num-pe:        total number of resources
 ; num-pe-subs:   number of subresources for every resource (use zero for flat layout)
 ; num-comp:      total number of components
 ; impl-per-comp: number of implementations per component
 ; mode-per-impl: number of modes per implementation
 ; [sw-reqc:      a function, given a impl name, returns, whether this impl should require the previous created component]
 (define create-system
   (case-lambda
     [(num-pe num-pe-subs num-comp impl-per-comp mode-per-impl)
      (create-system1 num-pe num-pe-subs num-comp impl-per-comp mode-per-impl #f)]
     [(num-pe num-pe-subs num-comp impl-per-comp mode-per-impl sw-reqc)
      (create-system1 num-pe num-pe-subs num-comp impl-per-comp mode-per-impl sw-reqc)]))

 (define (create-system1 num-pe num-pe-subs num-comp impl-per-comp mode-per-impl sw-reqc)
   (let* ([hw-result (create-hw num-pe num-pe-subs)]
          [load (car hw-result)]
          [freq (cadr hw-result)]
          [hw-root (caddr hw-result)]
          [sw-result (create-sw load freq num-comp impl-per-comp mode-per-impl sw-reqc)]
          [mp-names (car sw-result)]
          [prop-c1 (cadr sw-result)]
          [sw-root (caddr sw-result)])
     (:Root mquat-spec hw-root sw-root (create-request mp-names prop-c1)))))
