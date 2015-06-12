#!r6rs

(library
 (mquat ast-generation)
 (export create-hw create-sw create-system rand
         freq-name load-name mp-name node-name make-prov make-req)
 (import (rnrs) (racr core) (srfi :27)
         (mquat constants) (mquat ast) (mquat basic-ag) (mquat join) (mquat utils))
 
 ;;; Reusable nodes and names
 (define load-name 'load)
 (define freq-name 'frequency)
 (define load #f) (define freq #f)
 
 (define mp-name 'size)
 
 ;;; Utility definitions
 (define (node-name ident lon) (string->symbol (fold-right (lambda (n s) (string-append s "-" (number->string n)))
                                                           (symbol->string ident) lon)))
 
 (define (call-n-times proc n) (letrec ([cnt (lambda (n) (if (>= 0 n) (list) (cons (proc n) (cnt (- n 1)))))]) (cnt n)))
 (define (random n) (random-integer n))
 (define (rand maxVal digits offset) (let* ([factor (expt 10 digits)]
                                            [val (+ offset (inexact (/ (random (* factor maxVal)) factor)))])
                                       (lambda _ val)))
 
 (define (something v) v)
 (define (make-prov property comparator value) (:ProvClause mquat-spec property comparator value))
 (define (make-req property comparator value) (:ReqClause mquat-spec property comparator value))
 
 ; return a proc or #f
 (define (create-clause udfs default-fun name)
   (let ([udf? (udfs name)])
     (cond
       ((eq? #t udf?) default-fun) ; not in list, or marked as default
       ((not udf?) #f) ; marked as remove
       (else udf?)))) ; apply user-defined function
 
 ; udfs: function (res-name → function (property → clause)). ud-types: function (res-name → nr of res-type).
 ; Returns the HWRoot
 (define (create-hw load freq num-pe num-subs ud-clauses ud-types)
   (define types-al (list))
   (define (make-type nr) (:ResourceType mquat-spec (node-name 'type (list nr))
                                         (list (:PropertyRef mquat-spec load) (:PropertyRef mquat-spec freq))))
   (define (type nr)
     (let ([entry (assq nr types-al)])
       (if entry (cdr entry) (let ([new (make-type nr)]) (set! types-al (cons (cons nr new) types-al)) new))))
   (define (create-hw-clause udfs name property)
     (let ([f (create-clause udfs default-hw-clause-gen name)])
       (if f (let ([args (f (->name property))])
               (if (eq? args #t) (set! args (default-hw-clause-gen (->name property))))
               (if args ((car args) property (cadr args) (caddr args)) #f))
           #f)))
   (define (default-hw-clause-gen property-name)
     (cond
       ((eq? property-name load-name) (list make-prov comp-eq (rand 50 2 0))) ; provided load = [0.01, 50.00]
       ((eq? property-name freq-name) (list make-prov comp-eq (rand 1000 2 500))) ; provided freq = [500.01 - 1000.00]
       (else (error "default-hw-clause-gen" "Wrong property" property-name))))
   (letrec ([make-subs (lambda (outer-id total subs)
                         (call-n-times (lambda (sub-n)
                                         (make-res (node-name outer-id (list sub-n))
                                                   (floor (/ (- total 1) subs)) subs)) (min total subs)))]
            [make-res
             (lambda (id total subs)
               (let ([type-nr? (ud-types id)])
                 (:Resource mquat-spec
                            id (type (if type-nr? type-nr? 0)) (make-subs id total subs)
                            (filter something (list (create-hw-clause ud-clauses id load)
                                                    (create-hw-clause ud-clauses id freq))))))])
     (let ([subs (make-subs 'r num-pe (if (= 0 num-subs) num-pe num-subs))])
       (:HWRoot mquat-spec (map cdr types-al) subs (list load freq)))))
 
 ; udfs: function (mode-name → function (property → clause))
 ; returns (cons SWRoot target-property)
 (define (create-sw load freq num-comp impl-per-comp mode-per-impl ud-clauses reqc)
   (define last-comp-nr #f) (define last-comp #f)
   (define (new-comp comp comp-nr) (set! last-comp-nr comp-nr) (set! last-comp comp))
   (define energy (:RealProperty mquat-spec pn-energy 'J 'runtime 'decreasing 'sum))
   (define prop-al (list))
   (define (make-sw-prop n) (list (:RealProperty mquat-spec (node-name 'prop (list n)) 'u 'runtime 'increasing 'max)
                                  (:PropertyRef mquat-spec energy)))
   (define (prop n)
     (let ([entry (assq n prop-al)])
       (if entry (cdr entry) (let ([new (make-sw-prop n)]) (set! prop-al (cons (cons n new) prop-al)) new))))
   (define (make-mode comp impl-lon mode req? valid!?)
     (let* ([valid!? (and valid!? (= 1 mode))]
            [default-sw-clause-gen
              (lambda (property-name comp-nr)
                (let ([ps (prop comp-nr)]
                      [comp-nr+1 (+ comp-nr 1)])
                  (cond
                    ; required load max [50.01, 100.00] if valid!?, [0.01 - 100.00] otherwise
                    ((eq? property-name load-name) (list make-req comp-max-eq (if valid!? (rand 100 2 50) (rand 100 2 0))))
                    ; required freq min [0.01, 500.00]
                    ((eq? property-name freq-name) (list make-req comp-min-eq (rand 500 2 0)))
                    ; provided prop-n = [5.01 - 10.00] if valid!?, [0.01 - 10.00] otherwise
                    ((eq? property-name (->name (car ps))) (list make-prov comp-eq (if valid!? (rand 10 2 5) (rand 10 2 0))))
                    ; provided energy = [0.001 - 100.000]
                    ((eq? property-name pn-energy) (list make-prov comp-eq (rand 100 3 0)))
                    ; required prop-m max [10.01, 20.00]
                    ((eq? property-name (->name (car (prop comp-nr+1)))) (list make-req comp-max-eq (rand 20 2 10)))
                    (else (error "default-sw-clause-gen" "no suitable property"
                                 property-name (->name (car (prop comp-nr+1))) comp-nr)))))]
            [create-sw-clause
             (lambda (udfs name comp-nr property)
               (let ([f (create-clause udfs default-sw-clause-gen name)]
                     [real-property (=real property)])
                 (if f (let ([args (f (->name real-property) comp-nr)])
                         (if (eq? args #t) (set! args (default-sw-clause-gen (->name real-property) comp-nr)))
                         (if args ((car args) real-property (cadr args) (caddr args)) #f)) #f)))]
            [ps (prop comp)]
            [name (node-name 'm (cons mode impl-lon))]
            [cls (filter something (list (create-sw-clause ud-clauses name comp load)
                                         (create-sw-clause ud-clauses name comp freq)
                                         (create-sw-clause ud-clauses name comp (car ps))
                                         (create-sw-clause ud-clauses name comp (cadr ps))))]
            [req-cl? (and req? (create-sw-clause ud-clauses name comp (car (prop last-comp-nr))))])
       (:Mode mquat-spec name (if req-cl? (cons req-cl? cls) cls))))
   (define (make-impl comp impl)
     (let* ([lon (list impl comp)]
            [name (node-name 'i lon)]
            [does-req? (and last-comp-nr (reqc name))])
       (:Impl mquat-spec name
              (call-n-times (lambda (mode) (make-mode comp lon mode does-req? (= 1 impl))) mode-per-impl) ; Mode*
              (if does-req? (list last-comp) (list)) #f #f))) ; reqcomps selected-mode deployed-on
   (define (make-comp comp) (:Comp mquat-spec (node-name 'c (list comp))
                                   (call-n-times (lambda (impl) (make-impl comp impl)) impl-per-comp) #f
                                   (prop comp)))
   (let ([sw-root (:SWRoot
                   mquat-spec (call-n-times (lambda (n) (let ([comp (make-comp n)]) (new-comp comp n) comp)) num-comp)
                   (list energy))])
     (cons sw-root (car (prop last-comp-nr)))))
 
 (define (create-request target-property)
   (let* ([make-req (lambda (p maxVal digits offset) (:ReqClause mquat-spec p comp-min-eq (rand maxVal digits offset)))]
          [target (<<- target-property)])
     (:Request
      mquat-spec
      (list (:MetaParameter mquat-spec mp-name ((rand 100 2 0))))
      target (list (make-req target-property 1 2 0)) #f)))
 
 ; Creates a new system.
 ; num-pe:        total number of resources
 ; num-pe-subs:   number of subresources for every resource (use zero for flat layout)
 ; num-comp:      total number of components
 ; impl-per-comp: number of implementations per component
 ; mode-per-impl: number of modes per implementation
 ; [opts:         a list with (sw-reqc, ud-sw-clauses, ud-hw-clauses, ud-types). Each element eq? #f uses default behavior.]
 ;  sw-reqc:      a function, given a impl name, returns, whether this impl should require the previous created component
 ;  ud-sw-clauses:   a function, given a name of a mode, returns a clause-function
 ;  ud-hw-clauses:   a function, given a name of a resource, returns a clause-function
 ;   clause-function: a function given a name of the property, returns either #t (use default),
 ;    #f (do not include this property), or a list of (make-{prov|req} comparator value-function) (which is used instead)
 ;  ud-types:     a function, given a name of a resource, returns either #f, or the number of the resource type
 ;   #f means 0 as default value.
 (define create-system
   (case-lambda
     [(num-pe num-pe-subs num-comp impl-per-comp mode-per-impl)
      (create-system num-pe num-pe-subs num-comp impl-per-comp mode-per-impl (list #f #f #f #f))]
     [(num-pe num-pe-subs num-comp impl-per-comp mode-per-impl opts)
      (let ([d (lambda (default elem) (if elem elem default))])
        (create-system1 num-pe num-pe-subs num-comp impl-per-comp mode-per-impl
                        (d (lambda _ (> (random 2) 1)) (car opts)) ;sw-reqc
                        (d (lambda _ #t) (cadr opts)) ;ud-sw-clauses
                        (d (lambda _ #t) (caddr opts)) ;ud-hw-clauses
                        (d (lambda _ #f) (cadddr opts))))])) ;ud-types

 (define (create-system1 num-pe num-pe-subs num-comp impl-per-comp mode-per-impl sw-reqc ud-sw-clauses ud-hw-clauses ud-types)
   (let* ([types-al (list)]
          [prop-al (list)]
          [load (:RealProperty mquat-spec load-name '% 'runtime 'decreasing agg-sum)]
          [freq (:RealProperty mquat-spec freq-name 'MHz 'runtime 'increasing agg-max)]
          [sw-result (create-sw load freq num-comp impl-per-comp mode-per-impl ud-sw-clauses sw-reqc)]
          [sw-root (car sw-result)]
          [target-property (cdr sw-result)])
     (:Root mquat-spec (create-hw load freq num-pe num-pe-subs ud-hw-clauses ud-types)
            sw-root (create-request target-property)))))
