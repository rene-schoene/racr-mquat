#!r6rs

(library
 (mquat ast-generation)
 (export create-hw create-sw create-system rand
         freq-name load-name mp-name prop make-prov make-req)
 (import (rnrs) (racr core) (srfi :27)
         (mquat constants) (mquat ast) (mquat main) (mquat utils))
 
 ;;; Reusable nodes and names
 (define load-name 'load)
 (define freq-name 'frequency)
 (define load #f) (define freq #f)
 
 (define mp-name 'size)
 
 ;;; Utility definitions
 (define types-al (list))
 (define (make-type nr) (:ResourceType mquat-spec (node-name 'type (list nr)) (list load freq)))
 (define (type nr)
   (let ([entry (assq nr types-al)])
     (debug nr entry)
     (if entry (cdr entry) (let ([new (make-type (length types-al))])
                             (set! types-al (cons (cons nr new) types-al)) new))))
 (define prop-al (list)) (define last-comp-nr #f) (define last-comp #f)
 (define (new-comp comp comp-nr) (set! last-comp-nr comp-nr) (set! last-comp comp))
 (define (make-sw-prop n) (list (:Property mquat-spec (node-name 'prop (list n)) 'u 'runtime 'increasing 'max)
                                (:Property mquat-spec pn-energy 'J 'runtime 'decreasing 'sum)))
 (define (node-name ident lon) (string->symbol (fold-right (lambda (n s) (string-append s "-" (number->string n)))
                                                                     (symbol->string ident) lon)))
 (define (prop n)
   (let ([entry (assq n prop-al)])
     (if entry (cdr entry) (let ([new (make-sw-prop n)]) (set! prop-al (cons (cons n new) prop-al)) new))))
 
 (define (call-n-times proc n) (letrec ([cnt (lambda (n) (if (>= 0 n) (list) (cons (proc n) (cnt (- n 1)))))]) (cnt n)))
 (define (random n) (random-integer n)) ;find a suitable impl
 (define (rand max digits offset) (let ([factor (expt 10 digits)])
                                    (lambda _ (inexact (+ offset (/ (random (* factor max)) factor))))))
 
 (define (something v) v)
 (define (make-prov property comparator value) (:ProvClause mquat-spec property comparator value))
 (define (make-req property comparator value) (:ReqClause mquat-spec property comparator value))
 
 ;;; AST-Generation
 (define (default-hw-clause-gen property-name)
   (cond
     ((eq? property-name load-name) (list make-prov comp-eq (rand 1 3 0))) ; load = 0.001 - 1.000
     ((eq? property-name freq-name) (list make-prov comp-eq (rand 500 2 500))) ; freq = 500.01 - 1000.00
     (else (error "default-hw-clause-gen" "Wrong property" property-name))))

 (define (default-sw-clause-gen property-name comp-nr)
   (let ([ps (prop comp-nr)]
         [comp-nr-1 (+ comp-nr 1)])
     (cond
       ((eq? property-name load-name) (list make-req comp-max-eq (rand 1 2 0)))
       ((eq? property-name freq-name) (list make-req comp-max-eq (rand 480 1 510)))
       ((eq? property-name (->name (car ps))) (list make-prov comp-eq (rand comp-nr 3 0)))
       ((eq? property-name pn-energy) (list make-prov comp-eq (rand 100 3 0)))
       ((eq? property-name (->name (car (prop comp-nr-1)))) (list make-req comp-max-eq (rand comp-nr-1 2 0)))
       (else (error "default-sw-clause-gen" "no suitable property" property-name (->name (car (prop comp-nr-1))) comp-nr)))))

 (define (create-hw-clause udfs name property)
   (let ([f (create-clause udfs default-hw-clause-gen name)])
     (if f (let ([args (f (->name property))])
             (if (eq? args #t) (set! args (default-hw-clause-gen (->name property))))
             (if args ((car args) property (cadr args) (caddr args)) #f))
         #f)))
 (define (create-sw-clause udfs name comp-nr property)
   (let ([f (create-clause udfs default-sw-clause-gen name)])
     (if f (let ([args (f (->name property) comp-nr)])
             (if (eq? args #t) (set! args (default-sw-clause-gen (->name property) comp-nr)))
             (if args ((car args) property (cadr args) (caddr args)) #f))
         #f)))
 ; return a proc or #f
 (define (create-clause udfs default-fun name)
   (let ([udf? (udfs name)])
     (cond
       ((eq? #t udf?) default-fun) ; not in list, or marked as default
       ((not udf?) #f) ; marked as remove
       (else udf?)))) ; apply user-defined function
 
 ; udfs: function (res-name → function (property → clause)). ud-types: function (res-name → nr of res-type).
 ; Returns the HWRoot
 (define (create-hw num-pe num-subs ud-clauses ud-types)
   (with-specification
    mquat-spec
    (let* ([res-name (lambda (outer-id n) (string->symbol (string-append (symbol->string outer-id) "-" (number->string n))))]
           [type-nr? (ud-types res-name)])
      (letrec ([make-subs (lambda (outer-id total subs)
                            ;(debug '~make-subs total subs)
                            (call-n-times (lambda (sub-n)
                                            (make-res (res-name outer-id sub-n)
                                                      (floor (/ (- total 1) subs)) subs)) (min total subs)))]
               [make-res
                (lambda (id total subs)
                  ;(debug id total subs)
                  (:Resource mquat-spec
                             id (if type-nr? (type type-nr?) (type 0)) (make-subs id total subs)
                             (filter something (list (create-hw-clause ud-clauses id load)
                                                     (create-hw-clause ud-clauses id freq)))))])
        (let ([subs (make-subs 'res num-pe (if (= 0 num-subs) num-pe num-subs))])
          (:HWRoot mquat-spec (map cdr types-al) subs))))))
 
 ; udfs: function (mode-name → function (property → clause))
 ; returns the SWRoot
 (define (create-sw num-comp impl-per-comp mode-per-impl ud-clauses reqc)
   (let* ([make-mode (lambda (comp impl-lon mode req?)
                       (let* ([ps (prop comp)]
                              [name (node-name 'mode (cons mode impl-lon))]
                              [cls (filter something (list (create-sw-clause ud-clauses name comp load)
                                                           (create-sw-clause ud-clauses name comp freq)
                                                           (create-sw-clause ud-clauses name comp (car ps))
                                                           (create-sw-clause ud-clauses name comp (cadr ps))))]
                              [req-cl? (and req? (create-sw-clause ud-clauses name comp (car (prop last-comp-nr))))])
                         (debug name cls req-cl?)
                         (:Mode mquat-spec name (if req-cl? (cons req-cl? cls) cls))))]
          [make-impl (lambda (comp impl)
                       (let* ([lon (list impl comp)]
                              [name (node-name 'impl lon)]
                              [does-req? (and last-comp-nr (reqc name))])
                         (:Impl
                          mquat-spec name
                          (call-n-times (lambda (mode) (make-mode comp lon mode does-req?)) mode-per-impl) ; Mode*
                          (if does-req? (list last-comp) (list)) ; reqcomps
                          #f #f)))]
          [make-comp (lambda (comp) (let ([lon (list comp)])
                                      ;(debug lon comp)
                                      (:Comp
                                       mquat-spec (node-name 'comp lon)
                                       (call-n-times (lambda (impl) (make-impl comp impl)) impl-per-comp) #f
                                       (prop comp))))]
          [sw-root (:SWRoot
                    mquat-spec (call-n-times (lambda (n) (let ([comp (make-comp n)]) (new-comp comp n) comp)) num-comp))])
     sw-root))
 
 (define (create-request)
   (let* ([make-req (lambda (p max digits offset) (:ReqClause mquat-spec p comp-min-eq (rand max digits offset)))]
          [target-property (car (prop last-comp-nr))]
          [target (<<- target-property)])
     (:Request
      mquat-spec
      (list (:MetaParameter mquat-spec mp-name (rand 100 2 0)))
      target (list (make-req target-property 1 2 0)) #f)))
 
 ; Creates a new system.
 ; num-pe:        total number of resources
 ; num-pe-subs:   number of subresources for every resource (use zero for flat layout)
 ; num-comp:      total number of components
 ; impl-per-comp: number of implementations per component
 ; mode-per-impl: number of modes per implementation
 ; [opts:         a list with (sw-reqc, ud-sw-clauses, ud-hw-clauses, ud-types). Each element eq? #f uses default behavior.]
 ;  sw-reqc:      a function, given a impl name, returns, whether this impl should require the previous created component
 ;  ud-sw-clauses:   a function, given a name of a resource, returns a clause-function
 ;  ud-hw-clauses:   a function, given a name of a mode, returns a clause-function
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
   (set! types-al (list)) (set! prop-al (list))
   (set! load (:Property mquat-spec load-name '% 'runtime 'decreasing agg-sum))
   (set! freq (:Property mquat-spec freq-name 'MHz 'runtime 'increasing agg-max))
   (:Root mquat-spec (create-hw num-pe num-pe-subs ud-hw-clauses ud-types)
          (create-sw num-comp impl-per-comp mode-per-impl ud-sw-clauses sw-reqc) (create-request))))
