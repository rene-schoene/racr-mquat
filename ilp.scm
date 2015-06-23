#!r6rs

(library
 (mquat ilp)
 (export add-ilp-ags save-ilp make-ilp =to-ilp =ilp-eval-binvar)
 (import (rnrs) (racr core) (srfi :19) (prefix (only (srfi :13) string-map) srfi:)
         (mquat properties) (mquat constants) (mquat utils) (mquat ast) (mquat basic-ag))
 
 (define (=check-model n)               (att-value 'check-model n))
 (define (=to-ilp n)                    (att-value 'to-ilp n))
 (define (=ilp-name n)                  (att-value 'ilp-name n))
 (define (=ilp-binvar n)                (att-value 'ilp-binvar n))
 (define (=ilp-binvar-deployed n pe)    (debug "ibd" n pe) (att-value 'ilp-binvar-deployed n pe))
 (define (=ilp-binary-vars n)           (att-value 'ilp-binary-vars n))
 (define =ilp-objective
   (case-lambda ((n)                    (att-value 'ilp-objective n))
                ((n pe)                 (att-value 'ilp-objective n pe))))
 (define (=ilp-nego n)                  (att-value 'ilp-nego n))
 (define (=ilp-nego-sw n)               (att-value 'ilp-nego-sw n))
 (define (=every-clause-of n)           (att-value 'every-clause-of n))
 (define =ilp-nego-reqc
   (case-lambda ((n comp)               (att-value 'ilp-nego-reqc n comp))
                ((n clausetype comp)    (att-value 'ilp-nego-reqc n clausetype comp))))
 (define (=ilp-nego-hw n)               (att-value 'ilp-nego-hw n))
 (define (=req-hw-properties n)         (att-value 'required-hw-properties n))
 (define (=req-hw-clauses n)            (att-value 'required-hw-clauses n))
 (define (=every-clause n)              (att-value 'every-clause n))
 (define (=ilp-nego-hw0 n comp prop pe) (att-value 'ilp-nego-hw0 n comp prop pe))
 (define (=ilp-eval-binvar n pe)        (att-value 'ilp-eval-binvar n pe))
 
 (define prepend-sign (lambda (val) (if (< val 0) val (string-append "+ " (number->string val)))))
 ; TODO make bidirectional mapping: {_ - +} -> {_0 _1 _2}
 (define subs (list (list #\- #\_) (list #\+ #\_)))
 (define (ilp-conform-name name) (srfi:string-map (lambda (c) (let ([entry (assq c subs)]) (if entry (cadr entry) c))) name))
 
 (define (comp-name comp) (ilp-conform-name (comp->name comp)))
 (define (make-constraints provs max-reqs min-reqs request?)
   (fold-left ; fold over provisions
    (lambda (constraints prov-entry)
      (let ([max-req-entry (assq (car prov-entry) max-reqs)]
            [min-req-entry (assq (car prov-entry) min-reqs)])
        (cons-if (and max-req-entry (make-constraint (car prov-entry) (cadr prov-entry)
                                                     (cadr max-req-entry) comp-max-eq request?))
                 (cons-if (and min-req-entry (make-constraint (car prov-entry) (cadr prov-entry)
                                                              (cadr min-req-entry) comp-min-eq request?))
                          constraints)))) (list) provs))
 
 (define (make-constraint prov prov-entry req-entry comp request?)
   (if request?
       (append
        (list (string-append "request(" (=ilp-name prov) "_" (comp-name comp) "): "))
        (fold-left (lambda (constraint pair) (cons* (prepend-sign (car pair)) (cadr pair) constraint)) (list) prov-entry)
        (cons* (comp->rev-string comp) (car req-entry)))
       (let ([f-prov (if (eq? comp comp-max-eq)
                         ; prov for max: (maximum - val)
                         (lambda (constraint val name) (cons* (prepend-sign (- (=maximum prov) val)) name constraint))
                         (lambda (constraint val name) (cons* (prepend-sign val) name constraint)))] ; prov for other: val
             [f-req (if (eq? comp comp-max-eq)
                        ; req for max: - (maximum - val) = val - maximum
                        (lambda (constraint val name) (cons* (prepend-sign (- val (=maximum prov))) name constraint))
                        (lambda (constraint val name) (cons* (prepend-sign (- val)) name constraint)))]) ; req for other: -val
;         (debug "mc: prov-entry:" prov-entry ",req-entry:" req-entry ",maximum:" maximum ",name:" prov)
         (append
          (list (string-append (=ilp-name prov) "_" (comp-name comp) ": "))
          (fold-left (lambda (constraint pair) (f-prov constraint (car pair) (cadr pair))) (list) prov-entry)
          (fold-left (lambda (constraint pair) (f-req constraint (car pair) (cadr pair))) (list) req-entry)
          (list ">= 0")))))
 
 (define (cons-if x y) (if x (cons x y) y))
 (define (f-val-signed comp prop)
   (if (eq? comp comp-max-eq)
       (lambda (val) (prepend-sign (- (=maximum prop) val))) ; for max: (maximum - val)
       (lambda (val) (prepend-sign val))))
 (define (save-ilp path root) (save-to-file path (=to-ilp root)))
 (define (make-ilp root) (save-ilp "gen/ilp.txt" root))
 
 (define (eq-pair? p1 p2) (and (eq? (car p1) (car p2)) (eq? (cdr p1) (cdr p2))))
 (define (add-to-al-paired al key val op)
   (if (null? al) (list (list key (op val (list)))) ; make new entry
       (let ([entry (car al)])
         (if (eq-pair? (car entry) key)
             (cons (list key (op val (cadr entry))) (cdr al)) ; add to entry and return
             (cons entry (add-to-al-paired (cdr al) key val op)))))) ; recur
 (define (merge-paired-al al1 al2)
   (fold-left (lambda (big-al entry) (add-to-al-paired big-al (car entry) (cadr entry) append)) al1 al2))

 (define (maybe-names l) (map (lambda (x) (if (and (ast-node? x) (ast-has-child? 'name x)) (->name x) x)) l))
 (define times (list))
 (define (ctf att-name f . args) ; [c]ons [t]imed [f]unction
   (if timing? (let ([result (time-it (lambda _ (apply f args)))])
                 (set! times (cons (list (current-date-formatted) att-name (time-second (cdr result))
                                                 (time-nanosecond (cdr result)) (maybe-names args)) times))
                 (car result))
       (apply f args)))
 (define (write-result-list prefix ext) (when timing? (save-to-file (date-file-name prefix ext) times)))
 
 (define (add-ilp-ags mquat-spec)
   (with-specification
    mquat-spec

    (ag-rule
     check-model
     (Root
      (lambda (n)
        (when (= 0 (length (=every-pe n))) (warn "No resources found"))
        (when (= 0 (length (=every-container n))) (warn "No container found"))
        (when (= 0 (length (=every-comp n))) (warn "No component found for request"))
        (when (= 0 (length (=every-impl n))) (warn "No implementation found for request"))
        (when (= 0 (length (=every-mode n))) (warn "No modes found for request")))))
      
    ;;; ILP-Creation rules
    
    ; Return a list of 1 objective, some constraints, some bounds and some generals, everything
    ; packed inside a list, e.g. one constraint is also a list. ;(write-result-list "profiling/nego-hw" "time" nego-hw-times)
    (ag-rule
     to-ilp
     (Root
      (lambda (n)
        (=check-model n)
        (let* ([binary-vars (=ilp-binary-vars n)]
               [result
                (list
                 (list "Minimize")
                 (=ilp-objective n)
                 (list "Subject To")
                 (append
;                  (debug (list) "request")
                  (=to-ilp (<=request n)) ; request-constraints
;                  (debug (list) "arch-c")
                  (=to-ilp (->SWRoot n)) ; archtitecture-constraints
;                  (debug (list) "nfp-nego")
                  (=ilp-nego n)) ; NFP-negotiation
;                 (debug (list) "bounds")
                 (list "Bounds")
                 (append
                  (map (lambda (var) (list 0 "<=" var "<=" 1)) binary-vars))
;                 (debug (list) "generals")
                 (list "Generals")
                 binary-vars
                 (list "End"))])
               (write-result-list "profiling/att-measure" "time")
               result)))
     (Request (lambda (n) (=ilp-nego-sw n)))
;     (SWRoot (lambda (n) (recur n append =to-ilp ->Comp*)))
     (SWRoot (lambda (n) (fold-left (lambda (result c) (append (=to-ilp c) result))
                                    (list) (=every-comp n))))
     (Comp
      (lambda (n)
        (debug "Comp:" (->name n))
;        (display (=req-hw-clauses n))
        (cons
         (fold-left
          (lambda (result entry)
            (cons
             (append
              (list (string-append (=ilp-name n) "_requires_" (=ilp-name (car entry)) ": "))
              (fold-left (lambda (inner impl) (cons* "-" (=ilp-binvar impl) inner)) (list) (cadr entry))
              (fold-left (lambda (inner impl) (cons* "+" (=ilp-binvar impl) inner)) (list)
                         (->* (->Impl* (car entry))))
              (list ">=" 0)) result))
          (list) (=req-comp-map n))
         (recur n cons =to-ilp ->Impl*))))
     (Impl
      (lambda (n)
        (debug "Impl:" (->name n))
        (cons (string-append "single(" (=ilp-name n) "): ")
              (fold-left ; deploy one combination of mode and pe for this impl
               (lambda (result pe)
                 (append (fold-left (lambda (inner mode) (cons* "+" (=ilp-binvar-deployed mode pe) inner))
                                    (list) (->* (->Mode* n))) result))
               (list "-" (=ilp-binvar n) "=" 0)
               (=every-container n))))))
    
    (ag-rule request-target? (Comp (lambda (n) (eq? (->target (<=request n)) n))))

    (ag-rule
     ilp-objective
     (Root
      (lambda (n)
        (fold-left
         (lambda (result mode)
           (cons*
            (fold-left (lambda (inner pe) (append (=ilp-objective pe mode) inner))
                       (list) (=every-container n))
            result))
         (list) (=every-mode n))))
     (Resource (lambda (n mode) (debug n mode) (list (prepend-sign (=eval-on (=provided-clause mode (=objective-name n))
                                                                             (->type n)))
                                                     (=ilp-binvar-deployed mode n)))))
    
    ; Creates a list of NFP-negotiation constraints
    (ag-rule
     ilp-nego
     (Root (lambda (n) (remove (list) (=ilp-nego (->SWRoot n))))) ; remove empty constraints
     (SWRoot (lambda (n) (fold-left (lambda (result c) (append (=ilp-nego c) result))
                                    (list) (=every-comp n))))
     (Comp (lambda (n) (append (=ilp-nego-sw n)
                               (=ilp-nego-hw n)))))

    (ag-rule
     ilp-nego-sw
     (Comp
      (lambda (n)
        (fold-left ; fold over req-comp-map
         (lambda (result entry)
           (append
            (make-constraints
             (=ilp-nego-reqc (car entry) 'ProvClause comp-eq) ;provs
             (=ilp-nego-reqc n 'ReqClause comp-max-eq) ;max-reqs
             (=ilp-nego-reqc n 'ReqClause comp-min-eq) #f) ;min-reqs,request?
            result)) (list) (=req-comp-map n))))
     (Request
      (lambda (n)
        (cons (cons "request_target: " (fold-left (lambda (result impl) (cons* "+" (=ilp-binvar impl) result))
                                                  (list "=" 1) (->* (->Impl* (->target n)))))
              (make-constraints
               (=ilp-nego-reqc (->target n) 'ProvClause comp-eq) ;provs
               (=ilp-nego-reqc n comp-max-eq) ;max-reqs
               (=ilp-nego-reqc n comp-min-eq) #t))))) ;min-reqs,request?
    (ag-rule
     ilp-nego-reqc
     (Comp ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode of each impl
      (lambda (n clausetype comparator) (recur n merge-al =ilp-nego-reqc ->Impl* clausetype comparator)))
;      (lambda (n clausetype comparator)
;        (fold-left 
;         (lambda (result clause)
;           (if (and (ast-subtype? clause clausetype) (eq? (->comparator clause) comparator))
;               (fold-left ; fold over pe
;                (lambda (inner pe)
;                  (add-to-al inner (=real (->return-type clause))
;                             (list (=eval-on clause pe) (=ilp-binvar-deployed (<<- clause) pe))))
;;                             (=ilp-eval-binvar clause pe)))
;                result (=every-container n))
;               result))
;;         (list) (append (=every-clause-of n) (->* (->Constraints (<=request n)))))))
;         (list) (=every-clause-of n))))
                   
     (Impl ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode
      (lambda (n clausetype comparator) (recur n merge-al =ilp-nego-reqc ->Mode* clausetype comparator)))
     (Mode ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type
      (lambda (n clausetype comparator)
        (fold-left
         (lambda (result clause)
           (if (and (ast-subtype? clause clausetype) (eq? (->comparator clause) comparator))
               (fold-left ; fold over pe
                (lambda (inner pe)
                  (add-to-al inner (=real (->return-type clause))
                             (list (=eval-on clause pe) (=ilp-binvar-deployed n pe))))
                result (=every-container n))
               result))
;         (list) (att-value 'combined-reqs n))))
         (list) (->* (->Clause* n)))))
     (Request
      (lambda (n comparator)
        (fold-left
         (lambda (result clause)
           (if (eq? (->comparator clause) comparator)
               (add-to-al result (=real (->return-type clause)) (list (=eval-on clause #f) "")) ;use arbitrary target #f
               result))
         (list) (->* (->Constraints n))))))

    (ag-rule
     every-clause-of
     (Comp
      (lambda (n) (fold-left (lambda (result mode) (append (->* (->Clause* mode)) result)) (list) (=every-mode n)))))

;    (ag-rule
;     ilp-nego-rc-req
;     (Comp
;      (lambda (n) ; → all properties required in here. ([prop (clause ... )] ... )
;        (fold-left
;;         (lambda (result clause) (let ([prop (=real (->return-type clause))])
;;                                   (if (member prop result) result (cons prop result))))
;         (lambda (result clause) (add-to-al result (=real (->return-type clause)) clause))
;         (list) (=every-clause-of n)))))
;
;    (ag-rule
;     ilp-nego-sw-v2
;     (Comp
;      (lambda (n)
;        (fold-left (lambda (result pc) (make-constraints2 (cdr pc) (=every-prov-clause (car pc) comp-max-eq)
;                                                          (=every-prov-clause (car pc) comp-min-eq)))
;                   (list) (=ilp-nego-rc-req n)))))
;
;    (define (make-constraints2 prov-clauses req-clauses) #f)
;    (define (=ilp-nego-rc-req n) (att-value '=ilp-nego-rc-req n))

    (ag-rule combined-reqs (Mode (lambda (n) (append (->* (->Clause* n))
                                                     (->* (->Constraints (<=request n)))))))
    
    (ag-rule
     ilp-nego-hw
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result entry) ; entry = {(comparator . property) { clauses }}
           (append
            (fold-left
             (lambda (inner pe) (cons (append
                                       (ctf "ilp-nego-hw0" =ilp-nego-hw0 n (caar entry) (cdar entry) pe)
                                       (list "<=" ((f-val-signed (caar entry) (cdar entry))
                                                   (=eval-on (=provided-clause pe (->name (cdar entry))
                                                                               (->type pe)) pe))))
                                       inner))
             (list) (=every-container n))
            result))
         (list) (=req-hw-clauses n)))))
    
    (ag-rule
     ilp-nego-hw0
     (Comp
      (lambda (n comp prop pe)
        (debug "hw0-comp" (->name n) comp (->name prop) (->name pe))
            (let* ([cp (cons comp prop)]
;                   [__ (begin (debug (assp (lambda (x) (eq-pair? cp x)) (=req-hw-clauses n)))
;                              (debug (cadr (assp (lambda (x) (eq-pair? cp x)) (=req-hw-clauses n)))))]
                   [lop (fold-left ;here we have a [l]ist [o]f [p]airs (evaled-prop mode-on-pe-name)
                         (lambda (result cl) (cons (=ilp-eval-binvar cl pe) result))
                         (list) (cadr (assp (lambda (x) (eq-pair? cp x)) (=req-hw-clauses n))))]
                   [f (f-val-signed comp prop)]) ; for other: val
              (append (list (string-append (=ilp-name n) "_" (=ilp-name prop) "_"
                                           (=ilp-name pe) "_" (comp-name comp) ": "))
                      (fold-left (lambda (result p) #|(debug p)|# (cons* (f (car p)) (cadr p) result)) (list) lop)
;                      (list "<=" (f (=eval-on (=provided-clause pe (->name prop)
;                                                                (->type pe)) pe)))
                      )))))
    (ag-rule
     ilp-eval-binvar
     (Clause
      (lambda (n pe)
;        (let ([real-return-type (=real (->return-type n))])
        (debug "ilp-eval-binvar" n (->name pe))
;          (if (or (eq? (->type pe) (<<- real-return-type))
;                  (ast-subtype? (<<- real-return-type) 'HWRoot))
        (list (=eval-on n (->type pe)) (=ilp-binvar-deployed (<<- n) pe))
;              (begin (debug "not suitable" real-return-type pe) (list))))
        ))) ;empty pair if not a suitable clause
    
    (ag-rule
     required-hw-properties
     (Comp (lambda (n) (recur n union =req-hw-properties ->Impl*)))
     (Impl (lambda (n) (recur n union =req-hw-properties ->Mode*)))
     (Mode (lambda (n) (recur n union =req-hw-properties ->Clause*)))
     (Clause
      (lambda (n)
        (let ([prop (=real (->return-type n))])
          (if (and (ast-subtype? n 'ReqClause) (=hw? prop))
              (list (list (->comparator n) prop)) (list))))))
    
    (ag-rule
     required-hw-clauses ; computes {(comp . prop) {clause1 .. clauseN} ... } for each comp and prop + clauseI has comp and prop
     (Comp (lambda (n) (recur n merge-paired-al =req-hw-clauses ->Impl*)))
     (Impl (lambda (n) (recur n merge-paired-al =req-hw-clauses ->Mode*)))
     (Mode (lambda (n) (recur n merge-paired-al =req-hw-clauses ->Clause*)))
     (Clause
      (lambda (n)
        (let ([prop (=real (->return-type n))])
          (if (and (ast-subtype? n 'ReqClause) (=hw? prop))
              (list (list (cons (->comparator n) prop) (list n))) (list))))))
    
    (ag-rule
     ilp-binary-vars
     (Root
      (lambda (n)
        (append
         (map (lambda (impl) (=ilp-binvar impl)) (=every-impl n))
         (fold-left (lambda (result mode) (append (map (lambda (pe) (=ilp-binvar-deployed mode pe))
                                                       (=every-container n)) result))
                    (list) (=every-mode n))))))
    
    (ag-rule
     ilp-name
     (Property (lambda (n) (ilp-conform-name (->name n))))
     (Comp (lambda (n) (ilp-conform-name (->name n))))
     (Impl (lambda (n) (ilp-conform-name (->name n))))
     (Mode (lambda (n) (ilp-conform-name (->name n))))
     (Resource (lambda (n) (ilp-conform-name (->name n)))))
    
    (ag-rule
     ilp-binvar
     (Impl (lambda (n) (ilp-conform-name (string-append "b#" (->name (<=comp n)) "#" (if (lonely? n) "" (->name n))))))
     (Mode (lambda (n) (error "Should not be called for Modes"))))
    
    (ag-rule
     ilp-binvar-deployed
     (Mode (lambda (n pe)
             (let ([impl (<=impl n)])
               (ilp-conform-name (string-append "b#" (->name (<=comp impl)) "#"  (if (lonely? impl) "" (->name (<=impl n)))
                                                "#"  (if (lonely? n) "" (->name n)) "#"  (->name pe))))))))))
 