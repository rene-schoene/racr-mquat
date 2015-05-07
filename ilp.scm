#!r6rs

(library
 (mquat ilp)
 (export add-ilp-ags save-ilp make-ilp)
 (import (rnrs) (racr core) (racr testing)
         (mquat constants) (mquat utils))
 
 (define (add-ilp-ags mquat-spec)
   (with-specification
    mquat-spec
    
    ;;; ILP-Creation rules
    
    ; Return a list of 1 objective, some constraints, some bounds and some generals, everything
    ; packed inside a list, e.g. one constraint is also a list.
    (ag-rule
     to-ilp
     (Root
      (lambda (n)
        (let ([binary-vars (att-value 'ilp-binary-vars n)])
          (list
           (list "Minimize")
           (att-value 'ilp-objective n)
           (list "Subject To")
           (append
            (att-value 'to-ilp (ast-child 'Request n)) ; request-constraints
            (att-value 'to-ilp (ast-child 'SWRoot n)) ; archtitecture-constraints
            (att-value 'ilp-nego n)) ; NFP-negotiation
           (list "Bounds")
           (append
            (att-value 'to-ilp (ast-child 'HWRoot n))
            (map (lambda (var) (list 0 "<=" var "<=" 1)) binary-vars))
           (list "Generals")
           binary-vars
           (list "End")))))
     (Request (lambda (n) (att-value 'ilp-nego-sw n)))
     (SWRoot (lambda (n) (recur append n 'to-ilp 'Comp*)))
     (Comp
      (lambda (n)
        (debug "Comp:" (ast-child 'name n))
        (let ([ics (recur cons n 'to-ilp 'Impl*)])
          (cons
           (fold-left
            (lambda (result entry)
              (cons
               (append
                (list (string-append (att-value 'ilp-name n) "_requires_" (att-value 'ilp-name (car entry)) ": "))
                (fold-left (lambda (inner impl) (cons* "-" (att-value 'ilp-binvar impl) inner)) (list) (cadr entry))
                (fold-left (lambda (inner impl) (cons* "+" (att-value 'ilp-binvar impl) inner)) (list)
                           (ast-children (ast-child 'Impl* (car entry))))
                (list "=" 0)) result))
            (list) (att-value 'req-comp-map n))
           (if (att-value 'request-target? n)
               (cons
                (cons (string-append "request_target_" (att-value 'ilp-name n) ": ")
                      (fold-left (lambda (inner impl) (cons* "+" (att-value 'ilp-binvar impl) inner))
                                 (list "=" 1) (ast-children (ast-child 'Impl* n))))
                ics) ics))))) ; list of cons, and else-branch
     (Impl
      (lambda (n)
        (debug "Impl:" (ast-child 'name n))
        (cons (string-append "single(" (att-value 'ilp-name n) "): ")
              (fold-left ; deploy one combination of mode and pe for this impl
               (lambda (result pe)
                 (append (fold-left (lambda (inner mode) (cons* "+" (att-value 'ilp-binvar-deployed mode pe) inner))
                                    (list) (ast-children (ast-child 'Mode* n))) result))
               (list "-" (att-value 'ilp-binvar n) "=" 0)
               (att-value 'every-pe n)))))
     (HWRoot (lambda (n) (recur append n 'to-ilp 'Resource*)))
     (Resource
      (lambda (n)
        (fold-left
         (lambda (result c)
           (cons (list 0 "<=" (att-value 'ilp-propname c) "<=" (att-value 'eval-on c n)) result))
         (list) (ast-children (ast-child 'ProvClause* n))))))
    
    (ag-rule request-target? (Comp (lambda (n) (eq? (ast-child 'target (att-value 'get-request n)) n))))
    
    (define prepend-sign (lambda (val) (if (< val 0) val (string-append "+ " (number->string val)))))
    
    (ag-rule
     ilp-objective
     (Root
      (lambda (n)
        (fold-left
         (lambda (result mode)
           (cons*
            (fold-left (lambda (inner pe) (cons* (prepend-sign (att-value 'ilp-objective mode pe))
                                                 (att-value 'ilp-binvar-deployed mode pe) inner))
                       (list) (att-value 'every-pe n))
            result))
         (list) (att-value 'every-mode n))))
     (Mode (lambda (n pe) (att-value 'eval-on (att-value 'provided-clause n pn-energy) pe))))
    
    ; Creates a list of NFP-negotiation constraints
    (ag-rule
     ilp-nego
     (Root (lambda (n) (remove (list) (att-value 'ilp-nego (ast-child 'SWRoot n))))) ; remove empty constraints
     (SWRoot (lambda (n) (recur append n 'ilp-nego 'Comp*)))
     (Comp (lambda (n) (append (att-value 'ilp-nego-sw n)
                               (att-value 'ilp-nego-hw n)))))
    
    (ag-rule
     ilp-nego-sw
     (Comp
      (lambda (n)
        (fold-left ; fold over req-comp-map
         (lambda (result entry)
           (append
            (make-constraints
             (att-value 'ilp-nego-reqc (car entry) 'ProvClause comp-eq) ;provs
             (att-value 'ilp-nego-reqc n 'ReqClause comp-max-eq) ;max-reqs
             (att-value 'ilp-nego-reqc n 'ReqClause comp-min-eq) #f) ;min-reqs,request?
            result)) (list) (att-value 'req-comp-map n))))
     (Request
      (lambda (n)
        (make-constraints
         (att-value 'ilp-nego-reqc (ast-child 'target n) 'ProvClause comp-eq) ;provs
         (att-value 'ilp-nego-reqc n comp-max-eq) ;max-reqs
         (att-value 'ilp-nego-reqc n comp-min-eq) #t)))) ;min-reqs,request?
    
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
      (let* ([maximum (+ 1 (fold-left (lambda (max-val pair) (max (car pair) max-val)) 0 (append prov-entry req-entry)))]
             [f-prov (if (eq? comp comp-max-eq)
                         ; prov for max: (maximum - val)
                         (lambda (constraint val name) (cons* (prepend-sign (- maximum val)) name constraint))
                         (lambda (constraint val name) (cons* (prepend-sign val) name constraint)))] ; prov for other: val
             [f-req (if (eq? comp comp-max-eq)
                        ; req for max: - (maximum - val) = val - maximum
                        (lambda (constraint val name) (cons* (prepend-sign (- val maximum)) name constraint))
                        (lambda (constraint val name) (cons* (prepend-sign (- val)) name constraint)))]) ; req for other: -val
        (debug "mc: prov-entry:" prov-entry ",req-entry:" req-entry ",maximum:" maximum ",name:" prov "request?" request?)
        (if request?
            (append
             (list (string-append "request("(att-value 'ilp-name prov) "_" (comp->name comp) "): "))
             (fold-left (lambda (constraint pair) (f-prov constraint (car pair) (cadr pair))) (list) prov-entry)
             (cons* ">=" (f-req (list) (caar req-entry) "")))
            (append
             (list (string-append (att-value 'ilp-name prov) "_" (comp->name comp) ": "))
             (fold-left (lambda (constraint pair) (f-prov constraint (car pair) (cadr pair))) (list) prov-entry)
             (fold-left (lambda (constraint pair) (f-req constraint (car pair) (cadr pair))) (list) req-entry)
             (list ">= 0")))))
    
    (define (cons-if x y) (if x (cons x y) y))
    
    (ag-rule
     ilp-nego-reqc
     (Comp ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode of each impl
      (lambda (n clausetype comparator) (recur2 merge-al n 'ilp-nego-reqc 'Impl* clausetype comparator)))
     (Impl ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type in each mode
      (lambda (n clausetype comparator) (recur2 merge-al n 'ilp-nego-reqc 'Mode* clausetype comparator)))
     (Mode ;→ (prop ((prop-value deployed-mode-name) ... ))-pairs for each Clause with correct type
      (lambda (n clausetype comparator)
        (fold-left
         (lambda (result clause)
           (if (and (ast-subtype? clause clausetype) (eq? (ast-child 'comp clause) comparator))
               (fold-left ; fold over pe
                (lambda (inner pe)
                  (add-to-al inner (ast-child 'returntype clause)
                             (list (att-value 'eval-on clause pe) (att-value 'ilp-binvar-deployed n pe))))
                result (att-value 'every-pe n))
               result))
         (list) (att-value 'combined-reqs n))))
     (Request
      (lambda (n comparator)
        (fold-left
         (lambda (result clause)
           (if (eq? (ast-child 'comp clause) comparator)
               (add-to-al result (ast-child 'returntype clause) (list (att-value 'eval-on clause #f) "")) ;use arbitrary target #f
               result))
         (list) (ast-children (ast-child 'Constraints n))))))
    
    (ag-rule combined-reqs (Mode (lambda (n) (append (ast-children (ast-child 'Clause* n))
                                                     (ast-children (ast-child 'Constraints (att-value 'get-request n)))))))
    
    (ag-rule
     ilp-nego-hw
     (Comp
      (lambda (n)
        (fold-left
         (lambda (result cp) ;[c]omparator+[p]roperty
           (append
            (fold-left
             (lambda (inner pe) (cons (att-value 'ilp-nego-hw0 n (car cp) (cadr cp) pe) inner))
             (list) (att-value 'every-pe n))
            result))
         (list) (att-value 'required-hw-properties n)))))
    
    (ag-rule
     ilp-nego-hw0
     (Comp
      (lambda (n comp prop pe)
        (let* ([lop (fold-left ;here we have a [l]ist [o]f [p]airs (evaled-prop mode-on-pe-name)
                     (lambda (result impl) (append (att-value 'ilp-nego-hw0 impl comp prop pe) result))
                     (list) (ast-children (ast-child 'Impl* n)))]
               [maximum (+ 1 (fold-left (lambda (max-val pair) (max (car pair) max-val)) 0 lop))]
               [f (if (eq? comp comp-max-eq)
                      (lambda (val)
                        (prepend-sign (- maximum val))) ; for max: (maximum - val)
                      (lambda (val)
                        (prepend-sign val)))]) ; for other: val
          (append (list (string-append (att-value 'ilp-name n) "_" (att-value 'ilp-name prop) "_"
                                       (att-value 'ilp-name pe) "_" (comp->name comp) ": "))
                  (fold-left (lambda (result p) (cons* (f (car p)) (cadr p) result)) (list) lop)
                  (list "<=" (f (att-value 'eval-on (att-value 'provided-clause pe (ast-child 'name prop)
                                                               (ast-child 'type pe)) pe)))))))
     (Impl (lambda (n comp prop pe) (recur3 append n 'ilp-nego-hw0 'Mode* comp prop pe)))
     (Mode (lambda (n comp prop pe) (recur3 append n 'ilp-nego-hw0 'Clause* comp prop pe)))
     (Clause
      (lambda (n comp prop pe)
        (if (and (eq? prop (ast-child 'returntype n))
                 (eq? comp (ast-child 'comp n))
                 (eq? (ast-child 'type pe) (ast-pp (ast-child 'returntype n))))
            (list (list (att-value 'eval-on n pe) (att-value 'ilp-binvar-deployed (ast-pp n) pe)))
            (list))))) ;empty pair if not a suitable clause
    
    (ag-rule
     required-hw-properties
     (Comp (lambda (n) (recur union n 'required-hw-properties 'Impl*)))
     (Impl (lambda (n) (recur union n 'required-hw-properties 'Mode*)))
     (Mode (lambda (n) (recur union n 'required-hw-properties 'Clause*)))
     (Clause
      (lambda (n)
        (let ([prop (ast-child 'returntype n)])
          (if (and (ast-subtype? n 'ReqClause) (att-value 'is-hw prop))
              (list (list (ast-child 'comp n) prop)) (list))))))
    
    (ag-rule
     ilp-binary-vars
     (Root
      (lambda (n)
        (append
         (map (lambda (impl) (att-value 'ilp-binvar impl)) (att-value 'every-impl n))
         (fold-left (lambda (result mode) (append (map (lambda (pe) (att-value 'ilp-binvar-deployed mode pe))
                                                       (att-value 'every-pe n)) result))
                    (list) (att-value 'every-mode n))))))
    
    ; TODO make bidirectional mapping: {_ - +} -> {_0 _1 _2}
    (define subs (list (list #\- #\_) (list #\+ #\_)))
    (define (ilp-conform-name name)
      (list->string (map (lambda (c) (let ([entry (assq c subs)]) (if entry (cadr entry) c)))
                         (string->list name))))
    
    (ag-rule
     ilp-name
     (Property (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
     (Comp (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
     (Impl (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
     (Mode (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n)))))
     (Resource (lambda (n) (ilp-conform-name (symbol->string (ast-child 'name n))))))
    
    (ag-rule
     ilp-binvar
     (Impl (lambda (n) (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name (att-value 'get-comp n)))
                                                        "#"  (if (lonely? n) "" (symbol->string (ast-child 'name n)))))))
     (Mode (lambda (n) (error "Should not be called for Modes"))))
    
    (ag-rule
     ilp-binvar-deployed
     (Mode (lambda (n pe)
             (let ([impl (att-value 'get-impl n)])
               (ilp-conform-name (string-append "b#" (symbol->string (ast-child 'name (att-value 'get-comp impl)))
                                                "#"  (if (lonely? impl) "" (symbol->string (ast-child 'name (att-value 'get-impl n))))
                                                "#"  (if (lonely? n) "" (symbol->string (ast-child 'name n)))
                                                "#"  (symbol->string (ast-child 'name pe))))))))
    
    (ag-rule
     ilp-propname
     (Clause
      (lambda (n)
        (let ([pp (ast-parent (ast-parent n))]
              [rpname (symbol->string (ast-child 'name (ast-child 'returntype n)))])
          (ilp-conform-name (if (ast-subtype? pp 'Resource)
                                (string-append (symbol->string (ast-child 'name pp)) "#" rpname) ; Resource
                                (string-append (symbol->string (ast-child 'name (att-value 'get-impl pp)))
                                               "#" rpname))))))))) ; Mode
 
 (define (save-ilp path root) (save-to-file path (att-value 'to-ilp root)))
 (define (make-ilp root) (save-ilp "gen/ilp.txt" root)))
 