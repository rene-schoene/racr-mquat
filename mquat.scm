#!r6rs

(import (rnrs) (racr core)
        (mquat constants) (mquat utils) (mquat join) (mquat ast) (mquat basic-ag))

;; --- start coding below this line ---

(define ms mquat-spec)

; Properties for Cubieboards
(define load (:RealProperty ms "Load" "%" 'runtime 'decreasing agg-sum))
(define cpu-freq (:RealProperty ms "CPU.Frequency" "Mhz" 'runtime 'increasing 'agg))
(define mem-free (:RealProperty ms "Memory.Free" "MB" 'runtime 'increasing 'sum))
; Resource-Types
(define Switch (:ResourceType ms "Switch" #f (list)))
(define Cubieboard (:ResourceType ms "Cubieboard" #t (list cpu-freq load mem-free)))
; Properties for Sample-Component
(define energy (:RealProperty ms pn-energy "J" 'runtime 'decreasing 'sum))
(define response-time (:RealProperty ms "Response-time" "ms" 'runtime 'decreasing 'sum))
(define precision (:RealProperty ms "Precision" #f 'runtime 'increasing 'agg))
; MetaParameter-Names and -Nodes
(define workers "workers")
(define particles "particles")
(define mp-workers (:MetaParameter ms workers #f))
(define mp-particles (:MetaParameter ms particles #f))
; ReqClause construction
(define sample-response-time (:PropertyRef ms response-time))
(define sample-energy (:PropertyRef ms energy))
(define energy-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error 'energy-f "Unknown number of workers" (=value-of args workers))]))))
(define rt-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        ;TODO
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error 'rt-f "Unknown number of workers" (=value-of args workers))]))))
(define precision-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        ;TODO
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error 'precision-f "Unknown number of workers" (=value-of args workers))]))))
; Software entities
(define kld-mode (:Mode ms "KLD-mode"
                        (list (:ReqClause ms sample-energy comp-max-eq energy-f)
                              (:ReqClause ms sample-response-time comp-max-eq rt-f)
                              (:ReqClause ms precision comp-min-eq precision-f))))
(define kld (:Impl ms "KLD" (list kld-mode) (list) #f #f))
(define Sample (:Comp ms "Sampling" (list kld) #f (list precision sample-response-time sample-energy)))

(define ast
  (:Root ms
         (:HWRoot ms (list Cubieboard) (list) (list))
         (:SWRoot ms (list Sample) (list energy))
         (:Request ms (list mp-workers mp-particles) Sample (list) #f)))

(define (update-status worker-id status . valid-former-stati)
  (let ([pe (=search-pe ast (r worker-id))])
    (when (and (not (null? valid-former-stati)) (not (memq (->status pe) valid-former-stati)))
      (rewrite-terminal 'status pe error-state)
      (debug "worker" worker-id "now in error-state"))
    (rewrite-terminal 'status pe status)))

(define (search-parent id) (if (eq? 0 id) (->HWRoot ast) (=search-pe ast (r id))))
(define ° "racr-mquat>")
(define (r id) (string-append "r-" (number->string id)))

(define (event-work-complete id time work-id)
  ; TODO do something useful
  (display "event-work-complete\n") (flush-output-port (current-output-port))
  (debug ° "check if deadline was met" id time work-id))

(define (event-work-request time work-id load-size deadline)
  ; TODO do something useful
  (display "event-work-request\n") (flush-output-port (current-output-port))
  (debug ° "call \"schedule\"-attribute to find worker and insert request into workers queue" time work-id load-size deadline))

(define (event-worker-off worker-id time)
  (debug ° "event-worker-off" worker-id time) (flush-output-port (current-output-port))
  (update-status worker-id offline))

(define (event-worker-offline worker-id time)
  (debug ° "event-worker-offline" worker-id time)
  (update-status worker-id offline halting))

(define (event-worker-online worker-id time)
  (debug ° "event-worker-online" worker-id time) (flush-output-port (current-output-port))
  (update-status worker-id online))

(define (event-adapt time)
  ; TODO do something useful
  (debug ° "call adapt" time) (flush-output-port (current-output-port)))

(define (add-worker-to-ast id parent-id device-type time)
  (debug ° "add-worker-to-ast" id parent-id device-type time) (flush-output-port (current-output-port))
  (let ([new-pe (:Resource ms (r id) Cubieboard offline (list) (list))]
        [parent-pe (search-parent parent-id)])
    (rewrite-add (->SubResources parent-pe) new-pe)))

(define (add-switch-to-ast id parent-id time)
  (debug ° "add-switch-to-ast" id parent-id time) (flush-output-port (current-output-port))
  (let ([new-pe (:Resource ms (r id) Switch offline (list) (list))]
        [parent-pe (search-parent parent-id)])
    (rewrite-add (->SubResources parent-pe) new-pe)))

(define (set-backup-workers number)
  ; TODO do something useful
  (debug ° "set-backup-workers" number) (flush-output-port (current-output-port)))

(define (display-ast)
  (debug ° "displays-ast\n") (flush-output-port (current-output-port))
  (display-ast ast))
