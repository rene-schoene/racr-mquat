#!r6rs

(import (rnrs) (mquat constants) (mquat utils) (mquat join) (mquat ast) (mquat basic-ag))

;; --- start coding below this line ---

(define ms mquat-spec)

(define load (:RealProperty ms 'Load '% 'runtime 'decreasing 'sum))
(define cpu-freq (:RealProperty ms 'CPU.Frequency 'Mhz 'runtime 'increasing 'agg))
(define mem-free (:RealProperty ms 'Memory.Free 'MB 'runtime 'increasing 'sum))
(define Cubieboard (:ResourceType ms 'Cubieboard (list cpu-freq load mem-free)))
(define energy (:RealProperty ms 'Energy 'J 'runtime 'decreasing 'sum)) ; for Sample-Component
(define response-time (:RealProperty ms 'Response-time 'ms 'runtime 'decreasing 'sum))
(define precision (:RealProperty ms 'Precision '1 'runtime 'increasing 'agg))
(define workers 'workers)
(define particles 'particles)
(define mp-workers (:MetaParameter ms workers #f))
(define mp-particles (:MetaParameter ms particles #f))
(define sample-response-time (:PropertyRef ms response-time))
(define sample-energy (:PropertyRef ms energy))
(define energy-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error "energy-f" "Unknown number of workers" (=value-of args workers))]))))
(define rt-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error "energy-f" "Unknown number of workers" (=value-of args workers))]))))
(define precision-f
  (lambda (args type)
    (let ([p (=value-of args particles)])
      (case (=value-of args workers)
        [(1) (+ #e158.418575 (* #e-0.603838 particles) (* #e0.001126 particles particles))]
        [(3) (+ #e146.0164 (* #e0.2118 particles))]
        [(5) (+ #e783.600532 (* #e-2.254366 particles) (* #e0.002543 particles particles))]
        [else (error "energy-f" "Unknown number of workers" (=value-of args workers))]))))
(define kld-mode (:Mode ms 'KLD-1worker-100particles
                        (list (:ReqClause ms sample-energy comp-max-eq energy-f)
                              (:ReqClause ms sample-response-time comp-max-eq rt-f)
                              (:ReqClause ms precision comp-min-eq precision-f))))
(define kld (:Impl ms 'KLD (list kld-mode) (list) #f #f))
(define Sample (:Comp ms 'Sampling (list kld) #f (list precision sample-response-time sample-energy)))

(define ast
  (:Root ms
         (:HWRoot ms (list Cubieboard) (list) (list))
         (:SWRoot ms (list Sample) (list energy))
         (:Request ms (list mp-workers mp-particles) Sample (list) #f)))

(define (event-work-complete id time work-id)
  (display "event-work-complete\n") (flush-output-port (current-output-port))
  (debug "check if deadline was met" id time work-id))

(define (event-work-request time work-id load-size deadline)
  (display "event-work-request\n") (flush-output-port (current-output-port))
  (debug "call \"schedule\"-attribute to find worker and insert request into workers queue" time work-id load-size deadline))

(define (event-worker-off worker-id time)
  (display "event-worker-off\n") (flush-output-port (current-output-port))
  (debug "set state to OFF" worker-id time))

(define (event-worker-offline worker-id time)
  (display "event-worker-offline\n") (flush-output-port (current-output-port))
  (debug "if former state was not HALTING, set state to ERROR" worker-id time))

(define (event-worker-online worker-id time)
  (display "event-worker-online\n") (flush-output-port (current-output-port))
  (debug "set state to running" worker-id time))

(define (event-adapt time)
  (display "adapt\n") (flush-output-port (current-output-port))
  (debug "call adapt" time))

(define (add-worker-to-ast id parent-id device-type time)
  (display "add-worker-to-ast\n") (flush-output-port (current-output-port))
  (debug "add new worker" id parent-id device-type time))

(define (add-switch-to-ast id parent-id time)
  (display "add-switch-to-ast\n") (flush-output-port (current-output-port))
  (debug "add new switch" id parent-id time))

(define (set-backup-workers number)
  (display "set-backup-workers\n") (flush-output-port (current-output-port))
  (debug "set new number of backup workers" number))

(define (display-ast)
  (display "display-ast\n") (flush-output-port (current-output-port))
  (debug "displays the current asg\n"))
