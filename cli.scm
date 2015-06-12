#!r6rs

(import (rnrs) (mquat ilp-measurement) (mquat ilp-test) (mquat ag-test) (mquat utils))

(define (print-usage) (error "cli" "No valid arguments found, either use 'test' or 'measure' as first parameter."))

(define cmds (cdr (filter (lambda (arg) (not (cli-debugging-arg=? arg))) (command-line))))

(cond
  [(= 0 (length cmds)) (print-usage)]
  [(string=? "test" (car cmds)) (test-cli-call (cdr cmds))]
  [(string=? "measure" (car cmds)) (measurement-cli-call (cdr cmds))]
  [(string=? "ag" (car cmds)) (do-it (cdr cmds))]
  [else (print-usage)])

(define (event-work-complete id time work-id)
  (debug "check if deadline was met" id time work-id))

(define (event-work-request time work-id load-size deadline)
  (debug "call \"schedule\"-attribute to find worker and insert request into workers queue" time work-id load-size deadline))

(define (event-worker-off worker-id time)
  (debug "set state to OFF" worker-id time))

(define (event-worker-offline worker-id time)
  (debug "if former state was not HALTING, set state to ERROR" worker-id time))

(define (event-worker-online worker-id time)
  (debug "set state to running" worker-id time))

(define (event-adapt time)
  (debug "call adapt" time))

(define (add-worker-to-ast id parent-id device-type time)
  (debug "add new worker" id parent-id device-type time))

(define (add-switch-to-ast id parent-id time)
  (debug "add new switch" id parent-id time))

(define (set-backup-workers number)
  (debug "set new number of backup workers" number))

(define (display-ast)
  (debug "displays the current asg"))
