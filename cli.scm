#!r6rs

(import (rnrs) (mquat ilp-measurement) (mquat ilp-measurement-paper) (mquat ilp-test) (mquat ag-test) (mquat utils))

(define (print-usage) (error 'cli "No valid arguments found, either use 'test' or 'measure' as first parameter."))

(define cmds (cdr (command-line)))

(cond
  [(= 0 (length cmds)) (print-usage)]
  [(string=? "test" (car cmds)) (test-cli-call (cdr cmds))]
  [(string=? "measure" (car cmds)) (measurement-cli-call (cdr cmds))]
  [(string=? "measure-paper" (car cmds)) (measurement-paper-cli-call (cdr cmds))]
  [(string=? "ag" (car cmds)) (do-it (cdr cmds))]
  [else (print-usage)])
