#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat ag-test)
 (export do-it)
 (import (rnrs) (racr core) (racr testing)
         (mquat utils) (mquat ilp) (mquat join) (mquat basic-ag) (mquat ast)
         (mquat constants) (mquat ast-generation) (mquat ui))
 
;; Testing printing whole asts
;; num-pe=10, num-pe-subs=0, num-comp=3, impl-per-comp=4, mode-per-impl=5
 
 (define (display-part2 node)
   (define (print name) (cons name (lambda (v) v)))
   (define printer (list (print 'remote-unit)))
   (print-ast node printer (current-output-port)))

 (define (do-it . args)
   (let* ([ast (create-system 10 0 3 4 5)]
;          [clause (car (->* (->Clause* (car (->* (->Mode* (car (->* (->Impl* (car (->* (->Comp* (->SWRoot ast)))))))))))))]
          [pe (car (->* (->SubResources (->HWRoot ast))))]
          [prov-clause (car (->* (->ProvClause* pe)))])
;     (display (=ilp-eval-binvar clause pe))
;     (rewrite-terminal 'value prov-clause (rand 1 3 0))
;     (display (=ilp-eval-binvar clause pe))
;     (rewrite-terminal 'value prov-clause (rand 1 3 0))
;     (display (=ilp-eval-binvar clause pe))
     (display-part2 ast))))
