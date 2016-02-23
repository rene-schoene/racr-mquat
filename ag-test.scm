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
 
;; Testing some attributes
 
 (define (do-it . args)
   (let* ([ast (create-system 3 0 1 2 2)]
          [clause (car (->* (->Clause* (car (->* (->Mode* (car (->* (->Impl* (car (->* (->Comp* (->SWRoot ast)))))))))))))]
          [pe (car (->* (->SubResources (->HWRoot ast))))]
          [prov-clause (car (->* (->ProvClause* pe)))])
     (display (=ilp-eval-binvar clause pe))
     (rewrite-terminal 'value prov-clause (rand 1 3 0))
     (display (=ilp-eval-binvar clause pe))
     (rewrite-terminal 'value prov-clause (rand 1 3 0))
     (display (=ilp-eval-binvar clause pe)))))
