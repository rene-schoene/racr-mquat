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

 (define (do-it . args)
   (let ([ast (create-system 10 0 3 4 5)])
     (display-ast ast 'remote-unit 'remote-container 'remote-impls))))
