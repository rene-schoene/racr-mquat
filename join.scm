#!r6rs
; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: R. Schöne

(library
 (mquat join)
 (export mquat-spec)
 (import (rnrs) (racr core)
         (mquat ast) (mquat basic-ag) (mquat ilp))
 
 (define mquat-spec (create-specification))

 (when (= (specification->phase mquat-spec) 1)
   (specify&compile-ast mquat-spec))
 
 (when (= (specification->phase mquat-spec) 2)
   (add-basic-ags mquat-spec)
   (add-ilp-ags mquat-spec)
   (compile-ag-specifications mquat-spec)))
