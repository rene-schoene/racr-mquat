#!r6rs

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
