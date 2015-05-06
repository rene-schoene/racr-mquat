#!r6rs

(library
 (mquat main)
 (export mquat-spec)
 (import (rnrs) (racr core)
         (mquat ast)
         (mquat basic-ag)
         (mquat ilp)
         (mquat example-ast))
 
 (define mquat-spec (create-specification))

 (when (= (specification->phase mquat-spec) 1)
   (specify&compile-ast mquat-spec))
 
 (when (= (specification->phase mquat-spec) 2)
   (add-basic-ags mquat-spec)))
