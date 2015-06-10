#!r6rs

(library
 (mquat properties)
 (export timing? verbose? write-ilp?)
 (import (rnrs) (rnrs eval))

 (define (property-list) (eval (with-input-from-file "scheme.properties" (lambda () (read))) (environment '(rnrs))))
 (define (this-or-def entry default) (if entry (cdr entry) default))
 
 (define timing? (this-or-def (assq 'timing (property-list)) #f))
 (define verbose? (this-or-def (assq 'verbose (property-list)) #f))
 (define write-ilp? (this-or-def (assq 'write-ilp (property-list)) #f)))
