(define-library (niyarin scm-hu base)
   (import (scheme base))

   (export my-symbol gen-my-sym RETURN-TO-GLOBAL syntax-list)
   (begin
       (define-record-type <my-symbol>
            (my-symbol id)
             my-symbol? 
             (id my-symbol-ref))

       (define syntax-list
         '( define lambda set! quote begin))


       (define gen-my-sym
         (let ((id-counter 0))
           (lambda ()
             (set! id-counter (+ id-counter 1))
             (my-symbol id-counter)id-counter))
         )

       (define RETURN-TO-GLOBAL (gen-my-sym))
     ))
