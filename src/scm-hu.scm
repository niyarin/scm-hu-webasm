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
             (my-symbol id-counter)))
         )

       (define RETURN-TO-GLOBAL (gen-my-sym))
     ))

(define-library (niyarin scm-hu syntax-check-and-fix!)
   (import (scheme base)
           (scheme cxr)
           (srfi 1)
           (niyarin scm-hu base))

   (export syntax-check-and-fix!)

   (begin
      (define (look-up-stack sym stack)
          (fold 
            (lambda ( renamer res) 
              (if res 
                res
                 (assq sym renamer)))
            #f 
            stack))

      (define (formals->symbol-list formals)
        (let loop ((formals formals)
                   (res '()))
          (cond 
            ((null? formals)
             res)
            ((symbol? formals)
             (cons formals res))
            ((list? formals)
             (unless (symbol?(car formals))  (error "invalid formals" formals))
             (loop (cdr formals) (cons (car formals) res) )))))


      (define (syntax-check-and-fix! code)
        (let loop ((code code)
                   (rename-stack '()))

          (when (list? code)
              (let ((operator 
                      (cond ((look-up-stack (car code) rename-stack) => cdr) (else (car code)))))
                (case operator
                  ((lambda) 

                   (let SYNTAXCHECK ()
                      (when 
                        (or (not (list? code))
                            (null? (cdr code))
                            )
                        (error "syntax error:lambda" code))


                      (let ((formal-symbols (formals->symbol-list (cadr code)))) 
                          (fold 
                            (lambda (sym left-symbols ) 
                              (when (memq left-symbols sym)
                                 (error "duplicate parameter" (cadr code))))
                            '()
                            formal-symbols
                            )))


                   (when (null? (cddr code))
                      (set-cdr! (cdr code)
                        (list '())))

                   (let ((rename-list 
                           (fold 
                             (lambda (x res) (if (memq x syntax-list) (cons (cons x (gen-my-sym)) res) res))
                             '()
                             (formals->symbol-list (cadr code)))))

                     (let loop2 ((formals (if (symbol? (cadr code)) (cdr code) (cadr code))))
                       (cond
                         ((null? formals))
                         ((list? (cdr formals))
                           (cond ((assq (car formals) rename-list) => 
                                    (lambda (rename-pair)
                                      (set-car! formals (cdr rename-pair)))))
                           (loop2 (cdr formals)))
                         ((symbol? (cdr formals))
                           (cond ((assq (car formals) rename-list) => 
                                    (lambda (rename-pair)
                                      (set-car! formals (cdr rename-pair))))))
                           (cond ((assq (cdr formals) rename-list) => 
                                    (lambda (rename-pair)
                                      (set-cdr! formals (cdr rename-pair)))))))

                     (loop (caddr code) (cons rename-list rename-stack))))
                  ((quote)
                     (when (or (not (list? code)) (null? (cdr code)) )
                        (error "syntax error:quote" code)))
                  ((define)
                   (if (null? rename-stack);global?
                     (loop (caddr code) '())
                     ("Error: This Scheme imprementation does't support internal define")))
                  (else
                    (when (not (list? code))
                        (error "syntax error"))

                    (let loop2 ((ls code))
                      (cond
                        ((not (pair?  ls)))
                        ((symbol? (car ls)) 
                           (cond ((look-up-stack (car ls)  rename-stack) => 
                                    (lambda (rename-pair)
                                      (set-car! ls (cdr rename-pair)))))
                           (loop2 (cdr ls)))
                        (else
                          (loop (car ls) rename-stack)
                          (loop2 (cdr ls)))))
                    )

                  )))))
      ))

(define-library (niyarin scm-hu cps)
   (import (scheme base)
           (scheme cxr)
           (niyarin scm-hu base))
   (export scm-hu-cps)

   (begin 
      (define (not-have-continuation? expression)
         (cond 
           ((not (list? expression))
               (list expression))
           ((null? expression)
               (list '()))
           ((eq? (car expression) quote)
            (list expression))
           ((eq? (car expression) 'lambda)
            (list (scm-hu-cps (caddr scm-code))))
           (else 
             #f)))

      (define (scm-hu-cps scm-code)
         (cond 
           ((not-have-continuation? scm-code) =>  car)
           (else
              (let conv-loop ((code scm-code)
                              (continuation-symbol  RETURN-TO-GLOBAL)
                              (stack '()))
               (cond
                 ((not-have-continuation? code)
                     => (lambda (not-continuation) (car not-continuation)))
                 ((and (eq? (car  code)  'if) (not-have-continuation? (cadr code)))
                  =>
                  (lambda (test-exp)

                    (set-car! (cdr code) (car test-exp))

                    (let ((true-exp (conv-loop (caddr code) continuation-symbol stack)))
                      (set-car! (cddr code) true-exp))

                    (let ((false-exp (conv-loop (cadddr code) continuation-symbol stack)))
                      (set-car! (cdddr code) false-exp))


                     code
                  ))
                 (else 
                   (let loop ((ls code))
                     (cond 
                       ((and (null? ls)  (null? stack));外側
                        (set-cdr! 
                          code
                          (cons continuation-symbol (cdr code)))
                        code)
                       ((null? ls)
                        (set-cdr!
                          code
                          (cons `(lambda (,(caar stack))  ,(conv-loop (cdar stack) continuation-symbol (cdr stack))) (cdr code)))
                        code)
                        
                       ((not-have-continuation? (car ls))
                        (loop (cdr ls)))
                       (else;継続有り
                         (let ((new-my-sym (gen-my-sym))
                               (new-target-code (car ls)))

                           (set-car! ls new-my-sym)
                           (conv-loop new-target-code
                                      continuation-symbol
                                      (cons (cons new-my-sym code) stack ))))
                       )
                   )))))))))

(define-library (niyarin scm-hu internal-representation1)
   (import (scheme base) 
           (scheme cxr)
           (scheme write)
           (srfi 69)
           (niyarin scm-hu base))
   (export scm-hu-convert-internal-representation scm-hu-representation1-func-alist)
   (begin

     (define (scm-hu-representation1-func-alist)
       `((PUT-CONST-VALUE ,PUT-CONST-VALUE)
         (PULL-CONST-VALUE ,PULL-CONST-VALUE)))

     (define PUT-CONST-VALUE (gen-my-sym))
     (define PULL-CONST-VALUE (gen-my-sym))

     (define (scm-hu-convert-internal-representation code const-size const-code global global-size local-size)

         (let ((res
            (let conv ((code code)
                       (stack '()))
              (cond 
                ((not (list? code)) code)
                ((eq? (car code) 'quote)
                 (set! const-code 
                       (cons (list PUT-CONST-VALUE const-size (cadr code)) const-code))
                  (set! const-size (+ const-size 1))
                  (list PULL-CONST-VALUE (- const-size 1)))
               
                ((eq? (car code) 'define)
                    (let* ((in-global-hash (hash-table-exists? global (cadr code)))
                        (name-id
                          (if in-global-hash
                              (hash-table-ref gobal (cadr code))
                              (begin (hash-table-set! global (cadr code) global-size)
                                     (set! global-size  (+ global-size 1))
                                     (hash-table-ref  global (cadr code))))))
                       (set-car! (cdr code) name-id)
                       (set-car! (cddr code) (conv (caddr code) stack))
                       code
                       ))
                (else 
                  (map
                    (lambda (o)
                      (conv o stack))
                    code))))))
               res;TODO:const-codeとかあとで加える
           ))))
