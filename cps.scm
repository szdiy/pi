(import (ice-9 match)
        (ice-9 pretty-print)
        (srfi srfi-1))

(define *toplevel-funcs* (make-hash-table))
(define *toplevel-vars* (make-hash-table))
(define *toplevel-prims* (make-hash-table))
(define (toplevel)
  (append (hash-map->list (lambda (k) (lambda (v) `(define ,k ,v))) *toplevel-vars*)
          (hash-map->list (lambda (k) (lambda (v) `(define ,k ,v))) *toplevel-funcs*)))

(define (def-prim v e)
  (hash-set! *toplevel-prims* v e))

(define (def-lib v e)
  (hash-set! *toplevel-funcs* v e)
  #f)

(define (def-var v e)
  (hash-set! *toplevel-vars* v e)
  #f)

(define (->prim p)
  (let ((k (gensym "k-"))
        (args (gensym "->prim-args-")))
    `(lambda (,k) (lambda ,args (,k (apply ,p ,args))))))

(def-prim '&* (->prim '*))

(define (capture-kont k)
  (let ((x (string->symbol (format #f "capture-~a-arg" k))))
    `(lambda (,x) (,k ,x))))

(define (etaize expr k)
  (let ((x (gensym "etaize-arg-")))
    `(lambda (,x ,k) (,k (apply ,expr ,x)))))

(define (id? x)
  (and (symbol? x) (hash-ref *toplevel-vars* x)))

(define (prim? p)
  (and (symbol? p)
       (hash-ref *toplevel-prims* p)))

(define (proc? p)
  (and (symbol? p) (hash-ref *toplevel-funcs* p)))

(define (beta-reduce expr)
  (match expr
    ((('lambda (args ...) body ...) e ...)
     (format #t "args: ~a, body: ~a, e: ~a~%" args body e)
     (eval `((lambda ,args ,@body) ,@e) (current-module)))
    (else (error "no"))))

(define* (expr->cps expr #:optional (cont #f))
  (define k (if cont
                (match cont
                  (('lambda (ks) rest ...) ks)
                  (else (error "wrong cont" cont)))
                (gensym "kont-")))
  (define kont (or cont (capture-kont k)))
  ;;(format #t "CCC: ~a, ~a, ~a~%" expr k kont)
  (match expr
    (('lambda (args ...) body ...)
     `(lambda (,k)
        (lambda (,@args)
          ,@(map
             (lambda (e)
               (cps-comp e kont))
             body))))
    (`(if ,cnd ,b1 ,b2)
     `(lambda (,k)
        (if ,(cps-comp cnd kont)
            ,(cps-comp b1 kont)
            ,(cps-comp b2 kont))))
    (('begin rest ...)
     `(lambda (,k)
        ,@(map (lambda (e)
                 ;;`(lambda (,k) ,(cps-comp e kont))
                 (cps-comp e kont))
               rest)))
    ((e1 e2 ...)
     (let ((v1 (gensym "apply-var-1-"))
           (v2 (gensym "apply-var-2")))
       `(lambda (,k)
          ,(cps-comp
            e1
            `(lambda (,v1)
               ,(cps-comp
                 `($args ,@e2)
                 `(lambda (,v2)
                    ((,v1 ,kont) ,v2)))))))
     ;;`(lambda (,k) ,(cps-comp expr kont))
     )
    ((or (? proc?) (? id?) (? prim?))
     ;;`(lambda (,k) (,k ,expr))
     (cps-comp expr kont))
    (else `(lambda (,k) (,kont ,expr))
          )))

(define (cps-apply expr kont)
  (let ((k (gensym "cps-apply-k-"))
        (args (gensym "cps-apply-args-")))
    `(,expr ,kont)))

;; sexp -> \k.
(define (cps-comp c-expr kont)
  (format #t "BBB: ~a,  ~a~%" c-expr kont)
  (match c-expr
    (('define v ('lambda (args ...) body ...))
     (def-lib v (cps-comp (caddr c-expr) kont)))
    (('define v e)
     (let ((ee (cps-comp e kont)))
       (def-var v ee)))
    ((? id? v) (cps-apply (id? v) kont))
    ((? proc? p) (cps-apply (proc? p) kont))
    ((? prim? p) (cps-apply (prim? p) kont)
     #;
     (let ((args (gensym "arg-"))
     (k (gensym "k-")))
     `(lambda (,args ,k)
     (apply ,c-expr (map (lambda (x) (cps-comp x ,k)) ,args)))))
    ((or (? number?)
         (? string?)
         (? symbol?)
         (? char?)
         (? boolean?))
     ;;`(,kont ,(expr->cps c-expr))
     ;;(expr->cps c-expr kont)
     `(,kont ,c-expr)
     )
    (('lambda (args ...) body ...)
     ;;`(,kont ,(expr->cps c-expr))
     ;;(expr->cps c-expr)
     `(lambda (,@args) ,@(map (lambda (e) (cps-comp e kont)) body))
     )
    (('if cnd b1 b2)
     ;; It is necessary to expand here, since we only reduce 1 pass.
     `(if (cps-comp cnd kont)
          (cps-comp b1 kont)
          (cps-comp b2 kont)))
    (('$args args ...)
     `(,kont ,@args)
     )
    #;
    ((e1 e2)
    (let ((v1 (gensym "var-"))
    (v2 (gensym "var-")))
    (cps-comp
    e1
    `(lambda (,v1)
    ,(cps-comp
    e2
    `(lambda (,v2)
    (,v1 ,v2 ,kont)))))))
    (() `(,kont ()))
    (else
     ;;(format #t "AAA:   ~a~%" kont)
     ;;(expr->cps c-expr kont)
     `(,kont ,c-expr)
     ;;`(,kont (expr->cps c-expr))
     )))

(define (test expr)
  (let ((ec (expr->cps
             `(begin
                (define f (lambda (k) (lambda (x) ((&* k) x x))))
                ,expr))))
    ec;;(pretty-print ec)
    ;;(fold (lambda (e p) ((eval e (current-module)) identity)) '() ec)
    ))

(define (ftest expr)
  (lambda args
    (apply test expr args)))
