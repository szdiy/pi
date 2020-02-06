(import (ice-9 match)
        (ice-9 pretty-print))

(define (init-cont)
  (let ((x (gensym "init/k-")))
    `(lambda (,x) ,x)))

(define (atom? x)
  (or (integer? x)
      (string? x)
      (null? x)))

(define (var? x) (symbol? x))
(define *primitives*
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (format . ,format)))

(define (prim? x)
  (assoc-ref *primitives* x))

;; NOTE: cps-comp only accepts Weak-Normal-Form in ARS (Abstract Rewriting System), which means
;;       it can not be rewritten further. Weakly normalizing means SOME expressions can't be
;;       rewritten further, otherwise means ALL expressions can be rewritten further.
;;       This may includes:
;;       1. atomic values
;;       2. lambdas
;; NOTE; Calling kont to return every result is absolutly necessary, otherwise the context will be destoryed,
;;       since some of the context is missing when you ignore the kont.
(define (cps-comp expr kont)
  (match expr
    ((? prim? (= prim? o))
     ;;expr
     `(,kont ,expr)
     )
    ((or (? var?) (? atom?))
     ;;expr
     `(,kont ,expr)
     )
    (else "can't comp value")))

(define (def! v e)
  (module-define! (current-module) v e))

(define end-cont identity)

;; NOTE: The return value MUST be a uncurried CPS, e,g, \k.e is invalid CPS
(define* (expr->cps expr #:optional (kont 'end-cont))
  (format #t "$$$$$$ ~a       ~a~%" expr kont)
  (match expr
    ((? atom?) (cps-comp expr kont))
    ((? var?) (cps-comp `(lambda args (,expr ,identity args)) kont))
    (('lambda (args ...) body ...)
     (let ((k (gensym "kont-")))
       `(,kont (lambda (,k ,@args)
                 ,(expr->cps `(seq ,@body))))))
    #;
    (('lambda (args ...) body ...)
    `(lambda (k ,@args) ,@(map (lambda (e) (expr->cps e k)) body)))
    #;p
    (('define (v args ...) body ...)
     (let ((k (gensym "kont-")))
       `(,kont (def! ',v (lambda (,k ,@args)
                           ,@(map (lambda (e) (expr->cps e k)) body))))))
    (`(define ,v ,e)
     (let ((k `(lambda (r1) (,kont (def! ',v r1)))))
       `(,kont ,(expr->cps e k))))
    (`(set! ,v ,e)
     (let ((k `(lambda (r1) (,kont (set! ,v r1)))))
       `(,kont ,(expr->cps e k))))
    (`(if ,cnd ,b1 ,b2)
     (let ((k `(lambda (c1) (if c1 ,(expr->cps b1) ,(expr->cps b2)))))
       `(,kont ,(expr->cps cnd k))))
    (('args args ...) `(,kont (list ,@(map (lambda (e) (expr->cps e)) args))))
    (('seq e1 e2 ...)
     (if (null? e2)
         (expr->cps e1 kont)
         `(,(expr->cps e1) ,(lambda _ (expr->cps e2 kont))))
     )
    #;
    ((('lambda (args ...) body ...) e ...)
    (let ((k `(lambda (r1) (lambda (r2) (,kont (apply r1 r2))))))
    #;`(,(expr->cps (car expr) k) ,@(map (lambda (ee) (expr->cps ee kont)) e)) ; ; ; ;
    `(apply ,(expr->cps (car expr) k) )))
    (((? prim? (= prim? p)) e ...)
     (let ((pv (gensym "prim-"))
           (ev (gensym "e-")))
       (expr->cps
        p
        `(lambda (,pv)
           ,(expr->cps
             `(args ,@e)
             `(lambda (,ev) (apply ,pv ,ev)))))))
    ((f e ...)
     (let ((fv (gensym "f-"))
           (ev (gensym "e-")))
       (expr->cps
        f
        `(lambda (,fv)
           ,(expr->cps
             `(args ,@e)
             `(lambda (,ev) (apply ,fv ,kont ,ev)))))))
    (else "no")))

;; For better debugging.
(define (beta-reduce/end-cont expr)
  (match expr
    (('end-cont e) (beta-reduce/end-cont e))
    (('end-cont e ...) (beta-reduce/end-cont e))
    (('lambda (arg ...) body ...) `(lambda (,@arg) ,@(map beta-reduce/end-cont body)))
    (`(define ,v ,e) `(define ,v ,(beta-reduce/end-cont e)))
    (`(set! ,v ,e) `(set! ,v ,(beta-reduce/end-cont e)))
    (`(if ,cnd ,b1 ,b2) `(if ,(beta-reduce/end-cont cnd) ,(beta-reduce/end-cont b1) ,(beta-reduce/end-cont b2)))
    ((e1 e2 ...) `(,(beta-reduce/end-cont e1) ,@(beta-reduce/end-cont e2)))
    (else expr)))

(define (test expr)
  (let ((cps (expr->cps expr)))
    (pretty-print (beta-reduce/end-cont cps))
    (eval cps (current-module))))
