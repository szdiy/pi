(import (ice-9 match)
        (srfi srfi-1))

(define (prim? p)
  (memq p '(+ - * /)))

(define* (expr->cps expr #:optional (cont '(lambda (x) x)))
  (match expr
    (('lambda (v ...) body)
     (let ((k cont)
           (karg (gensym "k"))
           (x (gensym "x"))
           (m (gensym "m")))
       `(,k (lambda (,x ,karg)
              (,(expr->cps body) (lambda (,m) (,k ,m)))))
       #;
       `((lambda (,k ,@v)               ;
       ,(map (lambda (e) (expr->cps e k)) body)) ;
       ,cont)

       #;`(,cont ,(map (lambda (e) (expr->cps e k)) body))
       ))
    (('if cnd b1 b2)
     (let ((karg (gensym "k"))
           (tb (expr->cps b1 cont))
           (fb (expr->cps b2 cont)))
       `(lambda (,karg) (if ,cnd ,tb ,fb))))
    ((f e ...)
     (let* ((fn (gensym "f"))
            (el (map (lambda (_) (gensym "x")) e))
            (m (gensym "m"))
            (k cont)
            (f-expr `(,fn ,@el))
            (kf (gensym "k")))
       `(,k (,(expr->cps f)
             (lambda (,fn)
               ,(fold (lambda (ee ex p) `(,(expr->cps ee) (lambda (,ex) ,p)))
                      `(,(if (prim? f)
                             `(,k (,fn ,@el))
                             `((lambda (,kf ,@el) (,kf (,fn ,@el))) ,k ,@el))
                        (lambda (,m) (,k ,m)))
                      e el))))
       #;
       `(,cont ,(expr->cps e `(lambda (,x ,fn) ;
       ,(if (null? e*)                  ;
       `(,fn ,((expr->cps fn cont) ,x)) ;
       (let ((x* (gensym "x")))         ;
       `(lambda (,x* ,fn)               ;
       `(,fn ,((expr->cps fn cont) ,x ,x*))))))))
       #;
       (expr->cps f (map (lambda (x t)  ; ;
       `(lambda (,fn)                   ; ;
       ,(expr->cps x `(lambda (,t) (,fn ,t ,cont))))) ; ;
       e el))))
    (else `(,cont ,expr))))

;; capture free substitute
(define (cfs expr args el)
  (define (beta e)
    (cond
     ((list-index (lambda (x) (eq? x e)) args)
      => (lambda (i) (list-ref el i)))
     (else e)))
  (when (not (= (length args) (length el)))
    (error (format #f "BUG: args: ~a, el: ~a~%" args el)))
  (match expr
    (('lambda (_ ...) body) (cfs body args el))
    #;
    (((? symbol? s) rest ...)           ;
    `(,(beta s) ,@(cfs rest args el)))
    (((f e ...) rest ...)
     `((,(cfs f args el) ,@(map (lambda (ee) (cfs ee args el)) e))
       ,@(cfs rest args el)))
    ((e ...) (map (lambda (ee) (cfs ee args el)) e))
    (else (beta expr))))

(define (normalize expr)
  (match expr
    ((('lambda (args ...) body) e e* ...)
     (display "beta 0\n")
     (normalize (cfs (normalize body) args (normalize `(,e ,@e*)))))
    (((('lambda (args ...) body) e e* ...) rest ...)
     (display "beta 1\n")
     (normalize `(,(cfs (normalize body) args (normalize `(,e ,@e*)))
                  ,@(normalize rest))))
    (('lambda (args ...) body)
     (display "beta 2\n")
     (normalize `(lambda (,@args)
                   ,(normalize body))))
    (('lambda (arg) (f arg))
     (display "eta-0\n")
     (normalize f))
    ((('lambda (arg) (f arg)) rest ...)
     (display "eta-1\n")
     (normalize `(,(normalize f) ,@(normalize rest))))
    (else expr)))
