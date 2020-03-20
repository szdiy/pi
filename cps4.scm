(import (ice-9 match)
        (srfi srfi-1))

(define (reserved? x)
  (memq x '(init-cont)))

(define (prim? p)
  (memq p '(+ - * / add)))

(define (atom? x)
  (or (number? x)
      (string? x)
      (null? x)))

;; Should be a dedicated record rather than symbol
(define (id? x)
  (and (symbol? x)
       (not (prim? x))
       (not (reserved? x))))

;; TODO
(define (assign? x) #f)

(define (free-vars cps)
  (define (union l1 l2 . args) (apply lset-union eq? l1 l2 args))
  (define (diff l1 l2 . args) (apply lset-difference eq? l1 l2 args))
  (match cps
    ((? id? id) (list id))
    #;
    ((? assign? cps) ; all-subx-fv + assigned-var ;
    ;; NOTE: it's reasonable to union assigned var, since there could be ;
    ;;       self assigment, say, n=n+1. For such situation, the fv is ;
    ;;       U{n,n} = n.         ;
    (union (free-vars (ast-subx ast))    ;
    (free-vars (assign-var ast))))
    (('lambda (args ...) body)
     ;; all-subx-fv - closure-params
     (diff (free-vars body) args))
    (('letval ((v e)) body)
     (free-vars `((lambda (,v) ,body) ,e)))
    (('letcont ((v e)) body)
     (free-vars `((lambda (,v) ,body) ,e)))
    ((f e ...)
     (apply union (map free-vars cps)))
    (else '())))

;; capture free substitute
(define (cfs expr args el)
  (define (substitute e)
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
    `(,(substitute s) ,@(cfs rest args el)))
    (((f e ...) rest ...)
     `((,(cfs f args el) ,@(map (lambda (ee) (cfs ee args el)) e))
       ,@(cfs rest args el)))
    ((e ...) (map (lambda (ee) (cfs ee args el)) e))
    (else (substitute expr))))

(define (alpha-renaming expr args)
  (let ((el (map (lambda (arg) (gensym (symbol->string arg))) args)))
    (cfs expr args el)))

(define (beta-reduction expr)
  (match expr
    ((('lambda (args ...) body) e e* ...)
     (display "beta 0\n")
     (beta-reduction (cfs (beta-reduction body) args (beta-reduction `(,e ,@e*)))))
    (((('lambda (args ...) body) e e* ...) rest ...)
     (display "beta 1\n")
     (beta-reduction `(,(cfs (beta-reduction body) args (beta-reduction `(,e ,@e*)))
                       ,@(beta-reduction rest))))
    (('lambda (args ...) body)
     (display "beta 2\n")
     (beta-reduction `(lambda (,@args)
                        ,(beta-reduction body))))
    (('letval ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,e)))
    (('letcont ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,e)))
    (else expr)))

(define (eta-reduction expr)
  (match expr
    (('lambda (arg) (f arg))
     (display "eta-0\n")
     (eta-reduction f))
    ((('lambda (arg) (f arg)) rest ...)
     (display "eta-1\n")
     (eta-reduction `(,(eta-reduction f) ,@(eta-reduction rest))))
    (else expr)))

(define (normalize expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction
              eta-reduction)))

(define (apply-cps expr cont)
  (normalize (pk "apply" `(,cont ,expr))))

(define (comp-cps expr cont)
  ;;  (expr->cps expr cont)
  (match expr
    (('lambda (v ...) body)
     (let ((f (gensym "f"))
           (k (gensym "k")))
       `(letval ((,f (lambda (,k ,@v) (expr->cps body ,k))))
          (,cont ,f))))
    (('let ((v e)) body)
     (let ((j (gensym "j"))
           (x (gensym "x")))
       `(letcont ((,j (lambda (,x) ,(comp-cps body cont))))
          ,(expr->cps e j))))
    (('if cnd b1 b2)
     (let ((k (gensym "k"))
           (j (gensym "j"))
           (x (gensym "x"))
           (k1 (gensym "k"))
           (x1 (gensym "x"))
           (k2 (gensym "k"))
           (x2 (gensym "x")))
       (comp-cps cnd
                 `(lambda (,k)
                    ;; According to Kennedy's, we add a local continuation here
                    (letcont ((,j (lambda (,x) (,cont ,x))))
                      (letcont ((,k1 (lambda (,x1) ,(expr->cps b1 j))))
                        (letcont ((,k2 (lambda (,x2) ,(expr->cps b2 j))))
                          (if ,k ,k1 ,k2))))))))
    (else (expr->cps expr cont))))

(define* (expr->cps expr #:optional (cont 'init-cont))
  (match expr
    (('lambda (v ...) body)
     (let ((f (gensym "f")) (j (gensym "k")))
       `(letval ((,f (lambda (,j ,@v) ,(expr->cps body j))))
          (,cont ,f))))
    (('define func body)
     `(fix ,func ,(expr->cps body cont)))
    (('if cnd b1 b2)
     (let ((tb (comp-cps expr cont))
           (fb (expr->cps b2 cont))
           (ck (expr->cps cnd cont)))
       `(if ,ck ,tb ,fb)))
    (('let ((v e)) body)
     (let ((j (gensym "j")))
       `(letcont ((,j (lambda (,v) ,(expr->cps body cont))))
          ,(expr->cps e j))))
    (('if cnd b1 b2)
     (let ((k (gensym "k"))
           (k1 (gensym "k"))
           (x1 (gensym "x"))
           (k2 (gensym "k"))
           (x2 (gensym "x")))
       (comp-cps cnd
                 `(lambda (,k)
                    (letcont ((,k1 (lambda (,x1) ,(expr->cps b1 cont))))
                      (letcont ((,k2 (lambda (,x2) ,(expr->cps b2 cont))))
                        (if ,k ,k1 ,k2)))))))
    (('collection type e ...)
     (let ((v (gensym "x"))
           (ex (map (lambda (_) (gensym "e")) e)))
       (fold (lambda (ee ex p)
               (expr->cps ee `(lambda (,ex) ,p)))
             `(letval ((,v (,type ,@ex)))
                (,cont ,v))
             e ex)))
    (('begin e ...) (map expr->cps e))
    ((? atom? expr)
     (let ((x (gensym "x")))
       `(letval ((,x ,expr)) (,cont ,x))))
    ((? prim? p) `(,cont ,expr))
    ((? id? expr) `(,cont ,expr))
    ((f e ...)
     (let* ((fn (gensym "f"))
            (el (map (lambda (_) (gensym "x")) e))
            (m (gensym "m"))
            (k cont)
            (f-expr `(,fn ,@el))
            (kf (gensym "k")))
       (comp-cps f
                 `(lambda (,fn)
                    ,(fold (lambda (ee ex p) (comp-cps ee `(lambda (,ex) ,p)))
                           (if (prim? f)
                               `(,cont (,fn ,@el))
                               `(,fn ,cont ,@el))
                           e el)))))
    (else
     #;`(,cont ,expr)
     (throw 'expr->cps "wrong expr:" expr))))

(define (beta-collection cexp)
  (match cexp
    (('letval ((v clct)) ))
    )
  )

(define (shrink cps)
  #t)

;;; Local Variables:
;;; eval: (put 'letcont 'scheme-indent-function 1)
;;; eval: (put 'letval 'scheme-indent-function 1)
;;; End:
