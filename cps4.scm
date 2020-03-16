(import (ice-9 match)
        (srfi srfi-1))

(define (prim? p)
  (memq p '(+ - * / add)))

(define (atom? x)
  (or (number? x)
      (string? x)
      (null? x)))

;; Should be a dedicated record rather than symbol
(define (id? x) (symbol? x))

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
    (('letval ((v e)) body)
     (normalize `((lambda (,v) ,body) ,e)))
    (('letcont ((v e)) body)
     (normalize `((lambda (,v) ,body) ,e)))
    (else expr)))

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
    #;
    ((f e ...)                          ;
    (let* ((fn (gensym "f"))            ;
    (el (map (lambda (_) (gensym "x")) e)) ;
    (m (gensym "m"))                    ;
    (k cont)                            ;
    (f-expr `(,fn ,@el))                ;
    (kf (gensym "k"))                   ;
    (j (gensym "k"))                    ;
    (jx (gensym "x")))                  ;
    (comp-cps f                         ;
    `(lambda (,fn)                      ;
    ,(fold (lambda (ee ex p) (comp-cps ee `(lambda (,ex) ,p))) ;
    ;; NOTE: The application of comp-cps won't appear in ;
    ;;       tail call position, so we have to keep this ;
    ;;       local continuation ;
    `(letcont ((,j (lambda (,jx) (,cont ,jx)))) ;
    ,(if (prim? f)                      ;
    `(,j (,fn ,@el))                    ;
    `(,fn ,j ,@el)))                    ;
    e el)))))
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
     (let ((j (gensym "j"))
           (x (gensym "x")))
       `(letcont ((,j (lambda (,x) ,(expr->cps body cont))))
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

;;; Local Variables:
;;; eval: (put 'letcont 'scheme-indent-function 1)
;;; eval: (put 'letval 'scheme-indent-function 1)
;;; End:
