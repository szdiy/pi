(import (ice-9 match)
        (srfi srfi-1))

(define (reserved? x)
  (memq x '(init-cont)))

(define (prim? p)
  (memq p '(+ - * / add halt)))

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

(define (union . args) (apply lset-union eq? args))
(define (diff . args) (apply lset-difference eq? args))

(define (bind-special-form? x)
  (memq x '(letcont let letcst letval)))

(define (lambda-desugar cps)
  (match cps
    (((? bind-special-form?) ((v e)) body)
     `((lambda (,v) ,body) ,e))
    (else cps)))

(define (lambda-sugarize cps body)
  (match cps
    (((? bind-special-form? sf) ((v e)) body)
     `(sf ((,v ,e)) ))
    (else cps)))

(define (vars-fold rec acc cps)
  (let ((expr (lambda-desugar cps)))
    (match expr
      ((? id? id) (list id))
      #;
      ((? assign? cps) ; all-subx-fv + assigned-var ; ;
      ;; NOTE: it's reasonable to union assigned var, since there could be ; ;
      ;;       self assigment, say, n=n+1. For such situation, the fv is ; ;
      ;;       U{n,n} = n.         ;      ;
      (union (proc (ast-subx ast))    ; ;
      (proc (assign-var ast))))
      (((or 'define 'fix) f body)
       (acc (rec body) (list f)))
      (('lambda (args ...) body)
       (acc (rec body) args))
      (('if rest ...)
       (apply union (map rec rest)))
      ((f e ...)
       (apply union (map rec expr)))
      (else '()))))

(define (free-vars cps) (vars-fold free-vars diff cps))
(define (names cps) (vars-fold names union cps))
;; NOTE: free-vars <= names, so diff is enough
(define (bound-vars cps) (apply diff (names cps) (free-vars cps)))

(define (unique-binding cps)
  #t
  )

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

(define (beta-reduction/preserving expr)
  (match expr
    ((('lambda (args ...) body) e e* ...)
     (display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           args
           (beta-reduction/preserving `(,e ,@e*)))))
    (((('lambda (args ...) body) e e* ...) rest ...)
     (display "beta 1\n")
     (beta-reduction/preserving
      `(,(cfs (beta-reduction/preserving body)
              args
              (beta-reduction/preserving `(,e ,@e*)))
        ,@(beta-reduction/preserving rest))))
    (('lambda (args ...) body)
     (display "beta 2\n")
     `(lambda (,@args) ,(beta-reduction/preserving body)))
    (('if cnd b1 b2)
     `(if ,(beta-reduction/preserving cnd)
          ,(beta-reduction/preserving b1)
          ,(beta-reduction/preserving b2)))
    (else expr)))

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
     (format #t "beta 2 ~a \n" expr)
     `(lambda (,@args) ,(beta-reduction body)))
    (('letval ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,e)))
    (('letcont ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,e)))
    (('if cnd b1 b2)
     `(if ,(beta-reduction cnd)
          ,(beta-reduction b1)
          ,(beta-reduction b2)))
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

(define (normalize/preserving expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction/preserving
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

(define* (expr->cps expr #:optional (cont 'halt))
  (match expr
    (('lambda (v ...) body)
     (let ((f (gensym "f")) (j (gensym "k")))
       `(letval ((,f (lambda (,j ,@v) ,(expr->cps body j))))
          (,cont ,f))))
    (('define func body)
     `(fix ,func ,(expr->cps body cont)))
    (('let ((v e)) body)
     ;; FIXME: here we only support single local binding
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
             `(letcst ((,v (,type ,@ex)))
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
  #t
  #;
  (match cexp                           ;
  (('letval ((v clct)) ))               ;
  )
  )

;; FIXME: We don't have to compute free-vars redundantly
(define (is-referenced? cps v)
  (memq v (free-vars cps)))

(define (dead-variable-eliminate cps)
  (match cps
    (('letcont ((cv ('lambda (v) body))) cont-body)
     (if (is-referenced? body v)
         `(letcont ((,v ,(dead-variable-eliminate `(lambda (,v) ,body))))
            ,(dead-variable-eliminate cont-body))
         (dead-variable-eliminate body)))
    (((? bind-special-form? sf) ((v e)) body)
     (if (is-referenced? body v)
         `(,sf ((,v ,(dead-variable-eliminate e)))
               ,(dead-variable-eliminate body))
         (dead-variable-eliminate body)))
    (((('lambda (v) body) e))
     ;; TODO: Here we just keep the variable which is referenced in the body,
     ;;       however, it is possible to further optimize the body so that the
     ;;       referencing can be eliminated.
     ;;       A better way is to do it again after all optimizings.
     (if (is-referenced? body v)
         `((lambda (,v) ,(dead-variable-eliminate body))
           ,(dead-variable-eliminate e))
         (dead-variable-eliminate body)))
    (('lambda (v) body)
     (if (is-referenced? body v)
         `(lambda (,v) ,(dead-variable-eliminate body))
         (dead-variable-eliminate body)))
    (else cps)))

;; dead mutually recursive function elimination
(define (dead-mrf-eliminate cps)
  ;; Removes a bundle of mutually recursive functions if none of them occurs
  ;; in the rest of the term
  (define (dead-bundle cps)
    #t)
  ;; Removes afunction definition if it has no applied occurrences outside
  ;; its ownbody.
  (define (dead-fun cps) #t)
  #t)

(define (fold-branch cps)
  #t
  )

(define (shrink cps)
  #t)

;;; Local Variables:
;;; eval: (put 'letcont 'scheme-indent-function 1)
;;; eval: (put 'letval 'scheme-indent-function 1)
;;; eval: (put 'letcst 'scheme-indent-function 1)
;;; End:
