(import (ice-9 match)
        (srfi srfi-1)
        (rnrs))

(define-record-type unspecified)
(define *unspecified* (make-unspecified))

(define *top-level* (make-hash-table))
(define (topdef! x v) (hash-set! *top-level* x v) #f)
(define (topref x) (hash-ref *top-level* x))
(define (topdel! x) (hash-remove! *top-level* x))

(define (reserved? x)
  (memq x '(init-cont begin)))

(define (halt x) `(halt ,x))

(define (prim? p)
  (memq p '(+ - * / add halt)))

(define (atom? x)
  (or (number? x)
      (string? x)
      (boolean? x)
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
(define (insec . args) (apply lset-intersection eq? args))

(define (bind-special-form? x)
  (memq x '(letcont let letcst letval letfun)))

(define (lambda-desugar cps)
  (match cps
    (((? bind-special-form?) ((v e)) body)
     `((lambda (,v) ,body) ,e))
    (else cps)))

(define (vars-fold rec acc op cps)
  (let ((expr (lambda-desugar cps)))
    (match expr
      ((? id? id) (list id))
      #;
      ((? assign? cps) ; all-subx-fv + assigned-var
      ;; NOTE: it's reasonable to union assigned var, since there could be
      ;;       self assigment, say, n=n+1. For such situation, the fv is
      ;;       U{n,n} = n.
      (union (proc (ast-subx ast))
      (proc (assign-var ast))))
      (((or 'define 'fix) f body)
       (op (rec body) (list f)))
      (('lambda (args ...) body)
       (op (rec body) args))
      (('if rest ...)
       (apply acc (map rec rest)))
      (('begin e ...)
       (apply acc (map rec e)))
      ((f e ...)
       (apply acc (map rec expr)))
      (else '()))))

(define (free-vars cps) (vars-fold free-vars union diff cps))
(define (names cps) (vars-fold names union union cps))
;; NOTE: free-vars <= names, so diff is enough
(define (bound-vars cps) (apply diff (names cps) (free-vars cps)))
(define (all-ref-vars cps) (vars-fold all-ref-vars append append cps))

;; The all-ref-vars will count all appear variables, include the local definition,
;; so it has to be performed after these two steps:
;; 1. dead-variable-elimination
;; 2. alpha-renaming.
;; rules:
;; cnt == 1 means it's free-var
;; cnt == 2 means it should be inlined
;; cnt > 2, leave it as it is
(define (make-ref-table cps)
  (let ((vl (all-ref-vars cps))
        (ht (make-hash-table)))
    (for-each (lambda (v)
                (hash-set! ht v (1+ (hash-ref ht v 0))))
              vl)
    ht))

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
    (error 'cfs (format #f "BUG: expr: ~a, args: ~a, el: ~a~%" expr args el)))
  (match expr
    (('lambda (v ...) body)
     ;;(format #t "cfs 0 ~a~%" expr)
     `(lambda (,@v) ,(cfs body args el)))
    #;
    (((? symbol? s) rest ...)           ;
    `(,(substitute s) ,@(cfs rest args el)))
    (((f e ...) rest ...)
     ;;(format #t "cfs 1 ~a~%" expr)
     `((,(cfs f args el) ,@(map (lambda (ee) (cfs ee args el)) e))
       ,@(cfs rest args el)))
    (('begin e ...)
     ;;(format #t "cfs 2 ~a~%" expr)
     `(begin ,@(map (lambda (ee) (cfs ee args el)) e)))
    ((e ...)
     ;;(format #t "cfs 3 ~a~%" expr)
     (map (lambda (ee) (cfs ee args el)) e))
    (else
     ;;(format #t "cfs 4 ~a~%" expr)
     (substitute expr))))

(define (alpha-renaming expr old new)
  (define (rename e)
    (cond
     ((list-index (lambda (x) (eq? x e)) old)
      => (lambda (i) (list-ref new i)))
     (else e)))
  (match expr
    (('lambda (v ...) body)
     (if (null? (insec v old))
         `(lambda (,@v) ,(alpha-renaming body old new))
         ;; new binding, don't rename more deeply anymore
         expr))
    (((f e ...) rest ...)
     ;;(format #t "alpha 1 ~a~%" expr)
     `((,(alpha-renaming f old new)
        ,@(map (lambda (ee) (alpha-renaming ee old new)) e))
       ,@(alpha-renaming rest old new)))
    (('begin e ...)
     ;;(format #t "alpha 2 ~a~%" expr)
     `(begin ,@(map (lambda (ee) (alpha-renaming ee old new)) e)))
    ((e ...)
     ;;(format #t "alpha 3 ~a~%" expr)
     (map (lambda (ee) (alpha-renaming ee old new)) e))
    (else
     ;;(format #t "alpha 4 ~a~%" expr)
     (rename expr))))

(define (beta-reduction/preserving expr)
  (match expr
    ((('lambda (args ...) body) e e* ...)
     ;;(display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           args
           (beta-reduction/preserving `(,e ,@e*)))))
    (((('lambda (args ...) body) e e* ...) rest ...)
     ;;(display "beta 1\n")
     (beta-reduction/preserving
      `(,(cfs (beta-reduction/preserving body)
              args
              (beta-reduction/preserving `(,e ,@e*)))
        ,@(beta-reduction/preserving rest))))
    (('lambda (args ...) body)
     ;;(display "beta 2\n")
     `(lambda (,@args) ,(beta-reduction/preserving body)))
    (('if cnd b1 b2)
     `(if ,(beta-reduction/preserving cnd)
          ,(beta-reduction/preserving b1)
          ,(beta-reduction/preserving b2)))
    (('begin e ...)
     `(begin ,@(map beta-reduction/preserving e)))
    (((? symbol? f) args ...)
     `(,f ,@(map beta-reduction/preserving args)))
    (else expr)))

(define (beta-reduction expr)
  (match expr
    ((('lambda (args ...) body) e ...)
     ;;(format #t "beta 0 ~a~%" expr)
     (beta-reduction (cfs (beta-reduction body) args (map beta-reduction e))))
    (((('lambda (args ...) body) e ...) rest ...)
     ;;(format #t "beta 1 ~a~%" expr)
     (beta-reduction `(,(cfs (beta-reduction body) args (map beta-reduction e))
                       ,@(beta-reduction rest))))
    (('lambda (args ...) body)
     ;;(format #t "beta 2 ~a \n" expr)
     `(lambda (,@args) ,(beta-reduction body)))
    (('letval ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,e)))
    (('letcont ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,(beta-reduction e))))
    (('letfun ((v e)) body)
     (beta-reduction `((lambda (,v) ,body) ,(beta-reduction e))))
    (('if cnd b1 b2)
     `(if ,(beta-reduction cnd)
          ,(beta-reduction b1)
          ,(beta-reduction b2)))
    (('begin e ...)
     `(begin ,@(map beta-reduction e)))
    (((? symbol? f) args ...)
     `(,f ,@(map beta-reduction args)))
    (else expr)))

(define (eta-reduction expr)
  (match expr
    (('lambda (arg) (f arg))
     ;;(display "eta-0\n")
     (eta-reduction f))
    ((('lambda (arg) (f arg)) rest ...)
     ;;(display "eta-1\n")
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
           (k (gensym "k"))
           (nv (map (lambda (v) (gensym (symbol->string v))) v)))
       `(letval ((,f ,(alpha-renaming `(lambda (,k ,@nv) ,(expr->cps body k) v)
                                      v nv)))
          (,cont ,f))))
    (('let ((v e)) body)
     (let ((j (gensym "j"))
           (nv (gensym (symbol->string v))))
       `(letcont ((,j (lambda (,nv) ,(alpha-renaming (comp-cps body cont)
                                                     (list v) (list nv)))))
          ,(alpha-renaming (expr->cps e j) (list v) (list nv)))))
    (('if cnd b1 b2)
     (let ((k (gensym "k"))
           (j (gensym "j"))
           (x (gensym "x"))
           (k1 (gensym "k"))
           (k2 (gensym "k")))
       (comp-cps cnd
                 `(lambda (,k)
                    ;; According to Kennedy's, we add a local continuation here
                    (letcont ((,j (lambda (,x) (,cont ,x))))
                      (letcont ((,k1 (lambda () ,(expr->cps b1 j))))
                        (letcont ((,k2 (lambda () ,(expr->cps b2 j))))
                          (if ,k ,k1 ,k2))))))))
    (else (expr->cps expr cont))))

(define* (expr->cps expr #:optional (cont 'halt))
  (match expr
    ;; FIXME: distinct value and function for the convenient of fun-inline.
    (('lambda (v ...) body)
     (let ((f (gensym "f"))
           (j (gensym "k"))
           (nv (map (lambda (v) (gensym (symbol->string v))) v)))
       `(letfun ((,f ,(alpha-renaming `(lambda (,j ,@nv) ,(expr->cps body j))
                                      v nv)))
          (,cont ,f))))
    (('define func body)
     ;; NOTE: The local function definition should be converted to let-binding
     ;;       by AST builder. So all the definition here are top-level.
     `(fix ,func ,(expr->cps body cont)))
    (('let ((v e)) body)
     ;; FIXME: here we only support single local binding
     (let ((j (gensym "j"))
           (x (gensym "x"))
           (nv (gensym (symbol->string v))))
       `(letcont ((,j (lambda (,nv) ,(alpha-renaming (expr->cps body cont)
                                                     (list v) (list nv)))))
          ,(alpha-renaming (expr->cps e j) (list v) (list nv)))))
    (('if cnd b1 b2)
     (let ((k (gensym "k"))
           (k1 (gensym "k"))
           (k2 (gensym "k")))
       (comp-cps cnd
                 `(lambda (,k)
                    (letcont ((,k1 (lambda () ,(expr->cps b1 cont))))
                      (letcont ((,k2 (lambda () ,(expr->cps b2 cont))))
                        (if ,k ,k1 ,k2)))))))
    (('collection type e ...)
     (let ((v (gensym "x"))
           (ex (map (lambda (_) (gensym "e")) e)))
       (fold (lambda (ee ex p)
               (expr->cps ee `(lambda (,ex) ,p)))
             `(letcst ((,v (,type ,@ex)))
                (,cont ,v))
             e ex)))
    (('begin e ...)
     (let* ((el (filter-map expr->cps e))
            (ev (map (lambda (_) (gensym "k")) el)))
       (fold (lambda (e v p)
               `(letcont ((,v ,e)) ,p))
             `(,cont (begin ,@ev))
             el ev)))
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

;; This includes dead-continuation and dead-variable elimination
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

;; NOTE: Please notice that we've converted local function binding to let-binding
;;       during AST building step, so the top-level function definition is the only
;;       thing that I need to take care. Because the local binding will be handled
;;       by dead-variable-eliminate.
;;       That's what we concern in dead-fun and fun-inline

;; Removes a function definition if it has no applied occurrences outside
;; its ownbody.
(define (dead-fun cps)
  (let ((funcs (hash-map->list (lambda (v _) v) *top-level*))
        (fv (free-vars cps)))
    (map topdel! (diff funcs fv))
    cps))

;; NOTE: It is also called beta-contract, which means inlining the function that
;;       appears only once.
;; unfortunately, we don't have env in this direct CPS implementation, so it's
;; too hard to trace back the definition of the function.
;; NOTE: Must be applied after alpha-renaming.
(define* (fun-inline cps #:optional (refs (make-ref-table cps)))
  (define (inlineable-local-func? f) (= 2 (hash-ref refs f 0)))
  (match cps
    (('letcont ((v e)) (letfun ((fname fbody)) _))
     (when (inlineable-local-func? fname)
       (beta-reduction/preserving
        `(,(fun-inline e refs) ,(fun-inline fbody refs)))))
    ((('lambda (v) body) e)
     (cond
      ((and (id? e) (topref e))
       => (lambda (func-body)
            (topdel! e)
            (beta-reduction/preserving
             `((lambda (,v) ,(fun-inline body refs))
               ,(fun-inline func-body refs)))))
      (else `((lambda (,v) ,(fun-inline body refs)) ,e))))
    (((? bind-special-form? sf) ((v e)) body)
     `(,sf ((,v ,(fun-inline e refs))) ,(fun-inline body refs)))
    (else cps)))

;; dead mutually recursive function elimination
(define (dead-mrf-eliminate cps)
  ;; Removes a bundle of mutually recursive functions if none of them occurs
  ;; in the rest of the term
  (define (dead-bundle cps)
    #t)
  #t)

;; NOTE: not easy, we have to do effect-analysis first, only immutable projection
;;       could be folded.
(define (fold-projection cps)
  #t)

;; 1. (if 1 2 3) -k-> (if 1 2 3)
;; 2. (if 1 2 (+ 3 4)) -k-> (letcont ((k (halt (+ 4 3)))) (if 1 2 k))
(define (fold-constant cps)
  (match cps
    (('letval ((v e)) body)
     (fold-constant
      (normalize/preserving
       `((lambda (,v) ,(fold-constant body)) ,e))))
    (('letcont ((v ('halt (? atom? a)))) body)
     (fold-constant
      (normalize/preserving
       `((lambda (,v) ,(fold-constant body)) ,a))))
    ((('lambda (v) body) e ...)
     (fold-constant (normalize/preserving cps)))
    (((? bind-special-form? sf) ((v e)) body) ;
     `(,sf ((,v ,(fold-constant e))) ,(fold-constant body)))
    (else cps)))

;; NOTE: fold-constant should be applied before.
;; NOTE: after eliminate the dead branch, it's necessary to apply
;;       dead-variable-eliminate to reduce the unused branch continuation binding.
(define (fold-branch cps)
  (define (detect e)
    (match e
      ((? boolean? b) b)
      (else 'no)))
  (match cps
    (('if cnd b1 b2)
     (let ((result (detect cnd)))
       (match result
         ('no cps)
         (#t b1)
         (else b2))))
    ((('lambda (v) body) e ...)
     `((lambda (,v) ,(fold-branch body)) ,@(map fold-branch e)))
    (('lambda (v) body)
     `(lambda (,v) ,(fold-branch body)))
    (((? bind-special-form? sf) ((v e)) body)
     `(,sf ((,v ,(fold-branch e))) ,(fold-branch body)))
    (('begin rest ...)
     `(begin ,@(map fold-branch rest)))
    ((f args ...)
     `(,f ,@(map fold-branch args)))
    (else cps)))

;; NOTE: fold-constant must be applied before, otherwise it doesn't work.
;; FIXME: Only pure-functional primitives can be reduced.
(define (delta-reduction expr)
  (define (prim-fold p args)
    (if (every integer? args)
        (primitive-eval `(,p ,@args))
        `(,p ,@args)))
  (match expr
    ((('lambda (v) body) e ...)
     `((lambda (,v) ,(delta-reduction body)) ,@(map delta-reduction e)))
    (('lambda (v) body)
     `(lambda (,v) ,(delta-reduction body)))
    (((? bind-special-form? sf) ((v e)) body)
     `(,sf ((,v ,(delta-reduction e))) ,(delta-reduction body)))
    (('begin rest ...)
     `(begin ,@(map delta-reduction rest)))
    (((? id? f) args ...)
     `(,f ,@(map delta-reduction args)))
    (((? prim? p) args ...)
     (prim-fold p (map delta-reduction args)))
    (else expr)))

;; NOTE: We seperate dead-code-elimination to 2 steps:
;; 1. dead-variable-elimination: eliminate dead-variable and dead-continuation
;; 2. dead-mrf-eliminate: eliminate dead-mutually-recursive-function
;; 3. fold-branch: eliminate dead-branch code
(define (dead-code-eliminate cps)
  #t)

(define *optimizings*
  (list fun-inline
        dead-fun
        fold-constant
        delta-reduction
        fold-branch
        dead-variable-eliminate))

(define (shrink cps)
  (fold (lambda (o p) #;(format #t "~a: ~a~%" o p) (o p)
                )
        cps
        *optimizings*))

;;; Local Variables:
;;; eval: (put 'letcont 'scheme-indent-function 1)
;;; eval: (put 'letval 'scheme-indent-function 1)
;;; eval: (put 'letfun 'scheme-indent-function 1)
;;; eval: (put 'letcst 'scheme-indent-function 1)
;;; End:
