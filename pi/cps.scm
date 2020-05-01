;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019,2020
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Pi is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Pi is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (pi cps)
  #:use-module (pi utils)
  #:use-module (pi env)
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module (pi primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (ast->cps
            cps-list?))

(define-record-type cps ; super type for checking cps types
  (fields
   kont   ; The current continuation
   name)) ; The id of the continuation

;; kontext means kontinuation-context

;; According to <<Compiling with continuations, continued>> - Andrew Kennedy
;; The principle to design CPS IR:
;; 1. All intermediate results are named, that is to say, we use a special binding to hold
;;    the intermediate result.
;; 2. There're 2 kinds of application:
;;    a. Translation-time application, which will not be reduced to normal-form in the
;;       translation.
;;    b.
;; 3. One-pass CPS translation, which introduce no 'administrative-reduction' that must be
;;    removed in a seperated phase.
;;

(define (cps-list? lst)
  (make-object-list-pred lst cps?))

(define-typed-record cps
  (fields
   (kont (lambda (x) (or (eq? x prim:halt) (cps? x))))
   (name id?)
   ;; The result of the current expr will be bound to karg, and be passed to kont
   (karg id?)))

(define-typed-record lambda/k (parent cps)
  (fields
   (args id-list?)
   (body cps?)))
(define* (new-lambda/k args body #:key (kont prim:halt) (name (new-id "kont-"))
                       (karg (new-id "karg-")))
  (make-lambda/k (list kont name karg) args body))

(define-typed-record bind-special-form/k (parent cps)
  (fields
   (var id?)
   (value cps? constant?)
   (body cps?)))

(define-typed-record letval/k (parent bind-special-form/k))
(define* (new-letval/k var value body #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (karg (new-id "karg-")))
  (make-letval/k (list kont name karg) var value body))

(define-typed-record letfun/k (parent bind-special-form/k))
(define* (new-letfun/k var value body #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (karg (new-id "karg-")))
  (make-letfun/k (list kont name karg) var value body))

(define-typed-record letcont/k (parent bind-special-form/k))
(define* (new-letcont/k var value body #:key (kont prim:halt)
                        (name (new-id "kont-"))
                        (karg (new-id "karg-")))
  (make-letcont/k (list kont name karg) var value body))

(define-typed-record branch/k (parent cps)
  (fields
   (cnd cps?)
   (tbranch letcont/k?)
   (fbranch letcont/k?)))
(define* (new-branch/k cnd b1 b2 #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (karg (new-id "karg-")))
  (make-branch/k (list kont name karg) cnd b1 b2))

(define-typed-record collection/k (parent cps)
  (fields
   (var id?)
   (type symbol?)
   (size integer?)
   (value any?)))
(define* (new-collection/k cname type size value body
                           #:key (kont prim:halt)
                           (name (new-id "kont-"))
                           (karg (new-id "karg-")))
  (make-collection/k (list kont name karg) cname type size value body))

(define-typed-record seq/k (parent cps)
  (fields
   (exprs cps-list?)))
(define* (new-seq/k exprs #:key (kont prim:halt)
                    (name (new-id "kont-"))
                    (karg (new-id "karg-")))
  (make-seq/k (list kont name karg) exprs))

(define (applicable? x)
  (or (letfun/k? x) (primitive? x) (lambda/k? x)
      ;; FIXME: Not all id, should be the registered proc id
      (id? x)))
(define (valid-arg? x)
  (or (id-list? x)
      (cps-list? x)
      (cps? x)
      (id? x)
      (primitive? x)))
(define-typed-record app/k (parent cps)
  (fields
   (func applicable?)
   (args any?)))
(define* (new-app/k f args #:key (kont prim:halt)
                    (name (new-id "kont-"))
                    (karg (new-id "karg-")))
  (make-app/k (list kont name karg)
              f (if (list? args) args (list args))))

(define (cont-apply f e)
  (make-app/k (list prim:halt (new-id "kont-") (new-id "k-")) f e))

(define (lambda-desugar cps)
  (match cps
    ((? bind-special-form/k? bsf)
     (make-lambda/k (list (cps-kont bsf) (cps-name bsf) (cps-karg bsf))
                    (list (bind-special-form/k-var bsf))
                    (bind-special-form/k-value bsf)))
    (else cps)))

;; FIXME: Should we count karg?
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
      #;
      (($ define/k _ f body)            ;
      (op (rec body) (list f)))
      (($ lambda/k _ args body)
       (op (rec body) args))
      (($ branch/k _ cnd b1 b2)
       (apply acc (map rec (list cnd b1 b2))))
      (($ seq/k _ exprs)
       (apply acc (map rec exprs)))
      (($ app/k _ f args)
       (apply acc (map rec args)))
      (else '()))))

(define (union . args) (apply lset-union id-eq? args))
(define (diff . args) (apply lset-difference id-eq? args))
(define (insec . args) (apply lset-intersection id-eq? args))

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

;; capture free substitute
(define (cfs expr args el)
  (define (substitute e)
    (cond
     ((list-index (lambda (x) (eq? x e)) args)
      => (lambda (i) (list-ref el i)))
     (else e)))
  (when (not (= (length args) (length el)))
    (throw 'pi-error cfs
           (format #f "BUG: expr: ~a, args: ~a, el: ~a~%" expr args el)))
  (match expr
    (($ lambda/k ($ cps _ kont name karg) fargs body)
     ;;(format #t "cfs 0 ~a~%" expr)
     (new-lambda/k fargs (cfs body args el)
                   #:kont kont #:name name #:karg karg))
    (($ app/k ($ cps _ kont name karg) f e)
     ;;(format #t "cfs 1 ~a~%" expr)
     (new-app/k (cfs f args el) (cfs e args el)
                #:kont kont #:name name #:karg karg))
    (($ seq/k ($ cps _ kont name karg) exprs)
     ;;(format #t "cfs 2 ~a~%" expr)
     (new-seq/k (map (lambda (ee) (cfs ee args el)) exprs)
                #:kont kont #:name name #:karg karg))
    (else
     ;;(format #t "cfs 4 ~a~%" expr)
     (substitute expr))))

;; cps -> symbol-list -> id-list
(define (alpha-renaming expr old new)
  (define (rename e)
    (cond
     ((list-index (lambda (x) (eq? x e)) old)
      => (lambda (i) (list-ref new i)))
     (else e)))
  (match expr
    (($ lambda/k ($ cps _ kont name karg) fargs body)
     (if (null? (insec fargs old))
         (new-lambda/k fargs (alpha-renaming body old new)
                       #:kont kont #:name name #:karg karg)
         ;; new binding, don't rename more deeply anymore
         expr))
    (($ app/k ($ cps _ kont name karg) f e)
     ;;(format #t "alpha 1 ~a~%" expr)
     (new-app/k (alpha-renaming f old new)
                (map (lambda (ee) (alpha-renaming ee old new)) e)
                #:kont kont #:name name #:karg karg))
    (($ seq/k ($cps _ kont name karg) exprs)
     ;;(format #t "alpha 2 ~a~%" expr)
     (new-seq/k (map (lambda (ee) (alpha-renaming ee old new)) exprs)
                #:kont kont #:name name #:karg karg))
    (else
     ;;(format #t "alpha 4 ~a~%" expr)
     (rename expr))))

(define (beta-reduction expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           params
           (beta-reduction/preserving args))))
    (($ lambda/k ($ cps _ kont name karg) args body)
     ;;(display "beta 2\n")
     (new-lambda/k args (beta-reduction/preserving body)
                   #:kont kont #:name name #:karg karg))
    (($ branch/k ($ cps _ kont name karg) cnd b1 b2)
     (new-branch/k (beta-reduction/preserving cnd)
                   (beta-reduction/preserving b1)
                   (beta-reduction/preserving b2)
                   #:kont kont #:name name #:karg karg))
    (($ seq/k ($ cps _ kont name karg) e)
     (new-seq/k (map beta-reduction/preserving e)
                #:kont kont #:name name #:karg karg))
    ((? bind-special-form/k? bsf)
     (beta-reduction
      (new-app/k (make-lambda/k (list (cps-kont bsf) (cps-name bsf) (cps-karg bsf))
                                (list (bind-special-form/k-var bsf))
                                (bind-special-form/k-body bsf))
                 (beta-reduction (bind-special-form/k-value bsf)))))
    (else expr)))

(define (beta-reduction/preserving expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           params
           (beta-reduction/preserving args))))
    (($ lambda/k ($ cps _ kont name karg) args body)
     ;;(display "beta 2\n")
     (new-lambda/k args (beta-reduction/preserving body)
                   #:kont kont #:name name #:karg karg))
    (($ branch/k ($ cps _ kont name karg) cnd b1 b2)
     (make-branch/k (beta-reduction/preserving cnd)
                    (beta-reduction/preserving b1)
                    (beta-reduction/preserving b2)
                    #:kont kont #:name name #:karg karg))
    (($ seq/k ($ cps _ kont name karg) exprs)
     (new-seq/k (map beta-reduction/preserving exprs)
                #:kont kont #:name name #:karg karg))
    (else expr)))

(define (eta-reduction expr)
  (match expr
    (($ lambda/k _ arg ($ app/k _ f (? (lambda (a) (id-eq? a arg)))))
     ;;(display "eta-0\n")
     (eta-reduction f))
    (($ seq/k ($ cps _ kont name karg) exprs)
     ;;(display "eta-1\n")
     (make-seq/k (list kont name karg) (map eta-reduction exprs)))
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

(define (comp-cps expr cont)
  (match expr
    (('lambda (v ...) body)
     (let* ((fname (new-id "func-"))
            (fk (new-id "karg-"))
            (nv (map new-id v))
            (fun (alpha-renaming (new-lambda/k v (expr->cps body fk) #:karg fk)
                                 v nv)))
       (new-letfun/k fname fun (new-app/k cont fname))))
    (('let ((v e)) body)
     (let* ((jname (new-id "jcont-"))
            (nv (new-id v))
            (fk (new-id "letcont/k-"))
            (jcont (alpha-renaming (new-lambda/k v (comp-cps body cont))
                                   (list v) (list nv))))
       (new-letcont/k jname jcont
                      (alpha-renaming (expr->cps e jname) (list v) (list nv)))))
    (('if cnd b1 b2)
     (let* ((arg (new-id))
            (jname (new-id "jcont-"))
            (kname (new-id "karg-"))
            (k1 (new-id "letcont/k-"))
            (k2 (new-id "letcont/k-"))
            (kont2 (new-letcont/k k2 (new-lambda/k '() (expr->cps b2 jname))
                                  (new-branch/k kname k1 k2)))
            (kont1 (new-letcont/k k1 (new-lambda/k '() (expr->cps b1 jname))
                                  kont2))
            (kont3
             ;; According to Kennedy's, we add a local continuation here
             (new-lambda/k (list kname)
                           (new-letcont/k
                            jname
                            (new-lambda/k (list arg) (new-app/k cont arg))
                            kont1))))
       (comp-cps cnd kont3)))
    (else (expr->cps expr cont))))

(define* (expr->cps expr #:optional (cont prim:halt))
  (match expr
    ;; FIXME: distinct value and function for the convenient of fun-inline.
    (('lambda (v ...) body)
     (let* ((fname (new-id "func-"))
            (fk (new-id "karg-"))
            (nv (map new-id v))
            (fun (alpha-renaming (new-lambda/k v (expr->cps body fk) #:karg fk)
                                 v nv)))
       (new-letfun/k fname fun (new-app/k cont fname))))
    (('define func body)
     ;; NOTE: The local function definition should be converted to let-binding
     ;;       by AST builder. So the definition that appears here are top-level.
     (top-level-set! func (expr->cps body cont))
     *pi/unspecified*)
    (('let ((v e)) body)
     (let* ((jname (new-id "jcont-"))
            (nv (new-id v))
            (fk (new-id "letcont/k-"))
            (jcont (alpha-renaming (new-lambda/k v (expr->cps body cont))
                                   (list v) (list nv))))
       (new-letcont/k jname jcont
                      (alpha-renaming (expr->cps e jname) (list v) (list nv)))))
    (('if cnd b1 b2)
     (let* ((arg (new-id))
            (jname (new-id "jcont-"))
            (kname (new-id "karg-"))
            (k1 (new-id "letcont/k-"))
            (k2 (new-id "letcont/k-"))
            (kont2 (new-letcont/k k2 (new-lambda/k '() (expr->cps b2 jname))
                                  (new-branch/k kname k1 k2)))
            (kont1 (new-letcont/k k1 (new-lambda/k '() (expr->cps b1 jname))
                                  kont2))
            (kont3 (new-lambda/k (list kname) kont1)))
       (comp-cps cnd kont3)))
    (('collection type e ...)
     (let ((cname (new-id "c-"))
           (ex (map (lambda (_) (new-id "e-")) e)))
       (fold (lambda (ee ex p) (expr->cps ee (new-lambda/k ex p)))
             (new-collection/k cname type (length ex) ex (new-app/k cont cname))
             e ex)))
    (('begin e ...)
     (let* ((el (fold (lambda (x p)
                        (let ((ret (expr->cps x cont)))
                          (if (is-unspecified-node? ret) p (cons ret p))))
                      '() e))
            (ev (map (lambda (_) (new-id "k-")) el)))
       (fold (lambda (e v p) (new-letcont/k v e p))
             (new-app/k cont (new-seq/k ev))
             el ev)))
    ((? atom? a)
     (let ((x (new-id "x-")))
       (new-letval/k x (gen-constant a) (new-app/k cont x))))
    ((? is-op-a-primitive? p) (new-app/k cont (symbol->primitive p)))
    ((? id? x) (new-app/k cont x))
    ((? symbol? s) (new-id s))
    ((f e ...)
     (let* ((fn (new-id "f-"))
            (el (map (lambda (_) (new-id "x-")) e))
            (k (fold (lambda (ee ex p)
                       (comp-cps ee (new-lambda/k (list ex) p)))
                     (if (is-op-a-primitive? f)
                         (new-app/k cont (new-app/k fn el))
                         (new-app/k fn (append (list cont) el)))
                     e el)))
       (comp-cps f (new-lambda/k (list fn) k))))
    ;; TODO: Add more:
    ;; set!
    ;; collection-set! collection-ref
    (else
     #;`(,cont ,expr)
     (throw 'pi-error 'expr->cps "Wrong expr: " expr))))

;; FIXME: Move all karg to cps, not in args.

(define* (cps->expr cps #:optional (hide-begin? #t))
  (match cps
    (($ lambda/k _ args body)
     `(lambda (,@(map cps->expr args)) ,(cps->expr body)))
    (($ branch/k _ cnd b1 b2)
     `(if ,(cps->expr cnd) ,(cps->expr b1) ,(cps->expr b2)))
    (($ collection/k _ var type size value body)
     `(collection ,type ,@value))
    (($ seq/k _ exprs)
     `(begin ,@exprs))
    (($ letfun/k ($ bind-special-form/k-value _ fname fun body))
     `(letfun ((,fname ,(cps->expr fun))) ,(cps->expr body)))
    (($ letcont/k ($ bind-special-form/k-value _ jname jcont body))
     `(letcont ((,jname ,(cps->expr jcont))) ,(cps->expr body)))
    (($ letval/k ($ bind-special-form/k _  var value body))
     `(letval ((,(cps->expr var) ,(cps->expr value))) ,(cps->expr body)))
    (($ app/k _ f e)
     `(,(cps->expr f) ,@(map cps->expr e)))
    (($ id _ name _) name)
    (($ constant _ val type) val)
    (($ primitive _ name _ _ _) name)
    (else (throw 'pi-error 'cps->expr "Wrong cps: " cps))))

(define (top-level->src)
  (hash-map->list (lambda (v e) `(define ,v ,(cps->expr e))) *top-level*))
