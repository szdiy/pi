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
  #:export (cps-list?

            cps cps?
            cps-kont cps-kont-set!
            cps-name cps-name-set!
            cps-attr cps-attr-set!

            lambda/k lambda/k?
            lambda/k-args lambda/k-args-set!
            lambda/k-body lambda/k-body-set!
            make-lambda/k new-lambda/k

            closure/k closure/k?
            closure/k-env closure/k-env-set!
            closure/k-body closure/k-body-set!
            make-closure/k new-closure/k

            constant/k constant/k?
            constant/k-value constant/k-value-set!
            make-constant/k new-constant/k

            bind-special-form/k bind-special-form/k?
            bind-special-form/k-var bind-special-form/k-var-set!
            bind-special-form/k-value bind-special-form/k-value-set!
            bind-special-form/k-body bind-special-form/k-body-set!

            letval/k letval/k?
            make-letval/k new-letval/k

            letfun/k letfun/k?
            make-letfun/k new-letfun/k

            letcont/k letcont/k?
            make-letcont/k new-letcont/k

            branch/k branch/k?
            branch/k-cnd branch/k-cnd-set!
            branch/k-tbranch branch/k-tbranch-set!
            branch/k-fbranch branch/k-fbranch-set!
            make-branch/k new-branch/k

            collection/k collection/k?
            collection/k-var collection/k-var-set!
            collection/k-type collection/k-type-set!
            collection/k-size collection/k-size-set!
            collection/k-value collection/k-value-set!
            make-collection/k new-collection/k

            seq/k seq/k?
            seq/k-exprs seq/k-exprs-set!
            make-seq/k new-seq/k

            app/k app/k?
            app/k-func app/k-func-set!
            app/k-args app/k-args-set!
            make-app/k new-app/k

            cont-apply
            lambda-desugar

            union diff insec
            free-vars names bound-vars all-ref-vars
            make-ref-table

            alpha-renaming

            ast->cps
            cps->expr
            top-level->src))

;; kontext means kontinuation-context

;; According to <<Compiling with continuations, continued>> - Andrew Kennedy
;; The principle to design CPS IR:
;; 1. All intermediate results are named, that is to say, we use a special binding
;;    to hold the intermediate result.
;; 2. There're 2 kinds of application:
;;    a. Translation-time application, which will not be reduced to normal-form in
;;       the translation.
;;    b.
;; 3. One-pass CPS translation, which introduce no 'administrative-reduction' that
;;    must be removed in a seperated phase.

(define (valid-expr? x)
  (or (cps? x)
      (constant? x)
      (id? x)))

(define (expr-list? lst)
  (make-object-list-pred lst valid-expr?))

(define (cps-list? lst)
  (make-object-list-pred lst cps?))

(define-typed-record cps
  (fields
   ;; the current continuation
   ;; TODO: capture the current continuation
   (kont (lambda (x) (or (id? x) (primitive? x) (cps? x))))
   (name id?) ; the unique name of the continuation
   (attr list?))) ; attributes of the continuation

(define-typed-record lambda/k (parent cps)
  (fields
   (args id-list?)
   (body valid-expr?)))
(define* (new-lambda/k args body #:key (kont prim:halt) (name (new-id "kont-"))
                       (attr '()))
  (make-lambda/k (list kont name attr) args body))

(define-typed-record closure/k (parent cps)
  (fields
   (env id-list?)
   (body valid-expr?)))
(define* (new-closure/k args body #:key (kont prim:halt) (name (new-id "kont-"))
                        (attr '()))
  (make-lambda/k (list kont name attr) env body))

(define-typed-record constant/k (parent cps)
  (fields
   (value constant?)))
(define* (new-constant/k value #:key (kont prim:halt) (name (new-id "kont-"))
                         (attr '()))
  (make-constant/k (list kont name attr) value))

(define-typed-record bind-special-form/k (parent cps)
  (fields
   (var id?)
   (value cps?)
   (body cps?)))

(define-typed-record letval/k (parent bind-special-form/k))
(define* (new-letval/k var value body #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (attr '()))
  (make-letval/k (list kont name attr) var value body))

(define-typed-record letfun/k (parent bind-special-form/k))
(define* (new-letfun/k var value body #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (attr '()))
  (make-letfun/k (list kont name attr) var value body))

(define-typed-record letcont/k (parent bind-special-form/k))
(define* (new-letcont/k var value body #:key (kont prim:halt)
                        (name (new-id "kont-"))
                        (attr '()))
  (make-letcont/k (list kont name attr) var value body))

(define-typed-record branch/k (parent cps)
  (fields
   (cnd cps?)
   (tbranch letcont/k?)
   (fbranch letcont/k?)))
(define* (new-branch/k cnd b1 b2 #:key (kont prim:halt)
                       (name (new-id "kont-"))
                       (attr '()))
  (make-branch/k (list kont name attr) cnd b1 b2))

(define-typed-record collection/k (parent cps)
  (fields
   (var id?)
   (type symbol?)
   (size integer?)
   (value any?)))
(define* (new-collection/k cname type size value body
                           #:key (kont prim:halt)
                           (name (new-id "kont-"))
                           (attr '()))
  (make-collection/k (list kont name attr) cname type size value body))

(define-typed-record seq/k (parent cps)
  (fields
   (exprs expr-list?)))
(define* (new-seq/k exprs #:key (kont prim:halt)
                    (name (new-id "kont-"))
                    (attr '()))
  (make-seq/k (list kont name attr) exprs))

(define (applicable? x)
  (or (letfun/k? x) (primitive? x) (lambda/k? x) (closure/k? x)
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
   (args list?)))
(define* (new-app/k f args #:key (kont prim:halt)
                    (name (new-id "kont-"))
                    (attr '()))
  (make-app/k (list kont name attr)
              f (if (list? args) args (list args))))

(define (cont-apply f e)
  (make-app/k (list prim:halt (new-id "kont-") (new-id "k-")) f e))

(define (lambda-desugar cps)
  (match cps
    ((? bind-special-form/k? bsf)
     (make-lambda/k (list (cps-kont bsf) (cps-name bsf) (cps-attr bsf))
                    (list (bind-special-form/k-var bsf))
                    (bind-special-form/k-value bsf)))
    (else cps)))

(define (vars-fold rec acc op expr)
  (let ((expr (lambda-desugar expr)))
    (match expr
      ((? id? id) (list id))
      #;
      ((? assign? expr) ; all-subx-fv + assigned-var
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
(define (sym-insec . args) (apply lset-intersection eq? args))

(define* (free-vars expr #:optional (refresh? #f))
  (cond
   ((and (not refresh?) (assoc-ref (cps-attr expr) 'free-vars)) => identity)
   (else
    (let ((fv (vars-fold free-vars union diff expr))
          (attr (cps-attr expr)))
      (set! attr (cons (cons 'free-vars fv) attr))
      fv))))

(define* (names expr #:optional (refresh? #f))
  (cond
   ((and (not refresh?) (assoc-ref (cps-attr expr) 'var-names)) => identity)
   (else
    (let ((nv (vars-fold names union union expr))
          (attr (cps-attr expr)))
      (set! attr (cons (cons 'var-names nv) attr))
      nv))))

;; NOTE: free-vars <= names, so diff is enough
(define (bound-vars expr) (apply diff (names expr) (free-vars expr)))
(define (all-ref-vars expr) (vars-fold all-ref-vars append append expr))

;; The all-ref-vars will count all appear variables, include the local definition,
;; so it has to be performed after these two steps:
;; 1. dead-variable-elimination
;; 2. alpha-renaming.
;; rules:
;; cnt == 1 means it's free-var
;; cnt == 2 means it should be inlined
;; cnt > 2, leave it as it is
(define (make-ref-table expr)
  (let ((vl (all-ref-vars expr))
        (ht (make-hash-table)))
    (for-each (lambda (v)
                (hash-set! ht v (1+ (hash-ref ht v 0))))
              vl)
    ht))

;; cps -> symbol-list -> id-list
(define (alpha-renaming expr old new)
  (define (rename e)
    (cond
     ((list-index (lambda (x) (id-eq? x e)) old)
      => (lambda (i) (list-ref new i)))
     (else e)))
  (match expr
    (($ lambda/k _ fargs body)
     (cond
      ((null? (sym-insec fargs old))
       (lambda/k-body-set! expr (alpha-renaming body old new))
       expr)
      ;; new binding, don't rename more deeply anymore
      (else expr)))
    (($ app/k _ f e)
     ;;(format #t "alpha 1 ~a~%" expr)
     (app/k-func-set! expr (alpha-renaming f old new))
     (app/k-args-set! expr (map (lambda (ee) (alpha-renaming ee old new)) e))
     expr)
    (($ seq/k _ e)
     ;;(format #t "alpha 2 ~a~%" expr)
     (seq/k-exprs-set! expr (map (lambda (ee) (alpha-renaming ee old new)) e))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (alpha-renaming (bind-special-form/k-value expr) old new))
     (bind-special-form/k-body-set!
      expr (alpha-renaming (bind-special-form/k-body expr) old new))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (alpha-renaming cnd old new))
     (branch/k-tbranch-set! expr (alpha-renaming b1 old new))
     (branch/k-fbranch-set! expr (alpha-renaming b2 old new))
     expr)
    ((? id?) (rename expr))
    (else expr)))

(define (comp-cps expr cont)
  (match expr
    (($ closure ($ ast _ body) params _ _)
     (let* ((fname (new-id "func-"))
            (fk (new-id "kont-"))
            (nv (map new-id params))
            (fun (alpha-renaming (new-lambda/k params (ast->cps body fk)
                                               #:name fk #:kont cont)
                                 params nv)))
       (new-letfun/k fname fun (new-app/k cont fname #:kont cont) #:kont cont)))
    (($ binding ($ ast _ body) var val)
     (let* ((jname (new-id "jcont-"))
            (nv (new-id var))
            (fk (new-id "letcont/k-"))
            (jcont (alpha-renaming
                    (new-lambda/k (list nv) (comp-cps body cont) #:kont cont)
                    (list var) (list nv))))
       (new-letcont/k jname jcont
                      (alpha-renaming (ast->cps val jname) (list var) (list nv))
                      #:kont cont)))
    (($ branch ($ ast _ (cnd b1 b2)))
     (let* ((arg (new-id))
            (jname (new-id "jcont-"))
            (kname (new-id "kont-"))
            (k1 (new-id "letcont/k-"))
            (k2 (new-id "letcont/k-"))
            (kont2 (new-letcont/k k2 (new-lambda/k '() (ast->cps b2 jname)
                                                   #:kont cont)
                                  (new-branch/k kname k1 k2 #:kont cont)))
            (kont1 (new-letcont/k k1 (new-lambda/k '() (ast->cps b1 jname)
                                                   #:kont cont)
                                  kont2))
            (kont3
             ;; According to Kennedy's, we add a local continuation here
             (new-lambda/k (list kname)
                           (new-letcont/k
                            jname
                            (new-lambda/k (list arg) (new-app/k cont arg)
                                          #:kont cont)
                            kont1) #:kont cont)))
       (comp-cps cnd kont3)))
    (else (ast->cps expr cont))))

(define* (ast->cps expr #:optional (cont prim:halt))
  (match expr
    ;; FIXME: distinct value and function for the convenient of fun-inline.
    (($ closure ($ ast _ body) params _ _)
     (let* ((fname (new-id "func-"))
            (fk (new-id "kont-"))
            (nv (map new-id params))
            (fun (alpha-renaming
                  (new-lambda/k nv (ast->cps body fk) #:name fk)
                  params nv)))
       (new-letfun/k fname fun (new-app/k cont fname) #:kont cont)))
    (($ def ($ ast _ body) var)
     ;; NOTE: The local function definition should be converted to let-binding
     ;;       by AST builder. So the definition that appears here are top-level.
     ;; NOTE: And the local function definition will be lifted to top-level later.
     (top-level-set! var (ast->cps body cont))
     *pi/unspecified*)
    (($ binding ($ ast _ body) ($ ref _ var) value)
     (let* ((jname (new-id "jcont-"))
            (ov (new-id var #f))
            (nv (new-id var))
            (fk (new-id "letcont/k-"))
            (jcont (new-lambda/k
                    (list nv)
                    (alpha-renaming (ast->cps body cont) (list ov) (list nv))
                    #:kont cont)))
       (new-letcont/k jname jcont
                      (alpha-renaming (ast->cps value jname) (list ov) (list nv))
                      #:kont cont)))
    (($ branch ($ ast _ (cnd b1 b2)))
     (let* ((arg (new-id))
            (jname (new-id "jcont-"))
            (kname (new-id "kont-"))
            (k1 (new-id "letcont/k-"))
            (k2 (new-id "letcont/k-"))
            (kont2 (new-letcont/k k2 (new-lambda/k '() (ast->cps b2 jname))
                                  (new-branch/k kname k1 k2) #:kont cont))
            (kont1 (new-letcont/k k1 (new-lambda/k '() (ast->cps b1 jname)
                                                   #:kont cont)
                                  kont2 #:kont cont))
            (kont3 (new-lambda/k (list kname) kont1 #:kont cont)))
       (comp-cps cnd kont3)))
    (($ collection ($ ast _ vals) type size)
     (let ((cname (new-id "c-"))
           (ex (map (lambda (_) (new-id "e-")) vals)))
       (fold (lambda (e x p) (ast->cps e (new-lambda/k x p #:kont cont)))
             (new-collection/k cname type size ex (new-app/k cont cname #:kont cont)
                               #:kont cont)
             vals ex)))
    (($ seq ($ ast _ exprs))
     (let* ((el (fold (lambda (x p)
                        (let ((ret (ast->cps x)))
                          (if (is-unspecified-node? ret) p (cons ret p))))
                      '() exprs))
            (ev (map (lambda (_) (new-id "k-")) el)))
       (fold (lambda (e v p) (new-letcont/k v e p #:kont cont))
             (new-app/k cont (new-seq/k ev #:kont cont) #:kont cont)
             el ev)))
    (($ call _ ($ ref _ f) e)
     (let* ((fn (new-id "f-"))
            (el (map (lambda (_) (new-id "x-")) e))
            (is-prim? (is-op-a-primitive? f))
            (k (fold (lambda (ee ex p)
                       (comp-cps ee (new-lambda/k (list ex) p #:kont cont)))
                     (cond
                      (is-prim?
                       (new-app/k cont (new-app/k is-prim? el #:kont cont)
                                  #:kont cont))
                      (else
                       (new-app/k fn (append (list cont) el) #:kont cont)))
                     e el)))
       (comp-cps (or is-prim? (new-id f #f))
                 (new-lambda/k (list fn) k #:kont cont))))
    (($ ref _ sym)
     (cond
      ((is-op-a-primitive? sym)
       => (lambda (p)
            (new-app/k cont p #:kont cont)))
      ((symbol? sym) (new-app/k cont (new-id sym #f) #:kont cont))
      (else (throw 'pi-error 'ast->cps "BUG: ref should be symbol! `~a'" sym))))
    ((? id? id) (new-app/k cont id #:kont cont))
    ((? primitive? p) (new-app/k cont p #:kont cont))
    ((? constant? c)
     (let ((x (new-id "x-"))
           (cst (new-constant/k c)))
       (new-letval/k x cst (new-app/k cont x #:kont cont) #:kont cont)))
    ;; TODO: Add more:
    ;; set!
    ;; collection-set! collection-ref
    (else (throw 'pi-error 'ast->cps "Wrong expr: " expr))))

(define* (cps->expr cpse #:optional (hide-begin? #t))
  (match cpse
    (($ lambda/k _ args body)
     `(lambda (,@(map cps->expr args)) ,(cps->expr body)))
    (($ closure/k _ env body)
     `(lambda (,@(env->args env)) ,body))
    (($ branch/k _ cnd b1 b2)
     `(if ,(cps->expr cnd) ,(cps->expr b1) ,(cps->expr b2)))
    (($ collection/k _ var type size value body)
     `(collection ,type ,@value))
    (($ seq/k _ exprs)
     (if hide-begin?
         (map cps->expr exprs)
         `(begin ,@(map cps->expr exprs))))
    (($ letfun/k ($ bind-special-form/k _ fname fun body))
     `(letfun ((,(cps->expr fname) ,(cps->expr fun))) ,(cps->expr body)))
    (($ letcont/k ($ bind-special-form/k _ jname jcont body))
     `(letcont ((,(cps->expr jname) ,(cps->expr jcont))) ,(cps->expr body)))
    (($ letval/k ($ bind-special-form/k _  var value body))
     `(letval ((,(cps->expr var) ,(cps->expr value))) ,(cps->expr body)))
    (($ app/k _ f e)
     `(,(cps->expr f) ,@(map cps->expr e)))
    (($ constant/k _ ($ constant _ val type)) val)
    (($ id _ name _) name)
    (($ primitive _ name _ _ _) name)
    (else (throw 'pi-error 'cps->expr "Wrong cps: " cpse))))

(define (top-level->src)
  (hash-map->list (lambda (v e) `(define ,v ,(cps->expr e))) *top-level*))
