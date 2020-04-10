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
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (ast->cps))

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

(define-typed-record cps
  (fields
   (kont cps?)
   (name id?)
   ;; The result of the current expr will be bound to karg, and be passed to kont
   (karg id?)))

(define-typed-record bind-special-form/k (parent cps)
  (fields
   (var id?)
   (value cps?)))

(define-typed-record lambda/k (parent cps)
  (fields
   (args list?)
   (body cps?)))

(define-typed-record letval/k (parent bind-special-form/k))
(define-typed-record letfun/k (parent bind-special-form/k))
(define-typed-record letcont/k (parent bind-special-form/k))

(define-typed-record branch/k (parent cps)
  (fields
   (cnd cps?)
   (tbranch letcont/k?)
   (fbranch letcont/k?)))

(define-typed-record collection/k (parent cps)
  (fields
   (type symbol?)
   (size integer?)
   (value any?)))

(define-typed-record seq/k (parent cps)
  (fields
   (exprs list?)))

(define-typed-record define/k (parent cps)
  (fields
   (name id?)
   (value cps?)))

(define-typed-record app/k (parent cps)
  (fields
   (func letfun/k?)
   (args list?)))

(define (lambda-desugar cps)
  (match cps
    (($ bind-special-form/k ($cps kont name) var value)
     (make-lambda/k kont name (list var) value))
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
      (($ define/k _ f body)
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
     (make-lambda/k kont name karg fargs (cfs body args el)))
    (($ app/k ($ cps _ kont name karg) f e) ((f e ...) rest ...)
     ;;(format #t "cfs 1 ~a~%" expr)
     (make-app/k kont name karg (cfs f args el) (cfs e args el)))
    (($ seq/k ($ cps _ kont name karg) exprs)
     ;;(format #t "cfs 2 ~a~%" expr)
     (make-seq/k kont name karg (map (lambda (ee) (cfs ee args el)) exprs)))
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
    (($ lambda/k ($ cps _ kont name karg) fargs body)
     (if (null? (insec fargs old))
         (make-lambda/k kont name karg fargs (alpha-renaming body old new))
         ;; new binding, don't rename more deeply anymore
         expr))
    (($ app/k ($ cps _ kont name karg) f e)
     ;;(format #t "alpha 1 ~a~%" expr)
     (make-app/k kont name karg (alpha-renaming f old new)
                 (map (lambda (ee) (alpha-renaming ee old new)) e)))
    (($ seq/k ($cps _ kont name karg) exprs)
     ;;(format #t "alpha 2 ~a~%" expr)
     (make-seq/k kont name karg (map (lambda (ee) (alpha-renaming ee old new)) e)))
    (else
     ;;(format #t "alpha 4 ~a~%" expr)
     (rename expr))))

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
     (make-lambda/k kont name karg args (beta-reduction/preserving body)))
    (($ branch/k ($ cps _ kont name karg) cnd b1 b2)
     (make-branch/k kont name karg
                    (beta-reduction/preserving cnd)
                    (beta-reduction/preserving b1)
                    (beta-reduction/preserving b2)))
    (($ seq/k ($ cps _ kont name karg) e)
     (make-seq/k kont name karg (map beta-reduction/preserving e)))
    (else expr)))

(define* (cps->src cps #:optional (hide-begin? #t))
  (match cps
    ;; TODO
    ))
