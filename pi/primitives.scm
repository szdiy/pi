;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2020
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

(define-module (pi primitives)
  #:use-module (pi utils)
  #:use-module (pi sasm)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (symbol->primitive
            is-op-a-primitive?
            primitive-register!

            primitive
            primitive?
            primitive-name
            primitive-arity
            primitive-has-side-effact?
            primitive-impl
            define-primitive

            special-form
            make-special-form:if

            ;; ----------------------------------
            make-prim
            prim?
            prim-name prim-label prim-proc
            is-primitive?))

(define *primitives* (make-hash-table))
(define (symbol->primitive x) (hash-ref *primitives* x))
(define (is-op-a-primitive? x)
  (and (symbol? x)
       (symbol->primitive x)))
(define (primitive-register! p proc) (hash-set! *primitives* p proc))

;; NOTE: The implementation here, should be the generator or caller of
;;       any specific primitive.
(define-typed-record primitive
  (fields
   (name symbol?)
   (arity integer?)
   (has-side-effect? boolean?)
   (impl procedure?)))

(define-syntax define-primitive
  (syntax-rules (:has-side-effect)
    ((_ (name args ...) body ...)
     (define-primitive
       name #f
       (lambda (args ...) body ...)))
    ((_ (name args ...) :has-side-effect body ...)
     (define-primitive 'name #t (lambda (args ...) body ...)))
    ((_ name side-effect? func)
     (define-public
       name
       (make-primitive
        'name
        (car (procedure-minimum-arity func))
        side-effect?
        func)))))

;; halt can associate with primitive `halt', its activity is TOS.
(define-primitive (prim:halt x)
  #t)

#;
(define-primitive (+ args ...)
;; TODO: implemente + operator
#t)

#;
(define-primitive (set! v e)
;; TODO
#t)

;; (define-record-type special-form)
;; (define-typed-record special-form:if (parent special-form)
;;   (fields
;;    (cnd symbol?)
;;    (true symbol?)
;;    (false symbol?)))

;; ;; --------------------------------------------
;; ;; low-level abstraction of primitive
;; ;; Only used for code generation
;; (define-record-type prim (fields name label proc))

;; (define (caller-save x) #t)
;; (define (callee-save x) #t)
;; (define (emit-prim-template arity label . codes)
;;   `(,(caller-save arity)
;;     (label ,label)
;;     ,(callee-save arity)
;;     ,@codes
;;     (emit-ret)))

;; ;; halt can associate with primitive `halt', its activity is TOS.
;; (define (%%halt x) #t)
;; (define-primitive (%halt x)
;;   (emit-prim-template
;;    1 "__prim_halt"
;;    (%%halt x)))

;; (define (%%add1 x) #t)
;; (define-primitive (%add1 x)
;;   (emit-prim-template
;;    1 "__prim_add1"
;;    (%%add1 x)))

;; (define (%%sub1 x) #t)
;; (define-primitive (%sub1 x)
;;   (emit-prim-template
;;    1 "__prim_sub1"
;;    (%%sub1 x)))

;; (define-primitive (%%add args)
;;   (apply + args))
;; (define (%add . args)
;;   (emit-prim-template
;;    #f "__prim_add"
;;    (%%add args)))

;; (define-primitive (%%sub args)
;;   (apply + args))
;; (define-primitive (%sub . args)
;;   (emit-prim-template
;;    #f "__prim_add"
;;    (%%add args)))

;; (define *primitives-gen*
;;   `((halt  . ,(make-prim '- "__prim_halt"  %halt))
;;     (1+ . ,(make-prim '1+ "__prim_add1" %add1))
;;     (1- . ,(make-prim '1- "__prim_sub1" %sub1))
;;     (+  . ,(make-prim '+ "__prim_add"  %add))
;;     (-  . ,(make-prim '- "__prim_sub"  %sub))
;;     ))

;; (define (is-primitive-gen? op) (assoc-ref *primitives* op))

;; (define (get-prim op)
;;   (or (and=> (assq-ref *primitives* op) caddr)
;;       (throw 'pi-error "BUG: Invalid primitive!" op)))
