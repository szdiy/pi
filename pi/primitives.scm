;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2019
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
  #:use-module (pi sasm)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (primitive-name
            primitive-arity
            primitive-has-side-effact?
            primitive-impl
            define-primitive

            ;; ----------------------------------
            make-prim
            prim?
            prim-name prim-label prim-proc
            is-primitive?))

(define-typed-record primitive
  (fields
   (name symbol?)
   (arity integer?)
   (has-side-effect? boolean?)
   (impl procedure?)))

(define-syntax define-primitive
  (syntax-rules (#:with-side-effect)
    ((_ name func)
     (define-primitive name #f func))
    ((_ name #:with-side-effect func)
     (define-primitive 'name #t func))
    ((_ name side-effect? func)
     (make-primitive
      'name
      (procedure-arity func)
      side-effect?
      func))))


;; --------------------------------------------
(define-record-type prim (fields name label proc)) ; primitive

(define (caller-save x) #t)
(define (callee-save x) #t)
(define (emit-prim-template arity label . codes)
  `(,(caller-save arity)
    (label ,label)
    ,(callee-save arity)
    ,@codes
    (emit-ret)))

(define (%%add1 x) #t)
(define (%add1 x)
  (emit-prim-template
   1 "__prim_add1"
   (%%add1 x)))

(define (%%sub1 x) #t)
(define (%sub1 x)
  (emit-prim-template
   1 "__prim_sub1"
   (%%sub1 x)))

(define (%%add args)
  (apply + args))
(define (%add . args)
  (emit-prim-template
   #f "__prim_add"
   (%%add args)))

(define (%%sub args)
  (apply + args))
(define (%sub . args)
  (emit-prim-template
   #f "__prim_add"
   (%%add args)))

(define *primitives*
  `((1+ . ,(make-prim '1+ "__prim_add1" %add1))
    (1- . ,(make-prim '1- "__prim_sub1" %sub1))
    (+  . ,(make-prim '+ "__prim_add"  %add))
    (-  . ,(make-prim '- "__prim_sub"  %sub))
    ))

(define (is-primitive? op) (assoc-ref *primitives* op))

(define (get-prim op)
  (or (and=> (assq-ref *primitives* op) caddr)
      (throw 'pi-error "BUG: Invalid primitive!" op)))
