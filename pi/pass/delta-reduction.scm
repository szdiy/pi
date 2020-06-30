;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020
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

(define-module (pi pass delta-reduction)
  #:use-module (pi types)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (pi primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (cps-integer? expr)
  (match expr
    (($ constant/k _ ($ constant _ _ 'integer)) #t)
    (else #f)))

;; primitive -> cps-list -> cps
(define (prim-apply p args)
  (apply (primitive-impl p)
         (map (lambda (x) (constant-val (constant/k-value x))) args)))

(define (constant-integer? x)
  (match x
    (($ constant _ 'integer v) #t)
    (else #f)))

;; NOTE: fold-constant must be applied before, otherwise it doesn't work.
;; FIXME: Only pure-functional primitives can be reduced.
(define (delta expr)
  (define (prim-fold p args)
    (and (every constant-integer? args)
         (prim-apply p args)))
  (match expr
    (($ app/k _ ($ lambda/k _ v body) e)
     (lambda/k-body-set! (app/k-func expr) (delta body))
     (app/k-args-set! expr (map delta e))
     expr)
    (($ lambda/k _ v body)
     (lambda/k-body-set! expr (delta body))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (delta (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (delta (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map delta exprs))
     expr)
    (($ app/k _ (? id? f) args)
     (id-name f)
     (app/k-args-set! expr (map delta args))
     expr)
    (($ app/k _ (? primitive? p) args)
     (app/k-args-set! expr (map delta args))
     (if (and (every cps-integer? args) (not (eq? (primitive-name p) 'halt)))
         (new-constant/k (prim-apply p (app/k-args expr)))
         expr))
    (else expr)))

(define-pass delta-reduction expr (delta expr))
