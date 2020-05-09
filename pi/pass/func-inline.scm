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

(define-module (pi pass func-inline)
  #:use-module (pi utils)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (ice-9 match))

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

;; NOTE: It is also called beta-contract, which means inlining the function that
;;       appears only once.
;; unfortunately, we don't have env in this direct CPS implementation, so it's
;; too hard to trace back the definition of the function.
;; NOTE: Must be applied after alpha-renaming.
(define (func-inline cps #:optional (refs (make-ref-table cps)))
  (define (inlineable-local-func? f) (= 2 (hash-ref refs f 0)))
  (match cps
    (($ letcont/k ($ bind-special-form/k _ _ _
                     ($ letfun/k ($ bind-special-form/k _ fname fbody _))))
     (when (inlineable-local-func? fname)
       (beta-reduction/preserving
        (new-app/k (func-inline e refs) (func-inline fbody refs)))))
    (($ app/k _ ($ lambda/k _ v body) e)
     (cond
      ((and (id? e) (topref e))
       => (lambda (func-body)
            (top-level-delete! e)
            (lambda/k-body-set! (app/k-func cps) (func-inline body refs))
            (app/k-args-set! cps (func-inline func-body refs))
            (beta-reduction/preserving cps)))
      (else
       (lambda/k-body-set! (app/k-func cps) (func-inline body refs))
       cps)))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      cps
      (func-inline (bind-special-form/k-value cps) refs))
     (bind-special-form/k-body-set!
      cps
      (func-inline (bind-special-form/k-body cps) refs))
     cps)
    (else cps)))

(define-pass function-inline cps (func-inline cps))
