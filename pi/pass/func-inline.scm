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
  #:use-module (pi types)
  #:use-module (pi env)
  #:use-module (pi pass normalize)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match))

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

;; NOTE: It is also called beta-contract, which means inlining the function that
;;       appears only once.
;; unfortunately, we don't have env in this direct CPS implementation, so it's
;; too hard to trace back the definition of the function.
;; FIXME: trace back after closure-conversion
;; NOTE: Must be applied after alpha-renaming.
(define* (func-inline expr #:optional (refs (make-ref-table expr)))
  (define (inlineable-local-func? f) (= 2 (hash-ref refs f 0)))
  (match expr
    (($ letcont/k ($ bind-special-form/k _ jname jcont
                     ($ letfun/k ($ bind-special-form/k _ fname fbody body))))
     (=> fail!)
     (cond
      ((inlineable-local-func? fname)
       (beta-reduction/preserving
        (cfs body
             (list jname fname)
             (list (func-inline jcont) (func-inline fbody)))))
      (else (fail!))))
    (($ app/k _ ($ lambda/k _ v body) e)
     (cond
      ((and (id? e) (top-level-ref e))
       => (lambda (func-body)
            (when (not (eq? (current-kont) 'global))
              ;; It's too early to delete in toplevel optimizing
              (top-level-delete! e))
            (lambda/k-body-set! (app/k-func expr) (func-inline body refs))
            (app/k-args-set! expr (func-inline func-body refs))
            (beta-reduction/preserving expr)))
      (else
       (lambda/k-body-set! (app/k-func expr) (func-inline body refs))
       expr)))
    (($ app/k _ f args)
     (app/k-func-set! expr (func-inline f))
     (app/k-args-set! expr (map func-inline args))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr
      (func-inline (bind-special-form/k-value expr) refs))
     (bind-special-form/k-body-set!
      expr
      (func-inline (bind-special-form/k-body expr) refs))
     expr)
    (($ lambda/k _ args body)
     (lambda/k-body-set! expr (func-inline body))
     expr)
    (else expr)))

(define-pass function-inline expr (func-inline expr))
