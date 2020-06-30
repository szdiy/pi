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

(define-module (pi pass lambda-lifting)
  #:use-module (pi env)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (ice-9 match))

;; NOTE: This pass must be after normalize and closure-conversion
;;
;; After closure-conversion, there're no free variables, so we can lift the
;; some lambdas to be a top-level defined function:
;; 1. simple lambda: no deeper function definition inside
;; 2. lambdas that is no escaping closures, in this case we can lift the inner
;;    lambdas recursively.
;; This pass is useful to simplify the free variable analysis in codegen.
(define (ll expr)
  (match expr
    (($ letfun/k ($ bind-special-form/k _ fname func body))
     (top-level-set! fname func)
     (ll func)
     (ll body))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ll (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ll (bind-special-form/k-body expr)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ll cnd))
     (branch/k-tbranch-set! expr (ll b1))
     (branch/k-fbranch-set! expr (ll b2))
     expr)
    (($ app/k _ f e)
     ;; After normalize, there's no anonymouse function as f, so we skip f here
     (app/k-args-set! expr (ll e))
     expr)
    (($ lambda/k _ args body)
     (lambda/k-body-set! (ll body))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ll exprs))
     expr)
    (else expr)))

;; Lambda-lifting does two things:
;; 1. Lifting free variables to parameters
;; 2. Lifting functions to higher scoping as possible
(define-pass lambda-lifting expr (ll expr))
