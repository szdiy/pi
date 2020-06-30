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

(define-module (pi pass useless-cont)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (pi primitives)
  #:use-module (ice-9 match))

(define (uc expr)
  (match expr
    (($ letcont/k ($ bind-special-form/k _ _ value
                     ($ app/k _ prim:return ((? seq/k?)))))
     (uc value))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (uc (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (uc (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map uc exprs))
     expr)
    (($ app/k _ func args)
     (app/k-args-set! expr (map uc args))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (uc body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (uc cnd))
     (branch/k-tbranch-set! expr (uc b1))
     (branch/k-fbranch-set! expr (uc b2))
     expr)
    (else expr)))

(define-pass useless-cont expr (uc expr))
