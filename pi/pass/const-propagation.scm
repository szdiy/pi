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

(define-module (pi pass const-propagation)
  #:use-module (pi cps)
  #:use-module (pi utils)
  #:use-module (pi pass)
  #:use-module (pi pass normalize)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (cp expr)
  (match expr
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr)
                     jname jcont ($ app/k _ jname args)))
     (=> fail!)
     (cond
      ((every constant/k? args)
       ;; FIXME: For partial constant/k situation, we can still propagate them,
       ;;        and reduce the number of args.
       (beta-reduction/preserving
        (new-app/k (make-lambda/k (list kont name attr) (list jname)
                                  (cp (bind-special-form/k-body expr)))
                   (cp jcont))))
      (else (fail!))))
    (($ letcont/k ($ bind-special-form/k _ jname
                     ($ app/k _ prim:halt ((? constant/k? c)))
                     (? app/k?)))
     (bind-special-form/k-value expr))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (cp (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (cp (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map cp exprs))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (cp cnd))
     (branch/k-tbranch-set! expr (cp b1))
     (branch/k-fbranch-set! expr (cp b2))
     expr)
    (($ lambda/k _ args body)
     (lambda/k-body-set! expr (cp body))
     expr)
    (else expr)))

(define-pass constant-propagation expr (cp expr))
