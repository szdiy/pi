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

(define (cp cps)
  (match cps
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr)
                     jname jcont ($ app/k _ jname args)))
     (display "hit!!!!!\n")
     (cond
      ((every constant/k? (pk "args" args))
       (beta-reduction/preserving
        (new-app/k (make-lambda/k (list kont name attr) (list jname)
                                  (cp (bind-special-form/k-body cps)))
                   (cp jcont))))
      (else
       (bind-special-form/k-value-set! cps (cp (bind-special-form/k-value cps)))
       (bind-special-form/k-body-set! cps (cp (bind-special-form/k-body cps)))
       cps)))
    ((? bind-special-form/k?)
     (display "no!!!!!\n")
     (bind-special-form/k-value-set! cps (cp (bind-special-form/k-value cps)))
     (bind-special-form/k-body-set! cps (cp (bind-special-form/k-body cps)))
     cps)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! cps (map cp exprs))
     cps)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! cps (cp cnd))
     (branch/k-tbranch-set! cps (cp b1))
     (branch/k-fbranch-set! cps (cp b2))
     cps)
    (($ lambda/k _ args body)
     (lambda/k-body-set! cps (cp body))
     cps)
    (else cps)))

(define-pass constant-propagation cps (cp cps))
