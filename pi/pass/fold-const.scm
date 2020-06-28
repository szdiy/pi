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

(define-module (pi pass fold-const)
  #:use-module (pi cps)
  #:use-module (pi utils)
  #:use-module (pi pass)
  #:use-module (pi pass normalize)
  #:use-module (ice-9 match))

;; FIXME: We have to tweak fold-const when we have side-effect analysis.
;; TODO: For now, we haven't supported assignment yet. If we have it, then the
;;       modified variable has to change its fold value, or even can't be folded

;; 1. (if 1 2 3) -k-> (if 1 2 3)
;; 2. (if 1 2 (+ 3 4)) -k-> (letcont ((k (halt (+ 4 3)))) (if 1 2 k))
(define (fc cps)
  (match cps
    (($ letval/k ($ bind-special-form/k _ v e body))
     (fc (normalize/preserving (new-app/k (new-lambda/k (list v) (fc body)) e))))
    (($ letcont/k ($ bind-special-form/k _ v ($ app/k _ 'halt (? atom? a)) body))
     (fc (normalize/preserving
          (new-app/k (new-lambda/k v (fc body)) a))))
    (($ app/k? app)
     (fc (normalize/preserving app)))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! cps (fc (bind-special-form/k-value cps)))
     (bind-special-form/k-body-set! cps (fc (bind-special-form/k-body cps)))
     cps)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! cps (map fc exprs))
     cps)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! cps (fc cnd))
     (branch/k-tbranch-set! cps (fc b1))
     (branch/k-fbranch-set! cps (fc b2))
     cps)
    (else cps)))

(define-pass fold-constant cps (fc cps))
