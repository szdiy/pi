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
  #:use-module (pi primitives))

(define (cps-integer? cps)
  (match cps
    (($ constant _ _ type) (eq? type 'integer))
    (else #f)))

;; primitive -> cps-list -> cps
(define (prim-apply p args)
  (apply (symbol->primitive p) (map constant-val args)))

;; NOTE: fold-constant must be applied before, otherwise it doesn't work.
;; FIXME: Only pure-functional primitives can be reduced.
(define (delta cps)
  (define (prim-fold p args)
    (and (every integer? args)
         (prim-apply p args)))
  (match cps
    (($ app/k _ ($ lambda/k _ v body) e)
     (lambda/k-body-set! (app/k-func cps) (delta body))
     (app/k-args-set! cps (map delta e))
     cps)
    (($ lambda/k _ v body)
     (lambda/k-body-set! cps (delta boda))
     cps)
    ((? bind-special-form/k?) ((? bind-special-form? sf) ((v e)) body)
     (bind-special-form/k-value-set! cps (delta (bind-special-form/k-value cps)))
     (bind-special-form/k-body-set! cps (delta (bind-special-form/k-body cps)))
     cps)
    (($ seq/k _ exprs)
     (seq/k-set! cps (map delta exprs))
     cps)
    (($ app/k _ (? id? f) args)
     (app/k-args-set! cps (map delta args))
     cps)
    (($ app/k _ (? prim? p) args)
     (app/k-args-set! cps (map delta args))
     (if (every cps-integer? args)
         (prim-apply p (app/k-args cps))
         cps))
    (else cps)))

(define-pass delta-reduction cps delta)
