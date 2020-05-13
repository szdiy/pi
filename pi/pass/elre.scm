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

(define-module (pi pass elre)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (ice-9 match))

;; Eliminate all the redundant code:
;; 1. (begin (begin body ...)) -> (begin body ...)
(define (elre cps)
  (match cps
    (($ seq/k _ (($ seq/k _ _)))
     ;; Of course we can make sure no redundant seq was introduced in the parser,
     ;; however, you have to consider if users do it in their code, such like:
     ;; (let ((...)) (begin body ...))
     (elre (car (seq/k-exprs cps))))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! cps (elre e))
     (bind-special-form/k-body-set! cps (elre body))
     cps)
    (($ app/k _ func args)
     (app/k-args-set! (map elre args))
     cps)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! cps (elre body))
     cps)
    (else cps)))

(define-pass eliminate-redundant cps (elre cps))
