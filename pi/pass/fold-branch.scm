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

(define-module (pi pass fold-branch)
  #:use-module (pi types)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (ice-9 match))

;; NOTE: fold-constant should be applied before.
;; NOTE: after eliminate the dead branch, it's necessary to apply
;;       dead-variable-eliminate to reduce the unused branch continuation binding.
(define (fb cps)
  (define (detect e)
    (match e
      (($ constant _ val _) val)
      (else 'no)))
  (match cps
    (($ branch/k _ cnd b1 b2)
     (let ((result (detect cnd)))
       (match result
         ('no cps)
         (#f b2)
         (else b1))))
    (($ app/k _ func args)
     (app/k-func-set! cps (fb func))
     (app/k-args-set! cps (map fb args))
     cps)
    (($ lambda/k _ v body)
     (lambda/k-body-set! cps (fb body))
     cps)
    ((? bind-special-form/k sf)
     (bind-special-form/k-value-set! sf (fb (bind-special-form/k-value sf)))
     (bind-special-form/k-body-set! sf (fb (bind-special-form/k-body sf)))
     sf)
    (($ seq/k _ exprs)
     (seq/k-set! cps (map fb exprs)))
    (else cps)))

(define-pass fold-branch cps (fb cps))
