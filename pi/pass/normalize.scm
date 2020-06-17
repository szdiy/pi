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

(define-module (pi pass normalize)
  #:use-module (pi cps)
  #:use-module (pi types)
  #:use-module (pi pass)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; NOTE: Please notice that the normalize pass is actually normalize/preserving,
;;       since it only substitutes the regular let bindings, and preserving
;;       letval/letcont/letfun...etc, which are useful for CPS optimizing.
;;
;; NOTE: We also provide normalize function which substitute any kinds of let
;;       bindings for debugging.

;; capture free substitute
(define (cfs expr args el)
  (define (substitute e)
    (cond
     ((list-index (lambda (x) (eq? x e)) args)
      => (lambda (i) (list-ref el i)))
     (else e)))
  (when (not (= (length args) (length el)))
    (throw 'pi-error cfs
           (format #f "BUG: expr: ~a, args: ~a, el: ~a~%" expr args el)))
  (match expr
    (($ lambda/k ($ cps _ kont name attr) fargs body)
     ;;(format #t "cfs 0 ~a~%" expr)
     (lambda/k-body-set! expr (cfs body args el))
     expr)
    (($ app/k ($ cps _ kont name attr) f e)
     ;;(format #t "cfs 1 ~a~%" expr)
     (app/k-func-set! expr (cfs f args el))
     (app/k-args-set! expr (cfs e args el))
     expr)
    (($ seq/k ($ cps _ kont name attr) exprs)
     ;;(format #t "cfs 2 ~a~%" expr)
     (seq/k-exprs-set! expr (map (lambda (ee) (cfs ee args el)) exprs))
     expr)
    (else
     ;;(format #t "cfs 4 ~a~%" expr)
     (substitute expr))))

(define (beta-reduction expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction
      (cfs (beta-reduction body)
           params
           (beta-reduction args))))
    (($ lambda/k _ args body)
     ;;(display "beta 2\n")
     (lambda/k-body-set! expr (beta-reduction body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (beta-reduction cnd))
     (branch/k-tbranch-set! expr (beta-reduction b1))
     (branch/k-fbranch-set! expr (beta-reduction b2))
     expr)
    (($ seq/k _ e)
     (seq/k-exprs-set! expr (map beta-reduction e))
     expr)
    ((? bind-special-form/k? bsf)
     (beta-reduction
      (new-app/k (make-lambda/k (list (cps-kont bsf) (cps-name bsf) (cps-attr bsf))
                                (list (bind-special-form/k-var bsf))
                                (bind-special-form/k-body bsf))
                 (beta-reduction (bind-special-form/k-value bsf)))))
    (else expr)))

(define (beta-reduction/preserving expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           params
           (beta-reduction/preserving args))))
    (($ lambda/k _ args body)
     ;;(display "beta 2\n")
     (lambda/k-body-set! expr (beta-reduction/preserving body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (beta-reduction/preserving cnd))
     (branch/k-tbranch-set! expr (beta-reduction/preserving b1))
     (branch/k-fbranch-set! expr (beta-reduction/preserving b2))
     expr)
    (($ seq/k _ e)
     (seq/k-exprs-set! expr (map beta-reduction/preserving e))
     expr)
    (else expr)))

(define (eta-reduction expr)
  (match expr
    (($ lambda/k _ arg ($ app/k _ f (? (lambda (a) (id-eq? a arg)))))
     ;;(display "eta-0\n")
     (eta-reduction f))
    (($ seq/k ($ cps _ kont name attr) exprs)
     ;;(display "eta-1\n")
     (make-seq/k (list kont name attr) (map eta-reduction exprs)))
    (else expr)))

(define (normalize expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction
              eta-reduction)))

(define (normalize/preserving expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction/preserving
              eta-reduction)))

(define-pass normalize cps (normalize/preserving cps))
