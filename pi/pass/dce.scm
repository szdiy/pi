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

(define-module (pi pass dce)
  #:use-module (pi env)
  #:use-module (pi types)
  #:use-module (pi cps)
  #:use-module (pi pass)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; FIXME: We don't have to compute free-vars redundantly
(define (is-referenced? expr v)
  (let ((free (free-vars expr)))
    (match v
      ((? id?)
       (any (lambda (x) (id-eq? x v)) free))
      ((? id-list? v)
       (insec free v))
      (else (throw 'pi-error is-referenced? "BUG: Invalid pattern `~a'!" v)))))

(define (dve expr)
  (match expr
    (($ letcont/k ($ bind-special-form/k _ cv ($ lambda/k _ v body) cont-body))
     (cond
      ((is-referenced? body v)
       (bind-special-form/k-value-set! expr (dve (bind-special-form/k-value expr)))
       (bind-special-form/k-body-set! expr (dve cont-body))
       expr)
      (else (dve body))))
    ((? bind-special-form/k? sf)
     (cond
      ((is-referenced? (bind-special-form/k-body sf) (bind-special-form/k-var sf))
       (bind-special-form/k-value-set! sf (dve (bind-special-form/k-value sf)))
       (bind-special-form/k-body-set! sf (dve (bind-special-form/k-body sf)))
       expr)
      (else (dve (bind-special-form/k-body sf)))))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (seq/k-exprs-set! expr (map dve exprs)))
    (($ app/k _ ($ lambda/k _ v body) e)
     ;; TODO: Here we just keep the variable which is referenced in the body,
     ;;       however, it is possible to further optimize the body so that the
     ;;       referencing can be eliminated.
     ;;       A better way is to do it again after all optimizings.
     (cond
      ((is-referenced? body v)
       (lambda/k-body-set! (app/k-func expr) (dve body))
       (app/k-args-set! expr (map dve e))
       expr)
      (else
       ;; There could be side-effects, so the args should never be dropped.
       (seq/k-exprs-set! expr `(,@e ,@(seq/k-exprs body)))
       (dve expr))))
    (($ lambda/k _ v body)
     (cond
      ((is-referenced? body v)
       => (lambda (vv)
            ;; There could be side-effects, so we only drop the parameters
            (lambda/k-args-set! expr vv))))
     (lambda/k-body-set! expr (dve body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (dve cnd))
     (branch/k-tbranch-set! expr (dve b1))
     (branch/k-fbranch-set! expr (dve b2)))
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map dve exprs))
     expr)
    (else expr)))

;; This includes dead-continuation and dead-variable elimination
(define-pass dead-variable-elimination expr (dve expr))

;; NOTE: Please notice that we've converted local function binding to let-binding
;;       during AST building step, so the top-level function definition is the only
;;       thing that I need to take care. Because the local binding will be handled
;;       by dead-variable-eliminate.
;;       That's what we concern in dead-fun and fun-inline

;; Removes a function definition if it has no applied occurrences outside
;; its ownbody.
(define-pass dead-function-elimination expr
  (cond
   ((not (eq? (current-kont) 'global))
    (let ((funcs (top-level->body-list))
          (fv (free-vars expr)))
      (for-each top-level-delete! (insec funcs fv))
      expr))
   (else expr)))
