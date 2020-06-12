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

(define-module (pi closure-conversion)
  #:use-module (pi utils)
  #:use-module (pi types)
  #:use-module (pi cps)
  #:use-module (pi env)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (closure-conversion))

;; Add free vars
(define (add-frees! env frees)
  (let ((ef (env-frees env)))
    (for-each (lambda (fv) (queue-in! ef fv)) frees)))

(define (alive-frees env expr)
  (let ((outter-frees (queue-slot (env-frees env)))
        (inner-frees (free-vars expr)))
    (fold (lambda (x p)
            (if (env-exists? env x)
                (cons x p)
                p))
          '() (queue-slot (env-frees env)))))

;; NOTE:
;; 1. We only perform CC after DCE, so there's no unused binding.
;; 2. We distinct local bindings and free vars. Both of them are ordered in a queue. This is useful when we perform linearization for codegen.
;; 3.
(define* (closure-conversion cps #:optional (env *top-level*))
  (match cps
    (($ lambda/k ($ cps _ kont name karg) args body)
     (let ((nenv (new-env args))
           (frees (alive-frees env cps)))
       (add-frees! nenv frees)
       (make-closure/k kont name karg nenv
                       (closure-conversion body nenv))))
    (($ branch/k ($ cps _ kont name karg) cnd b1 b2)
     (make-branch/k kont name karg
                    (closure-conversion cnd env)
                    (closure-conversion b1 env)
                    (closure-conversion b2 env)))
    (($ collection/k ($ cps _ kont name karg) var type size value body)
     (make-collection/k kont name karg var type size value
                        (closure-conversion body env)))
    (($ seq/k ($ cps _ kont name karg) exprs)
     (make-seq/k kont name karg (map (lambda (e) (closure-conversion e env)) exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name karg) fname fun body))
     (make-letfun/k kont name karg kname
                    (closure-conversion fun env)
                    (closure-conversion body env)))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name karg) jname jcont body))
     (make-letcont/k kont name karg jname
                     (closure-conversion jcont env)
                     (closure-conversion body env)))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name karg) var value body))
     (make-letval/k kont name karg var
                    (closure-conversion value env)
                    (closure-conversion body env)))
    (($ app/k ($ cps _ kont name karg) f e)
     (make-app/k kont name karg
                 (closure-conversion f env)
                 (closure-conversion e env)))
    ((? id?)
     (cond
      ((bindings-index env id) => make-lvar)
      ((frees-index env id) => make-fvar)
      (else (throw 'pi-error closure-conversion "Undefined variable `~a'!"))))
    (else cps)))
