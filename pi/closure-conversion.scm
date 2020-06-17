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
          '() outter-frees)))

(define (get-fvar-base env)
  (length (queue-length (env-bindings env))))

;; NOTE:
;; 1. We only perform CC after DCE, so there's no unused binding.
;; 2. We distinct local bindings and free vars. Both of them are ordered in a
;;    queue. This is useful when we perform linearization for codegen.
;; 3. We use flat-closure design, which put all free variables with values in
;;    a single record of the function. This can save some RAMs for embedded
;;    system.
;; 4. According to Appel's book, we must perform heap-exhausted test. However,
;;    we leave this duty to the VM when it calls procedure each time. This may
;;    save some RAMs.
;; 5. CPS has no explicit loops, this may cause redundant heap-exhausted test.
;;    We may do specific optimizings for tail call in the future.
;; 6. Different from the passes, we use CPS constructor here for taking advantage of
;;    type checking in record type.
(define* (closure-conversion cps #:optional (env (new-env)))
  (match cps
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let ((frees (alive-frees env cps)))
       (add-frees! env frees)
       (make-closure/k kont name attr nenv
                       (closure-conversion body env))))
    (($ branch/k ($ cps _ kont name attr) cnd b1 b2)
     (make-branch/k kont name attr
                    (closure-conversion cnd env)
                    (closure-conversion b1 env)
                    (closure-conversion b2 env)))
    (($ collection/k ($ cps _ kont name attr) var type size value body)
     (make-collection/k kont name attr var type size value
                        (closure-conversion body env)))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (make-seq/k kont name attr
                 (map (lambda (e) (closure-conversion e env)) exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname fun body))
     (make-letfun/k kont name attr kname
                    (closure-conversion fun env)
                    (closure-conversion body env)))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
     (make-letcont/k kont name attr jname
                     (closure-conversion jcont env)
                     (closure-conversion body env)))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name attr) var value body))
     (make-letval/k kont name attr var
                    (closure-conversion value env)
                    (closure-conversion body env)))
    (($ app/k ($ cps _ kont name attr) f e)
     (make-app/k kont name attr
                 (closure-conversion f env)
                 (closure-conversion e env)))
    ((? id?)
     (cond
      ((bindings-index env id) => make-lvar)
      ((frees-index env id) => make-fvar)
      (else (throw 'pi-error closure-conversion "Undefined variable `~a'!"))))
    (else cps)))
