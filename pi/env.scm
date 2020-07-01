;;  Copyright (C) 2015,2020
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (pi env)
  #:use-module (pi utils)
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (env
            env?
            env-bindings env-prev

            toplevel?
            top-level-ref
            top-level-set!
            top-level-delete!
            top-level-for-each
            top-level->body-list

            extend-env
            bindings-index
            frees-index
            binding-exists?

            *top-level*
            new-env
            env->args))

;; NOTE:
;; 1. Only toplevel is used for storing actual value.
;; 2. We use closures to manage bindings, the env is defined for conversion.
;; 3. You can check existance in env, not referring the value from it.

(define-record-type env
  (fields
   (mutable prev)
   (mutable bindings)
   (mutable frees)))

(define-record-type toplevel (parent env) (fields definition))
(define (new-toplevel)
  (make-toplevel #f #f #f (make-hash-table)))

(define (new-env params)
  (let ((bindings (list->queue params))
        (frees (new-queue)))
    (make-env #f bindings frees)))

(define *top-level* (new-toplevel))

(define (top-level->body-list)
  (hash-map->list (lambda (v _) v) (toplevel-definition *top-level*)))

(define (top-level-ref k)
  (hash-ref (toplevel-definition *top-level*) k))

(define (top-level-set! k v)
  (hash-set! (toplevel-definition *top-level*) k v))

(define (top-level-delete! k)
  (hash-remove! (toplevel-definition *top-level*) k))

(define (top-level-for-each proc)
  (hash-for-each proc (toplevel-definition *top-level*)))

(define (extend-env to new)
  (env-prev-set! to new))

(define (id-index env ref id)
  (when (not (env? env))
    (throw 'pi-error id-index "`~a' is invalid env for ~a!" env ref))
  (slot-index (ref env) (lambda (x) (id-eq? x id))))

(define (bindings-index env k)
  (id-index env env-bindings k))

(define (frees-index env k)
  (id-index env env-frees k))

(define (binding-exists? env id)
  (let ((bindings (env-bindings env))
        (prev (env-prev env))
        (pred (lambda (x) (id-eq? x id))))
    (or (and bindings (slot-index bindings id))
        (and prev (binding-exists? prev id))
        (top-level-ref id))))

(define (env->args env)
  (hash-map->list (lambda (k _) k) (env-bindings env)))
