;;  Copyright (C) 2015
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
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (env env?
            env-bindings env-prev
            
            top-level-ref
            top-level-set!
            
            extend-env
            binding-ref
            binding-set!
            
            *top-level*
            new-env))

;; FIXME: try some pure, but don't low down perf
(define-record-type env
  (fields
   (mutable prev)
   (mutable bindings)))

(define (new-env . params)
  (let ((bindings (make-hash-table)))
    (for-each (lambda (v)
                (hash-set! bindings (binding-id v) (ast-constant 'special 'unspecifed)))
              params)
    (make-env #f bindings)))

(define *top-level* (new-env))

(define (top-level-ref k)
  (hash-ref (env-bindings *top-level*) k))

(define (top-level-set! k v)
  (hash-set! (env-bindings *top-level*) k v))

(define (extend-env to new)
  (env-prev-set! to new))

(define (binding-set! env k v)
  (hash-set! (env-bindings env) k v))

(define (binding-ref env k)
  (let ((bindings (env-bindings env))
        (prev (env-prev env)))
    (or (and bindings (hash-ref bindings k))
        (and prev (binding-ref prev k))
        (top-level-ref k))))
