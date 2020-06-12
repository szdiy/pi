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
  #:use-module ((srfi srfi-1) #:select (zip lset-intersection))
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (closure-conversion))

(define (bind-el! env el)
  (for-each (lambda (x) (binding-set! env (car x) (cadr x))) el))

(define (free-bindings el expr)
  (define (get-free-var expr)
    (let ((fv (free-vars expr)))
      (zip fv (map (const 'registered) (iota (length fv))))))
  (define (bind-eq? x y)
    (id-eq? (car x) (car y)))
  (apply lset-intersection bind-eq? el (free-var expr)))

;; NOTE:
;; 1. We only perform CC after DCE, so there's no unused binding
;; 2.
(define* (closure-conversion cps #:optional (el '()))
  (define (add-to-env k v)
    (cond
     ((null? el)
      (top-level-set! k v)
      el)
     (else (cons (cons k v) el))))
  (match cps
    (($ lambda/k ($ cps _ kont name karg) args body)
     (let ((env (apply new-env args)))
       (bind-el! env el)
       (make-closure/k kont name karg env (closure-conversion body env))))
    (($ branch/k ($ cps _ kont name karg) cnd b1 b2)
     (make-branch/k kont name karg
                    (closure-conversion cnd el)
                    (closure-conversion b1 el)
                    (closure-conversion b2 el)))
    (($ collection/k ($ cps _ kont name karg) var type size value body)
     (make-collection/k kont name karg var type size value
                        (closure-conversion body el)))
    (($ seq/k ($ cps _ kont name karg) exprs)
     (make-seq/k kont name karg (map (lambda (e) (closure-conversion e el)) exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name karg) fname fun body))
     (make-letfun/k kont name karg kname
                    (closure-conversion fun el)
                    (closure-conversion body el)))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name karg) jname jcont body))
     (make-letcont/k kont name karg jname
                     (closure-conversion jcont el)
                     (closure-conversion body el)))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name karg) var value body))
     (make-letval/k kont name karg var
                    (closure-conversion value el)
                    (closure-conversion body el)))
    (($ app/k ($ cps _ kont name karg) f e)
     (make-app/k kont name karg
                 (closure-conversion f el)
                 (closure-conversion e el)))
    (else cps)))
