;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019,2020
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

(define-module (pi cps)
  #:use-module (pi utils)
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module (srfi srfi-1)
  #:export (ast->cps))

(define-record-type cps ; super type for checking cps types
  (fields
   kont   ; The current continuation
   name)) ; The id of the continuation

;; kontext means kontinuation-context

;; According to <<Compiling with continuations, continued>> - Andrew Kennedy
;; The principle to design CPS IR:
;; 1. All intermediate results are named, that is to say, we use a special binding to hold
;;    the intermediate result.
;; 2. There're 2 kinds of application:
;;    a. Translation-time application, which will not be reduced to normal-form in the
;;       translation.
;;    b.
;; 3. One-pass CPS translation, which introduce no 'administrative-reduction' that must be
;;    removed in a seperated phase.
;;

(define-typed-record cps
  (fields
   (kont cps?)
   (name symbol?)
   ;; The result of the current expr will be bound to karg, and be passed to kont
   (karg symbol?)))

(define-typed-record lambda/k (parent cps)
  (fields
   (args list?)
   (body cps?)))

(define-typed-record letval/k (parent cps)
  (fields
   (value cps?)))

(define-typed-record letfun/k (parent cps)
  (fields
   (value cps?)))

(define-typed-record letcont/k (parent cps)
  (fields
   (value cps?)))

(define-typed-record branch/k (parent cps)
  (fields
   (cnd cps?)
   (tbranch letcont/k?)
   (fbranch letcont/k?)))

(define-typed-record collection/k (parent cps)
  (fields type size value))

(define-typed-record seq/k (parent cps)
  (fields
   (exprs list?)))

(define-typed-record define/k (parent cps)
  (fields
   (name symbol?)
   (value cps?)))

(define-typed-record app/k (parent cps)
  (fields
   (func letfun/k?)
   (args list?)))

(define* (cps->src cps #:optional (hide-begin? #t))
  (match cps
    ;; TODO
    ))
