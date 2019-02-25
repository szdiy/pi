;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019
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
  #:use-module (pi ast)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select define-record-type)
  #:export (ast->cps))

(define-record-type cps ; super type for checking cps types
  (fields
   kont   ; The current continuation
   name)) ; The id of the continuation

(define-syntax cps-define
  (lambda (x)
    (syntax-case x ()
      ((_ name (clauses ...)) (identifier? #'name)
       #`(define-record-type
           #,(datum->syntax #'name (symbol-append (syntax->datum #'name) '/k))
           (parent cps)
           (fields clauses ...))))))

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

(cps-define let (vars vals))
(cps-define branch (cnd true false))
