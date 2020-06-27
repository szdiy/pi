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

(define-module (pi object)
  #:use-module (pi utils)
  #:use-module (pi types)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (create-object
            object?

            integer-object
            integer-object?
            make-integer-object

            list-object
            list-object?
            make-list-object

            vector-object
            vector-object?
            make-vector-object

            char-object
            char-object?
            make-char-object))

;; NOTE:
;; 1. If the value can be unboxed, then we store them in unboxed style
;; 2. char and boolean are globally unique, so no unboxing available

(define-record-type object)

(define (object-list? lst)
  (make-object-list-pred lst object?))

(define-typed-record integer-object (parent object)
  (fields
   (value integer?)))

(define-typed-record list-object (parent object)
  (fields
   (value object-list?)
   (size positive?)))

(define-typed-record vector-object (parent object)
  (fields
   ;; we use list to hold the vector, it'll become real vector in codegen
   (value object-list?)
   (size positive?)))

(define-typed-record char-object (parent object)
  (fields
   (value is-char-node?)))

(define-typed-record char-object (parent object)
  (fields
   (value is-boolean-node?)))

;; constant -> object
(define (create-object c)
  (let ((val (constant-val c)))
    (match (constant-type c)
      ('integer (make-integer-object val))
      ('list (make-list-object val))
      ('vector (make-vector-object val))
      ('char (make-integer-object val))
      (else (throw 'pi-error create-object "Invalid type `~a'!"
                   (constant-type c))))))

;; TODO: finish others
