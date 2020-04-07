;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2019,2020
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

(define-module (pi types)
  #:use-module (pi env)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (srfi srfi-1)
  #:export (constant
            make-constant
            constant-val
            constant-type
            detect-literal-type
            gen-constant
            is-integer-node?
            is-boolean-node?
            is-char-node?
            is-immediate-node?
            is-unspecified-node?
            integer-check
            pred-constant
            is-immediate?

            make-primitive
            primitive?
            primitive-name
            primitive-proc

            make-id
            id?
            id-name
            id-orig))

(define-record-type constant (fields val type))

(define (detect-literal-type x)
  (cond
   ((symbol? x) 'symbol)
   ((char? x) 'char)
   ((integer? x) 'integer)
   ;; TODO: support real number
   ((string? x) 'string)
   ((boolean? x) 'boolean)
   ((pair? x) 'pair)
   ((list? x) 'list)
   ((vector? x) 'vector)
   ((unspecified? x) 'unspecified)
   (else (throw 'pi-error "Invalid literal type!" x))))

(define *pi/unspecified* (gen-constant 'unspecified))
(define *pi/chars* (list->vector
                    (map (lambda (i) (make-constant 'char (integer->char i)))
                         (iota 128))))

(define *global-constant-type*
  `((unspecified . ,(lambda (_) *pi/unspecified*))
    (char . ,(lambda (c) (vector-ref *pi/chars* (char->integer c))))))

(define (global-constant-type? t)
  (assoc-ref *global-constant-type* t))

(define (gen-constant x)
  (let ((type (detect-literal-type x)))
    (cond
     ((global-constant-type? type)
      => (lambda (generator) (generator x)))
     (else (make-constant x type)))))

(define (pred-constant x type)
  (and (constant? x) (eq? (constant-type x) type)))

(define (is-boolean-node? x) (pred-constant x 'boolean))
(define (is-char-node? x) (pred-constant x 'char))
(define (is-integer-node? x) (pred-constant x 'integer))
(define (is-unspecified-node? x) (pred-constant x 'unspecified))

;; NOTE: not all constant are immediate, e.g, strings are constant
;;       but not immediate.
(define *immediate-type-nodes*
  '(integer char boolean pair list string vector))
(define (is-immediate-node? x)
  (and (constant? x)
       (memq (constant-type x) *immediate-type-nodes*)))

(define *integer-range*
  '((u8 . (0 . 255))
    (s8 . (-128 . 127))
    (u16 . (0 . 65535))
    (s16 . (-32768 . 32767))
    (u32 . (0 . 4294967295))
    (s32 . (-2147483648 . 2147483647))))

(define (integer-check x subtype)
  (if (integer? x)
      (let ((range (assoc-ref *integer-range* x)))
        (and (> x (car range)) (< x (cdr range))))
      (throw 'pi-error (format #f "'~a' is not an integer!" x))))

(define *immediates-pred*
  (list integer? string? char? boolean? pair? list? vector?))

(define (is-immediate? x)
  (any (lambda (c) (c x)) *immediates-pred*))

(define-typed-record primitive
  (fields
   (name symbol?)
   ;; proc is not the final primitive, but it can be used for delta-reduction
   (proc procedure?)))

(define-typed-record id
  (fields
   (name symbol?)
   ;; For example, the orig of x123 is x
   (orig symbol?)))
