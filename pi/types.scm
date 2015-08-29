;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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
  #:export (make-var
            var-val
            var-type
            make-constant
            constant-val
            constant-type
            gen-uniq-constant
            gen-integer
            gen-char
            is-integer?
            is-boolean?
            is-char?
            is-immediate?
            integer-check))

(define-record-type var (fields val type)) ; NOTE: var is immutable
(define-record-type constant (parent var))

;; could be optimized later, let it be now.
(define *uniq-constant-list*
  `((unspecified . ,(make-constant 'unspecified 'special))
    (true . ,(make-constant 'true 'boolean))
    (false . ,(make-constant 'false 'boolean))))

(define (gen-uniq-constant what)
  (or (assoc-ref *uniq-constant-list* what)
      (throw 'pi-error "BUG: Invalid constant " what)))

;; FIXME: char should be uniq in global
(define (gen-char c) (make-constant c 'char))
(define (gen-integer i) (make-constant i 'integer))
(define (gen-string s) (make-constant s 'string))
(define (gen-boolean b) (gen-uniq-constant b))
(define (gen-pair p) (make-constant p 'pair))
(define (gen-list l) (make-constant l 'list))
(define (gen-vector v) (make-constant v 'vector))
(define (gen-unspecified) (gen-uniq-constant 'unspecified))

(define (pred-constant x type)
  (and (constant? x) (eq? (constant-type x) type)))

(define (is-boolean? x) (pred-constant x 'boolean))
(define (is-char? x) (pred-constant x 'char))
(define (is-integer? x) (pred-constant x 'integer))

;; NOTE: not all constant are immediate, e.g, strings are constant
;;       but not immediate.
(define *immediate-types* '(integer char boolean))
(define (is-immediate? x)
  (and (constant? x)
       (memq (constant-type x) *immediate-types*)))

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
