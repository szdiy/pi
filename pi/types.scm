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
  #:use-module (pi utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (srfi srfi-1)
  #:export (constant
            constant?
            make-constant
            constant-val
            constant-type
            detect-minimum-range
            detect-literal-type
            *pi/unspecified*
            gen-constant
            is-integer-node?
            is-boolean-node?
            is-char-node?
            is-immediate-node?
            is-unspecified-node?
            integer-check
            pred-constant
            is-immediate?

            id
            make-id
            id?
            id-name
            id-orig
            new-id
            id-eq?
            id-list?
            id->string

            lvar lvar?
            make-lvar
            lvar-offset

            fvar fvar?
            make-fvar
            lvar-offset))

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
   (else (throw 'pi-error 'detect-literal-type "Invalid literal type!" x))))

(define *pi/unspecified* (make-constant 'unspecified 'unspecified))
(define *pi/chars* (list->vector
                    (map (lambda (i) (make-constant 'char (integer->char i)))
                         (iota 128))))
(define *pi/true* (make-constant 'boolean #t))
(define *pi/false* (make-constant 'boolean #f))

(define *global-constant-type*
  `((unspecified . ,(lambda (_) *pi/unspecified*))
    (char . ,(lambda (c) (vector-ref *pi/chars* (char->integer c))))
    (boolean . ,(lambda (b) (if b *pi/true* *pi/false*)))))

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
  '((u4 . (0 . 15))
    (s8 . (-128 . 127))
    (u8 . (0 . 255))
    (s16 . (-32768 . 32767))
    (u16 . (0 . 65535))
    (s32 . (-2147483648 . 2147483647))
    (u32 . (0 . 4294967295))))

(define (detect-minimum-range i)
  (or (any (lambda (t) (and (integer-check i t) t)) *integer-range*)
      (throw 'pi-error detect-minimum-range "Out of integer range `~a'" i)))

(define (integer-check x subtype)
  (if (integer? x)
      (let ((range (assoc-ref *integer-range* x)))
        (and (> x (car range)) (< x (cdr range))))
      (throw 'pi-error integer-check "`~a' is not an integer!" x)))

(define *immediates-pred*
  (list integer? string? char? boolean? pair? list? vector?))

(define (is-immediate? x)
  (any (lambda (c) (c x)) *immediates-pred*))

(define-typed-record id
  (fields
   (name symbol?)
   ;; For example, the orig of x-123 is x
   (orig symbol?)))

(define* (new-id #:optional (orig "x-") (rename? #t))
  (let* ((orig-fix (cond
                    ((string? orig) (string->symbol orig))
                    ((symbol? orig) orig)
                    (else (throw 'pi-error new-id "Inavlid orig `~a'" orig))))
         (name (if rename? (newsym orig-fix) orig-fix)))
    (make-id name orig-fix)))

(define (id->string id)
  (symbol->string (id-name id)))

(define (id-eq? x y)
  (cond
   ((or (not (id? x)) (not (id? y)))
    #f)
   (else
    (eq? (id-name x) (id-name y)))))

(define (id-list? lst)
  (make-object-list-pred lst id?))

;; local variable
(define-typed-record lvar
  (fields
   (offset positive?)))

;; free variable
(define-typed-record fvar
  (fields
   (label string?)
   (offset positive?)))
