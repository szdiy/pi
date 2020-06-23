;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2020
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

(define-module (pi sasm)
  #:use-module (pi utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (sasm-emit
            get-all-sasm
            sasm-output

            sasm-true
            sasm-false))

(define *sasm-queue* (new-queue))
(define (get-all-sasm) (queue-slots *sasm-queue*))
(define (sasm-output)
  (define* (indent-spaces #:optional (mode 'stay))
    (define level 0)
    (define (gen-spaces)
      (fold (lambda (x p) (string-append " " p)) "" (iota level)))
    (match mode
      ('stay (gen-spaces))
      ('in (set! level (1+ level)))
      ('out (set! level (1- level)))))
  (format #t "(lef~%")
  (for-each
   (lambda (pattern)
     (match pattern
       ((insr . descp)
        (format #t "~a~a ; ~a~%" (indent-spaces) insr descp))
       (('label-begin label)
        (indent-spaces 'in)
        (format #t "~a(~a ; Label ~a begin~%" (indent-spaces) label label))
       (('label-end label)
        (format #t "~a)~a ; Label ~a end~%" (indent-spaces) label label)
        (indent-spaces 'out))
       ('prog-begin
        (format #t "(program~%")
        (indent-spaces 'in))
       ('prog-end
        (format #t "~a) ; Program end~%" (indent-spaces))
        (indent-spaces 'out))
       ('memory-begin
        (format #t "(memory~%")
        (indent-spaces 'in))
       ('memory-end
        (format #t "~a) ; Memory end~%" (indent-spaces))
        (indent-spaces 'out))
       ('clean-begin
        (format #t "(clean~%")
        (indent-spaces 'in))
       ('clean-end
        (format #t "~a) ; Clean end~%" (indent-spaces))
        (indent-spaces 'out))
       (('closure-prelude argc)
        (format #t "~a) ; Clean end~%" (indent-spaces))
        (indent-spaces 'out))
       (else (throw 'pi-error 'sasm-output "Invalid pattern `~a'!" pattern))))
   (get-all-sasm))
  (format #t ") ; End LEF~%"))

(define (sasm-emit expr) (queue-in! *sasm-queue* expr))

(define-public (sasm-true)
  (sasm-emit
   '((push-4bit-const 1) . "Boolean true")))

(define-public (sasm-true)
  (sasm-emit
   '((push-4bit-const 0) . "Boolean false")))

(define-public (emit-constant type i)
  (if (integer-check i type)
      (sasm-imit `((push-4bit-const ,i) . (format #f "Constant 0x~X" ,i)))
      (throw 'pi-error "Invalid integer value!" i)))

(define-public (emit-integer-object i)
  (emit-sasm `((push-integer-object ,i) . "")))

(define-public (emit-boolean b)
  (match b
    (($ constant _ 'boolean v)
     (if v
         (sasm-true)
         (sasm-false)))
    (else (throw 'pi-error 'emit-boolean "Invalid boolean value `~a'!" b))))

(define-public (emit-char c)
  (if (char? c)
      (let ((i (char->integer c)))
        (if (integer-check i 'u8)
            (current-backend-integer i)
            (throw 'pi-error "emit-char: Unicode is not supported!" i)))
      (throw 'pi-error "Invalid char value!" c)))

(define-public (emit-integer i)
  (emit-constant (detect-minimum-range i) i))

;; This is only for const unboxing
;; constant -> unspecified
(define-public (emit-const-imm x)
  (cond
   ((is-integer? x) (emit-integer (constant-val x)))
   ((is-boolean? x) (emit-boolean (constant-val x)))
   ((is-char? x) (emit-char (constant-val x)))
   (else (throw 'pi-error emit-imm "Invalid immediate `~a`!" x))))

(define-public (emit-call-proc argc label)
  (sasm-emit `((call-proc ,argc ,label) . "")))

(define-public (emit-prim-call argc p)
  (sasm-emit `((prim-call ,argc ,(primitive->number p))
               . (format #f "Call primitive `~a'" (primitive-name p)))))

(define-public (sasm-program-begin)
  (sasm-emit 'prog-start))

(define-public (sasm-program-end)
  (sasm-emit 'prog-end))

(define-public (sasm-memory-begin)
  (sasm-emit 'memory-start))

(define-public (sasm-memory-begin)
  (sasm-emit 'memory-start))

(define-public (sasm-clean-begin)
  (sasm-emit 'clean-start))

(define-public (sasm-clean-begin)
  (sasm-emit 'clean-start))

(define-public (sasm-label-begin label)
  (sasm-emit '(label-start ,label)))

(define-public (sasm-label-end label)
  (sasm-emit `(label-end ,label)))

(define-public (sasm-closure-prelude argc)
  (sasm-emit '((pop-16bit-const ,argc) . (format #f "Pop ~a args" argc))))
