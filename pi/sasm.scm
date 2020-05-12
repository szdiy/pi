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
  #:use-module (ice-9 match)
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
  (call-with-output-string
    (lambda (port)
      (format port "(lef~%")
      (for-each
       (lambda (pattern)
         (match pattern
           ((insr . descp)
            (format port "~a~a ; ~a~%" (indent-spaces) insr descp))
           (('label-begin label)
            (indent-spaces 'in)
            (format port "~a(~a ; Label ~a begin~%" (indent-spaces) label label))
           (('label-end label)
            (format port "~a)~a ; Label ~a end~%" (indent-spaces) label label)
            (indent-spaces 'out))
           ('prog-begin
            (format port "(program~%")
            (indent-spaces 'in))
           ('prog-end
            (format port "~a) ; Program end~%" (indent-spaces))
            (indent-spaces 'out))
           ('memory-begin
            (format port "(memory~%")
            (indent-spaces 'in))
           ('memory-end
            (format port "~a) ; Memory end~%" (indent-spaces))
            (indent-spaces 'out))
           ('clean-begin
            (format port "(clean~%")
            (indent-spaces 'in))
           ('clean-end
            (format port "~a) ; Clean end~%" (indent-spaces))
            (indent-spaces 'out))
           (('closure-prelude argc)
            (format port "~a) ; Clean end~%" (indent-spaces))
            (indent-spaces 'out))
           (else (throw 'pi-error 'sasm-output "Invalid pattern `~a'!" pattern)))))
      (get-all-sasm)
      (format port ") ; End LEF~%"))))

(define (sasm-emit expr) (queue-in! *sasm-queue* expr))

(define (sasm-true)
  (sasm-emit
   '((push-4bit-const 1) . "Boolean true")))

(define (sasm-true)
  (sasm-emit
   '((push-4bit-const 0) . "Boolean false")))

(define (sasm-program-begin)
  (sasm-emit 'prog-start))

(define (sasm-program-end)
  (sasm-emit 'prog-end))

(define (sasm-memory-begin)
  (sasm-emit 'memory-start))

(define (sasm-memory-begin)
  (sasm-emit 'memory-start))

(define (sasm-clean-begin)
  (sasm-emit 'clean-start))

(define (sasm-clean-begin)
  (sasm-emit 'clean-start))

(define (sasm-label-begin label)
  (sasm-emit '(label-start ,label)))

(define (sasm-label-end label)
  (sasm-emit `(label-end ,label)))

(define (sasm-closure-prelude argc)
  (sasm-emit '((pop-16bit-const ,argc) . (format #f "Pop ~a args" argc))))
