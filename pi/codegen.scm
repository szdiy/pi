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

(define-module (pi codegen)
  #:use-module (pi primitives)
  #:use-module (pi utils)
  #:use-module (pi types)
  #:use-module (pi env)
  #:use-module (pi lir)
  #:use-module (pi sasm)
  #:use-module (pi object)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (codegen
            lir->sasm-string
            lir->sasm))

(define (gen-binding-frame env)
  (let lp ((next (env-bindings env)) (ret '()))
    (match next
      (() (reverse ret))
      (((? id? id) rest ...)
       (lp (cdr next) (cons (bindings-index env id) ret)))
      (else (lp cdr next) ret))))

;; lir -> unspecified
(define (emit-sasm lir)
  (match lir
    (($ insr-proc _ label _ _ body)
     (sasm-label-begin label)
     (emit-sasm body)
     (sasm-label-end label))
    (($ insr-call _ label argc)
     (emit-call-proc argc label))
    (($ insr-pcall _ label argc)
     (emit-prim-call argc label))
    (($ integer-object _ i)
     ;; TODO: how to associate the variable?
     (emit-integer-object i))
    (($ insr-prim _ p num)
     (emit-prim p num))
    (($ insr-label _ label insrs)
     (sasm-label-begin label)
     (for-each emit-sasm insrs)
     (sasm-label-end label))
    (($ insr-fjump _ label)
     (emit-fjump label))
    (else (throw 'pi-error emit-sasm "Invalid lir `~a'!" lir))))

(define (emit-sasm-memory lir)
  (sasm-nop))

(define (emit-sasm-clean lir)
  (sasm-nop))

(define (gen-sasm lir)
  (sasm-memory-begin)
  (emit-sasm-memory lir)
  (sasm-memory-end)

  (sasm-program-begin)
  (emit-sasm lir)
  (sasm-program-end)

  (sasm-clean-begin)
  (emit-sasm-clean lir)
  (sasm-clean-end))

(define (codegen lir filename)
  (gen-sasm lir)
  (call-with-output-file filename sasm-output))

;; debug helper function
(define (lir->sasm-string lir)
  (gen-sasm lir)
  (call-with-output-string sasm-output))

(define (lir->sasm lir)
  (call-with-input-string (lir->sasm-string lir) read))
