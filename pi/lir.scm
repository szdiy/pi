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

(define-module (pi lir)
  #:use-module (pi utils)
  #:use-module (pi env)
  #:use-module (pi cps)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (make-insr
            insr-subx

            make-insr-lit insr-lit-val
            make-insr-ref insr-ref-var
            make-insr-set insr-set-var
            make-insr-cnd
            make-insr-prim insr-prim-op
            make-insr-app

            make-insr-closure
            insr-closure-env insr-closure-code

            make-insr-seq
            insr-seq-exprs

            make-ctx ctx-code ctx-env ctx-upper))

;; Instruction (insr) is a simple low-level IR, which is a instruction set of
;; ACM (Abstract Continuation Machine).
;; The ACM program is a linear sequence of instructions, labels, and literal data.
;; It is essentially an assembly-language program.
;;
;; TODO: The ACM is stackVM. We hope to refactor it as registerVM in the future.
;;
;; The features of insr-ir:
;; 1. All continuations are mapping to labels which is actually kont-name.
;; 2. No seq abstract, it's merged into procedure.
;; 3. Branches are replaced with comparison and jumping.
;; 4. No ref abstract, its value is stored into a lookup table, and replace with
;;    lookup by offset.
;; 5. The inner defined functions are all sorted as closures.
;; 6. The procedure must be defined in toplevel.

(define-record-type insr)

(define (insr-list? lst)
  (make-object-list-pred lst insr?))

(define-typed-record insr-proc (parent insr)
  (fields
   (entry id?) ; entry should be a label
   (env env?)))

(define-typed-record insr-label (parent insr)
  (fields
   (label symbol?)
   (body insr-list?)))

(define-record-type insr-lit (parent insr) (fields val)) ; literal
(define-record-type insr-set (parent insr) (fields var)) ; var assignment

;; Push value to TOS
;; NOTE: the value must be encoded to an integer first.
(define-typed-record insr-push (parent insr)
  (fields
   (value integer?)))

;; Pop ss[offset] to TOS
(define-typed-record insr-local (parent insr)
  (fields
   (offset integer?)))

;; Pop ss[offset] to TOS
(define-typed-record insr-free (parent insr)
  (fields
   ;; We convert the label to string since we will generate label pattern in codegen
   (label string?)
   (offset integer?)))

;; jump if TOS is false
(define-typed-record insr-fjump (parent insr))

(define-record-type insr-prim (parent insr) (fields op)) ; primitive

;; application
(define-typed-record insr-app (parent insr)
  (fields
   (label string?)
   (args list?)))

;; closure
(define-typed-record insr-closure (parent insr)
  (fields
   (env env?)
   (code insr?)))

(define (cps->lir cps)
  (($ lambda/k ($ cps _ kont name attr) args body)
   )
  (($ closure/k ($ cps _ kont name attr) env body)
   )
  (($ branck/k ($ cps _ kont name attr) cnd b1 b2)
   (let ((ce (cps->lir cnd env))
         (bt (cps->lir b1 env))
         (bf (cps->lir b2 env)))
     (make-insr-label
      name
      (list
       (make-insr-push ce)
       (make-insr-fjump (cps-name bf))
       bt
       bf))))
  (($ collection/k ($ cps _ kont name attr) var type size value body)
   )
  (($ seq/k ($ cps _ kont name attr) exprs)
   (make-insr-label name (map cps->lir exprs)))
  (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname fun body))
   (let ((label (id->string fname)))
     ))
  (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
   )
  (($ letval/k ($ bind-special-form/k ($ cps _ kont name attr) var value body))
   )
  (($ app/k ($ cps _ kont name attr) func args)
   (make-insr-app (cps->lir func) (map cps->lir args)))
  (($ lvar _ offset)
   (make-insr-local offset))
  (($ fvar _ label offset)
   (make-insr-free (id->string label) offset))
  (else (throw 'pi-error cps->lir "Invalid cps `~a'!" cps)))
