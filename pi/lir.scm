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
;;
;;
;; The optimizings in LIR:
;; 1. Integer unboxing
;;    TODO: The independent const integer can be unboxed from object struct,
;;          however, we have to substitute all the references of that const integer.


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
(define-typed-record insr-fjump (parent insr)
  (fields
   (label string?)))

(define-typed-type insr-prim (parent insr)
  (fields
   (op primitive?)))

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
   (make-insr-proc ))
  #;
  (($ closure/k ($ cps _ kont name attr) env body) ;
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
  #;
  ;; TODO                               ;
  (($ collection/k ($ cps _ kont name attr) var type size value body) ; ;
  )
  (($ seq/k ($ cps _ kont name attr) exprs)
   (make-insr-label name (map cps->lir exprs)))
  (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname fun body))
   ;; NOTE:
   ;; 1. For common function, after lambda-lifting, the function must be lifted to a
   ;;    function which can be looked up from top-level.
   ;; 2. For escaping function, there must be a closure. So we will take advantage
   ;;    of the specific instruction of the VM.
   (let* ((label (id->string fname))
          (cont (cps->lir body))
          (insr (make-insr-label label (cps->lir fun))))
     (cond
      ((lir? cont) (make-insr-label name (list insr cont)))
      ((list? cont) (make-insr-label name `(,insr ,@cont)))
      (else (throw 'pi-error cps->lir "Invalid cont `~a' in letfun/k!" cont)))
     ;; TODO:
     ;; Don't forget this is based on lambda-lifting that we haven't done.
     ))
  (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
   ;; NOTE: All the inside-defined bindings are flattened, and pushed into the env
   ;;       frame of the function.
   ;; TODO: The ref checking should be in closure-conversion.
   '())
  (($ letval/k ($ bind-special-form/k
                  ($ cps _ kont name attr) var ($ constant/k _ value) body))
   ;; NOTE: value is constant type.
   ;; NOTE: char and boolean shouldn't be unboxed.
   (let ((obj (create-object value))
         (cont (cps->lir body)))
     ;; TODO: substitute all the var reference to the ref-number
     (cond
      ((lir? cont) (make-insr-label name (list insr cont)))
      ((list? cont) (make-insr-label name `(insr ,@cont)))
      (else (throw 'pi-error cps->lir "Invalid cont `~a' in letval/k!" cont)))))
  (($ app/k ($ cps _ kont name attr) func args)
   ;; NOTE: After normalize, the func never be anonymous function, it must be an id.
   (let ((f (cps->lir func))
         (e (map cps->lir args)))
     (cond
      ((primitive? f) (make-prim-call f e))
      ((id? f) (make-insr-app f e))
      (else (throw 'pi-error cps->lir "Invalid func `~a'!" f)))))
  (($ constant/k _ value)
   (create-object value))
  (($ lvar _ offset)
   (make-insr-local offset))
  (($ fvar _ label offset)
   (make-insr-free (id->string label) offset))
  (else (throw 'pi-error cps->lir "Invalid cps `~a'!" cps)))
