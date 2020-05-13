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
  #:use-module (pi cps)
  #:use-module (pi sasm)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (cps->sasm))

;; For now, we just support 32bit integer
(define (emit-integer i)
  (if (or (integer-check i 'u32) (integer-check i 's32))
      (current-backend-integer i)
      (throw 'pi-error "Invalid integer value!" i)))

(define (emit-boolean b)
  (match b
    (($ constant _ 'boolean v)
     (if v
         (sasm-true)
         (sasm-false)))
    (else (throw 'pi-error 'emit-boolean "Invalid boolean value `~a'!" b))))

(define (emit-char c)
  (if (char? c)
      (let ((i (char->integer c)))
        (if (integer-check i 'u8)
            (current-backend-integer i)
            (throw 'pi-error "emit-char: Unicode is not supported!" i)))
      (throw 'pi-error "Invalid char value!" c)))

(define (immediate-rep x)
  (cond
   ((is-integer? x) (emit-integer (constant-val x)))
   ((is-boolean? x) (emit-boolean (constant-val x)))
   ((is-char? x) (emit-char (constant-val x)))
   (else (throw 'pi-error "Invalid immediate!" x))))

;; FIXME: what if %eax is occupied?
(define (emit-immediate x)
  `(mov ,(immediate-rep x) reg:arith))

(define (emit-primitive op . args)
  (apply (get-prim op) args))

(define (emit-instruction expr nb-pop nb-push ctx)
  (let* ((env (ctx-env ctx))
         (stk (stk-extend #f nb-push
                          (stk-discard nb-pop (env-local env)))))
    (ctx-add-instr (ctx-change-env ctx (env-change-local env stk)) instr)))

(define (cps->sasm cps)
  (match cps
    (($ lambda/k ($ cps _ _ label _) args body)
     (sasm-label-begin label)
     (sasm-closure-prelude (length args))
     (sasm-label-end label))

    ))

(define (codegen expr ctx)
  (sasm-emit
   (match expr
     ((? is-immediate? i) (emit-immediate i))
     (((? is-primitive? p) rest ...) (apply emit-primitive p rest))
     (`(entry ,nparams ,rest?)
      (emit-instruction expr 0 0 ctx))
     ((? var? var)
      #t)
     (
      (else #t)))))
