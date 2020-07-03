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

(define-module (pi pass closure-conversion)
  #:use-module (pi utils)
  #:use-module (pi types)
  #:use-module (pi cps)
  #:use-module (pi env)
  #:use-module (pi primitives)
  #:use-module (pi pass)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (closure-ref))

(define *closure-lookup-table* (make-hash-table))
(define (closure-set! label bindings)
  (hash-set! *closure-lookup-table* label bindings))
(define (closure-ref label)
  ;; FIXME: Shouldn't create new env
  (hash-ref *closure-lookup-table* label (new-env '())))

;; NOTE:
;; 1. We only perform CC after DCE, so there's no unused binding.
;; 2. We distinct local bindings and free vars. Both of them are ordered in a
;;    queue. This is useful when we perform linearization for codegen.
;; 3. We use flat-closure design, which put all free variables with values in
;;    a single record of the function. This can save some RAMs for embedded
;;    system.
;; 4. According to Appel's book, we must perform heap-exhausted test. However,
;;    we leave this duty to the VM when it calls procedure each time. This may
;;    save some RAMs compared to the code injection.
;; 5. CPS has no explicit loops, this may cause redundant heap-exhausted test.
;;    We may do specific optimizings for tail call in the future.
;; 6. Different from the passes, we use CPS constructor here for taking advantage of
;;    type checking in record type.
(define* (cc expr)
  (match expr
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let ((env (new-env args)))
       (closure-set! name env)
       (parameterize ((current-env env)
                      (current-kont expr))
         (make-lambda/k (list kont name attr) args (cc body))))
     ;; TODO:
     ;; 1. recording the current bindings by the label to lookup table
     ;; 2. replacing all the appeared free variable to `fvar' by label and order num
     ;; 3. counting frame size for each closure env in lir
     )
    #;
    (($ closure/k ($ cps _ kont name attr) env body)
    ;; TODO: The escaping function will be converted to closure/k.
    ;;       This will need escaping analysis or liveness analysis.
    )
    (($ branch/k ($ cps _ kont name attr) cnd b1 b2)
     (make-branch/k (list kont name attr)
                    (cc cnd)
                    (cc b1)
                    (cc b2)))
    (($ collection/k ($ cps _ kont name attr) var type size value)
     (make-collection/k (list kont name attr)
                        var type size
                        (cc value)))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (make-seq/k (list kont name attr) (map cc exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname func body))
     (let ((env (new-env (list fname))))
       (closure-set! name env)
       (parameterize ((current-env env)
                      (current-kont expr))
         (make-letfun/k (list kont name attr)
                        fname
                        (cc func)
                        (cc body)))))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
     (make-letcont/k (list kont name attr)
                     jname
                     (cc jcont)
                     (cc body)))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name attr) var value body))
     (make-letval/k (list kont name attr)
                    var
                    (cc value)
                    (cc body)))
    (($ app/k ($ cps _ kont name attr) f args)
     (make-app/k (list kont name attr)
                 (cc f)
                 #;(map cc args)
                 (cc args)
                 ))
    ((? id? id)
     (let ((env (current-env))
           (label (cps-name (current-kont)))
           (name (pk "cc name"(id-name id))))
       ;;(pk "bindings" (map id-name (car (env-bindings env))))
       (cond
        ((not (toplevel? env))
         (cond
          ((bindings-index env id)
           => (lambda (offset)
                (new-lvar id offset)))
          ((frees-index env id)
           => (lambda (index)
                (new-fvar id label index)))
          (else (throw 'pi-error cc "Undefined variable `~a'!" name))))
        ((top-level-ref name)  (new-gvar id))
        (else (throw 'pi-error cc "Undefined variable `~a'!" name)))))
    (else expr)))

(define-pass closure-conversion expr (cc expr))
