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

(define-module (pi ast)
  #:use-module (pi utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (ast make-ast ast?
            ast-subx
            
            def make-def def?
            def-var

            ref make-ref ref?
            ref-var

            assign make-assign assign?
            assign-var
            
            cnd make-cnd cnd?

            pcall make-pcall pcall?
            pcall-op pcall-args

            call make-call call?
            call-op call-args
            
            lam make-lam lam?
            lam-params lam-has-opt?

            seq make-seq seq?

            binding make-binding binding?
            binding-id
            
            var make-var var?
            var-uid var-global?
            new-var

            ->special-form

            macro-expander))

;; AST type
(define-record-type ast (fields subx))

(define-record-type ref (parent ast) (fields var)) ; var ref
(define-record-type def (parent ast) (fields var)) ; var define
(define-record-type assign (parent ast) (fields var)) ; var assignment
(define-record-type cnd (parent ast))              ; condition
(define-record-type pcall (parent ast) (fields op args)) ; prim-call 
;; calling a function, ast is a list: (func args ...)
(define-record-type call (parent ast) (fields op args))
(define-record-type lam (parent ast) (fields params has-opt?))  ; lambda
(define-record-type seq (parent ast))              ; sequence
(define-record-type macro (parent ast) (fields expander))

;; for env, var, and macros
(define-record-type binding (fields id))
(define-record-type var (parent binding) (fields uid global?))
(define* (new-var id #:optional (global? #f)) (make-var id (newsym id) global?))

;; In Scheme, there'd be these special forms (at least):
;; define lambda let let* letrec quote quasiquote set! if case
;; cond begin do and or let-syntax letrec-syntax delay
;; And macros!
(define-record-type special-form (parent binding) (fields expander))

(define (macro-expander m)
  #t)
