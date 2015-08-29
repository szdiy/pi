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

(define-module (pi ir)
  #:use-module (pi utils)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (make-insr insr-subx
            make-insr-lit insr-lit-val
            make-insr-ref insr-ref-var
            make-insr-set insr-set-var
            make-insr-cnd
            make-insr-prim insr-prim-op
            make-insr-app
            make-insr-lam insr-lam-par
            make-insr-seq

            make-ctx ctx-code ctx-env ctx-upper))

;; instruction (insr) is a simple IR
;; TODO: we may need more IRs for better optimizing
(define-record-type insr (fields subx))

(define-record-type insr-lit (parent insr) (fields val)) ; literal
(define-record-type insr-ref (parent insr) (fields var)) ; var ref
(define-record-type insr-set (parent insr) (fields var)) ; var assignment
(define-record-type insr-cnd (parent insr))              ; condition
(define-record-type insr-prim (parent insr) (fields op)) ; primitive
(define-record-type insr-app (parent insr))              ; application
(define-record-type insr-lam (parent insr) (fields par))  ; lambda
(define-record-type insr-seq (parent insr))              ; sequence

;; for context
(define-record-type ctx
  (fields
   code
   env ; current env
   upper)) ; upper level env
