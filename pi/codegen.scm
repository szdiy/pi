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
  #:use-module (pi lir)
  #:use-module (pi sasm)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (cps->sasm))

;; lir -> unspecified
(define (emit-sasm lir)
  (match lir

    ))

(define (codegen lir filename)
  (call-with-output-file filename sasm-output))

;; debug helper function
(define (lir->sasm lir)
  (emit-sasm lir)
  (call-with-input-string (call-with-output-string sasm-output) read))
