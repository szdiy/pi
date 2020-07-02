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

(define-module (pi module)
  #:use-module (pi utils)
  #:use-module (pi env)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (mod
            mod?
            mod-filename
            mod-path
            mod-exprs
            mod-env

            read-as-mod))

;; TODO: Support r7rs modules
;; TODO: Replace top-level with mod-env
(define-typed-record mod
  (fields
   (filename string?)
   (path list?)
   (exprs list?)
   (env env?)))

;; If mod-path is #f, then it's the main script
(define* (read-as-mod filename #:optional (mod-path #f))
  (define (read-all-exprs)
    (define port (open-file filename "r"))
    (let lp ((ret '()))
      (cond
       ((eof-object? (peek-char port))
        (close port)
        `(begin ,@(reverse! ret)))
       (else (lp (cons (read port) ret))))))
  (make-mod filename mod-path (read-all-exprs) (new-env)))
