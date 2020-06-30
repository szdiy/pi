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

(define-module (pi pass)
  #:use-module (pi utils)
  #:use-module (ice-9 match)
  #:export (define-pass
             run-pass))

(define *pass-table* (make-hash-table))
(define (get-pass name) (hash-ref *pass-table* name identity))

(define-syntax-rule (define-pass name cps body ...)
  (begin
    (define (name cps) body ...)
    (hash-set! *pass-table* 'name name)))

(define-syntax-rule (run-pass cps lst ...)
  (fold (lambda (item last)
          (match item
            ((name (? integer? cnt))
             (=> fail!)
             (format #t "PASS 0: ~a~%" name)
             (cond
              ((get-pass name)
               => (lambda (pass)
                    (fold (lambda (_ p) (pass p)) last (iota cnt))))
              (else (fail!))))
            (name
             (=> fail!)
             (format #t "PASS 1: ~a~%" item)
             (cond
              ((get-pass name)
               => (lambda (pass)
                    (pass last)))
              (else (fail!))))
            (else (throw 'pi-error 'run-pass "Invalid pass: `~a'!" 'item))))
        cps (list 'lst ...)))
