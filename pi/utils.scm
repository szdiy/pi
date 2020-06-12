;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2019,2020
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

(define-module (pi utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (newsym
            extract-ids
            extract-keys
            new-stack
            new-queue
            stack-slots
            queue-slots
            slot-index
            stack-pop!
            stack-push!
            stack-top
            stack-remove!
            stack-length
            stack->list
            queue-out!
            queue-in!
            queue-head
            queue-tail
            queue-remove!
            queue-empty?
            queue-length
            queue->list
            list->stack
            list->queue
            define-typed-record
            make-object-list-pred
            symbol-list?
            any?
            immediate?
            collection?
            atom?
            args-with-keys
            get-all-defs))

(define (newsym sym) (gensym (symbol->string sym)))

(define (extract-ids pattern)
  (define (sym-list? x)
    (and (list? x)
         (every symbol? x)))
  (match pattern
    ((? symbol?) (list pattern)) ; (lambda args ...)
    ((? sym-list?) pattern) ; (lambda (a b c) ...)
    (((? symbol? a) . b) `(,a ,@(extract-ids b))) ; (lambda (a b . c) ...)
    (() '()) ; (lambda () ...)
    (else (throw 'pi-error "lambda: parameter must be an identifier!" pattern))))

(define (extract-keys pattern)
  #f)

(define (%q-remove-with-key! q key)
  (assoc-remove! (car q) key)
  (sync-q! q))

(define new-stack make-q)
(define new-queue make-q)
(define stack-slots car)
(define queue-slots car)

(define (slot-index s/q pred)
  (let ((slots (car s/q)))
    (any (lambda (x i) (and (pred x) i)) slots (iota slots))))

(define stack-pop! q-pop!)
(define stack-push! q-push!)
(define stack-top q-front)
(define stack-remove! %q-remove-with-key!)
(define stack-empty? q-empty?)
(define stack-length q-length)
(define (stack->list stk) (list-copy (stack-slots stk)))

(define queue-out! q-pop!)
(define queue-in! enq!)
(define queue-head q-front)
(define queue-tail q-rear)
(define queue-remove! %q-remove-with-key!)
(define queue-empty? q-empty?)
(define queue-length q-length)
(define (queue->list q) (list-copy (queue-slots q)))

(define* (list->stack lst #:optional (stk (new-stack))) ; NOTE: mak
  (for-each (lambda (x) (stack-push! stk x)) lst)
  stk)

(define* (list->queue lst #:optional (queue (new-queue)))
  (for-each (lambda (x) (queue-in! queue x)) lst)
  queue)

(define-syntax-rule (type-check tr o preds ...)
  (or (any (lambda (p) (p o)) (list preds ...))
      (throw 'pi-error
             (format #f "Wrong type in ~a, `~a' expect ~{`~a'~^,~}"
                     'tr o (list 'preds ...)))))

(define-syntax define-typed-record
  (syntax-rules (parent fields)
    ((_ tr (parent p))
     (define-record-type tr (parent p)))
    ((_ tr (parent p) (fields (f t t* ...) ...))
     (define-record-type tr
       (parent p)
       (fields (mutable f) ...)
       (protocol
        (lambda (new)
          (lambda (pf f ...)
            (type-check tr f t t* ...) ...
            ((apply new pf) f ...))))))
    ((_ tr (fields (f t t* ...) ...))
     (define-record-type tr
       (fields (mutable f) ...)
       (protocol
        (lambda (new)
          (lambda (f ...)
            (type-check tr f t t* ...) ...
            (new f ...))))))))

(define (make-object-list-pred lst check)
  (and (list? lst) (every check lst)))

(define (symbol-list? lst)
  (make-object-list-pred lst symbol?))

(define (any? o) #t)

(define (pi-list? x) #f)
(define (pi-pair? x) #f)

(define (immediate? x)
  (or (number? x)
      (string? x)
      (symbol? x)
      (vector? x)
      (number? x)))

(define (collection? x)
  (or (pi-list? x)
      (pi-pair? x)))

(define (atom? x)
  (or (immediate? x)
      (collection? x)))

(define (args-with-keys args)
  (any keyword? args))

(define (get-all-defs exprs)
  (let lp ((e exprs) (ret '()))
    (when (null? e)
      (throw 'pi-error 'get-all-defs
             "No expressions in body in form `~a'" exprs))
    (match (car e)
      (('define rest ...) (lp (cdr e) (cons (car e) ret)))
      (else
       (if (null? (cdr e))
           (throw 'pi-error 'get-all-defs
                  "No expressions in body in form `~a'" exprs)
           (values (cdr e) (reverse ret)))))))
