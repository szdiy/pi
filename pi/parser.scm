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

(define-module (pi parser)
  #:use-module (pi env)
  #:use-module (pi ast)
  #:use-module (pi types)
  #:use-module (pi utils)
  #:use-module (pi primitives)
  #:use-module (ice-9 match)
  #:export (parser ast->src))

;; NOTE: we don't allow primitives-redefine, so this list is for checking.
(define *wrong-op-lst*
  '(quote quasiquote unquote unquote-splicing lambda if set!
          cond and or case let let* letrec begin do define delay))

;; NOTE: we don't support forward-reference, although I'm willing to...
(define* (parse-it expr env #:key (top? #f) (body-begin? #f) (use 'test) (op? #f))
  (match expr
    (('define pattern e ...)
     (cond
      ((null? e)
       ;; R6Rs supports definition without expression, which implies to define
       ;; a var with the value `unspecifed'.
       ;; With respect to the future Scheme, we support it anyway.
       (gen-constant 'unspecified))
      ((and (not top?) (not body-begin?))
       ;; According to R5Rs, there're only two situations to use `define':
       ;; 1. In the top-level (toplevel definition).
       ;; 2. In the beginning of body (inner definition).
       (throw 'pi-error "definition in expression context, where definitions are not allowed, in form " expr))
      (else
       (match pattern
         ((? symbol? v) (make-def (parse-it (car e) env #:body-begin? #t) v))
         (((? symbol? v) args ...) (make-def (parse-it `(lambda ,args ,@e) env #:body-begin? #t) v))
         (else (throw 'pi-error "define: no pattern to match!" expr))))))
    (('set! id val)
     (let ((var (binding-ref id env)))
       (cond
        ((var? var)
         (make-assign (parse-it val env) var))
        (else (throw 'pi-error "not an identifier: " var)))))
    (('if tst then els ...)
     (let* ((e (parse-it tst env #:use 'test))
            (b1 (parse-it then env #:body-begin? #t))
            (b2 (match els
                  (()
                   ;; for (if #f e) situation, this expr should return `unspecified',
                   ;; so we generate `unspecified' here for later use.
                   (gen-constant 'unspecified))
                  ((e) (parse-it e env #:body-begin? #t))
                  ((e redundant ...)
                   (throw 'pi-error "if: redundant expr follow the second branch!" expr))
                  (else (throw 'pi-error "if: can't match any cases!" expr)))))
       (make-cnd (list e b1 b2))))
    (('cond body ...)
     (let ((tmpvar (gensym "cond.tmp.var-")))
       ;; Because I don't have time at present to write macro system, I have to write `cond'
       ;; as a built-in special form here. It'd be a macro (external special form) in the future. 
       (match body
         ((('else rhs ...))
          (parse-it `(begin ,@rhs) env))
         (((tst '=> rhs) rest ...)
          (let ((x (tmpvar)))
            (parse-it `(let ((,x tst))
                         (if ,x
                             (rhs ,x)
                             (cond ,@rest)))
                      env)))
         (((tst rhs ...) rest ...)
          (parse-it `(if tst
                         (begin ,@rhs)
                         (cond ,@rest))
                    env)))))
    (('lambda pattern body ...)
     (when (null? body)
           (throw 'pi-error "lambda: bad lambda in form " expr))
     (let* ((ids (extract-ids pattern))
            (params (map new-var ids))
            (nenv (extend-env env (apply new-env params)))
            (has-opt? (or (pair? pattern) (symbol? pattern))))
       (make-lam (parse-it (cons 'begin body) nenv #:body-begin? #t)
                 params has-opt?)))
    (('begin body ...)
     (let lp((next body) (p #t) (ret '()))
       (cond
        ((null? next) (make-seq (reverse ret)))
        (else
         (match (car next)
           (('define whatever ...) ; make sure inner definitions are available in a row
            (lp (cdr next) p (cons (parse-it (car next) env #:body-begin? p) ret)))
           (else (lp (cdr next) #f (cons (parse-it (car next) env #:body-begin? #f) ret))))))))
    (('letrec ((ks vs) ...) body ...)
     (letrec ((dispatch
                (lambda (kk vv)
                  (cond
                   ((and (null? kk) (null? vv)) `(begin ,@body))
                   (else `(let ((,(car kk) #f))
                            (set! ,(car kk) ,(car vv))
                            ,(dispatch (cdr kk) (cdr vv))))))))
              (parse-it (dispatch ks vs) env)))
    (('let ((ks vs) ...) body ...) ; common let
     (parse-it `((lambda ,ks ,@body) ,vs) env))
    (('let id ((ks vs) ...) body ...) ; named let
     (parse-it `(letrec ((,id (lambda ,@ks ,@body))) (,id ,@vs)) env))
    (('let () body ...)
     (parse-it `(begin ,@body) env))
    (('let* () body ...)
     (parse-it `(let () ,@body) env))
    (('let* ((ks vs) ...) body ...)
     (letrec ((dispatch
                (lambda (kk vv)
                  (cond
                   ((and (null? kk) (null? vv)) `(begin ,@body))
                   (else `(let ((,(car kk) ,(car vv)))
                            ,(dispatch (cdr kk) (cdr vv))))))))
              (parse-it (dispatch ks vs) env)))
    (('or rest ...)
     (let ((tmpvar (gensym "or.tmp.var-")))
       (cond
        ((null? rest) (gen-constant 'false))
        ((null? (cdr rest)) (parse-it (car rest) env))
        (else
         (let ((b1 (tmpvar))
               (b2 (tmpvar)))
           (parse-it `((lambda (,b1 ,b2) (if ,b1 ,b1 (,b2)))
                       ,(car rest) (lambda () (or ,@(cdr rest))))
                     env))))))
    (('and rest ...)
     (let ((tmpvar (gensym "and.tmp.var-")))
       (cond
        ((null? rest) (gen-constant 'true))
        ((null? (cdr rest)) (parse-it (car rest) env))
        (else
         (let ((b1 (tmpvar))
               (b2 (tmpvar)))
           (parse-it `((lambda (,b1 ,b2) (if ,b1 (,b2) ,b1))
                       ,(car rest) (lambda () (and ,@(cdr rest))))
                     env))))))
    ((op args ...)
     (cond
      ((is-primitive? op)
       => (lambda (p)
            (make-pcall p (map (lambda (e) (parse-it e env #:use 'value)) args))))
      (else
       (let ((f (parse-it op env #:use 'value #:op? #t)))
         (cond
          ((not f) (throw 'pi-error (format #f "PROC `~a': unbound variable: " op)))
          ((macro? f) ((macro-expander f) args env))
          (else (make-call f (map (lambda (e) (parse-it e env #:use 'value)) args))))))))
    ;; TODO: add quote/unquote/quansiquote...
    ((? symbol? k)
     (let ((v (binding-ref env k)))
       ;; TODO: v shouldn't be #f, it'll be created at top-level for forward-reference
       (cond
        (v (make-ref k))
        ((is-primitive? k) => identity)
        (else (throw 'pi-error (format #f "~a: unbound variable!" k))))))
    ;; NOTE: immediate check has to be the last one!!!
    ((? is-immediate? i) (gen-constant i))
    (else
     (throw 'pi-error
            (format #f
                    "source expression failed to match any pattern in form `~a'"
                    expr)))))

(define (parser expr)
  (catch 'pi-error
         (lambda () (parse-it expr *top-level* #:top? #t))
         (lambda (k . e) (format #t "PI Compile error: ~{~a~^ ~}~%" e))))

(define (ast->src node)
  (match node
    (($ constant _ val type) (unless (eq? 'unspecified type) val))
    (($ def ($ ast _ subx) v) `(define ,(ast->src v) ,(ast->src subx)))
    (($ ref _ v) (ast->src v))
    (($ assign ($ ast _ subx) v) `(set! ,(ast->src v) ,(ast->src subx)))
    (($ cnd ($ ast _ subx))
     (match subx
       ((c b1 b2) `(if ,(ast->src c) ,(ast->src b1) ,(ast->src b2)))
       (else (throw 'pi-error "I don't know what's wrong dude!!!" subx))))
    (($ pcall _ op args) `(,(prim-name op) ,@(map ast->src args)))
    (($ call _ op args) `(,(ast->src op) ,@(map ast->src args)))
    (($ lam ($ ast _ subx) params _) `(lambda ,(map ast->src params) ,(ast->src subx)))
    (($ seq ($ ast _ subx)) `(begin ,@(map ast->src subx)))
    (($ var ($ binding _ id) uid _) id)
    (else node)))
