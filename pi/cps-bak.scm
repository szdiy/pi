;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2018
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

(define-module (pi cps)
  #:use-module (pi ast)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select define-record-type)
  #:export ())

(define-record-type continuation) ; super type for checking cps types
(define-syntax cps-define
  (lambda (x)
    (syntax-case x ()
      ((_ name clauses ...) (identifier? #'name)
       #`(define-record-type
           #,(datum->syntax #'name (symbol-append 'cps- (syntax->datum #'name)))
           (parent continuation)
           (fields clauses ...))))))

;; kontext means kontinuation-context

;; According to <<Compiling with continuations, continued>> - Andrew Kennedy
;; The principle to design CPS IR:
;; 1. All intermediate results are named, that is to say, we use a special binding to hold
;;    the intermediate result.
;; 2. There're 2 kinds of application:
;;    a. Translation-time application, which will not be reduced to normal-form in the
;;       translation.
;;    b.
;; 3. One-pass CPS translation, which introduce no 'administrative-reduction' that must be
;;    removed in a seperated phase.
;;

(define-record-type cps
  (fields
   kontext ; current continuation
   name)) ; the id of the continuation

;; ** Terms
;; ----------------------------------------
;; letval x = V in K
;; Bind value V to variable x in the term K.
(cps-define letval/k (parent cps) vars vals)
;; let x = ref(i,y) in K
;; Bind the i-th element of pair y to x in K.
(cps-define letpair/k (parent cps) var pair index)
;; letcont kont x = body in K
;; a local-continuation kont whose single argument is x and body is body.
;; index is either 0 or 1
(cps-define letcont/k kont var body kontext)
;; k x
;; continuation application, contains two cases:
;; 1. jump to somewhere (kont is local-continuation)
;; 2. return (kont is return-continuation to a value)
(cps-define kapp/k kont arg)
;; f k x
;; function application as CPS
;; If k is the return continuation for the nearest enclosing lambda,
;; then the application is a `tail call'.
(cps-define fapp/k func kont arg)
;; case x of k1 | k2
;; x is the tag of a collection stored values y(i), which will be the arg of kont-i.
(cps-define case/k x kont-1 kont-2)
;; ----------------------------------------
;;
;; ** Values
;; ----------------------------------------
;; unit null-list ()
(cps-define unit/k)
(define *cps-unit* (make-cps-unit/k)) ; make the const unique for saving memory.
;; pair(x, y)
(cps-define pair/k x y)
;; tag(x, i)
(cps-define tag/k x i)
;; abstract \k x. K
;; NOTE: In this cps-abs, the well-formed kont must be return-continuation only,
;;       so that we need letcont to define local-continuation.
(cps-define abs/k kont x kontext)

;; get free variables
;; term `free variable' refers to variables used in a function that are
;; not local variables nor parameters of that function
;; NOTE: This algorithm isn't optimized, maybe drag compiling performace.
;;       But fine with the excecuting performance.
(define (free-vars ast)
  (define (union l1 l2) (lset-union eq? l1 l2))
  (define (diff l1 l2) (lset-difference eq? l1 l2))
  (cond
   ((ref? ast) ; A reference to a variable, just return the var as a list
    (list (ref-var ast)))
   ((assign? ast) ; all-subx-fv + assigned-var
    ;; NOTE: it's reasonable to union assigned var, since there could be
    ;;       self assigment, say, n=n+1. For such situation, the fv is
    ;;       U{n,n} = n.
    (union (free-vars (ast-subx ast))
           (free-vars (assign-var ast))))
   ((closure? ast) ; all-subx-fv - closure-params
    (diff (free-vars (ast-subx ast))
          (closure-params ast)))
   (else
    ;; FIXME: is this correct?
    (apply union (map free-vars (ast-subx ast))))))

(define (init-cont)
  (let ((k (new-var "k")))
    (make-closure (list k) (list k))))

;; We use CPS for handling closures/continuations properly.
;; For simplifying our compiler design, we just support a subset of CPS.
;; Here're the rules for AST -> CPS:
;; P is primodial continuation, r is the result of the whole program,
;; ri (i=0,1,2...n) is the result of reducing Ei,
;; pi is parameter, k is a new null-continuation,
;; C is current-continuation, c is a const, v is a variable,
;; CC means Convert-to-CPS
;; 1.  P <- (lambda (r) (%halt r))
;; 2.  CC <- (lambda (x) (->cps x C))
;; 3.  (CC c) <- (C c)
;; 4.  (CC v) <- (C v)
;; 5.  (CC (set! v E1)) <- (lambda (r1) (C (set! v r1)))
;; 6.  (CC (if E1 E2 E3)) <- (lambda (r1) (if r1 (CC E2) (CC E3)))
;; 7.  (CC (begin E1 E2)) <- (lambda (r1) (CC E2))
;; 8.  (CC (+ E1 E2)) <- (lambda (r1) (lambda (r2) (C (+ r1 r2))))
;; 9.  (CC (lambda (p1 p2 ... pn) E0)) <- (C (lambda (k p1 p2 ... pn) (k E0)))
;; 10. (CC (E0)) <- (lambda (r0) (C r0))
;; 11. (CC (E0 E1)) <- (lambda (r0) (lambda (r1) (r0 C r1)))
;; 12. (CC (E0 E1 E2)) <- (lambda (r0) (lambda (r1) (lambda (r2) (r0 C r1 r2))))
;; 13. (CC ((lambda () E0))) <- (CC E0)
;; 14. (CC ((lambda (p1) E0) E1)) <- (lambda (p1) (CC E0)) # NOTE: E1 is in continuation
;; 15. (CC ((lambda (p1 p2) E0) E1 E2)) <- (lambda (p1) (lambda (p2) (CC E0)))

(define* (ast->cps ast #:optional (cont (init-cont)))
  (match ast
    ((? lit?)
     (make-letval/k (new-var "x") ast cont))

    ))

;; NOTE: (make-call (list A B)) <==> (A B), it's an application form actually.
(define* (->cps ast #:optional (cont (init-cont)))
  (match ast
    ;; 3.  (CC c) <- (C c)
    ((? lit?)
     (make-letval (new-var "x") ast cont))
    ;; 4.  (CC v) <- (C v)
    ((? lit?)
     (cont ast))
    ;; 5.  (CC (set! v E1)) <- (lambda (r1) (C (set! v r1)))
    (($ assign ($ ast _ subx) v)
     (let ((r (new-var "r")))
       (make-closure (make-call (list cont (make-assign r v)))
                     (list r))))
    ;; 6.  (CC (if E1 E2 E3)) <- (lambda (r1) (if r1 (CC E2) (CC E3)))
    (($ cnd ($ ast _ subx))
     (let* ((r (new-var "r")))
       (if (ref? cont) ; prevent combinatorial explosion
           (make-closure (make-cnd (list r (->cps (cadr subx)) (->cps (caddr subx))))
                         (list r))
           (let ((closure (make-closure (list (make-ref '() r)) (list r))))
             (make-call (list closure cont))))))
    ;; 7.  (CC (begin E1 E2)) <- (lambda (r1) (CC E2))
    ((seq? ast)
     (cps-seq (ast-subx ast) cont))
    ;; 8.  (CC (+ E1 E2)) <- (lambda (r1) (lambda (r2) (C (+ r1 r2))))
    ((prim? ast)
     (cps-list (ast-subx ast)
               (lambda (args)
                 (make-app
                  (list cont
                        (make-prim args
                                   (prim-op ast)))))))
    ;; 9.  (CC (lambda (p1 p2 ... pn) E0)) <- (C (lambda (k p1 p2 ... pn) (k E0)))
    ;; 10. (CC (E0)) <- (lambda (r0) (C r0))
    ;; 11. (CC (E0 E1)) <- (lambda (r0) (lambda (r1) (r0 C r1)))
    ;; 12. (CC (E0 E1 E2)) <- (lambda (r0) (lambda (r1) (lambda (r2) (r0 C r1 r2))))
    ((app? ast)
     (let ((fn (car (ast-subx ast))))
       (if (closure? fn)
           (cps-list (cdr (ast-subx ast))
                     (lambda (vals)
                       (make-app
                        (cons (make-closure
                               (list (cps-seq (ast-subx fn)
                                              cont))
                               (closure-params fn))
                              vals))))
           (cps-list (ast-subx ast)
                     (lambda (args)
                       (make-app
                        (cons (car args)
                              (cons cont
                                    (cdr args)))))))))
    ;; 13. (CC ((lambda () E0))) <- (CC E0)
    ;; 14. (CC ((lambda (p1) E0) E1)) <- (lambda (p1) (CC E0)) # NOTE: E1 is in continuation
    ;; 15. (CC ((lambda (p1 p2) E0) E1 E2)) <- (lambda (p1) (lambda (p2) (CC E0)))
    ((closure? ast)
     (let ((k (new-var "k")))
       (make-app
        (list cont
              (make-closure
               (list (cps-seq (ast-subx ast)
                              (make-ref '() k)))
               (cons k (closure-params ast)))))))

    (else
     (error "unknown ast" ast))))

(define (cps-list asts inner)
  (define (body x)
    (cps-list (cdr asts)
              (lambda (new-asts)
                (inner (cons x new-asts)))))
  (cond
   ((null? asts) (inner '()))
   ((or (lit? (car asts)) (ref? (car asts)))
    (body (car asts)))
   (else
    (let ((r (new-var "r")))
      (cps (car asts)
           (make-closure (list (body (make-ref '() r)))
                         (list r)))))))
