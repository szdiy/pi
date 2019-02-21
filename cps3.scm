(import (ice-9 match)
        (ice-9 pretty-print)
        (srfi srfi-1)
        (rnrs))

(define-syntax-rule (type-check o pred)
  (when (not (pred o))
    (throw 'pi-error (format #f "Wrong type, expect `~a'" 'pred))))

(define-syntax define-typed-record
  (syntax-rules (parent fields)
    ((_ tr (parent p) (fields (f t) ...))
     (define-record-type tr (parent p)
       (fields f ...)
       (protocol
        (lambda (new)
          (lambda (f ...)
            (type-check f t) ...
            (new f ...))))))
    ((_ tr (fields (f t) ...))
     (define-record-type tr
       (fields f ...)
       (protocol
        (lambda (new)
          (lambda (f ...)
            (type-check f t) ...
            (new f ...))))))))

(define (immediate? x)
  (or (number? x)
      (string? x)
      (symbol? x)
      (list? x)
      (pair? x)))

;; 1. CPS is isomophic to a subset of Scheme
;; 2. No control stack or other unbounded temporary storage is required
;; 3. No decisions as to the order of evaluation of (non-trivial) sub-expressions
;; Once the CPS has been generated, the remainder of the compilation process is fairly easy.
;; There is a reasonably direct correspondence between constructs in the CPS and machine-language.
;; CPS is differ from Scheme in only 2 respects:
;; 1. Each primitive function is different, in that it returns no value.
;;    Instead, it accepts an additional argument, the continuation, which must be a function of one
;;    argument, and by definition invokes the continuation tail-recursively, giving it as an argument the
;;    computed "value" of the primitive function. Continuations, however, do not themselves take continuations
;;    as arguments.
;; 2. No combination may have a non-trivial argument. In strict CPS, this implies that no combination can have
;;    another combination as an argument.
;; 3. The program is splited to bunch of primitive operations, each result of an operation is always uniquely named.
;; 4. The CPS is used to represent code blocks which can be "jumped to" from several locations, by invoking the continuation.
;; 5. The CPS is used to represent the code to be executed after a function calling, so the function has to receive the relavent continuation
;;    as an argument, and call this continuation with its returned value.
;; 6. The redundant eta-redex can be reduced by introducing a function call in translation pass.
(define-record-type cps
  (fields
   (kont cps?) ; current continuation
   (kont-name symbol?) ; the id of the continuation
   ))

(define-record-type value/k (parent cps))
;; Immediate value
(define-record-type imm/k (parent value/k)
  (fields
   (val immediate?)))
(define-record-type let/k (parent value/k)
  (fields
   (vars symbol?)
   (vals cps?)))
(define-record-type lambda/k (parent value/k)
  (fields
   (kont-arg symbol?)
   (args symbol?)
   (body cps?)))

(define-record-type seq/k (parent cps)
  (fields
   ;; TODO
   ))

;; we unify both pari/list to list
(define-record-type list/k (parent cps)
  (fields
   (index number?)))

;; Local continuation is actually labelled basic-block
(define-record-type local-cont/k (parent cps)
  (fields
   (arg cps?)
   (body cps?)))

;; Application of a continuation
(define-record-type cont-app/k (parent cps)
  (fields
   (arg cps?)))

(define-record-type application/k (parent cps))
(define-record-type func-app/k (parent application/k)
  (fields
   (func cps?)
   (arg cps?)))

(define-record-type prim-app/k (parent application/k)
  (fields
   (primitive primitive?)
   (arg cps?)))

;; cnd should be value, true/false should be continuation
;; In this case, the kont is useless and should be #f.
;; The actual continuation is in true or false branch.
(define-record-type branch/k (parent cps)
  (fields
   (cnd value/k?)
   (true cps?)
   (false cps?)))

(define-record-type )
(define end-cont identity)

;; capture-free substitution
(define (cfs expr sub-list)
  (match expr
    (($ lambda/k ($ cps kont kont-name) kont-arg args body)
     (make-lambda/k
      (cfs kont sub-list) kont-name
      kont-arg args (cfs body sub-list)))
    (($ let/k ($ cps kont kont-name) vars vals)
     (make-let/k
      (cfs kont sub-list) kont-name
      vars (map (lambda (v) (cfs v sub-list)) vals)))
    (else expr)))

(define* (expr->cps expr #:optional (kont 'end-cont))
  ;;(format #t "$$$$$$ ~a       ~a~%" expr kont)
  (match expr
    ((? atom?) (cps-comp expr kont))
    ((? var?) (cps-comp expr kont))
    (('lambda (args ...) body ...)
     `(lambda (k ,@args) ,@(map (lambda (e) (expr->cps e k)) body)))
    (('define (v args ...) body ...)
     `(,kont (def! ',v (lambda (k ,@args)
                         ,@(map (lambda (e) (expr->cps e k)) body)))))
    (`(define ,v ,e)

     (let ((k `(lambda (r1) (,kont (def! ',v r1)))))
       `(,kont ,(expr->cps e k))))
    (`(set! ,v ,e)
     (let ((k `(lambda (r1) (,kont (set! ,v r1)))))
       `(,kont ,(expr->cps e k))))
    (`(if ,cnd ,b1 ,b2)
     (let ((k `(lambda (c1) (if c1 ,(expr->cps b1) ,(expr->cps b2)))))
       `(,kont ,(expr->cps cnd k))))
    (('args args ...) `(,kont (list ,kont ,@(map (lambda (e) (expr->cps e kont)) args))))
    #;
    ((('lambda (args ...) body ...) e ...)
    (let ((k `(lambda (r1) (lambda (r2) (,kont (apply r1 r2))))))
    #;`(,(expr->cps (car expr) k) ,@(map (lambda (ee) (expr->cps ee kont)) e)) ; ; ; ;
    `(apply ,(expr->cps (car expr) k) )))
    ((e1 e2 ...)
     (let ((k `(lambda (k r1) (lambda r2 (,kont (apply r1 k r2))))))
       `(apply ,(expr->cps e1 k)  #;,(pk "kont"kont) ,(expr->cps `(args ,@e2) kont)
               )))
    (else "no")))
