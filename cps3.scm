(import (ice-9 match)
        (ice-9 pretty-print)
        (srfi srfi-1)
        (rnrs)
        (pi primitives)
        (pi utils))

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
(define-typed-record cps
  (fields
   (kont cps?) ; current continuation
   (kont-name symbol?) ; the id of the continuation
   ))

(define-typed-record end/k
  (fields (kont-name symbol?)))

(define end-cont (make-end/k (newsym "kont-")))

(define-typed-record end/k (parent cps))

(define-typed-record value/k (parent cps))

;; Immediate value
(define-typed-record imm/k (parent value/k)
  (fields
   (val immediate?)))

(define-typed-record let/k (parent value/k)
  (fields
   (vars symbol-list?)
   (vals cps-list?)
   (body list?)))

(define-typed-record lambda/k (parent value/k)
  (fields
   (kont-arg symbol?)
   (args symbol-list?)
   (body list?)))

(define-typed-record seq/k (parent cps)
  (fields
   (body list?)))

;; we unify both pair/list to list
(define-typed-record list/k (parent value/k)
  (fields
   (size number?)
   (content list?)))

;; Local continuation is actually labelled basic-block
(define-typed-record local-cont/k (parent cps)
  (fields
   (arg symbol-list?)
   (body cps?)))

;; Application of a continuation
(define-typed-record cont-app/k (parent cps)
  (fields
   (arg cps?)))

(define-typed-record application/k (parent cps))
(define-typed-record func-app/k (parent application/k)
  (fields
   (func cps?)
   (args cps-list?)))

(define-typed-record prim-app/k (parent application/k)
  (fields
   (primitive primitive?)
   (arg cps?)))

;; cnd should be value, true/false should be continuation.
(define-typed-record branch/k (parent cps)
  (fields
   ;; Although we will wrap the actual branching into the new
   ;; continuation, we still leave these 3 slots here for
   ;; storing them, it's a trick for type checking here.
   (cnd value/k?)
   (true cps?)
   (false cps?)))

(define-typed-record def/k (parent cps)
  (fields
   (names symbol?)
   (vals cps?)))

;; capture-free substitution
(define (cfs expr sub-list)
  (define (filter-bound-var bound-vars sub-list)
    (filter-map (lambda (v)
                  (if (memq (car v) bound-vars)
                      #f
                      v))
                sub-list))
  (match expr
    ;; value cps
    (($ lambda/k ($ value/k ($ cps _ kont kont-name)) kont-arg args body)
     (let ((new-sub-list (filter-bound-var args sub-list)))
       (make-lambda/k
        (cfs kont new-sub-list) kont-name
        kont-arg args (cfs body new-sub-list))))
    (($ let/k ($ value/k ($ cps _ kont kont-name)) vars vals)
     ;; The cfs to let/k is the typical beta-reduction
     (cfs
      (make-seq/k
       end-cont kont-name
       kont)
      (map (lambda (var val)
             (or (assoc-ref sub var) val))
           vars vals)))
    (($ list/k ($ value/k ($ cps _ kont kont-name)) size lst)
     (make-list/k
      (cfs kont sub-list) kont-name
      size
      (map (lambda (e) (cfs e sub-list)) lst)))
    ((or (? end/k?) (? imm/k?)) expr)    ; immediate value can't be subsituted
    ;; common cps
    (($ seq/k ($ cps _ kont kont-name) body)
     (make-seq/k
      (cfs kont sub-list) kont-name
      (map (lambda (e) (cfs e sub-list)) body)))
    (($ branch/k ($ cps _ kont kont-name) cnd true false)
     (make-branch/k
      (cfs kont sub-list) kont-name
      (cfs cnd sub-list)
      (cfs true sub-list)
      (cfs false sub-list)))
    (else expr)))

(define* (expr->cps expr #:optional (kont end-cont))
  ;; With respect to security, we need to add "\" to each variable name, in case
  ;; it conflicts with the kont-arg name by intended constructing.
  (define (fix-var x) (symbol-append '## x))
  ;;(format #t "$$$$$$ ~a       ~a~%" expr kont)
  (match expr
    ((? atom?) (cps-comp expr kont))
    ((? var?) (cps-comp expr kont))
    (('lambda (args ...) body ...)
     (make-lambda/k
      (newsym "lambda/k-") end-cont
      (newsym "kont-arg-")
      (map fix-var args)
      (map (lambda (e) (expr->cps e end-cont)) body))
     `(lambda (k ,@args) ,@(map (lambda (e) (expr->cps e k)) body)))
    (('define (v args ...) body ...)
     (make-def/k
      (cps-kont-name kont) (cps-kont kont)
      (fix-var v)
      (make-lambda/k
       (newsym "lambda/k-") 'value/k
       (newsym "kont-arg-")
       (map fix-var args)
       (map (lambda (e) (expr->cps e kont)) body))))
    (`(define ,v ,e)
     (make-def/k
      (cps-kont-name kont) (cps-kont kont)
      (fix-var v)
      (expr->cps e kont)))
    (`(set! ,v ,e)
     ;; NOTE: set! is a special primitive since we need to fix
     ;;       naming of the var with fix-var.
     ;;       So we pick it out here.
     (make-prim-app/k
      (cps-kont-name kont) kont
      primitive/set!
      (list (fix-var v) (expr->cps e kont))))
    (`(if ,cnd ,b1 ,b2)
     (let* ((true-branch-var (gensym "true-kont-"))
            (false-branch-var (gensym "false-kont-"))
            (true-branch (expr->cps b1 kont))
            (false-branch (expr->cps b2 kont))
            (cnd-var (gensym "cnd-kont-"))
            (cnd-val (expr->cps cnd kont))
            (current-kont
             (make-let/k
              (newsym "let/k-") kont
              (list true-branch-var false-branch-var cnd-var)
              (list true-branch false-branch cnd-val)
              (list
               (make-special-form:if
                cnd-var
                true-branch-var
                false-branch-var)))))
       (make-branch/k
        (newsym "branch/k-") current-kont
        cnd-val
        true-branch
        false-branch)))
    (((? is-op-a-primitive? p) args ...)
     (make-prim-app/k
      (cps-kont-name kont) kont
      (symbol->primitive p)
      (map (lambda (e) (expr->cps e kont)) args)))
    ((e1 e2 ...)
     (let ((func (expr->cps e1 kont))
           (args (lambda (e) (expr->cps e kont)) e2))
       (make-func-app/k
        (cps-kont-name kont) kont
        func
        args)))
    (else (throw 'pi-error expr->cps
                 "Invalid expr " expr))))
