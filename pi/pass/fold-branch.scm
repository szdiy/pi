;; NOTE: fold-constant should be applied before.
;; NOTE: after eliminate the dead branch, it's necessary to apply
;;       dead-variable-eliminate to reduce the unused branch continuation binding.
(define (fold-branch cps)
  (define (detect e)
    (match e
      ((? boolean? b) b)
      (else 'no)))
  (match cps
    (('if cnd b1 b2)
     (let ((result (detect cnd)))
       (match result
         ('no cps)
         (#t b1)
         (else b2))))
    ((('lambda (v) body) e ...)
     `((lambda (,v) ,(fold-branch body)) ,@(map fold-branch e)))
    (('lambda (v) body)
     `(lambda (,v) ,(fold-branch body)))
    (((? bind-special-form? sf) ((v e)) body)
     `(,sf ((,v ,(fold-branch e))) ,(fold-branch body)))
    (('begin rest ...)
     `(begin ,@(map fold-branch rest)))
    ((f args ...)
     `(,f ,@(map fold-branch args)))
    (else cps)))
