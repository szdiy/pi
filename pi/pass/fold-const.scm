;; 1. (if 1 2 3) -k-> (if 1 2 3)
;; 2. (if 1 2 (+ 3 4)) -k-> (letcont ((k (halt (+ 4 3)))) (if 1 2 k))
(define (fold-constant cps)
  (match cps
    (('letval ((v e)) body)
     (fold-constant
      (normalize/preserving
       `((lambda (,v) ,(fold-constant body)) ,e))))
    (('letcont ((v ('halt (? atom? a)))) body)
     (fold-constant
      (normalize/preserving
       `((lambda (,v) ,(fold-constant body)) ,a))))
    ((('lambda (v) body) e ...)
     (fold-constant (normalize/preserving cps)))
    (((? bind-special-form? sf) ((v e)) body) ;
     `(,sf ((,v ,(fold-constant e))) ,(fold-constant body)))
    (else cps)))
