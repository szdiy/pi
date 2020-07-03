(use-modules (pi parser)
             (pi cps)
             (pi lir)
             (pi env)
             (pi codegen)
             (pi primitives)
             (pi pass normalize)
             (ice-9 pretty-print))





#;
(define e '(let ((y 5))                 ; ;
(let ((x 1))                            ; ;
(+ 2 x))))
;;(define e '(let ((x 123)) x))
;;(define e '(+ 1 1))
;;(define e '(let ((x 1) (y 5)) (+ x 2)))
(define e '(begin (define (func x) (+ x x)) (func 1)))
;;(display "AST:\n")
(define a (parser e))
;;(pretty-print a)
(pretty-print (ast->src a))
(display "CPS:\n")
(define c (ast->cps a))
(pretty-print (cps->expr c))
(display "optimized!\n")
(define o ((@@ (pi compile) optimize) c))
(display "--------------------toplevel------------------\n")
(top-level-for-each (lambda (v e)
                      (format #t "~a:~%" v)
                      (pretty-print (cps->expr e))
                      (display "----------\n")))
(display "--------------------end------------------\n")
(pretty-print (cps->expr o))
(print-primitives)
(parameterize ((current-kont 'global))
  (top-level-for-each
   (lambda (v e)
     (top-level-set! v (cps->lir e)))))
(define l (cps->lir o))
(display "LIR:\n")
(pretty-print (lir->expr l))
(define s (lir->sasm l))
(display "SASM:\n")
(pretty-print s)
