(use-modules (pi parser)
             (pi cps)
             (pi pass normalize)
             (ice-9 pretty-print))





(define e '(let ((y 5))                 ; ;
             (let ((x 1))                            ; ;
               (+ 2 x))))
;;(define e '(let ((x 123)) x))
;;(define e '(+ 1 1))
(display "AST:\n")
(define a (parser e))
(pretty-print a)
(pretty-print (ast->src a))
(display "CPS:\n")
(define c (ast->cps a))
(pretty-print c)
(pretty-print (cps->expr c))
(define o ((@@ (pi compile) optimize) c))
(pretty-print (cps->expr o))
