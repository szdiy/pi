(use-modules (pi parser)
             (pi cps)
             (ice-9 pretty-print))

(define e '(+ 1 1))
(display "AST:\n")
(define a (parser e))
(pretty-print a)
(pretty-print (ast->src a))
(display "CPS:\n")
(define c (ast->cps a))
(pretty-print c)
(pretty-print (cps->expr c))
