#!/usr/bin/guile \
-e main -L ./
!#

(use-modules (pi parser)
             (pi cps)
             (ice-9 match)
             (ice-9 pretty-print))

(define (main args)
  (match args
    ((prog file)
     (when (not (file-exists? file)) (format #t "File doesn't exist!~%"))
     (pretty-print (cps->expr (ast->cps (parser (call-with-input-file file read))))))
    (else (format #t "no file!~%"))))
