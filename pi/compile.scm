;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2020
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

(define-module (pi compile)
  #:use-module (pi utils)
  #:use-module (pi env)
  #:use-module (pi parser)
  #:use-module (pi pass)
  #:use-module (pi cps)
  #:use-module (pi lir)
  #:use-module (pi types)
  #:use-module (pi codegen)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:export (compile))

(define (init-optimizations)
  (process-use-modules
   (map (lambda (s) `((pi pass ,(string->symbol (file-basename s)))))
        (scandir (string-append (dirname (current-filename)) "/pass")
                 (lambda (s) (string-match "\\.scm" s))))))

(define (reader port)
  (let lp((ret '()))
    (cond
     ((eof-object? (peek-char port)) (reverse! ret))
     (else (lp (cons (read port) ret))))))

(define (optimize cps)
  (define (do-optimize cps)
    (run-pass
     cps
     normalize
     ;;function-inline
     ;;dead-function-elimination
     ;;fold-constant
     ;;delta-reduction
     ;;fold-branch
     ;;dead-variable-elimination
     ;;elre
     ;;closure-conversion
     ;;lambda-lifting
     ))
  (display "optimize\n")
  (init-optimizations)
  (top-level-for-each do-optimize)
  (do-optimize cps))

(define (compile filename)
  (define outfile (gen-outfile filename))
  (when (not (file-exists? filename))
    (error "File doens't exist!" filename))
  (when (file-exists? outfile)
    (delete-file outfile))
  (let* ((exprs (call-with-input-file filename reader))
         (ast (map parser exprs))
         (cps (ast->cps ast))
         (cooked (optimize cps))
         (lir (cps->lir cooked)))
    (codegen lir outfile)))
