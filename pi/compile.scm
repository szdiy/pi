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
  #:use-module (pi module)
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

(define (optimize cexpr)
  (define (do-optimize cexpr)
    (run-pass
     cexpr
     normalize
     function-inline
     dead-function-elimination
     fold-constant
     (constant-propagation 2)
     useless-cont
     delta-reduction
     fold-branch
     dead-variable-elimination
     elre
     closure-conversion
     lambda-lifting))
  (display "optimize\n")
  (init-optimizations)
  (parameterize ((current-kont 'global))
    ;; Prevent unecessary lifting and inline for global functions
    (top-level-for-each (lambda (_ e) (do-optimize e))))
  (do-optimize cexpr))

(define (compile filename)
  (define outfile (gen-outfile filename))
  (when (not (file-exists? filename))
    (error "File doens't exist!" filename))
  (when (file-exists? outfile)
    (delete-file outfile))
  (let* ((mod (read-as-mod filename))
         (exprs (mod-exprs mod))
         (ast (map parser exprs))
         (cexpr (ast->cps ast))
         (cooked (optimize cexpr))
         (lir (cps->lir cooked)))
    (codegen lir outfile)))
