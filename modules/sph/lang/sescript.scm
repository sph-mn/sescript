(library (sph lang sescript)
  (export
    ses-default-load-paths
    sescript->ecmascript
    sescript->ecmascript-string
    sescript-use-strict
    sph-lang-sescript-description)
  (import
    (guile)
    (ice-9 match)
    (sph)
    (sph conditional)
    (sph filesystem)
    (sph io)
    (sph lang ecmascript expressions)
    (sph lang sescript expressions)
    (sph string)
    (sph tree)
    (except (srfi srfi-1) map)
    (only (sph alist) list->alist)
    (only (sph list)
      any->list
      any->list-s
      contains?
      containsq?
      improper-list-split-at-last
      length-one?
      list-replace-last))

  (define sph-lang-sescript-description
    "s-expression language that compiles to javascript/ecmascript.
     introducing features that arent part of ecmascript should be avoided
     to keep it being only a syntax translation")

  (define ses-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SES_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define (descend-expr->sescript prefix a compile load-paths)
    "this is applied when descending the tree, and the result will be parsed again"
    (case prefix
      ((case) (ses-case a compile))
      ((cond) (ses-cond a compile))
      ( (define)
        (match a
          (((name formals ...) . body) (list (q define) name (pairs (q lambda) formals body))) (_ #f)))
      ((let) (ses-let a compile))
      ((let*) (ses-let* a compile))
      ((nullary) (qq (lambda () (unquote-splicing a))))
      ((ses-include) (ses-include a compile load-paths))
      (else #f)))

  (define (descend-expr->ecmascript prefix a compile)
    "this is applied when descending the tree, and the result will not be parsed again.
     this is for expressions that create output that can not be created with other ses syntax"
    (case prefix
      ((+ - * /) (ses-identical-infix prefix a compile))
      ((= < > <= >=) (ses-numeric-boolean prefix a compile))
      ( (and or modulo bit-xor bit-and bit-or bit-shift-right bit-shift-left bit-shift-right-fill)
        (ses-translated-infix prefix a compile))
      ((begin) (string-join (map compile a) ";"))
      ((chain) (apply es-chain a))
      ((declare) (es-declare (map ses-identifier a)))
      ((define) (apply es-define a))
      ((not) (string-append "!" (apply string-append a)))
      ((object) (es-object (list->alist a)))
      ((get) (apply ses-get a))
      ((array) (es-vector a))
      ((new) (string-append "new " (ses-apply (first a) (tail a))))
      ((environment) (ses-environment a))
      ((return) (if (null? a) "return" (ses-apply (first a) a)))
      ((throw) (ses-wrapped-throw a compile))
      ((if) (apply ses-if (map compile a)))
      ((if*) (apply es-if-expression (map compile a)))
      ((ses-insert) (apply string-append a))
      ((set!) (apply es-set a))
      ((lambda l) (ses-lambda a compile))
      ((make-regexp) (es-regexp a))
      ((while) (ses-while a compile))
      (else (ses-apply prefix a))))

  (define (descend-proc load-paths)
    (l (a compile) "list procedure -> (match-result parse-again?)"
      (let*
        ((prefix (first a)) (a (tail a)) (b (descend-expr->sescript prefix a compile load-paths)))
        (if b (list b #t)
          (let (b (descend-expr->ecmascript prefix a compile)) (if b (list b #f) (list #f #t)))))))

  (define* (sescript->ecmascript-string expr #:optional (load-paths ses-default-load-paths))
    "expression -> string" (tree-transform expr (descend-proc load-paths) identity ses-value))

  (define* (sescript->ecmascript exprs port #:optional (load-paths ses-default-load-paths))
    "(expression ...) port ->"
    (each (l (a) (display (sescript->ecmascript-string a load-paths) port) (display ";" port))
      exprs))

  (define (sescript-use-strict port)
    "port -> string
     writes a \"use strict\"; command to port to the following
     code to be interpreted in the so called strict-mode.
     this can appear multiple times in the output without being an error"
    (display "\"use strict\";\n" port)))
