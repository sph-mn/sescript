(library (sph lang sescript)
  (export
    ses-default-load-paths
    ses-descend-ecmascript
    ses-descend-sescript
    sescript->ecmascript
    sescript->ecmascript-string
    sescript-use-strict
    sph-lang-sescript-description)
  (import
    (sph)
    (sph hashtable)
    (sph lang ecmascript expressions)
    (sph lang sescript expressions)
    (sph list)
    (only (guile)
      display
      getenv
      string-join
      string-split)
    (only (sph filesystem) ensure-trailing-slash)
    (only (sph tree) tree-transform))

  (define sph-lang-sescript-description
    "compiles s-expressions to javascript/ecmascript.
     mostly avoids introducing new features to be mostly a mapping")

  (define ses-default-load-paths
    (map ensure-trailing-slash (let (a (getenv "SES_LOAD_PATH")) (if a (string-split a #\:) null))))

  (define-as ses-descend-sescript ht-create-symbol
    object* ses-object*
    let ses-let
    let* ses-let* nullary (l (a compile) (qq (lambda () (unquote-splicing a)))) ses-include)

  (define-as identical-infix list-q + - * /)
  (define-as numeric-boolean list-q = > < >= <=)

  (define-as translated-infix list-q
    and bit-and bit-or bit-shift-right bit-shift-left bit-shift-right-fill bit-xor modulo or)

  (define-as ses-descend-ecmascript ht-create-symbol
    array (l (a compile) (es-array (map compile a)))
    begin ses-begin
    case ses-case
    chain ses-chain
    cond ses-cond
    declare (l (a compile) (es-declare (map ses-identifier a)))
    define ses-define
    for ses-for
    get ses-get
    if ses-if
    if* ses-if*
    l ses-lambda
    lambda ses-lambda
    make-regexp (l (a compile) (apply es-regexp a))
    new (l (a compile) (string-append "new " (ses-apply a compile)))
    not (l (a compile) (string-append "!" (compile (first a))))
    object ses-object
    return ses-return
    ses-insert (l (a compile) (apply string-append a))
    ses-comment (l (a compile) (string-append "\n/* " (string-join a "\n  ") " */\n"))
    set ses-set while ses-while)

  (for-each
    (l (prefixes f)
      (for-each
        (l (prefix) (ht-set! ses-descend-ecmascript prefix (l (a compile) (f prefix a compile))))
        prefixes))
    (list translated-infix identical-infix numeric-boolean)
    (list ses-translated-infix ses-identical-infix ses-numeric-boolean))

  (define descend-f
    (let
      ( (to-sescript
          (l (prefix a compile load-paths) "and the result will be parsed again"
            (if (eq? (q ses-include) prefix) (ses-include a compile load-paths)
              (and-let* ((f (ht-ref ses-descend-sescript prefix))) (f a compile)))))
        (to-ecmascript
          (l (prefix a compile)
            "the result will not be parsed again.
            this is for expressions that create output that can not be created with other ses syntax"
            (let (f (ht-ref ses-descend-ecmascript prefix))
              (if f (f a compile) (ses-apply (pair prefix a) compile))))))
      (l (load-paths)
        (l (a compile)
          "this is applied when descending the tree
          list procedure -> (string/list:match-result parse-again?)"
          (let* ((prefix (first a)) (a (tail a)) (b (to-sescript prefix a compile load-paths)))
            (if b (list b #t)
              (let (b (to-ecmascript prefix a compile)) (if b (list b #f) (list #f #t)))))))))

  (define* (sescript->ecmascript-string expr #:optional (load-paths ses-default-load-paths))
    "expression -> string"
    (tree-transform (list (q begin) expr) (descend-f load-paths) identity ses-value))

  (define* (sescript->ecmascript expressions port #:optional (load-paths ses-default-load-paths))
    "(expression ...) port ->"
    (for-each (l (a) (display (sescript->ecmascript-string a load-paths) port)) expressions))

  (define (sescript-use-strict port)
    "port -> string
     writes a \"use strict\"; command to port for the following
     code to be interpreted in the so called strict-mode.
     this can appear multiple times in the output without it being an error"
    (display "\"use strict\";\n" port)))
