(library (sph lang sescript expressions)
  (export
    ses-apply
    ses-case
    ses-chain
    ses-cond
    ses-define
    ses-environment
    ses-get
    ses-identical-infix
    ses-identifier
    ses-if
    ses-if*
    ses-include
    ses-lambda
    ses-let
    ses-let*
    ses-numeric-boolean
    ses-object
    ses-return
    ses-set
    ses-translated-infix
    ses-value
    ses-while
    ses-wrapped-throw)
  (import
    (ice-9 match)
    (sph)
    (sph lang ecmascript expressions)
    (sph lang scheme)
    (sph list)
    (except (srfi srfi-1) map)
    (only (guile)
      compose
      make-regexp
      raise
      string-join)
    (only (sph alist) alist list->alist)
    (only (sph filesystem) search-load-path)
    (only (sph string) parenthesise regexp-match-replace)
    (only (sph tree) tree-contains?)
    (only (sph two) alist->regexp-match-replacements))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ; format: (regexp search-string . replacement)
      ; replaced in order
      (alist ".-" (pair "-" "_")
        ".!$" (pair "!" "_x")
        "->" "_to_" ".&$" (pair "&" "_ampersand") "\\?" "_p" "./." (pair "/" "_slash_"))))

  (define-syntax-rule (add-begin a) (if (length-one? a) (first a) (pair (q begin) a)))
  (define (add-return-to-define a) "any -> list" (list (list (q begin) a (q (return undefined)))))
  (define (contains-set? a) "list -> boolean" (and (list? a) (tree-contains? a (q set))))
  (define statement-prefixes (q (define while set!)))

  (define (add-return-to-set! a) "list -> list"
    (list
      (if (> (length a) 3)
        (list (q begin) (drop-right a 2) (list (q return) (pair (q set!) (take-right a 2))))
        (list (q return) a))))

  (define* (add-return-statement a #:optional compile) "(any:sescript ...) boolean -> list"
    (if (list? a)
      (list-replace-last a
        (l (a-last) "non-expressions can not be used in a return statement"
          ; some cases might still be missing here
          (if (and (list? a-last) (not (null? a-last)))
            (case (first a-last)
              ( (begin)
                ; continue search for last expression in body
                (list (add-return-statement a-last)))
              ( (while return)
                ; do not add any return
                (list a-last))
              ((define) (add-return-to-define a-last))
              ( (if* if)
                (let (a-last-tail (tail a-last))
                  (list
                    (pairs (q if*) (first a-last-tail)
                      (apply append (map (compose add-return-statement list) (tail a-last-tail)))))))
              ((set!) (add-return-to-set! a-last))
              (else (list (list (q return) a-last))))
            (list (list (q return) a-last)))))
      (list (q return) a)))

  (define (contains-return-statement? a) "list -> boolean"
    (any
      (l (a)
        (match a (((quote return) _ ...) #t)
          (((quote begin) rest ...) (contains-return-statement? rest)) (_ #f)))
      a))

  (define (contains-statement? a) "list -> boolean"
    (match a
      ( ( (? symbol? prefix) rest ...)
        (or (containsq? statement-prefixes prefix) (contains-statement? rest)))
      (_ (and (list? a) (any contains-statement? a)))))

  (define (ses-apply a compile) (es-apply (compile (first a)) (map compile (tail a))))
  (define (ses-return a compile) (if (null? a) "return" (ses-apply (pair (q return) a) compile)))

  (define (ses-case a compile)
    (match a
      ( (value (test consequent ...) ...)
        (es-switch (compile value)
          (map (l (a b) (pair (if (eq? (q else) a) a (compile a)) (map compile b))) test consequent)))))

  (define (ses-chain a compile) (match (map compile a) ((name base a ...) (es-chain name base a))))

  (define (ses-cond a compile)
    (match a
      ((only-cond) (es-if (compile (first only-cond)) (compile (add-begin (tail only-cond)))))
      ( (first-cond middle-cond ... last-cond)
        (string-append (es-if (compile (first first-cond)) (compile (add-begin (tail first-cond))))
          (apply string-append
            (map
              (l (a)
                (string-append "else " (es-if (compile (first a)) (compile (add-begin (tail a))))))
              middle-cond))
          (let
            ((test (compile (first last-cond))) (consequent (compile (add-begin (tail last-cond)))))
            (string-append "else"
              (if (eq? (q else) (first last-cond)) (es-curly-brackets consequent)
                (string-append " " (es-if test consequent)))))))))

  (define (ses-define a compile)
    (match a
      ( ( (name formals ...) body ...)
        (es-function (compile (add-return-statement (add-begin body))) (map ses-identifier formals)
          (ses-identifier name)))
      ( (name value name/value ...)
        (es-define (map-slice 2 (l (a b) (pair (ses-identifier a) (compile b))) a)))))

  (define (ses-set a compile) (es-set (list->alist (map compile a))))

  (define (ses-environment a compile) "list -> string"
    (pair (q object) (fold-right (l (a result) (pairs a a result)) null a)))

  (define (ses-for a compile)
    (let
      (comma-join
        (l (a)
          (match a (((quote begin) a ...) (es-comma-join (map compile a)))
            (((? symbol?) _ ...) (compile a)) (_ (es-comma-join (map compile a))))))
      (match a
        ( ( (init test update) body ...)
          (es-for (comma-join init) (compile test) (comma-join update) (compile (add-begin body)))))))

  (define (ses-get a compile) "string string ... -> string" (apply es-get (map compile a)))

  (define (ses-identical-infix prefix a compile)
    (parenthesise (string-join (map compile a) (symbol->string prefix))))

  (define (ses-identifier a) "symbol/any -> string/any"
    (if (symbol? a) (regexp-match-replace (symbol->string a) identifier-replacements) a))

  (define (ses-if a compile) (apply es-if (map compile a)))
  (define (ses-if* a compile) (apply es-if-expression (map compile a)))

  (define (ses-include a compile load-paths) "-> sc"
    (add-begin
      (file->datums
        (any (search-load-path (string-append (first a) ".ses") load-paths)
          (search-load-path (string-append (first a) ".sjs") load-paths)))))

  (define (ses-lambda a compile)
    (es-function (compile (add-return-statement (add-begin (tail a))))
      (map ses-identifier (first a))))

  (define (ses-let a compile) "-> sc"
    (match a
      ( ( ( (formals vals) ...) body ...)
        (qq ((lambda (unquote formals) (unquote-splicing body)) (unquote-splicing vals))))
      ( ( (formal val) body ...)
        (qq ((lambda ((unquote formal)) (unquote-splicing body)) (unquote val))))
      ( (name ((formals vals) ...) body ...)
        (qq
          ( (lambda ((unquote name))
              (set! (unquote name) (lambda (unquote formals) (unquote-splicing body)))
              ((unquote name) (unquote-splicing vals))))))
      (_ (raise (q ses-syntax-error-for-let)))))

  (define (ses-let* a compile) "-> sc"
    (match a
      ( ( ( (formals vals) ...) body ...)
        (qq
          ( (lambda ((unquote (first formals)))
              (unquote-splicing
                (append
                  (map (l name+value (pair (q define) name+value)) (tail formals) (tail vals)) body)))
            (unquote (first vals)))))
      (_ (raise (q ses-syntax-error-let*)))))

  (define (ses-numeric-boolean prefix a compile) "for infix operators that return booleans"
    (let (operator (if (eq? (q =) prefix) "===" (symbol->string prefix)))
      (string-join
        (map-segments 2 (l (a b) (parenthesise (string-append a operator b)))
          (map (l (a) (if (contains-set? a) (parenthesise (compile a)) (compile a))) a))
        "&&")))

  (define (ses-object a compile) (es-object (list->alist (map compile a))))

  (define (ses-translated-infix prefix a compile)
    (parenthesise
      (string-join
        (map
          (l (a) "consider cases like a&&b=c"
            (if (contains-set? a) (parenthesise (compile a)) (compile a)))
          a)
        (case prefix
          ((and) "&&")
          ((bit-and) "&")
          ((bit-or) "|")
          ((bit-shift-left) "<<")
          ((bit-shift-right) ">>")
          ((bit-shift-right-fill) ">>>")
          ((bit-xor) "^")
          ((modulo) "%")
          ((or) "||")))))

  (define (ses-wrapped-throw a compile)
    "throw cannot occur in an if-expression as is, example of this: 1?2:throw(3). but if wrapped in a function it can"
    (es-apply (es-function (string-append "throw" (parenthesise (ses-value a))))))

  (define (ses-value a) "any -> string" (if (symbol? a) (ses-identifier a) (es-value a)))

  (define (ses-while a compile)
    (match a
      ( (test body ...)
        (string-append "while"
          (if (and (list? test) (eqv? (q set) (first test))) (parenthesise (compile test))
            (compile test))
          (parenthesise (compile (add-begin body))))))))
