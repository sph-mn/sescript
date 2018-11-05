(library (sph lang sescript expressions)
  (export
    ses-apply
    ses-begin
    ses-chain
    ses-cond
    ses-define
    ses-for
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
    ses-object*
    ses-return
    ses-set
    ses-switch
    ses-translated-infix
    ses-value
    ses-while)
  (import
    (ice-9 match)
    (sph)
    (sph lang ecmascript expressions)
    (sph lang scheme)
    (sph list)
    (only (guile)
      compose
      make-regexp
      raise
      string-suffix?
      string-join)
    (only (sph alist) alist list->alist)
    (only (sph filesystem) search-load-path)
    (only (sph string) parenthesise regexp-match-replace)
    (only (sph tree) tree-contains?)
    (only (srfi srfi-1) drop-right take-right))

  (define (alist->regexp-match-replacements a)
    "automatically converts strings at the prefix position to regular expressions"
    (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

  (define identifier-replacements
    (alist->regexp-match-replacements
      ; format: (regexp search-string . replacement)
      ; replaced in order
      (alist ".!$" (pair "!" "_x")
        "->" "_to_"
        ".-" (pair "-" "_") ".&$" (pair "&" "_ampersand") "\\?" "_p" "./." (pair "/" "_slash_"))))

  (define-syntax-rule (add-begin a) (if (length-one? a) (first a) (pair (q begin) a)))
  (define (contains-set? a) "list -> boolean" (and (list? a) (tree-contains? a (q set))))

  (define (add-return-statement a)
    "(any:sescript ...) -> (any:sescript ...)
     add return statements to return the last expression or undefined at all exit points.
     it has to differentiate between expressions and other statements that cant be returned"
    (list-replace-last a
      (l (a-last)
        "any -> (any:replacements ...)
        only expressions that themselves return a value can be used in a return statement"
        (if (and (list? a-last) (not (null? a-last)))
          ; check prefix of last expression.
          ; some cases might still be missing here
          (case (first a-last)
            ( (begin)
              ; continue search for last expression in body
              (list (add-return-statement a-last)))
            ( (while for return)
              ; do not add any return in these statements
              (list a-last))
            ((define) (list (list (q begin) a-last (q (return undefined)))))
            ( (if)
              (let (a-last-tail (tail a-last))
                (list
                  (pairs (first a-last) (first a-last-tail)
                    (apply append (map (compose add-return-statement list) (tail a-last-tail)))))))
            ( (switch)
              (list
                (pair (first a-last)
                  (match (tail a-last)
                    ( (value b ...)
                      (pair value
                        (map
                          (l (b)
                            (match b
                              ( (test consequent ... (quote break))
                                (pair test
                                  (append (add-return-statement consequent) (list (q break)))))
                              ((test consequent ...) (pair test (add-return-statement consequent)))))
                          b)))))))
            ( (set)
              ; at return to the set of the last variable
              (list
                (list (q begin) (drop-right a-last 2)
                  (list (q return) (pair (q set) (take-right a-last 2))))))
            (else (list (list (q return) a-last))))
          (list (list (q return) a-last))))))

  (define (ses-apply a compile) (es-apply (compile (first a)) (map compile (tail a))))
  (define (ses-return a compile) (if (null? a) "return" (ses-apply (pair (q return) a) compile)))

  (define (ses-begin a compile)
    (apply string-append
      (map
        (l (a)
          (let (a (compile a))
            (if (or (string-suffix? "*/\n" a) (string-suffix? ";" a)) a (string-append a ";"))))
        a)))

  (define (ses-switch a compile)
    (match a
      ( (expression (value consequent ...) ...)
        (es-switch (compile expression)
          (map
            (l (a b)
              (pair (if (eq? (q default) a) a (if (list? a) (map compile a) (compile a)))
                (map compile b)))
            value consequent)))))

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
        (es-function (compile (add-begin (add-return-statement body))) (map ses-identifier formals)
          (ses-identifier name)))
      ( (name value name/value ...)
        (es-define (map-slice 2 (l (a b) (pair (ses-identifier a) (compile b))) a)))))

  (define (ses-set a compile) (es-set (list->alist (map compile a))))

  (define (ses-object* a compile)
    "list -> sxml
     special form of (object) where variable identifiers are the keys.
     (object* a b c) = (object a a b b c c)"
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
    (es-function (compile (add-begin (add-return-statement (tail a))))
      (map ses-identifier (first a))))

  (define (ses-let a compile) "-> sc"
    (match a
      ( ( ( (formals vals) ...) body ...)
        (qq ((lambda (unquote formals) (unquote-splicing body)) (unquote-splicing vals))))
      ( ( (formal val) body ...)
        (qq ((lambda ((unquote formal)) (unquote-splicing body)) (unquote val))))
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

  (define (ses-numeric-boolean prefix a compile)
    "for infix operators that return booleans.
     (= a b c) does not mean a===b===c in javascript but (a===b&&b===c)"
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

  (define (ses-value a) "any -> string" (if (symbol? a) (ses-identifier a) (es-value a)))

  (define (ses-while a compile)
    (match a
      ( (test body ...)
        (string-append "while"
          (if (and (list? test) (eqv? (q set) (first test))) (parenthesise (compile test))
            (compile test))
          (parenthesise (compile (add-begin body))))))))
