(define-module (sph lang sescript))

(use-modules (srfi srfi-1) (srfi srfi-2)
  (ice-9 match) (sph)
  (sph string) (sph hashtable)
  (sph list) (sph alist)
  ((sph filesystem) #:select (ensure-trailing-slash search-load-path))
  ((sph lang scheme) #:select (file->datums)) ((sph tree) #:select (tree-contains? tree-transform)))

(export ses-default-load-paths ses-descend-ecmascript
  ses-descend-sescript sescript->ecmascript
  sescript->ecmascript-string sescript-use-strict sph-lang-sescript-description)

(define sph-lang-sescript-description
  "compiles s-expressions to javascript/ecmascript.
   sescript tries to avoid adding syntax to ecmascript and be mostly a mapping.
   bindings with es- take only strings or lists of strings and return strings, ses- may take other scheme types")

(define (alist->regexp-match-replacements a)
  "automatically converts strings at the prefix position to regular expressions"
  (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

"format: (regexp search-string . replacement)"
"replaced in order"

(define identifier-replacements
  (alist->regexp-match-replacements
    (alist ".!$" (pair "!" "_x")
      "->" "_to_"
      ".-" (pair "-" "_") ".&$" (pair "&" "_ampersand") "\\?" "_p" "./." (pair "/" "_slash_"))))

(define-syntax-rule (add-begin a) (if (= 1 (length a)) (first a) (pair (q begin) a)))
(define (contains-set? a) "list -> boolean" (and (list? a) (tree-contains? a (q set))))

(define (add-return-statement a)
  "(any:sescript ...) -> (any:sescript ...)
   add return statements to return the last expression or undefined at all exit points.
   it has to differentiate between expressions and other statements that cant be returned"
  (map-last-n 1 a
    (l (a-last)
      "any -> (any:replacements ...)
       only expressions that themselves return a value can be used in a return statement"
      "check prefix of last expression. some cases might still be missing here"
      (if (and (list? a-last) (not (null? a-last)))
        (case (first a-last)
          ( (begin) "continue search for last expression in body"
            (list (add-return-statement a-last)))
          ((while for return) "do not add any return in these statements" (list a-last))
          ((define) (list (list (q begin) a-last (q (return undefined)))))
          ( (if)
            (let (a-last-tail (tail a-last))
              (list
                (pairs (first a-last) (first a-last-tail)
                  (apply append (map (compose add-return-statement list) (tail a-last-tail)))))))
          ( (set) "at return to the set of the last variable"
            (list
              (list (q begin) (drop-right a-last 2)
                (list (q return) (pair (q set) (take-right a-last 2))))))
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
          (else (list (list (q return) a-last))))
        (list (list (q return) a-last))))))

(define es-escape-single-char
  (alist "\"" "\\\"" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

(define-syntax-rule (or-null-string a ...) (or a ... ""))
(define (parenthesise-ensure a) (if (parenthesised? a) a (parenthesise a)))

(define* (es-apply name #:optional a) "string (string ...) -> string"
  (string-append name (if a (parenthesise-ensure (es-comma-join a)) "()")))

(define (es-chain name base a) "string string (string ...) -> string"
  (es-apply (string-append base "." name) a))

(define (es-comma-join a) (string-join a ","))
(define (es-curly-brackets a) (string-append "{" a "}"))
(define (es-declare names) "list -> string" (string-append "var " (es-comma-join names)))

(define* (es-define a) "((key . value) ...) -> string"
  (string-append "var " (es-comma-join (map (l (a) (string-append (first a) "=" (tail a))) a))))

(define (es-for init test update body)
  (string-append "for" (parenthesise (string-append init ";" test ";" update))
    (es-curly-brackets body)))

(define* (es-function #:optional body formals name)
  ( (if name identity parenthesise)
    (string-append "function" (if name (string-append " " name) "")
      (parenthesise (es-comma-join formals)) (es-curly-brackets body))))

(define (es-get a . keys) (string-append a (es-square-brackets (string-join keys "]["))))

(define (es-identifier a)
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) a)
    (else (raise (list (q es-error-cannot-convert-identifier) a)))))

(define* (es-if-expression test consequent #:optional alternate)
  (parenthesise (string-append test "?" consequent ":" (or alternate "undefined"))))

(define* (es-if test consequent #:optional alternate)
  (string-append "if" (parenthesise-ensure test)
    (es-curly-brackets consequent)
    (or-null-string (and alternate (string-append "else" (es-curly-brackets alternate))))))

(define (es-object key/value) "((key . value) ...) -> string"
  (es-curly-brackets (es-comma-join (alist-map (l (a b) (string-append a ":" b)) key/value))))

(define (es-new name a) (string-append "new " (es-apply name a)))
(define (es-square-brackets a) (string-append "[" a "]"))

(define* (es-regexp pattern #:optional modifiers)
  (string-append "/" pattern "/" (or-null-string modifiers)))

(define (es-set a) "((name . value) ...) -> string"
  (string-join (map (l (a) (string-append (first a) "=" (tail a))) a) ";"))

(define (es-string a)
  (string-enclose
    (fold (l (a result) (string-replace-string result (first a) (tail a))) a es-escape-single-char)
    "\""))

(define (es-switch expression cases)
  "expression ((value/symbol:default consequent ...) ...) -> string"
  (string-append "switch" (parenthesise-ensure expression)
    (es-curly-brackets
      (string-join
        (map
          (l (a)
            (let (test (first a))
              (if (eq? (q default) test) (string-append "default:" (string-join (tail a) ";"))
                (string-append
                  (apply string-append (map (l (a) (string-append "case " a ":")) (any->list test)))
                  (string-join (tail a) ";")))))
          cases)
        ";"))))

(define* (es-try-catch-finally try #:optional catch-formal catch finally)
  (string-append "try" (es-curly-brackets try)
    (or-null-string
      (and catch
        (string-append "catch" (parenthesise (or-null-string catch-formal))
          (es-curly-brackets catch))))
    (or-null-string (and finally (string-append "finally" (es-curly-brackets finally))))))

(define (es-value a) "handles the default conversions between scheme and ecmascript types"
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) (es-string a))
    ((number? a) (number->string a))
    ((vector? a) (es-array (vector->list a)))
    ((boolean? a) (if a "true" "false"))
    ((char? a) (string-enclose (any->string a) "\""))
    (else (raise (q es-error-cannot-convert-value)))))

(define (es-array contents) "(string ...) -> string" (es-square-brackets (es-comma-join contents)))
(define (ses-array a compile) (es-array (map compile a)))
(define (ses-apply a compile) (es-apply (compile (first a)) (map compile (tail a))))
(define (ses-declare a compile) (es-declare (map ses-identifier a)))
(define (ses-regexp a compile) (apply es-regexp a))
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
  (match a ((only-cond) (es-if (compile (first only-cond)) (compile (add-begin (tail only-cond)))))
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
  (es-function (compile (add-begin (add-return-statement (tail a)))) (map ses-identifier (first a))))

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
              (append (map (l name+value (pair (q define) name+value)) (tail formals) (tail vals))
                body)))
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
        (parenthesise (compile (add-begin body)))))))

(define ses-default-load-paths
  (map ensure-trailing-slash (let (a (getenv "SES_LOAD_PATH")) (if a (string-split a #\:) null))))

(define ses-descend-sescript
  (ht-create-symbol-q object* ses-object* let ses-let let* ses-let* ses-include))

(define identical-infix (q (+ - * /)))
(define numeric-boolean (q (= > < >= <=)))

(define translated-infix
  (q (and bit-and bit-or bit-shift-right bit-shift-left bit-shift-right-fill bit-xor modulo or)))

(define ses-descend-ecmascript
  (ht-create-symbol-q array ses-array
    begin ses-begin
    switch ses-switch
    chain ses-chain
    cond ses-cond
    declare ses-declare
    define ses-define
    for ses-for
    get ses-get
    if ses-if
    if* ses-if*
    l ses-lambda
    lambda ses-lambda
    make-regexp ses-regexp
    new (l (a compile) (string-append "new " (ses-apply a compile)))
    not (l (a compile) (string-append "!" (compile (first a))))
    object ses-object
    return ses-return
    ses-insert (l (a compile) (apply string-append a))
    ses-comment (l (a compile) (string-append "\n/* " (string-join a "\n  ") " */\n"))
    set ses-set while ses-while))

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
  (display "\"use strict\";\n" port))
