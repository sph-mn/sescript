(define-module (sph sescript))

(use-modules (sph sescript sph) (srfi srfi-1)
  (srfi srfi-2) (ice-9 match)
  (ice-9 regex) (rnrs io ports) (rnrs hashtables) ((rnrs hashtables) #:prefix rnrs-))

(export ses-default-load-paths ses-descend-ecmascript
  ses-descend-sescript sescript->ecmascript
  sescript->ecmascript-string sescript-use-strict sph-lang-sescript-description)

(define sph-lang-sescript-description
  "compiles s-expressions to javascript/ecmascript.
   sescript tries to avoid adding syntax to ecmascript and be mostly a mapping.
   bindings with es- take only strings or lists of strings and return strings, ses- may take other scheme types")

(define ht-make rnrs-make-hashtable)
(define ht-hash-equal rnrs-equal-hash)
(define ht-hash-symbol rnrs-symbol-hash)

(define* (ht-from-list a #:optional (equal-proc equal?) (hash-proc ht-hash-equal))
  "convert a list to an r6rs standard library hashtable. nested lists are not converted to a hash.
   example
   (ht-ref (ht-from-list (list 'a 1 'b 2)) 'b #f)
   -> 2"
  (let (result (ht-make hash-proc equal-proc))
    (fold (l (value key) (if key (begin (ht-set! result key value) #f) value)) #f a) result))

(define-syntax-rules ht-ref ((h k d) (rnrs-hashtable-ref h k d)) ((h k) (hashtable-ref h k #f)))
(define ht-set! hashtable-set!)

(define-syntax-rules ht-create-symbol-q (() (ht-make ht-hash-symbol eq?))
  ((associations ...) (ht-from-list (quote-odd associations ...) eq? ht-hash-symbol)))

(define alist-prepend acons)

(define (fold-multiple f a . custom-state-values)
  (if (null? a) custom-state-values
    (apply fold-multiple f (tail a) (apply f (first a) custom-state-values))))

(define list->alist
  (let (proc (l (a alt prev r) (if alt (list #f #f (alist-prepend prev a r)) (list #t a r))))
    (lambda (lis)
      (if (null? lis) lis
        (let (r (fold-multiple proc (tail lis) #t (first lis) (list)))
          (reverse!
            (if (first r) (pair (list (list-ref r 1)) (first (tail (tail r))))
              (first (tail (tail r))))))))))

(define (alist . key/value) (list->alist key/value))
(define (alist-map proc a) (map (l (a) (proc (first a) (tail a))) a))

(define* (search-load-path path #:optional (load-paths %load-path))
  (any
    (l (base-path)
      (let (full-path (string-append base-path path)) (if (file-exists? full-path) full-path #f)))
    load-paths))

(define (ensure-trailing-slash str) "string -> string"
  (if (or (string-null? str) (not (eqv? #\/ (string-ref str (- (string-length str) 1)))))
    (string-append str "/") str))

(define (any->list a) (if (list? a) a (list a)))

(define (map-last-n n b f)
  (if (null? b) b
    (apply-values (l (last-n rest) (append (reverse rest) (any->list (apply f (reverse last-n)))))
      (split-at (reverse b) n))))

(define (map-segments size f a)
  (fold-segments size (l (result . a) (append result (list (apply f a)))) (list) a))

(define (map-slice slice-length f a)
  (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r (list)))
    (if (null? rest) (reverse (if (null? slice) r (pair (apply f (reverse slice)) r)))
      (if (= slice-length slice-ele-length)
        (loop (tail rest) (list (first rest)) 1 (pair (apply f (reverse slice)) r))
        (loop (tail rest) (pair (first rest) slice) (+ 1 slice-ele-length) r)))))

(define (fold-segments size f init a)
  (let loop ((rest a) (buf (list)) (r init) (count size))
    (if (null? rest) (if (null? buf) r (apply f r buf))
      (if (< count 1) (loop (tail rest) (append (tail buf) (list (first rest))) (apply f r buf) 0)
        (loop (tail rest) (append buf (list (first rest))) r (- count 1))))))

(define (regexp-match-replace a replacements)
  (fold
    (l (a result)
      (fold-matches (first a) result
        result
        (l (match result)
          (if (pair? (tail a))
            (string-replace-string result (match:substring match)
              (string-replace-string (match:substring match) (first (tail a)) (tail (tail a))))
            (string-replace-string result (match:substring match) (tail a))))))
    a replacements))

(define (string-enclose a enclose-str) (string-append enclose-str a enclose-str))
(define (parenthesize a) (string-append "(" a ")"))
(define (parenthesized? a) (string-brackets-enclosed? a #\( #\)))

(define (string-indices a search-string)
  (let ((search-string-length (string-length search-string)) (a-length (string-length a)))
    (let loop ((index (string-contains a search-string)))
      (if index
        (if (= index a-length) (list index)
          (pair index
            (loop (string-contains a search-string (+ index (max 1 search-string-length))))))
        (list)))))

(define (string-brackets-enclosed? a start-char end-char)
  (let (len (string-length a))
    (and (< 1 len) (eq? start-char (string-ref a 0))
      (eq? end-char (string-ref a (- len 1)))
      (let loop ((index 1) (depth 1))
        (cond
          ((= depth 0) (= len index))
          ( (< index len)
            (loop (+ 1 index)
              (cond
                ((eqv? start-char (string-ref a index)) (+ depth 1))
                ((eqv? end-char (string-ref a index)) (- depth 1))
                (else depth))))
          (else #f))))))

(define* (tree-contains? a search-value #:optional (equal? equal?))
  (any (l (a) (or (equal? a search-value) (and (list? a) (tree-contains? a search-value)))) a))

(define (tree-transform-ascend rest leaf-list recurse-descend ascend terminal states)
  (if (null? rest) (apply ascend (reverse leaf-list) states)
    (let (a (first rest))
      (apply
        (l (a . states)
          (tree-transform-ascend (tail rest) (pair a leaf-list)
            recurse-descend ascend terminal states))
        (apply (if (list? a) recurse-descend terminal) a states)))))

(define (tree-transform* a descend ascend terminal . states)
  (letrec
    ( (recurse-descend
        (lambda (a . states)
          (if (and (list? a) (not (null? a)))
            (apply
              (l (r continue? . states)
                (if continue?
                  (if r (apply recurse-descend r states)
                    (tree-transform-ascend a (list) recurse-descend ascend terminal states))
                  (pair r states)))
              (apply descend a recurse-descend states))
            (apply terminal a states)))))
    (apply recurse-descend a states)))

(define (tree-transform a descend ascend terminal)
  (first
    (tree-transform* a (l (a re-descend) (descend a (compose first re-descend)))
      (compose list ascend) (compose list terminal))))

(define (alist->regexp-match-replacements a)
  "automatically converts strings at the prefix position to regular expressions"
  (map (l (e) (pair (if (string? (first e)) (make-regexp (first e)) (first e)) (tail e))) a))

(define* (port->datums port #:optional (get-datum get-datum))
  (let loop ((a (get-datum port))) (if (eof-object? a) (list) (pair a (loop (get-datum port))))))

(define* (file->datums path #:optional (get-datum get-datum))
  (call-with-input-file path (l (port) (port->datums port get-datum))))

(define (any->string a)
  "any -> string
   generalized string conversion function.
   get the string representation for the value of an object.
   symbols like \".\" are converted to \"#{.}\" using display."
  (if (string? a) a
    (if (symbol? a) (symbol->string a) (call-with-output-string (l (port) (display a port))))))

(define (string-replace-string a replace replacement)
  "string string string -> string
   replace all occurences of string \"replace\" inside string \"a\" with string \"replacement\".
   tests have shown it to be up to 28x times faster than regexp-substitute/global (guile2.06 22x, guile3 5.75-28x).
   this procedure is quite nice to debug - comment out one or all string-copy! calls and
   the result string will be a partial result"
  (let (indices (string-indices a replace))
    (if (null? indices) a
      (let
        ( (replace-length (string-length replace)) (replacement-length (string-length replacement))
          (a-length (string-length a)))
        "calculate result string size and create result string"
        (let ((r-length (+ a-length (* (length indices) (- replacement-length replace-length)))))
          (let (r (make-string r-length))
            "each match index, copy characters before match-end to the result string"
            (let loop
              ((r-index 0) (prev-index 0) (cur-index (first indices)) (rest (tail indices)))
              (string-copy! r r-index a prev-index cur-index)
              (let (r-index (- r-index (- prev-index cur-index)))
                (string-copy! r r-index replacement)
                (if (null? rest)
                  (begin
                    (if (< (+ cur-index replace-length) a-length)
                      (string-copy! r (+ r-index replacement-length)
                        a (+ cur-index replace-length) a-length))
                    r)
                  (loop (+ r-index replacement-length) (+ cur-index replace-length)
                    (first rest) (tail rest)))))))))))

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
(define (parenthesize-ensure a) (if (parenthesized? a) a (parenthesize a)))

(define* (es-apply name #:optional a) "string (string ...) -> string"
  (string-append name (if a (parenthesize-ensure (es-comma-join a)) "()")))

(define (es-chain name base a) "string string (string ...) -> string"
  (es-apply (string-append base "." name) a))

(define (es-comma-join a) (string-join a ","))
(define (es-curly-brackets a) (string-append "{" a "}"))
(define (es-declare names) "list -> string" (string-append "var " (es-comma-join names)))

(define* (es-define a) "((key . value) ...) -> string"
  (string-append "var " (es-comma-join (map (l (a) (string-append (first a) "=" (tail a))) a))))

(define (es-for init test update body)
  (string-append "for" (parenthesize (string-append init ";" test ";" update))
    (es-curly-brackets body)))

(define* (es-function #:optional body formals name)
  ( (if name identity parenthesize)
    (string-append "function" (if name (string-append " " name) "")
      (parenthesize (es-comma-join formals)) (es-curly-brackets body))))

(define (es-get a . keys) (string-append a (es-square-brackets (string-join keys "]["))))

(define (es-identifier a)
  (cond
    ((symbol? a) (symbol->string a))
    ((string? a) a)
    (else (raise (list (q es-error-cannot-convert-identifier) a)))))

(define* (es-if-expression test consequent #:optional alternate)
  (parenthesize (string-append test "?" consequent ":" (or alternate "undefined"))))

(define* (es-if test consequent #:optional alternate)
  (string-append "if" (parenthesize-ensure test)
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
  (string-append "switch" (parenthesize-ensure expression)
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
        (string-append "catch" (parenthesize (or-null-string catch-formal))
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
  (parenthesize (string-join (map compile a) (symbol->string prefix))))

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
      (map-segments 2 (l (a b) (parenthesize (string-append a operator b)))
        (map (l (a) (if (contains-set? a) (parenthesize (compile a)) (compile a))) a))
      "&&")))

(define (ses-object a compile) (es-object (list->alist (map compile a))))

(define (ses-translated-infix prefix a compile)
  (parenthesize
    (string-join
      (map
        (l (a) "consider cases like a&&b=c"
          (if (contains-set? a) (parenthesize (compile a)) (compile a)))
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
        (if (and (list? test) (eqv? (q set) (first test))) (parenthesize (compile test))
          (compile test))
        (parenthesize (compile (add-begin body)))))))

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
