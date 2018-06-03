(library (sph lang ecmascript expressions)
  (export
    es-apply
    es-array
    es-chain
    es-comma-join
    es-curly-brackets
    es-declare
    es-define
    es-escape-single-char
    es-for
    es-function
    es-get
    es-identifier
    es-if
    es-if-expression
    es-new
    es-object
    es-regexp
    es-set
    es-square-brackets
    es-string
    es-switch
    es-try-catch-finally
    es-value
    sph-lang-ecmascript-expressions-description)
  (import
    (sph)
    (sph list)
    (only (guile) raise string-join)
    (only (sph alist) alist alist-map)
    (only (sph string)
      any->string
      parenthesise
      parenthesised?
      string-enclose
      string-replace-string))

  (define sph-lang-ecmascript-expressions-description
    "create ecmascript syntax strings.
     procedures should only receive and return strings")

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
      (fold (l (a result) (string-replace-string result (first a) (tail a))) a
        es-escape-single-char)
      "\""))

  (define (es-switch value cases) "string ((test/symbol:else consequent ...) ...) -> string"
    (string-append "switch" (parenthesise-ensure value)
      (es-curly-brackets
        (string-join
          (map
            (l (a)
              (let (test (first a))
                (if (eq? (q else) test)
                  (string-append "default:" (string-join (append (tail a) (list "break")) ";"))
                  (string-append
                    (apply string-append
                      (map (l (a) (string-append "case " a ":")) (any->list test)))
                    (string-join (append (tail a) (list "break")) ";")))))
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

  (define (es-array contents) (es-square-brackets (es-comma-join contents))))
