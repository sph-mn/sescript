(library (sph lang ecmascript expressions)
  (export
    es-apply
    es-chain
    es-comma-join
    es-curly-brackets
    es-declare
    es-define
    es-escape-single-char
    es-for
    es-function
    es-identifier
    es-if
    es-if-expression
    es-new
    es-object
    es-ref
    es-regexp
    es-set
    es-square-brackets
    es-string
    es-try-catch-finally
    es-value
    es-vector
    sph-lang-ecmascript-expressions-description)
  (import
    (sph)
    (only (guile) raise string-join)
    (only (sph alist)
      alist
      alist-map
      list-alist?)
    (only (sph list) map-slice)
    (only (sph string)
      any->string
      parenthesise
      string-enclose
      string-replace-string))

  (define sph-lang-ecmascript-expressions-description
    "create ecmascript syntax strings.
     procedures should only receive and return strings")

  (define es-escape-single-char
    (alist "\"" "\\\"" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v"))

  (define-syntax-rule (or-null-string a ...) (or a ... ""))
  (define* (es-apply name #:optional a) (string-append name (parenthesise (or-null-string a))))
  (define (es-chain name base a) (es-apply (string-append base "." name) a))
  (define (es-comma-join a) (string-join a ","))
  (define (es-curly-brackets a) (string-append "{" a "}"))
  (define (es-declare names) "list -> string" (string-append "var " (es-comma-join names)))

  (define* (es-define a) "(name value name/value ...) -> string"
    (string-append "var " (es-comma-join (map-slice 2 (l (a b) (string-append a "=" b)) a))))

  (define (es-for init test update body)
    (string-append "for" (parenthesise (string-append init ";" test ";" update))
      (es-curly-brackets body)))

  (define* (es-function #:optional body formals name)
    (let
      (result (string-append "function " name " " (es-comma-join formals) (es-curly-brackets body)))
      (if name result (parenthesise result))))

  (define (es-identifier a)
    (cond
      ((symbol? a) (symbol->string a))
      ((string? a) a)
      (else (raise (list (q es-error-cannot-convert-identifier) a)))))

  (define* (es-if-expression test consequent #:optional alternate)
    (string-append test "?" consequent ":" (or alternate "undefined")))

  (define* (es-if test consequent #:optional alternate)
    (string-append "if" (parenthesise test)
      (es-curly-brackets consequent)
      (or-null-string (and alternate (string-append "else" (es-curly-brackets alternate))))))

  (define (es-object key/value) "((key . value) ...) -> string"
    (es-curly-brackets (es-comma-join (alist-map (l (a b) (string-append a ":" b)) key/value))))

  (define (es-new name a) (string-append "new " (es-apply name a)))
  (define (es-square-brackets a) (string-append "[" a "]"))
  (define (es-ref a key) (string-append a (es-square-brackets key)))

  (define* (es-regexp pattern #:optional modifiers)
    (string-append "/" pattern "/" (or-null-string modifiers)))

  (define (es-set a) "(name value name/value ...) -> string"
    (string-join (map-slice 2 (l (name value) (string-append name "=" value)) a) ";"))

  (define (es-string a)
    (string-enclose
      (fold (l (a result) (string-replace-string result (first a) (tail a))) a
        es-escape-single-char)
      "\""))

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
      ((vector? a) (es-vector (vector->list a)))
      ((boolean? a) (if a "true" "false"))
      ((list? a) (if (list-alist? a) (es-object a) (es-vector a)))
      ((pair? a) (es-vector (list (first a) (tail a))))
      ((char? a) (string-enclose (any->string a) "\""))
      (else (raise (q es-error-cannot-convert-value)))))

  (define (es-vector contents) (es-square-brackets (es-comma-join contents))))
