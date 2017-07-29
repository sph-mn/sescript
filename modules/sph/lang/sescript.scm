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
      improper-list-split-at-last
      length-one?
      list-replace-last))

  (define sph-lang-sescript-description "a scheme data to ecmascript compiler")

  (define ses-default-load-paths
    (map ensure-trailing-slash
      (if-pass (getenv "SES_LOAD_PATH") (l (a) (string-split a #\:)) (list))))

  (define (add-return-to-define a) "any -> list" (list (list (q begin) a (q (return undefined)))))

  (define (add-return-to-begin a) "list -> list"
    (list (list-replace-last a (l (a-last) (list (add-return-statement (last a)))))))

  (define (add-return-to-set! a) "list -> list"
    (list
      (if (> (length a) 3)
        (list (q begin) (drop-right a 2) (list (q return) (pair (q set!) (take-right a 2))))
        (list (q return) a))))

  (define* (add-return-statement a #:optional compile) "any boolean -> list"
    (if (list? a)
      (list-replace-last a
        (l (a-last)
          ; non-expressions can not be used in a return statement
          (if (and (list? a-last) (not (null? a-last)))
            (case (first a-last) ((begin) (add-return-to-begin a-last))
              ((define) (add-return-to-define a-last))
              ( (if*)
                (let (a-last-tail (tail a-last))
                  (pair (first a-last-tail) (map add-return-statement (tail a-last-tail)))))
              ((return) (list a-last)) ((set!) (add-return-to-set! a-last))
              ((while) (list a-last)) (else (list (list (q return) a-last))))
            (list (list (q return) a-last)))))
      (list (q return) a)))

  (define statement-prefixes (q (define while)))

  (define (contains-statement? a) "list -> boolean"
    (match a
      ( ( (? symbol? prefix) rest ...)
        (or (contains? statement-prefixes prefix) (contains-statement? rest)))
      (_ (and (list? a) (any contains-statement? a)))))

  (define (ses-if a compile)
    (if (contains-return-statement? a) (compile (pair (q if*) a))
      (if (contains-statement? a) (compile (qq ((nullary (unquote (pair (q if*) a))))))
        (parenthesise
          (apply es-if
            (map
              (l (e)
                (match e
                  (((quote begin) body ...) (parenthesise (string-join (map compile body) ",")))
                  (_ (compile e))))
              a))))))

  (define (contains-return-statement? a) "list -> boolean"
    (any
      (l (e)
        (match e (((quote return) _ ...) #t)
          (((quote begin) rest ...) (contains-return-statement? rest)) (_ #f)))
      a))

  (define-syntax-rules ses-function
    ( (compile body formals rest-formal)
      (begin
        (es-function (compile (pair (q begin) (add-return-statement body compile)))
          (map ses-identifier formals) #:rest (ses-identifier rest-formal))))
    ((a ...) (ses-function a ... #f)))

  (define (ses-ref base . keys) "string string ... -> string"
    (string-append base "[" (string-join keys "][") "]"))

  (define (ses-library-name a) "list -> string" (string-join (map ses-identifier a) "."))

  (define (ses-library name exports imports body) "list (list ...) (list ...) list -> list"
    (qq
      (module.define (unquote (ses-library-name name))
        (unquote (pair (q vector) (map ses-library-name imports)))
        (lambda (exports)
          (unquote-splicing
            (append body (list (list (q exports) (list (q quote) (ses-environment exports))))))))))

  (define-as identical-infix-token list "+" "-" "<" ">" "<=" ">=" "*" "/" "%")

  (define-as translated-infix-token list
    "eqv_p" "string_append" "=" "or" "and" "string_equal_p" "equal_p" "eq_p" "string=" "modulo")

  (define (translate-infix-token a)
    (string-case a (("=" "eqv_p" "eq_p" "string_equal_p" "string=") "==")
      ("string_append" "+") ("and" "&&")
      ("or" "||") ("equal_p" "===") ("modulo" "%") (else (raise (q fail-translate-infix-token)))))

  (define-syntax-rule (add-begin-if-multiple a) (if (length-one? a) (first a) (pair (q begin) a)))

  (define (ses-case a)
    (match a
      ( (expr cond ...)
        (quasiquote
          ( (lambda (___v)
              (cond
                (unquote-splicing
                  (map
                    (l (e)
                      (if (equal? (q else) (first e)) e
                        (pair
                          (pair (q or)
                            (map (l (a) (list (q equal?) (q ___v) a)) (any->list-s (first e))))
                          (tail e))))
                    cond))))
            (unquote expr))))))

  (define (ascend-expr->ecmascript a)
    "list/any -> string/any
     called when ascending the tree and the arguments have already been processed"
    (string-case (first a) ("set_x" (apply es-set-nc! (tail a)))
      ("chain" (apply es-chain (tail a))) ("begin" (string-join (tail a) ";"))
      ("define" (apply es-define-nc (tail a)))
      ("not" (string-append "!" (apply string-append (tail a))))
      ("object" (es-object-nc (list->alist (tail a)))) ("ref" (apply ses-ref (tail a)))
      (identical-infix-token (parenthesise (string-join (tail a) (first a))))
      (translated-infix-token
        (parenthesise (string-append (string-join (tail a) (translate-infix-token (first a))))))
      ("#t" "true") ("#f" "false")
      (("array" "vector" "list") (es-vector-nc (tail a)))
      (("length") (string-append (apply string-append (tail a)) ".length"))
      ("new" (string-append "new " (ses-apply (first (tail a)) (tail (tail a)))))
      ("environment" (ses-environment (tail a)))
      ("return" (if (null? (tail a)) "return" (ses-apply (first a) (tail a))))
      ("declare" (apply es-declare-nc (tail a)))
      ; throw cannot occur in an if-expression as is, example of this: 1?2:throw(3). but if wrapped in a function it can
      ("throw" (es-apply-nc (es-function-nc (string-append "throw(" (ses-value (tail a)) ")"))))
      (else (if (list? a) (ses-apply (first a) (tail a)) a))))

  (define (descend-expr->sescript a compile load-paths)
    ;this is applied when descending the tree, and the result will be parsed again
    (case (first a) ((first) (qq (ref (unquote-splicing (tail a)) 0)))
      ((pair) (qq (chain unshift (unquote (list-ref a 2)) (unquote (list-ref a 1)))))
      ((tail) (qq (chain slice (unquote (list-ref a 1)) 1)))
      ( (let)
        (match (tail a)
          ( ( ( (formals vals) ...) body ...)
            (qq ((lambda (unquote formals) (unquote-splicing body)) (unquote-splicing vals))))
          ( ( (formal val) body ...)
            (qq ((lambda ((unquote formal)) (unquote-splicing body)) (unquote val))))
          ( (name ((formals vals) ...) body ...)
            (qq
              ( (lambda ((unquote name))
                  (set! (unquote name) (lambda (unquote formals) (unquote-splicing body)))
                  ((unquote name) (unquote-splicing vals))))))
          (_ (throw (q syntax-error-for-let)))))
      ( (define)
        (match (tail a)
          (((name formals ...) . body) (list (q define) name (pairs (q lambda) formals body))) (_ #f)))
      ( (include-sjs)
        (pair (q begin)
          (file->datums (search-load-path (string-append (first (tail a)) ".sjs") load-paths) read)))
      ((map) (match (tail a) ((proc lis) (qq (chain map (unquote lis) (unquote proc))))))
      ((each) (match (tail a) ((proc lis) (qq (chain forEach (unquote lis) (unquote proc))))))
      ( (string-join)
        (match (tail a)
          ((compound combinator) (qq (chain join (unquote compound) (unquote combinator))))))
      ( (fold)
        (match (tail a)
          ( (proc init lis)
            (qq
              (chain reduce (unquote lis)
                (let (___proc (unquote proc))
                  (lambda (prev e index arr) (___proc e prev index arr)))
                (unquote init))))))
      ((append) (qq (chain concat (unquote (first (tail a))) (unquote-splicing (tail (tail a))))))
      ( (let*)
        (match (tail a)
          ( ( ( (formals vals) ...) body ...)
            (qq
              ( (lambda ((unquote (first formals)))
                  (unquote-splicing
                    (append
                      (map (l name+value (pair (q define) name+value)) (tail formals) (tail vals))
                      body)))
                (unquote (first vals)))))
          (_ (throw (q syntax-error-for-let*)))))
      ( (apply)
        (match (tail a)
          ( (proc args ... last-a)
            (qq
              (chain apply (unquote proc)
                this
                (unquote (if (null? args) last-a (list (q append) (pair (q vector) args) last-a))))))
          (_ (throw (q syntax-error-for-apply)))))
      ( (cond)
        (or
          (let (cond (reverse (tail a)))
            (fold
              (l (cond alternate)
                (list (q if) (first cond) (add-begin-if-multiple (tail cond)) alternate))
              (match (first cond)
                (((quote else) consequent ...) (add-begin-if-multiple consequent))
                ((test consequent ...) (list (q if) test (add-begin-if-multiple consequent))))
              (tail cond)))
          (q false)))
      ((case) (ses-case (tail a))) ((nullary) (qq (lambda () (unquote-splicing (tail a)))))
      ( (library)
        (match (tail a)
          ( ( (name ...) ((quote export) . exports) ((quote import) . imports) . body)
            (ses-library name exports imports body))
          (((name ...) ((quote export) . exports) . body) (ses-library name exports (list) body))
          (_ (throw (q syntax-error-for-library-form)))))
      ( (with-libraries)
        (match (tail a)
          ( (imports body ...)
            (qq
              (module
                (unquote
                  (pair (q vector) (map (l (e) (string-join (map symbol->string e) ".")) imports)))
                (lambda () (unquote-splicing (append body (list (q undefined))))))))
          (_ (throw (q syntax-error-for-with-libraries-form)))))
      (else #f)))

  (define (descend-expr->ecmascript a compile)
    ;this is applied when descending the tree, and the result will not be parsed again.
    ;this is for expressions that create syntax that can not be created with other ses syntax
    (case (first a) ((if) (ses-if (tail a) compile))
      ( (lambda l)
        (let ((formals (first (tail a))) (body (tail (tail a))))
          (match formals ((or (formal ...) (? symbol? formal)) (ses-function compile body formal))
            ( (formal . rest)
              (let (formals+rest-formal (improper-list-split-at-last rest))
                (ses-function compile body
                  (pair formal (first formals+rest-formal)) (tail formals+rest-formal))))
            (_ (q syntax-error-for-lambda-formals)))))
      ((make-regexp) (apply es-regexp-nc (tail a)))
      ( (guard)
        (match (tail a)
          ( (cond body ...)
            (es-apply-nc
              (es-function-nc
                (es-try-catch-finally-nc (compile (pair (q begin) (add-return-statement body)))
                  (ses-value (first cond)) (compile (list (q return) (pair (q cond) (tail cond))))))))
          (_ (throw (q syntax-error-for-guard)))))
      ( (while)
        (match (tail a)
          ( (test body ...)
            (es-statement-nc "while" (compile (pair (q begin) body))
              (if (and (list? test) (eqv? (q set) (first test))) (parenthesise (compile test))
                (compile test))))))
      ((quote) (list-ref a 1)) ((if*) (apply es-if-statement (map compile (tail a)))) (else #f)))

  (define (descend-proc load-paths)
    (l (a compile) "list procedure -> (match-result is-parsed-again)"
      (let (r (descend-expr->sescript a compile load-paths))
        (if r (list r #t)
          (let (r (descend-expr->ecmascript a compile)) (if r (list r #f) (list #f #t)))))))

  (define* (sescript->ecmascript exprs port #:optional (load-paths ses-default-load-paths))
    "(expression ...) port ->"
    (each (l (e) (display (sescript->ecmascript-string e load-paths) port) (display ";" port))
      exprs))

  (define* (sescript->ecmascript-string expr #:optional (load-paths ses-default-load-paths))
    "expression -> string"
    (tree-transform expr (descend-proc load-paths) ascend-expr->ecmascript ses-value))

  (define (sescript-use-strict port)
    "port -> string
     writes a \"use strict\"; command to port to the following
     code to be interpreted in the so called strict-mode.
     this can appear multiple times in the output without being an error"
    (display "\"use strict\";\n" port)))
