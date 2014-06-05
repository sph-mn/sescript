;a scheme-data to ecmascript compiler
(library (sph lang sescript)
  (export sescript->ecmascript sescript->string sescript-use-strict)
  (import
    (rnrs base)
    (except (srfi srfi-1) map)
    (guile)
    (ice-9 match)
    (only (sph alist) list->alist)
    (only (sph list) any->list map-slice contains? improper-list-split-at-last length-eq-one? list-replace-last)
    (sph)
    (sph string)
    (only (sph tree) tree-transform)
    (sph lang ecmascript expressions)
    (sph lang sescript expressions))

  (define (add-return-to-if arg)
    (list (pairs (q if) (first (tail arg)) (map add-return-statement (tail (tail arg))))))

  (define (add-return-to-define arg) (list (list (q begin) arg (q (return undefined)))))

  (define (add-return-to-begin arg)
    (list (list-replace-last arg
      (l (arg-last)
        (list (add-return-statement (last arg)))))))

  (define (add-return-to-set! arg)
    (if (> (length arg) 3)
      (list (list (q begin) (drop-right arg 2) (list (q return) (pair (q set!) (take-right arg 2)))))
      (list (list (q return) arg))))

  (define* (add-return-statement arg #:optional compile)
    (if (list? arg)
      (list-replace-last arg
        (l (arg-last)
          ;non-expressions can not be used in a return statement. the following catches only a few cases
          (if (and (list? arg-last) (not (null? arg-last)))
            (case (first arg-last)
              ((begin) (add-return-to-begin arg-last))
              ((define) (add-return-to-define arg-last))
              ((return) (list arg-last))
              ((set!) (add-return-to-set! arg-last))
              (else (list (list (q return) arg-last))))
            (list (list (q return) arg-last)))))
      (list (q return) arg)))

  (define-syntax-rules ses-function
    ((compile body formals rest-formal)
      (begin
        (es-function (compile (pair (q begin) (add-return-statement body compile)))
        (map ses-identifier formals)
        #:rest (ses-identifier rest-formal)))
      )
    ((args ...) (ses-function args ... #f)))

  (define (ses-ref base . keys)
    (string-append base "[" (string-join keys "][") "]"))

  (define (ses-library-name arg)
    (string-join (map ses-identifier arg) "."))

  (define (ses-library name exports imports body)
    (qq
      (module.define (unquote (ses-library-name name)) (unquote (cons (q vector) (map ses-library-name imports)))
        (lambda (exports)
          (unquote-splicing
            (append body
              (list (list (q exports) (list (q quote) (ses-environment exports))))))))))

  (define identical-infix-token (q ("+" "-" "<" ">" "<=" ">=" "*" "/")))
  (define translated-infix-token (q (
        "eqv_p"
        "string_append"
        "="
        "or"
        "and"
        "string_equal_p"
        "equal_p"
        "eq_p"
        "string=")))

  (define (translate-infix-token arg)
    (string-case arg
      (("=" "eqv_p" "eq_p" "string_equal_p" "string=") "==")
      ("string_append" "+")
      ("and" "&&")
      ("or" "||")
      ("equal_p" "===")
      ("modulo" "%")
      (raise (q cannot-convert-symbol-to-ecmascript))))

  (define (ascend-expr->ecmascript arg)
    ;these are applied when ascending the tree
    (string-case (first arg)
      ("set_x" (apply es-set-nc! (tail arg)))
      ("chain" (apply es-chain (tail arg)))
      ("begin" (string-join (tail arg) ";"))
      ("ref" (apply ses-ref (tail arg)))
      ("define" (apply es-define-nc (tail arg)))
      ("not" (string-append "!" (apply string-append (tail arg))))
      ("#t" "true")
      ("#f" "false")
      (("vector" "list") (es-vector-nc (tail arg)))
      (identical-infix-token (parenthesise (string-join (tail arg) (first arg))))
      (translated-infix-token
        (parenthesise (string-append (string-join (tail arg) (translate-infix-token (first arg))))))
      ("assoc" (es-object-nc (list->alist (tail arg))))
      (("length")
        (string-append (apply string-append (tail arg)) ".length"))
      ("new" (string-append "new " (ses-apply (first (tail arg)) (tail (tail arg)))))
      ("environment" (ses-environment (tail arg)))
      ("return" (if (null? (tail arg)) "return" (ses-apply (first arg) (tail arg))))
      ;throw cannot occur in an if-expression as is, example: 1?2:throw(3)
      ("throw" (es-apply-nc (es-function-nc (string-append "throw(" (ses-value (tail arg)) ")"))))
      (if (list? arg) (ses-apply (first arg) (tail arg)) arg)))

  (define-syntax-rule (add-begin-if-multiple arg)
    (if (length-eq-one? arg) (first arg) (pair (q begin) arg)))

  (define (descend-expr->sescript arg compile)
    ;these are applied when descending the tree, and the result is parsed again
    (case (first arg)
      ((first)
        (qq (ref (unquote-splicing (tail arg)) 0)))
      ((pair)
        (qq (chain unshift (unquote (list-ref arg 2)) (unquote (list-ref arg 1)))))
      ((tail)
        (qq (chain slice (unquote (list-ref arg 1)) 1)))
      ((let)
        (match (tail arg)
          ((((formals vals) ...) body ...)
            (qq ((lambda (unquote formals) (unquote-splicing body)) (unquote-splicing vals))))
          (((formal val) body ...)
            (qq ((lambda ((unquote formal)) (unquote-splicing body)) (unquote val))))
          ((name ((formals vals) ...) body ...)
            (qq
              ((lambda ((unquote name))
                  (set! (unquote name) (lambda (unquote formals) (unquote-splicing body)))
                  ((unquote name) (unquote-splicing vals))))))
          (_ (raise (q syntax-error-for-let)))))
      ((define)
        (match (tail arg)
          (((name formals ...) . body)
            (list (q define) name (pairs (q lambda) formals body)))
          (_ #f)))
      #;((set!)
        (let (arguments (tail arg))
          (if (> (length arguments) 2)
            (pair (q begin)
              (reverse (map-slice (l (name value) (list (q set!) name value)) 2 arguments)))
            #f)))
      ((map)
        (match (tail arg)
          ((proc lis) (qq (chain map (unquote lis) (unquote proc))))))
      ((each)
        (match (tail arg)
          ((proc lis) (qq (chain forEach (unquote lis) (unquote proc))))))
      ((string-join)
        (match (tail arg)
          ((compound combinator)
            (qq (chain join (unquote compound) (unquote combinator))))))
      ((fold)
        (match (tail arg)
          ((proc init lis)
            (qq
              (chain reduce (unquote lis)
                (let (___proc (unquote proc))
                  (lambda (prev ele index arr) (___proc ele prev index arr)))
                (unquote init))))))
      ((append)
        (qq (chain concat (unquote (first (tail arg))) (unquote-splicing (tail (tail arg))))))
      ((let*)
        (match (tail arg)
          ((((formals vals) ...) body ...)
            (qq
              ((lambda ((unquote (first formals)))
                  (unquote-splicing
                    (append
                      (map (l name+value (pair (q define) name+value))
                        (tail formals) (tail vals))
                      body)))
                (unquote (first vals)))))
          (_ (raise (q syntax-error-for-let*)))))
      ((apply)
        (match (tail arg)
          ((proc args ... last-arg)
            (qq
              (chain apply (unquote proc) this
                (unquote
                  (if (null? args) last-arg
                    (list (q append)
                      (cons (q vector) args)
                      last-arg))))))
          (_ (raise (q syntax-error-for-apply)))))
      ((cond)
        (let (cond (reverse (tail arg)))
          (fold
            (l (cond alternate)
              (list (q if) (first cond) (add-begin-if-multiple (tail cond)) alternate))
            (match (first cond)
              (((quote else) body ...) (add-begin-if-multiple body))
              ((test consequent ...)
                (list (q if) test (add-begin-if-multiple consequent))))
            (tail cond))))
      ((case)
        (match (tail arg)
          ((expr cond ...)
            (quasiquote
              ((lambda (___v)
                  (cond
                    (unquote-splicing
                      (map
                        (l (ele)
                          (if (eq? (q else) (first ele)) ele
                            (cons (list (q eqv?) (q ___v) (first ele)) (tail ele))))
                        cond))))
                (unquote expr))))))
      ((library)
        (match (tail arg)
          (((name ...) ((quote export) . exports) ((quote import) . imports) . body)
            (ses-library name exports imports body))
          (((name ...) ((quote export) . exports) . body)
            (ses-library name exports (list) body))
          (_ (raise (q syntax-error-for-library-form)))))
      ((with-libraries)
        (match (tail arg)
          ((imports body ...)
            (qq
              (module
                (unquote (cons (q vector)
                    (map (l (ele) (string-join (map symbol->string ele) ".")) imports)))
                (lambda () (unquote-splicing (append! body (list (q undefined))))))))
          (_ (raise (q syntax-error-for-with-libraries-form)))))
      (else #f)))

  (define (contains-return-statement? arg)
    (any
      (l (ele)
        (match ele
          (((quote return) _ ...) #t)
          (((quote begin) rest ...) (contains-return-statement? rest))
          (_ #f)))
      arg))

  (define (descend-expr->ecmascript arg compile)
    ;these are applied when descending the tree, and the result is not parsed again.
    ;this is for expressions that create syntax that can not be created with ses or javascript syntax
    (case (first arg)
      ;((if) (apply es-if-statement (map compile (tail arg))))
      ((if)
        (parenthesise (apply es-if
          (map
            (l (ele)
              (match ele
                (((quote begin) body ...)
                  (parenthesise (string-join (map compile body) ",")))
                (_ (compile ele))))
            (tail arg)))))
      ((lambda l)
        (let ((formals (first (tail arg))) (body (tail (tail arg))))
          (match formals
            ((or (formal ...) (? symbol? formal)) (ses-function compile body formal))
            ((formal . rest)
              (let (formals+rest-formal (improper-list-split-at-last rest))
                (ses-function compile body
                  (pair formal (first formals+rest-formal))
                  (tail formals+rest-formal))))
            (_ (q syntax-error-for-lambda-formals)))))
      ((make-regexp) (apply es-regexp-nc (tail arg)))
      ((guard)
        (match (tail arg)
          ((cond body ...)
            (es-apply-nc
              (es-function-nc
                (es-try-catch-finally-nc
                  (compile (cons (q begin) (add-return-statement body)))
                  (ses-value (first cond))
                  (compile
                    (list (q return)
                      (cons (q cond)
                        (map
                          (l (ele)
                            (if (eq? (q else) (first ele)) ele
                              (cons (list (q eqv?) (first cond) (first ele)) (tail ele))))
                          (tail cond)))))))))
          (_ (raise (q syntax-error-for-guard)))))
      ((while)
        (match (tail arg)
          ((test body ...)
            (es-statement-nc "while"
              (compile (cons (q begin) body))
              (if (and (list? test) (eqv? (q set) (first test)))
                (parenthesise (compile test))
                (compile test))))))
      ((quote) (list-ref arg 1))
      (else #f)))

  (define (descend arg compile)
    (let (res (descend-expr->sescript arg compile))
      (if res (list res #t)
        (let (res (descend-expr->ecmascript arg compile))
          (if res (list res #f) (list #f #t))))))

  (define (sescript->ecmascript exprs port)
    "(expression ...) port ->"
    (each (l (ele) (display (sescript->ecmascript-string ele) port) (display ";" port)) exprs))

  (define (sescript->ecmascript-string expr)
    "expression -> string"
    (tree-transform expr descend ascend-expr->ecmascript ses-value))

  (define (sescript-use-strict port)
    "writes a \"use strict\"; command to port to set the following
    code to be interpreted in the so called strict-mode.
    this can appear multiple times in the output"
    (display "\"use strict\";" port)))
