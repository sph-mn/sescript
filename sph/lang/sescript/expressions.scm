(library (sph lang sescript expressions)
  (export
    ses-apply
    ses-environment
    ses-identifier
    ses-value
    translate-identifier)
  (import
    (ice-9 match)
    (rnrs base)
    (sph)
    (sph lang ecmascript expressions)
    (only (guile) make-regexp string-join)
    (only (sph alist) alist)
    (only (sph one) regexp-replace)
    (only (sph string) string-equal?))

  (define (ses-apply proc args) "symbol/string/any list -> string"
    (es-apply-nc (ses-identifier proc) (string-join (map ses-identifier args) ",")))

  (define identifier-replacements
    (map (l (e) (pair (make-regexp (first e)) (tail e)))
      (alist "->" "_to_" "-" "_" "\\?$" "_p" "!$" "_x")))

  (define (ses-environment a)
    (es-object-nc
      (map
        (l (e)
          (if (list? e) (pair (ses-identifier (first e)) (ses-identifier (first (tail e))))
            (pair (ses-identifier e) (ses-identifier e))))
        a)))

  (define (ses-identifier a) "symbol/any -> string/any"
    (if (symbol? a) (translate-identifier (symbol->string a)) a))

  (define (ses-value a) "any -> string"
    (if (symbol? a) (translate-identifier (symbol->string a)) (es-value a)))

  (define (translate-identifier a) "string -> string"
    (if (string-equal? "-" a) a
      (fold (l (e r) (regexp-replace r (first e) (tail e))) a identifier-replacements))))