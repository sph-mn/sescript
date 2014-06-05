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
    (only (sph one) regexp-replace))

  (define (ses-apply proc args)
    (es-apply-nc (ses-identifier proc) (string-join (map ses-identifier args) ",")))

  (define identifier-replacements
    (map (l (ele) (cons (make-regexp (first ele)) (tail ele)))
      (alist "->" "_to_" "-" "_" "\\?$" "_p" "!$" "_x")))

  (define (ses-environment arg)
    (es-object-nc
      (map
        (l (ele)
          (if (list? ele) (pair (ses-identifier (first ele)) (ses-identifier (first (tail ele))))
            (pair (ses-identifier ele) (ses-identifier ele))))
        arg)))

  (define (ses-identifier arg) (if (symbol? arg) (translate-identifier (symbol->string arg)) arg))

  (define (ses-value arg)
    (if (symbol? arg) (translate-identifier (symbol->string arg)) (es-value arg)))

  (define (translate-identifier arg)
    (fold (l (ele prev) (regexp-replace prev (first ele) (tail ele))) arg identifier-replacements)))