(import
  (ice-9 pretty-print)
  (sph lang sescript)
  (sph)
  (sph test)
  (sph two))

(define data-1
  (compile
    (list (q quasiquote)
      (read-file
        (string-append (dirname (current-filename)) "/data-secmascript.sec")))))

;(pretty-print data-1)
;(display "---------------------------------------------\n" )

(define-test (secmascript->ecmascript inp eout name)
  (display (apply secmascript->ecmascript inp))
  #t)

(execute-tests-quasiquote
  (secmascript->ecmascript ((unquote data-1)) #t))
