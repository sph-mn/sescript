(import
  (rnrs base)
  (sph)
  (sph test)
  (sph lang sescript))

(define (test-sescript->ecmascript inp)
  (call-with-output-string
    (l (port)
      (sescript->ecmascript (list inp) port))))

(execute-tests-quasiquote
  (sescript->ecmascript
    (lambda (a) 1) "(function(a){return(1)});"
    ((lambda (a) 1)) "(function(a){return(1)})();"
    (let (a 1) 2) "(function(a){return(2)})(1);"
    (let ((a 1) (b 2)) #t) "(function(a,b){return(true)})(1,2);"
    (let* ((a 1) (b 2) (c 3)) 4) "(function(a){var b=2;var c=3;return(4)})(1);"
    (if a (if b c d) e) "(a?(b?c:d):e);"
    (if a (if (if b c) d) e) "(a?((b?c:undefined)?d:undefined):e);"
    (define a 1) "var a=1;"
    (define a (lambda (a b) 1 2 3)) "var a=(function(a,b){1;2;return(3)});"
    (lambda (a b . c) c)
    (unquote
      (string-append
        "(function(a,b){var c=new Array(arguments.length);for(var ___i=2;"
        "___i<arguments.length;___i+=1)"
        "{c.push(arguments[___i])}return(c)});"))
    (set! a 1) "a=1;"
    (ref a 1) "a[1];"
    (ref a 1 2 3) "a[1][2][3];"
    (ref (ref a 1) 2) "a[1][2];"
    ;cons wont work as expected with internet explorer <= 8,
    ;  because there unshift returns "undefined"
    (pair 1 (list))
    "[].unshift(1);"
    (first (list))
    "[][0];"
    (append (list 1 2) (list 3 4))
    "[1,2].concat([3,4]);"
    (append a b)
    "a.concat(b);"
    (tail (list 1 2))
    "[1,2].slice(1);"
    #\a "\"a\";"
    (vector 1 2 3) "[1,2,3];"
    (list 1 2) "[1,2];"
    (assoc a 2 b 3) "{\"a\":2,\"b\":3};"
    (environment a b c) "{\"a\":a,\"b\":b,\"c\":c};"
    (chain c (chain a b) 1 2 "d") "b.a().c(1,2,\"d\");"
    "\"" "\"\\\"\";"
    (assoc) "{};"
    (new Obj 3 5) "new Obj(3,5);"
    (not 1) "!1;"
    (if (not 1) a b) "(!1?a:b);"
    (and 1 2 3) "(1&&2&&3);"
    (= 1 2 3) "(1==2==3);"
    (equal? a 1) "(a===1);"
    #t "true;"
    ;(sph test) cannot handle #f - sescript can, tested manually
    ;#f "false"
    (begin 1 (begin 2 3)) "1;2;3;"
    (cond
      ((= a 1) (equal? b 2))
      ((equal? c 3) #t))
    "((a==1)?(b===2):((c===3)?true:undefined));"
    (cond
      ((= a 1) (equal? b 2))
      ((equal? c 3) #f #t)
      (else #t #f))
    "((a==1)?(b===2):((c===3)?(false,true):(true,false)));"
    (case (+ 1 2) (2 #f) (3 #t))
    "(function(___v){return(((___v==2)?false:((___v==3)?true:undefined)))})((1+2));"
    (case (+ 1 2) (2 #t #f) (3 #t) (else 4 5))
    "(function(___v){return(((___v==2)?(true,false):((___v==3)?true:(4,5))))})((1+2));"
    (guard (exc (else #f)) 1)
    "(function(){try{return(1)}catch(exc){return(false)}})();"
    (guard (exc (#t 1) (test 2)) 3 4)
    "(function(){try{3;return(4)}catch(exc){return((true?1:(test?2:undefined)))}})();"
    (guard (exc (#t 1) (test 2) (else 5)) 3 4)
    "(function(){try{3;return(4)}catch(exc){return((true?1:(test?2:5)))}})();"
    (library (a b c) (export d e) (import (f) (g)) 1 2 3)
    "module.define(\"a.b.c\",[\"f\",\"g\"],(function(exports){1;2;3;return(exports({\"d\":d,\"e\":e}))}));"
    (library (a b c) (export d e) 1 2 3)
    "module.define(\"a.b.c\",[],(function(exports){1;2;3;return(exports({\"d\":d,\"e\":e}))}));"
    (with-libraries ((a b c) (d e) (f g)) 1 2 3)
    "module([\"a.b.c\",\"d.e\",\"f.g\"],(function(){1;2;3;return(undefined)}));"
    (map (lambda (a) #t) (vector 1 2 3))
    "[1,2,3].map((function(a){return(true)}));"
    (fold f (vector) (vector))
    "[].reduce((function(___proc){return((function(prev,ele,index,arr){return(___proc(ele,prev,index,arr))}))})(f),[]);"
    (each (lambda (ele) ele) (vector))
    "[].forEach((function(ele){return(ele)}));"
    (string-join (vector "a" "b") "/")
    "[\"a\",\"b\"].join(\"/\");"
    (make-regexp "[^a-b0-9]" "g")
    "/[^a-b0-9]/g;"
    (length (list 1))
    "[1].length;"
    (string-append "a" "b" "c")
    "(\"a\"+\"b\"+\"c\");"
    (quote "var a = 3")
    "var a = 3;"
    (let next ((a 1) (b 2))
      (next (+ a 2) (+ b 1)))
    "(function(next){next=(function(a,b){return(next((a+2),(b+1)))});return(next(1,2))})();"
    (apply a 1 2 (list 3 4))
    "a.apply(this,[1,2].concat([3,4]));"
    ;a return keyword may not have an empty argument list appended
    (return) "return;"
    (return 1 2) "return(1,2);"
    (if 1 (lambda () #t) 2)
    "(1?(function(){return(true)}):2);"
    ;testing "add-return-statement"
    (l () (define a 3))
    "(function(){var a=3;return(undefined)});"
    (l () (begin 1 2))
    "(function(){1;return(2)});"
    (set! a 1 b 2 c 3)
    "a=1;b=2;c=3;"
    (l () (set! a 1 b 2 c 3))
    "(function(){a=1;b=2;return(c=3)});"
    (l () (set! a 1))
    "(function(){return(a=1)});"
    (throw "test")
    "(function(){throw([\"\\\"test\\\"\"])})();"
))