(define-test-module (test module sph lang sescript)
  (import
    (sph list)
    (sph lang sescript))

  (define-test (sescript->ecmascript input)
    (call-with-output-string (l (port) (sescript->ecmascript (list input) port))))

  (test-execute-procedures-lambda
    (sescript->ecmascript
      (case (+ 1 2)
        (2 #t #f)
        (3 #t)
        (else 4 5))
      "switch(1+2){case 2:true;false;break;case 3:true;break;default:4;5;break};"
      (= 1 2 3) "(1===2)&&(2===3);"
      (and 1 2 3) "(1&&2&&3);"
      (array 1 2 3) "[1,2,3];"
      (begin 1 (begin 2 3)) "1;2;3;"
      (chain c (chain a b) 1 2 "d") "b.a().c(1,2,\"d\");"
      (cond
        ((= a 1) (= b 2))
        ((= c 3) #t))
      "if(a===1){(b===2)}else if(c===3){true};"
      (cond
        ((= a 1) (= b 2))
        ((= c 3) #f #t)
        (else #t #f))
      "if(a===1){(b===2)}else if(c===3){false;true}else{true;false};"
      (declare a) "var a;"
      (declare a b c) "var a,b,c;"
      (define a 1) "var a=1;"
      (define a 1 b 2) "var a=1,b=2;"
      (define a (lambda (a b) 1 2 3)) "var a=(function(a,b){1;2;return(3)});"
      (get a 1) "a[1];"
      (get a 1 2 3) "a[1][2][3];"
      (get (get a 1) 2) "a[1][2];"
      (if* a (if* b c d) e) "(a?(b?c:d):e);"
      (if* a (if* (if* b c) d) e) "(a?((b?c:undefined)?d:undefined):e);"
      (lambda (a) 1) "(function(a){return(1)});"
      ((lambda (a) 1)) "(function(a){return(1)})();"
      (let (a 1) 2) "(function(a){return(2)})(1);"
      (let ((a 1) (b 2)) #t) "(function(a,b){return(true)})(1,2);"
      (let* ((a 1) (b 2) (c 3)) 4) "(function(a){var b=2;var c=3;return(4)})(1);"
      (let loop ((a 1) (b 2)) (loop (+ a 2) (+ b 1)))
      "(function(loop){loop=(function(a,b){return(loop((a+2),(b+1)))});return(loop(1,2))})();"
      (object a 2 b-c 3 "d" 4) "{a:2,b_c:3,\"d\":4};"
      (environment a b c) "{a:a,b:b,c:c};"
      (new obj 3 5) "new obj(3,5);"
      (not 1) "!1;"
      (object) "{};"
      (if* (not 1) a b) "(!1?a:b);"
      (make-regexp "[^a-b0-9]" "g") "/[^a-b0-9]/g;"
      ; a return keyword may not have an empty argument list appended
      (return) "return;"
      (return 1 2) "return(1,2);"
      (ses-insert "var a = 3") "var a = 3;"
      (if* 1 (lambda () #t) 2) "(1?(function(){return(true)}):2);"
      ; testing "add-return-statement"
      (l () (define a 3)) "(function(){var a=3;return(undefined)});"
      (l () (begin 1 2)) "(function(){1;return(2)});"
      (set! a 1) "a=1;"
      (set! a 1 b 2 c 3) "a=1;b=2;c=3;"
      (l () (set! a 1 b 2 c 3)) "(function(){a=1;b=2;return(c=3)});"
      (l () (set! a 1)) "(function(){return(a=1)});"
      (throw "test") "(function(){throw([\"\\\"test\\\"\"])})();"
      (array #\a) "[\"a\"];" (array "\"") "[\"\\\"\"];" (array #t) "[true];"



      )))

#;(define-test-module (test module sph lang ecmascript expressions)
  (import
    (sph lang ecmascript expressions)
    (sph hashtable))

  (test-execute-procedures-lambda
    (es-apply
      ("a" ("1" 2)) "a(\"1\",2)")
    (es-chain
      ("proc" "base" "arg1" "arg2") "base.proc(arg1,arg2)")
    (es-define
      ("a") "var a"
      ("a" 2) "var a=2"
      ("a" "2") "var a=\"2\"")
    (es-environment
      ((a "b" c)) "{\"a\":a,\"b\":\"b\",\"c\":c}")
    (es-function
      #f "(function(){})"
      (#f ("a" b "c")) "(function(a,b,c){})"
      ("return a" ("a" b "c")) "(function(a,b,c){return a})"
      (#f ("a") #:name b #:rest rest) "function b (a){var rest=new Array(arguments.length);for(var ___i=1;___i<arguments.length;___i+=1){rest.push(arguments[___i])}}")
    (es-identifier
      (a) "a"
      "a" "a")
    (es-if
      ("a==1" "false") "a==1?false:undefined"
      ("1" "false" "true") "1?false:true")
    (es-object
      ((("a" . 1) ("b" . 2))) "{\"a\":1,\"b\":2}")
    (es-ref
      (a "b") "a[\"b\"]"
      ("a[\"b\"]" "c") "a[\"b\"][\"c\"]")
    (es-set!
      (a 1) "a=1"
      ("a" 2) "a=2"
      (a "2") "a=\"2\"")
    (es-value
      1 "1"
      a "a"
      #t "true"
      #\a "\"a\""
      "1" "\"1\""
      (((1 . 2))) "{\"1\":2}"
      ((1 2 "3")) "[1,2,\"3\"]"
      (("1" . 2)) "[\"1\",2]"
      (unquote (vector 1 2 "3")) "[1,2,\"3\"]"
      (unquote (ht-create 1 2 "3" 4)) "{\"3\":4,\"1\":2}")
    (es-vector
      (1 2 "3" 4) "[1,2,\"3\",4]")
    (list->es-vector
      ((1 #t)) "[1,true]")))
