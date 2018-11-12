(define-test-module (test module sph lang sescript)
  (import
    (sph lang sescript))

  (define-test (sescript->ecmascript input)
    (call-with-output-string (l (port) (sescript->ecmascript (list input) port))))

  (test-execute-procedures-lambda
    (sescript->ecmascript
      (array #\a) "[\"a\"];"
      (array "\"") "[\"\\\"\"];"
      (array #t) "[true];"
      (begin a->b a-b a! a& a?)
      "a_to_b;a_b;a_x;a_ampersand;a_p;"
      (switch a (1 "b" break) (2 "c" "d") (default "e") (3 "f"))
      "switch(a){case 1:\"b\";break;case 2:\"c\";\"d\";default:\"e\";case 3:\"f\"};"
      (begin 1 a #t #\a "1" )
      "1;a;true;\"a\";\"1\";"
      (begin 1 (begin 2 3)) "1;2;3;"
      (= 1 2 3) "(1===2)&&(2===3);"
      (and 1 2 3) "(1&&2&&3);"
      (array 1 2 3) "[1,2,3];"
      (chain c (chain a b) 1 2 "d") "b.a().c(1,2,\"d\");"
      (cond
        (a b)
        ((= c 3) #f #t)
        (else #t #f))
      "if(a){b}else if(c===3){false;true;}else{true;false;};"
      (cond (a b)) "if(a){b};"
      (declare a) "var a;"
      (declare a b c) "var a,b,c;"
      (define a 1) "var a=1;"
      (define a 1 b 2) "var a=1,b=2;"
      (define a (lambda (a b) 1 2 3)) "var a=(function(a,b){1;2;return(3);});"
      (define (a b c-d) 1 2 3) "function a(b,c_d){1;2;return(3);};"
      (for ((set index 0) (< index len) (set index (+ 1 index))) #t)
      "for(index=0;(index<len);index=(1+index)){true};"
      (for (((set a 0) (set b 1)) (< index len) ((set a (+ 1 a)) (set b (+ 2 b)))) #t)
      "for(a=0,b=1;(index<len);a=(1+a),b=(2+b)){true};"
      (for ((begin a b) (< c d) (begin e f)) #t)
      "for(a,b;(c<d);e,f){true};"
      (get a 1) "a[1];"
      (get a 1 2 3) "a[1][2][3];"
      (get (get a 1) 2) "a[1][2];"
      (if* a (if* b c d) e) "(a?(b?c:d):e);"
      (if* a (if* (if* b c) d) e) "(a?((b?c:undefined)?d:undefined):e);"
      (if* 1 (lambda () #t) 2) "(1?(function(){return(true)}):2);"
      (if* (not 1) a b) "(!1?a:b);"
      (lambda (a) 1) "(function(a){return(1)});"
      (lambda () (if* a b c))
      "(function(){return(a?b:c)});"
      ((lambda (a) 1)) "(function(a){return(1)})();"
      (let (a 1) 2) "(function(a){return(2)})(1);"
      (let ((a 1) (b 2)) #t) "(function(a,b){return(true)})(1,2);"
      (let* ((a 1) (b 2) (c 3)) 4) "(function(a){var b=2;var c=3;return(4);})(1);"
      (make-regexp "[^a-b0-9]" "g") "/[^a-b0-9]/g;"
      (new obj) "new obj();"
      (new obj 3 5) "new obj(3,5);"
      (not 1) "!1;"
      (nullary #t) "(function(){return(true)});"
      (object) "{};"
      (object a 2 b-c 3 "d" 4) "{a:2,b_c:3,\"d\":4};"
      (object* a b c) "{a:a,b:b,c:c};"
      ; a return keyword may not have an empty argument list appended
      (return) "return;"
      (return 1 2) "return(1,2);"
      (ses-comment "test" "comment") "\n/* test\n  comment */\n"
      (ses-insert "var a = 3") "var a = 3;"
      (set a 1) "a=1;"
      (set a 1 b 2 c 3) "a=1;b=2;c=3;"
      (a (if* b c d) (l (e) f))
      "a((b?c:d),(function(e){return(f)}));"
      )))
