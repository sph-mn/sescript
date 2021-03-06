* a simpler ecmascript/javascript syntax. ecmascript written as scheme-like s-expressions
* the output, after formatting, is supposed to be as if originally written in ecmascript
* supports all es5
* command-line compiler and scheme library
* possibly also useful as an intermediate language for applications that want to compile to ecmascript
* status: should work, been around for a while, easy to maintain and extend
* license: gpl3+. does not apply to generated code. generated code has your license
* you can try it out [here](http://sph.mn/dynamic/syntax/sescript/javascript)

# examples
```
echo "(define (test x y) (return #t))" | ses
->
function test(x, y){return(true)};
```

```
(declare d e f)

(define myobject
  (object
    a 1
    b (object c 2)))

(set
  d 1
  e 3
  f 9)
```

## accessors
```
myobject.a.b
(get myobject "a" "b")
(get myarray 0 1)
```

# extensions
* adds return statement to the last expression in a function. lambda just feels wrong without this behaviour
* let, let*

# command-line application
```
$ ses --help
parameters
  options ... paths ...
description
  compile sescript to ecmascript. read from files or standard input,
  and write to a file or standard output depending on if paths are given as arguments.
  input/output depends on the number of given paths:
  none: read from standard input and write to standard output
  one: read from file and write to standard output
  two: read from the first file and write to the second
  more: read from all files except the last one and write to the last one
options
  --compress | -c  compress output with uglifyjs
  --format | -f  format output with uglifyjs
  --help | -h
  --interface
```

# dependencies
* [guile](https://www.gnu.org/software/guile) >= 2
* [sph-lib](https://github.com/sph-mn/sph-lib)

# installation
run the install script, "./exe/install". it supports one optional argument, an install path prefix.

alternatively
* copy everything under modules/ into a directory that is in guiles default load path or $GUILE_LOAD_PATH
* copy exe/ses to /usr/bin or wherever executables are to be installed

try to run ``ses --help`` to see if it works

# usage from scheme
```
(import (sph lang sescript))
```

examples
```
(sescript->ecmascript-string (quote (begin (define a 1) (set a 2))))
(sescript->ecmascript (quote ((define a 1) (set a 2))) (current-output-port))

(define scheme-value 8)

(define code
  (quasiquote
    (begin
      (define a 1)
      (set a (unquote scheme-value)))))

(sescript->ecmascript-string code)
```

# extension
how to add your own syntax: this is currently only possible when compiling with scheme and not via the sescript command-line interface. create a scheme file with new bindings, example below, and then use sescript->ecmascript-string or similar as usual
```
(import (sph lang sescript) (rnrs hashtables))

(hashtable-set! ses-descend-sescript (quote myprefix)
  (lambda (a compile)
    "(any:sescript-argument ...) procedure:recurse -> any:sescript
    create sescript that will be parsed again"
    (list (quote if) (car a) #t #f)))

(hashtable-set! ses-descend-ecmascript (quote myprefix2)
  (lambda (a compile)
    "(any:sescript-argument ...) procedure:recurse -> string:ecmascript
     create final ecmascript strings directly"
     (string-join (map compile a) "\n")))

(define code (quote (myprefix "test")))

(sescript->ecmascript-string code)
```

# other
* filename extension .sjs or .ses
* --format and --compress option on the command-line if uglifyjs is installed
* js-beautify is another recommended auto formatter
* sescript only outputs valid ecmascript syntax
* a benefit of using sescript is that editor modes for scheme syntax and structural editing can be used
* [other languages that compile to javascript](https://github.com/jashkenas/coffeescript/wiki/List-of-languages-that-compile-to-JS)

# syntax reference
ses expression and the ecmascript result. taken from the automated tests

```
(array #\a)
->
["a"];

(array "\"")
->
["\""];

(array #t)
->
[true];

(begin a->b a-b a! a& a?)
->
a_to_b;a_b;a_x;a_ampersand;a_p;

(case a ((b c d) #t))
->
switch(a){case b:case c:case d:true;break};

(begin 1 a #t #\a "1")
->
1;a;true;"a";"1";

(begin 1 (begin 2 3))
->
1;2;3;

(case (+ 1 2) (2 #t #f) (3 #t) (else 4 5))
->
switch(1+2){case 2:true;false;break;case 3:true;break;default:4;5;break};

(= 1 2 3)
->
(1===2)&&(2===3);

(and 1 2 3)
->
(1&&2&&3);

(array 1 2 3)
->
[1,2,3];

(chain c (chain a b) 1 2 "d")
->
b.a().c(1,2,"d");

(cond (a b) ((= c 3) #f #t) (else #t #f))
->
if(a){b}else if(c===3){false;true;}else{true;false;};

(cond (a b))
->
if(a){b};

(declare a)
->
var a;

(declare a b c)
->
var a,b,c;

(define a 1)
->
var a=1;

(define a 1 b 2)
->
var a=1,b=2;

(define a (lambda (a b) 1 2 3))
->
var a=(function(a,b){1;2;return(3);});

(define (a b c-d) 1 2 3)
->
function a(b,c_d){1;2;return(3);};

(for ((set index 0) (< index len) (set index (+ 1 index))) #t)
->
for(index=0;(index<len);index=(1+index)){true};

(for (((set a 0) (set b 1)) (< index len) ((set a (+ 1 a)) (set b (+ 2 b)))) #t)
->
for(a=0,b=1;(index<len);a=(1+a),b=(2+b)){true};

(for ((begin a b) (< c d) (begin e f)) #t)
->
for(a,b;(c<d);e,f){true};

(get a 1)
->
a[1];

(get a 1 2 3)
->
a[1][2][3];

(get (get a 1) 2)
->
a[1][2];

(if* a (if* b c d) e)
->
(a?(b?c:d):e);

(if* a (if* (if* b c) d) e)
->
(a?((b?c:undefined)?d:undefined):e);

(if* 1 (lambda () #t) 2)
->
(1?(function(){return(true)}):2);

(if* (not 1) a b)
->
(!1?a:b);

(lambda (a) 1)
->
(function(a){return(1)});

(lambda () (if* a b c))
->
(function(){return(a?b:c)});

((lambda (a) 1))
->
(function(a){return(1)})();

(let (a 1) 2)
->
(function(a){return(2)})(1);

(let ((a 1) (b 2)) #t)
->
(function(a,b){return(true)})(1,2);

(let* ((a 1) (b 2) (c 3)) 4)
->
(function(a){var b=2;var c=3;return(4);})(1);

(make-regexp "[^a-b0-9]" "g")
->
/[^a-b0-9]/g;

(new obj)
->
new obj();

(new obj 3 5)
->
new obj(3,5);

(not 1)
->
!1;

(object)
->
{};

(object a 2 b-c 3 "d" 4)
->
{a:2,b_c:3,"d":4};

(object* a b c)
->
{a:a,b:b,c:c};

(return)
->
return;

(return 1 2)
->
return(1,2);

(ses-comment "test" "comment")
->
/* test
  comment */

(ses-insert "var a = 3")
->
var a = 3;

(set a 1)
->
a=1;

(set a 1 b 2 c 3)
->
a=1;b=2;c=3;
```

# possible enhancements and ideas
* support docstrings as is already implemented in sc
* port the macro system from sc
* es6 syntax: support new syntax (class, string templates, default parameters, object destructuring, async/await, import/export, const, let) and/or replace old syntax with new one (arrow functions for lambdas, still need a way to use the function keyword, coffeescript has -> => for that)
* translate scheme comments. scheme comments dont appear in the output, only ``(ses-comment "comment string")`` or ses-insert can be used. could use guile-reader but because that is a difficult to handle dependency it might be better to preprocess and replace all comments with ses-comment expressions
* add a command-line option to load custom syntax extensions from a file
* more syntax checks and error messages
