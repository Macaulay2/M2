x
x := 4
x
g = () -> ( p := 444; p )
g()
p
g = () -> ( p := 444; () -> p )
g()
oo ()
g = () -> ( p := 444; (() -> p, i -> p = i))
(b,c) = g()
b()
c 555
b()
a := b := 44
a
b
f = i -> (i,i^2)
(r,s) := f 9
r
s
String * String := peek;
"left" * "right"
String * Number := peek;
"left" * 33
"left" * 3.3
ZZ + ZZ := (x,y) -> x+y+100
3 + 4
CC + CC := (w,z) -> w*z
ii + ii
- String := peek;
- "foo"
- String := peek;
- "foo"
String ~ := peek;
"foo" ~
String ~ := peek;
"foo" ~
source String := peek;
source "foo"
source String := peek;
source "foo"
source(String,Number) := peek;
source("foo",33)
source("foo",3.3)
source(String,String) := peek;
source("foo","bar")
