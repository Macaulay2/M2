-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needs "Dmodules.m2"

x = symbol x; Dx = symbol Dx; 
y = symbol y; Dy = symbol Dy; 
a = symbol a; b = symbol b; c = symbol c; d = symbol d; 
A =  (QQ [a,b,c,d]) [x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
Dtrace 1
pInfo(1, "testing paramBpoly...")
bf = paramBpoly(
     a*x^2,
     "quadratic2" 
     )
assert(listForm value first bf == {({2},1/1), ({1},3/2), ({0},1/2)})
