--		Copyright 1994 by Daniel R. Grayson

CC = new Field of BasicList
CC.isCommutative = true

ii = new CC from {0,1}

CC.char = 0
CC#0 = new CC from {0,0}
CC#1 = new CC from {1,0}

CC.degreeLength = 0

mathML CC := z -> concatenate(
     "<cn type='complex'>",string realPart z, "<sep/>", string imaginaryPart z, "</cn>"
     )     

conjugate ZZ := identity
realPart ZZ := identity
imaginaryPart ZZ := z -> 0
conjugate QQ := identity
realPart QQ := identity
imaginaryPart QQ := z -> 0
conjugate RR := identity
realPart RR := identity
imaginaryPart RR := z -> 0
conjugate CC := z -> new CC from {z#0,-z#1}
realPart CC := z -> z#0
imaginaryPart CC := z -> z#1
exprI := quote ii
expression CC := z -> z#0 + z#1 * hold exprI
name CC := z -> name expression z
net CC := z -> net expression z

CC + CC := (x,y) -> new CC from {x#0+y#0,x#1+y#1}

   - CC := x -> new CC from {-x#0,-x#1}
CC - CC := (x,y) -> new CC from {x#0-y#0,x#1-y#1}
CC ^ ZZ := BinaryPowerMethod
InverseMethod CC := y -> (
     m := y#0^2 + y#1^2;
     new CC from {y#0/m,-y#1/m})	  
CC * CC := (x,y) -> new CC from { x#0*y#0 - x#1*y#1 , x#0*y#1 + x#1*y#0 }
CC / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from { (x#0*y#0 + x#1*y#1)/m , (x#1*y#0 - x#0*y#1)/m })
CC + RR := (z,x) -> new CC from {z#0+x,z#1}
CC - RR := (z,x) -> new CC from {z#0-x,z#1}
CC * RR := (z,x) -> new CC from {z#0*x,z#1*x}
CC / RR := (z,x) -> new CC from {z#0/x,z#1/x}
RR + CC := (x,z) -> new CC from {x+z#0, z#1}
RR - CC := (x,z) -> new CC from {x-z#0,-z#1}
RR * CC := (x,z) -> new CC from {x*z#0,x*z#1}
RR / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from { x*y#0/m , - x*y#1/m })
CC + QQ := (z,x) -> new CC from {z#0+x,z#1}
CC - QQ := (z,x) -> new CC from {z#0-x,z#1}
CC * QQ := (z,x) -> new CC from {z#0*x,z#1*x}
CC / QQ := (z,x) -> new CC from {z#0/x,z#1/x}
QQ + CC := (x,z) -> new CC from {x+z#0, z#1}
QQ - CC := (x,z) -> new CC from {x-z#0,-z#1}
QQ * CC := (x,z) -> new CC from {x*z#0,x*z#1}
QQ / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from {x*y#0/m , - x*y#1/m })
CC + ZZ := (z,x) -> new CC from {z#0+x,z#1}
CC - ZZ := (z,x) -> new CC from {z#0-x,z#1}
CC * ZZ := (z,x) -> new CC from {z#0*x,z#1*x}
CC / ZZ := (z,x) -> new CC from {z#0/x,z#1/x}
ZZ + CC := (x,z) -> new CC from {x+z#0, z#1}
ZZ - CC := (x,z) -> new CC from {x-z#0,-z#1}
ZZ * CC := (x,z) -> new CC from {x*z#0,x*z#1}
ZZ / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from { x*y#0/m , - x*y#1/m })
CC == ZZ := (z,i) -> z#0 == i and z#1 == 0
ZZ == CC := (i,z) -> z#0 == i and z#1 == 0
CC == QQ := (z,i) -> z#0 == i and z#1 == 0
QQ == CC := (i,z) -> z#0 == i and z#1 == 0
CC == RR := (z,i) -> z#0 == i and z#1 == 0
RR == CC := (i,z) -> z#0 == i and z#1 == 0
