--		Copyright 1994 by Daniel R. Grayson

CC = new Field of BasicList

document { quote CC,
     TT "CC", " -- the class of all complex numbers.",
     PARA,
     "The symbol ", TO "ii", " represents the square root of -1.",
     PARA, 
     EXAMPLE "z = 3-4*ii",
     EXAMPLE "z^5",
     EXAMPLE "1/z",
     PARA,
     "Here are some functions for use with complex numbers.",
     MENU {
	  TO "realPart",
	  TO "imaginaryPart",
	  TO "conjugate"
	  },
     PARA,
     SEEALSO "numbers"
     }


ii = new CC from {0,1}
document { quote ii,
     TT "ii", " -- the square root of -1.",
     PARA,
     SEEALSO( "CC")
     }

CC.char = 0
CC#0 = new CC from {0,0}
CC#1 = new CC from {1,0}

CC.degreeLength = 0

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
exprI := hold quote ii
expression CC := z -> z#0 + z#1*exprI
name CC := z -> name expression z
net CC := z -> net expression z
document { quote realPart,
     TT "realPart z", " -- return the real part of a complex number z."
     }
document { quote imaginaryPart,
     TT "imaginaryPart z", " -- return the imaginary part of a complex number z."
     }

document { quote conjugate,
     TT "conjugate z", " -- the complex conjugate of the complex number z."
     }

CC + CC := { CC, (x,y) -> new CC from {x#0+y#0,x#1+y#1} }

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
CC + RR := { CC, (z,x) -> new CC from {z#0+x,z#1} }
CC - RR := { CC, (z,x) -> new CC from {z#0-x,z#1} }
CC * RR := (z,x) -> new CC from {z#0*x,z#1*x}
CC / RR := (z,x) -> new CC from {z#0/x,z#1/x}
RR + CC := { CC, (x,z) -> new CC from {x+z#0, z#1} }
RR - CC := (x,z) -> new CC from {x-z#0,-z#1}
RR * CC := (x,z) -> new CC from {x*z#0,x*z#1}
RR / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from { x*y#0/m , - x*y#1/m })
CC + QQ := {CC, (z,x) -> new CC from {z#0+x,z#1}}
CC - QQ := (z,x) -> new CC from {z#0-x,z#1}
CC * QQ := (z,x) -> new CC from {z#0*x,z#1*x}
CC / QQ := (z,x) -> new CC from {z#0/x,z#1/x}
QQ + CC := {CC, (x,z) -> new CC from {x+z#0, z#1}}
QQ - CC := (x,z) -> new CC from {x-z#0,-z#1}
QQ * CC := (x,z) -> new CC from {x*z#0,x*z#1}
QQ / CC := (x,y) -> (
	  m := y#0^2 + y#1^2;
	  new CC from {x*y#0/m , - x*y#1/m })
CC + ZZ := {CC, (z,x) -> new CC from {z#0+x,z#1}}
CC - ZZ := (z,x) -> new CC from {z#0-x,z#1}
CC * ZZ := (z,x) -> new CC from {z#0*x,z#1*x}
CC / ZZ := (z,x) -> new CC from {z#0/x,z#1/x}
ZZ + CC := {CC, (x,z) -> new CC from {x+z#0, z#1}}
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

TEST "
     use CC
     z = 2 - 3*ii
     w = 4 + 5*ii
     x = 2 + ii - ii
     assert( z*w == 23  - 2*ii )
     assert( z/w == -7/41 + -22/41 * ii )
     assert( 1 + z == 3 - 3*ii )
     assert( 2*w == 8 + 10*ii )
     assert( z + w == 6 + 2*ii )
     assert( name w == \"4+5*ii\" )
     assert( conjugate z == 2 + 3*ii )
     assert( x == 2 )
     assert( x == 2. )
     assert( x == 2/1 )
     assert( net (2-3*ii) === \"2 - 3 ii\"^0 )
     "
