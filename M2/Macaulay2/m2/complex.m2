--		Copyright 1993-1999,2004 by Daniel R. Grayson


new CC from Sequence := (CC,v) -> newCC v

CC.isBasic = true
CC.synonym = "complex number"
CC.isCommutative = true
CC.texMath = ///{\mathbb C}///
CC.isField = true
CC.baseRings = {ZZ}
CC.RawRing = rawCC 10.^-10
ii = new CC from (0.,1.)
CC.char = 0
CC#0 = new CC from (0.,0.)
CC#1 = new CC from (1.,0.)
CC.degreeLength = 0
raw CC := x -> rawFromNumber(raw CC, x)
new CC from RawRingElement := (CC,n) -> rawToComplex n

promote(RR,CC) := (r,CC) -> newCC(r,0.)

lift(CC,ZZ) := lift(CC,QQ) := (z,R) -> if imaginaryPart z == 0 then lift(realPart z, R)
lift(CC,RR) := (z,RR) -> if imaginaryPart z == 0 then realPart z
lift(CCC,RRR):= (z,RRR) -> if imaginaryPart z == 0 then realPart z
lift(CCC,QQ) := lift(CCC,ZZ) := lift(CCC,RR) := (z,R) -> if imaginaryPart z == 0 then lift(realPart z, R)
lift(CCC,CC) := (z,CC) -> new CC from (lift(realPart z, RR), lift(imaginaryPart z, RR))

CCC.isBasic = true
CCC.synonym = "big complex number"
CCC.isCommutative = true
CCC.texMath = ///{\mathbb C}///
CCC.baseRings = {}
CCC.isField = true
CCC.RawRing = rawCCC()
CCC.char = 0
CCC.degreeLength = 0
conjugate CCC := notImplemented

conjugate CC := CC => z -> new CC from (realPart z,-imaginaryPart z)
exprI := symbol ii
expression CC := z -> realPart z + imaginaryPart z * hold exprI
toExternalString CC := toString CC := z -> toString expression z
net CC := z -> net expression z

CC + CC := (x,y) -> new CC from (realPart x+realPart y,(imaginaryPart x)+(imaginaryPart y))

   - CC := x -> new CC from (-realPart x,-(imaginaryPart x))
CC - CC := (x,y) -> new CC from (realPart x-realPart y,(imaginaryPart x)-(imaginaryPart y))
CC ^ ZZ := BinaryPowerMethod
CC.InverseMethod = y -> (
     m := (realPart y)^2 + (imaginaryPart y)^2;
     new CC from (realPart y/m,-(imaginaryPart y)/m))	  
CC * CC := (x,y) -> new CC from ( (realPart x)*(realPart y) - (imaginaryPart x)*(imaginaryPart y) , (realPart x)*(imaginaryPart y) + (imaginaryPart x)*(realPart y) )
CC / CC := (x,y) -> (
	  m := (realPart y)^2 + (imaginaryPart y)^2;
	  new CC from ( ((realPart x)*(realPart y) + (imaginaryPart x)*(imaginaryPart y))/m , ((imaginaryPart x)*(realPart y) - (realPart x)*(imaginaryPart y))/m ))
CC + RR := (z,x) -> new CC from ((realPart z)+x,(imaginaryPart z))
CC - RR := (z,x) -> new CC from ((realPart z)-x,(imaginaryPart z))
CC * RR := (z,x) -> new CC from ((realPart z)*x,(imaginaryPart z)*x)
CC / RR := (z,x) -> new CC from ((realPart z)/x,(imaginaryPart z)/x)
RR + CC := (x,z) -> new CC from (x+(realPart z), (imaginaryPart z))
RR - CC := (x,z) -> new CC from (x-(realPart z),-(imaginaryPart z))
RR * CC := (x,z) -> new CC from (x*(realPart z),x*(imaginaryPart z))
RR / CC := (x,y) -> (
	  m := (realPart y)^2 + (imaginaryPart y)^2;
	  new CC from ( x*(realPart y)/m , - x*(imaginaryPart y)/m ))
CC + QQ := (z,x) -> new CC from ((realPart z)+x,(imaginaryPart z))
CC - QQ := (z,x) -> new CC from ((realPart z)-x,(imaginaryPart z))
CC * QQ := (z,x) -> new CC from ((realPart z)*x,(imaginaryPart z)*x)
CC / QQ := (z,x) -> new CC from ((realPart z)/x,(imaginaryPart z)/x)
QQ + CC := (x,z) -> new CC from (x+(realPart z), (imaginaryPart z))
QQ - CC := (x,z) -> new CC from (x-(realPart z),-(imaginaryPart z))
QQ * CC := (x,z) -> new CC from (x*(realPart z),x*(imaginaryPart z))
QQ / CC := (x,y) -> (
	  m := (realPart y)^2 + (imaginaryPart y)^2;
	  new CC from (x*(realPart y)/m , - x*(imaginaryPart y)/m ))
CC + ZZ := (z,x) -> new CC from ((realPart z)+x,(imaginaryPart z))
CC - ZZ := (z,x) -> new CC from ((realPart z)-x,(imaginaryPart z))
CC * ZZ := (z,x) -> new CC from ((realPart z)*x,(imaginaryPart z)*x)
CC / ZZ := (z,x) -> new CC from ((realPart z)/x,(imaginaryPart z)/x)
ZZ + CC := (x,z) -> new CC from (x+(realPart z), (imaginaryPart z))
ZZ - CC := (x,z) -> new CC from (x-(realPart z),-(imaginaryPart z))
ZZ * CC := (x,z) -> new CC from (x*(realPart z),x*(imaginaryPart z))
ZZ / CC := (x,y) -> (
	  m := (realPart y)^2 + (imaginaryPart y)^2;
	  new CC from ( x*(realPart y)/m , - x*(imaginaryPart y)/m ))
CC == ZZ := (z,i) -> (realPart z) == i and (imaginaryPart z) == 0
ZZ == CC := (i,z) -> (realPart z) == i and (imaginaryPart z) == 0
CC == QQ := (z,i) -> (realPart z) == i and (imaginaryPart z) == 0
QQ == CC := (i,z) -> (realPart z) == i and (imaginaryPart z) == 0
CC == RR := (z,i) -> (realPart z) == i and (imaginaryPart z) == 0
RR == CC := (i,z) -> (realPart z) == i and (imaginaryPart z) == 0
CC == CC := (w,z) -> w === z
isConstant CC := i -> true

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
