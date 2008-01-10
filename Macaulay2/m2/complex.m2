--		Copyright 1993-1999,2004 by Daniel R. Grayson

-- CC#0 = toCC(defaultPrecision, 0, 0)			    -- oops, do we really need this??
-- CC#1 = toCC(defaultPrecision, 1, 0)
ii = toCC(defaultPrecision, 0, 1)				    -- to be changed

lift(CC,RR):= (z,RR) -> if imaginaryPart z == 0 then realPart z
lift(CC,QQ) := lift(CC,ZZ) := (z,R) -> if imaginaryPart z == 0 then lift(realPart z, R)

CC.isBasic = true
CC.synonym = "complex number"
CC.texMath = ///{\mathbb C}///
conjugate CC := z -> toCC(precision z, realPart z, - imaginaryPart z)

ComplexNumberRing = new Type of {* ImmutableType *} BigNumberRing
ComplexNumberRing.synonym = "real number ring"
new ComplexNumberRing of CC from ZZ := memoize(
     (ComplexNumberRing,CC,prec) -> newClass(ComplexNumberRing,CC,
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol isBasic => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol RawRing => rawCC prec
	       }))
liftable(CC,ComplexNumberRing) := R -> true
ZZ _ ComplexNumberRing :=
QQ _ ComplexNumberRing :=
RR _ ComplexNumberRing :=
CC _ ComplexNumberRing :=
lift(ZZ,ComplexNumberRing) := 
lift(RR,ComplexNumberRing) := 
lift(CC,ComplexNumberRing) := 
lift(QQ,ComplexNumberRing) := 
promote(ZZ,ComplexNumberRing) := 
promote(RR,ComplexNumberRing) := 
promote(CC,ComplexNumberRing) := 
promote(QQ,ComplexNumberRing) := (x,R) -> toCC(R.precision,x)
expression ComplexNumberRing := R -> new Subscript from {symbol CC, R.precision}
CC.BigNumberRing = ComplexNumberRing
toString ComplexNumberRing := R -> concatenate("CC_",toString R.precision)
ring CC := x -> new ComplexNumberRing of CC from precision x
CC ^ ZZ := BinaryPowerMethod
CC.InverseMethod = y -> conjugate y / y^2
expression CC := z -> realPart z + imaginaryPart z * hold symbol ii
toExternalString CC := toExternalString0
net CC := z -> simpleToString z
isConstant CC := i -> true
new CC from RawRingElement := (CCC,x) -> (
     assert( CCC === CC );				    -- the danger is that maybe CCC === CC_53, for example
     rawToCC x)
promote(RawRingElement,CC) := (x,R) -> new CC from x

CC.random = opts -> C -> rawRandomCC C.precision

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
