--		Copyright 1993-2008 by Daniel R. Grayson

-- ImmutableType

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType

-- these types are pre-defined

RR.synonym = "real number"
CC.synonym = "complex number"
RR.texMath = ///{\mathbb R}///
CC.texMath = ///{\mathbb C}///
Number.synonym = "number"
BigNumberType.synonym = "big number ring"
BigNumber.synonym = "big number"

-- new types

BigNumberRing = new Type of Ring
BigNumberRing.synonym = "big number ring"
raw BigNumberRing := R -> R.RawRing

RR.BigNumberRing = RealNumberRing    = new Type of BigNumberRing   ; RealNumberRing.synonym = "real number ring"
CC.BigNumberRing = ComplexNumberRing = new Type of BigNumberRing; ComplexNumberRing.synonym = "complex number ring"
NumberParent  = new Type of Nothing
RR.NumberParent = RRParent = new Type of NumberParent
CC.NumberParent = CCParent = new Type of NumberParent
new RealNumberRing of NumberParent from ZZ := memoize (
     (RealNumberRing,NumberParent,prec) -> newClass(RealNumberRing,NumberParent,
	  hashTable { 
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol isBasic => true,
	       symbol RawRing => rawRR prec
	       }))
new ComplexNumberRing of NumberParent from ZZ := memoize(
     (ComplexNumberRing,NumberParent,prec) -> newClass(ComplexNumberRing,NumberParent,
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol isBasic => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol RawRing => rawCC prec
	       }))
BigNumberType _ ZZ := (T,prec) -> new T.BigNumberRing of T.NumberParent from prec
default BigNumberType := R -> R_defaultPrecision

-- lift and promote between real or complex rings

Number _ BigNumberType := (x,RR) -> x_(default RR)

promote(RawRingElement,RRParent) := (x,R) -> new RR from x
promote(RawRingElement,CCParent) := (x,R) -> new CC from x
promote(RawRingElement,Number) := (x,R) -> new R from x
promote(RawRingElement,RingElement) := (x,R) -> new R from x
promote(Number,BigNumber) := (x,RR) -> promote(x,default RR)
promote(RR,RRParent) := (i,K) -> toRR(K.precision,i)
promote(CC,CCParent) := (i,K) -> toCC(K.precision,i)
lift(CC,RRParent):= opts -> (z,RR) -> (
     if imaginaryPart z == 0 then realPart z
     else if opts.Verify then error "can't lift given complex number to real number"
     )

-- lift and promote to and from other rings

numeric VisibleList := x -> apply(x,numeric)
numeric(ZZ,VisibleList) := (prec,x) -> apply(x, t -> numeric(prec,t))
numeric Number := x -> numeric(defaultPrecision, x)
numeric CC := identity
numeric RR := identity
numeric(ZZ,Number) := toRR
numeric(ZZ,CC) := toCC

ZZ _ RealNumberRing :=
QQ _ RealNumberRing :=
RR _ RealNumberRing := (x,R) -> toRR(R.precision,x)
ZZ _ ComplexNumberRing :=
QQ _ ComplexNumberRing :=
RR _ ComplexNumberRing :=
CC _ ComplexNumberRing := (x,R) -> toCC(R.precision,x)

approx := (r,limit) -> (
     if r == 0 then return 0/1;
     r' := r;
     m := mutableIdentity(ZZ,2);
     while true do (
	  a := floor r';
	  columnSwap(m,0,1);
	  columnAdd(m,0,a,1);
	  r' = r' - a;
	  if r' == 0 or abs(r - m_(0,0) / m_(1,0)) < limit then return m_(0,0) / m_(1,0);
	  r' = 1/r' ;
	  ))
lift(RR,QQ) := opts -> (r,QQ) -> approx(r,abs r / 2^(precision r - 16))
lift(RR,ZZ) := opts -> (r,ZZ) -> (
     i := floor r; 
     if r == i then i 
     else if opts.Verify then error "can't lift to ZZ")
lift(CC,QQ) := lift(CC,ZZ) := opts -> (z,R) -> if imaginaryPart z == 0 then lift(realPart z, R) else if opts.Verify then error "can't lift given complex number to real number"

ring RR := x -> new RealNumberRing of RRParent from precision x
ring CC := x -> new ComplexNumberRing of CCParent from precision x

new RR from RawRingElement := (RRR,x) -> ( assert( RRR === RR ); rawToRR x )
new CC from RawRingElement := (CCC,x) -> ( assert( CCC === CC ); rawToCC x)

-- arithmetic operations

RR.InverseMethod = x -> 1/x
CC.InverseMethod = y -> conjugate y / y^2
CC ^ ZZ := BinaryPowerMethod

scan((QQ,RR,CC), F -> (
	  F // F := (x,y) -> if y == 0 then 0_F else x/y;
	  F % F := (x,y) -> if y == 0 then x else 0_F;
	  F // ZZ := (x,y) -> x // y_F;
	  F % ZZ := (x,y) -> x % y_F;
	  ))

scan((RR,CC), F -> (
	  F // QQ := (x,y) -> x // y_F;
	  F % QQ := (x,y) -> x % y_F;
	  ))

CC // RR := (x,y) -> x // y_CC;
CC % RR := (x,y) -> x % y_CC;

-- functions

conjugate CC := z -> toCC(precision z, realPart z, - imaginaryPart z)
isConstant Number := i -> true

round RR := round0
round(ZZ,RR) := (n,x) -> (
     p := 10^n;
     toRR(precision x,round(x*p)/p))

random RR := RR => opts -> x -> x * rawRandomRR precision x
RRParent.random = opts -> R -> rawRandomRR R.precision
CCParent.random = opts -> C -> rawRandomCC C.precision
random RingFamily := opts -> R -> random(default R,opts)

-- algebraic operations and functions

RR.isBasic = CC.isBasic = true

BigNumberType Array := (T,X) -> (default T) X
Thing ** BigNumberType := (X,T) -> X ** default T

dim BigNumberType := R -> 0
char BigNumberType := R -> 0
degreeLength BigNumberType := R -> 0
isField BigNumberType := R -> true
frac BigNumberType := R -> R

generators BigNumberRing := opts -> R -> {}
isField BigNumberRing := R -> true
degreeLength BigNumberRing := R -> 0
frac BigNumberRing := identity
numgens BigNumberRing := R -> 0
dim BigNumberRing := R -> 0
char BigNumberRing := R -> 0

-- printing

toString RealNumberRing := R -> concatenate("RR_",toString R.precision)
toString ComplexNumberRing := R -> concatenate("CC_",toString R.precision)

expression RealNumberRing := R -> new Subscript from {symbol RR, R.precision}
expression ComplexNumberRing := R -> new Subscript from {symbol CC, R.precision}
expression RR := x -> if x < 0 then new Minus from {-x} else new Holder from {x}
expression CC := z -> expression realPart z + expression imaginaryPart z * hold symbol ii

net BigNumberRing := R -> net expression R
net CC := z -> simpleToString z
toExternalString RR := toExternalString0
toExternalString CC := toExternalString0
logten2 := log 10. / log 2.
BigNumber#{Standard,Print} = x ->  (
     << newline << concatenate(interpreterDepth:"o") << lineNumber << " = ";
     save := printingPrecision;
     try printingPrecision = max(printingPrecision, floor (precision x / logten2));
     try << x
     else try << toString x
     else << "<<a number that fails to print>>";
     printingPrecision = save;
     << newline << flush;
     );

BigNumber#{Standard,AfterPrint} = x -> (
     << endl;                             -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : " << ring x;
     << endl;
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
