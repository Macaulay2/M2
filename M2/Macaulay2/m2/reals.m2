--		Copyright 1993-2008 by Daniel R. Grayson

-- ImmutableType

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType

-- these types are pre-defined

RR.synonym = "real number"
RRi.synonym = "real interval"
CC.synonym = "complex number"
RR.texMath = ///{\mathbb R}///
RRi.texMath = ///{\square\mathbb R}///
CC.texMath = ///{\mathbb C}///
Number.synonym = "number"
InexactFieldFamily.synonym = "inexact field family"
InexactNumber.synonym = "inexact number"

-- built-in functions 

precision InexactNumber := precision0
precision Number := x -> infinity
precision Ring := R -> precision 0_R

-- new types

InexactField = new Type of EngineRing
InexactField.synonym = "inexact field"
raw InexactField := R -> R.RawRing

RR.InexactField = RealField    = new Type of InexactField   ; RealField.synonym = "real field"
RRi.InexactField = RealIntervalField    = new Type of InexactField   ; RealIntervalField.synonym = "real interval field"
CC.InexactField = ComplexField = new Type of InexactField; ComplexField.synonym = "complex field"

Nothing' = Nothing					    -- maybe we'll want to rename it later...
RingFamily_* := RR -> RR#(symbol _*)
RingFamily_* := RRi -> RRi#(symbol _*)
RingFamily_* = (RR,e) -> RR#(symbol _*) = e
RingFamily_* = (RRi,e) -> RRi#(symbol _*) = e
InexactNumber' = new Type of Nothing'
RR_* = RR' = new Type of InexactNumber'
RRi_* = RRi' = new Type of InexactNumber'
CC_* = CC' = new Type of InexactNumber'

setAttribute(CC',PrintNet,"CC" | "*"^-1)
setAttribute(RR',PrintNet,"RR" | "*"^-1)
setAttribute(RRi',PrintNet,"RRi" | "*"^-1)
setAttribute(CC',PrintNames,"CC_*")
setAttribute(RR',PrintNames,"RR_*")
setAttribute(RRi',PrintNames,"RRi_*")
setAttribute(InexactNumber',PrintNet,"InexactNumber" | "*"^-1)

protect back
RR'.back = RR
RRi'.back = RRi
CC'.back = CC
new RealField of Nothing' from ZZ := memoize (
     (RealField,Nothing',prec) -> newClass(RealField,Nothing',
	  hashTable { 
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol isBasic => true,
	       symbol RawRing => rawRR prec
	       }))
new ComplexField of Nothing' from ZZ := memoize(
     (ComplexField,Nothing',prec) -> newClass(ComplexField,Nothing',
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol isBasic => true,
	       symbol baseRings => {ZZ,QQ,RR},
	       symbol RawRing => rawCC prec
	       }))
new RealIntervalField of Nothing' from ZZ := memoize (
     (RealIntervalField,Nothing',prec) -> newClass(RealIntervalField,Nothing',
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol isBasic => true,
	       symbol RawRing => rawRRi prec
	       }))
precision InexactField := R -> R.precision
InexactFieldFamily _ ZZ := (T,prec) -> new T.InexactField of T#(symbol _*) from prec -- oops...
default InexactFieldFamily := R -> R_defaultPrecision

diameter' = diameter
diameter = method()
diameter RRi := diameter'

-- lift and promote between real or complex rings

Number _ InexactFieldFamily := (x,RR) -> x_(default RR)

promote(RawRingElement,RR') := (x,R) -> new RR from x
promote(RawRingElement,RRi') := (x,R) -> new RRi from x
promote(RawRingElement,CC') := (x,R) -> new CC from x
promote(RawRingElement,Number) := (x,R) -> new R from x
promote(RawRingElement,RingElement) := (x,R) -> new R from x
promote(Number,InexactNumber) := (x,RR) -> promote(x,default RR)
promote(ZZ,RR') := 
promote(QQ,RR') := 
promote(RR,RR') := (i,K) -> toRR(K.precision,i)
promote(ZZ,CC') := 
promote(QQ,CC') := 
promote(RR,CC') := 
promote(CC,CC') := (i,K) -> toCC(K.precision,i)
promote(ZZ,RRi') :=
promote(QQ,RRi') :=
promote(RR,RRi') := (i,K) -> toRRi(K.precision,i,i)
promote(RRi,RRi') := (i,K) -> toRRi(K.precision,left(i),right(i))
lift(Number,InexactNumber) := opts -> (x,RR) -> lift(x,default RR,opts)

liftable(Number,InexactNumber) := (x,RR) -> liftable(x,default RR)
liftable(CC,RR'):= (z,RR) -> imaginaryPart z == 0
lift(CC,RR'):= opts -> (z,RR) -> (
     if imaginaryPart z == 0 then realPart z
     else if opts.Verify then error "lift: complex number is not real"
     )

liftable(RRi,RR) := (z,RR) -> diameter(z) == 0
lift(RRi,RR') := opts -> (r,RR) -> (
     if diameter(r) == 0 then lift(midpoint(r),RR)
     else if opts.Verify then error "lift: interval has positive diameter"
)

-- lift and promote to and from other rings

numeric VisibleList := x -> apply(x,numeric)
numeric(ZZ,VisibleList) := (prec,x) -> apply(x, t -> numeric(prec,t))
numeric Number := x -> numeric(defaultPrecision, x)
numeric CC := identity
numeric RR := identity
numeric RRi := identity
numeric(ZZ,Number) := toRR
numeric(ZZ,RRi) := (prec,x) -> toRRi(prec,left(x),right(x))
numeric(ZZ,CC) := toCC
infty := prec -> 1/toRR(prec,0)
numeric InfiniteNumber := infinity -> infinity#0 * infty defaultPrecision
numeric(ZZ, InfiniteNumber) := (prec,infinity) -> infinity#0 * infty prec

ZZ _ RealField :=
QQ _ RealField :=
RR _ RealField := (x,R) -> toRR(R.precision,x)
ZZ _ RealIntervalField :=
QQ _ RealIntervalField :=
RR _ RealIntervalField := (x,R) -> toRRi(R.precision,x,x)
RRi _ RealIntervalField := (x,R) -> toRRi(R.precision,left(x),right(x))
ZZ _ ComplexField :=
QQ _ ComplexField :=
RR _ ComplexField :=
CC _ ComplexField := (x,R) -> toCC(R.precision,x)

lift(RR,QQ) := opts -> (r,QQ) -> (
     if r == 0 then return 0/1;
     r' := r;
     p := precision r;
     p2 := 2^p;
     m := mutableIdentity(ZZ,2);
     while true do (
	  a := round r';
	  columnSwap(m,0,1);
	  columnAdd(m,0,a,1);
	  r' = r' - a;
	  n := m_(0,0);
	  d := m_(1,0);
	  q := n / d;
	  if r === numeric(p,q) then return q;
	  if r' == 0 or abs(n*d) > p2 then return promote(r,QQ);
	  r' = 1/r' ;
	  ))
lift(RR,ZZ) := opts -> (r,ZZ) -> (
     i := floor r; 
     if r == i then i 
     else if opts.Verify then error "lift: real number is not integer")
lift(CC,QQ) := lift(CC,ZZ) := opts -> (z,R) -> (
     if imaginaryPart z == 0 then lift(realPart z, R) 
     else if opts.Verify then error "lift: complex number not real"
     )
promote(RR,QQ) := (z,QQ) -> if z === 0. then 0/1 else if isFinite z then (
     (prec,sgn,expt,m,numbits) := partsRR z;
     sgn * m / 2^(numbits - expt)
     ) else error "promote(RR,QQ): non-finite number encountered"
liftable(RRi,QQ) := (z,RR) -> diameter(z) == 0
liftable(RRi,ZZ) := (z,RR) -> diameter(z) == 0
lift(RRi,QQ) := opts -> (r,QQ) -> (
     if diameter(r) == 0 then lift(midpoint(r),QQ)
     else if opts.Verify then error "lift: interval has positive diameter"
)
lift(RRi,ZZ) := opts -> (r,ZZ) -> (
     if diameter(r) == 0 then lift(midpoint(r),ZZ)
     else if opts.Verify then error "lift: interval has positive diameter"
)

ring RR := x -> new RealField of RR' from precision x
ring RRi := x -> new RealIntervalField of RRi' from precision x
ring CC := x -> new ComplexField of CC' from precision x

new RR from RawRingElement := (RRR,x) -> ( assert( RRR === RR ); rawToRR x)
new RRi from RawRingElement := (RRRi,x) -> ( assert( RRRi === RRi ); rawToRRi x)
new CC from RawRingElement := (CCC,x) -> ( assert( CCC === CC ); rawToCC x)

-- arithmetic operations

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
realPart Number := realPart0
imaginaryPart Number := imaginaryPart0
conjugate CC := z -> toCC(precision z, realPart z, - imaginaryPart z)
isConstant Number := i -> true

round RR := round CC := round0
round(ZZ,RR) := (n,x) -> (
     prec := precision x;
     p := (toRR(prec,10))^n;
     toRR(prec,round(x*p)/p))

random RR := RR => opts -> x -> x * rawRandomRR precision x
random(RR,RR) := opts -> (x,y) -> x + random(y-x)
RR'.random = opts -> R -> rawRandomRR R.precision
CC'.random = opts -> C -> rawRandomCC C.precision
random RingFamily := opts -> R -> random(default R,opts)

-- algebraic operations and functions

RR.isBasic = CC.isBasic = RRi.isBasic = true

InexactFieldFamily Array := (T,X) -> (default T) X
Thing ** InexactFieldFamily := (X,T) -> X ** default T

generators InexactField := opts -> R -> {}
isField InexactField := R -> true
degreeLength InexactField := R -> 0
frac InexactField := identity
numgens InexactField := R -> 0
dim InexactField := R -> 0
char InexactField := R -> 0

-- symbolic/numeric constant expressions

pi = new Constant from { symbol pi, pi0, piRRi0 }
EulerConstant = new Constant from { symbol EulerConstant, mpfrConstantEuler, eRRi0}
ii = new Constant from { symbol ii, ConstantII}

lngamma = method()
lngamma ZZ := lngamma QQ := lngamma RR := x -> (
     (y,s) := lgamma x;
     if s == -1 then y + ii * numeric_(precision y) pi else y
     )

expression Constant := hold
toString Constant := net Constant := c -> toString c#0
toExternalString Constant := c -> toString c#0
numeric Constant := c -> c#1 defaultPrecision
numeric(ZZ,Constant) := (prec,c) -> c#1 prec
numericInterval Constant := c -> if #c < 3 then interval(0,-1,Precision=>defaultPrecision) else c#2 defaultPrecision
numericInterval(ZZ,Constant) := (prec,c) -> if #c < 3 then interval(0,-1,Precision=>prec) else c#2 prec
exp Constant := c -> exp numeric c

constantTexMath := new HashTable from {
    symbol pi => "\\pi",
    symbol EulerConstant => "\\gamma",
    symbol ii => "\\mathbf{i}"
    }
texMath Constant := c -> if constantTexMath#?(c#0) then constantTexMath#(c#0) else texMath toString c#0

Constant + Constant := (c,d) -> numeric c + numeric d
Constant + RingElement := 
Constant + InexactNumber := (c,x) -> numeric(precision x,c) + x
RingElement + Constant :=
InexactNumber + Constant := (x,c) -> x + numeric(precision x,c)
+ Constant := c -> numeric c
- Constant := c -> - numeric c
Constant - Constant := (c,d) -> numeric c - numeric d
Constant - RingElement :=
Constant - InexactNumber := (c,x) -> numeric(precision x,c) - x
RingElement - Constant :=
InexactNumber - Constant := (x,c) -> x - numeric(precision x,c)
Constant * Constant := (c,d) -> numeric c * numeric d
Constant * RingElement :=
Constant * InexactNumber := (c,x) -> numeric(precision x,c) * x
RingElement * Constant :=
InexactNumber * Constant := (x,c) -> x * numeric(precision x,c)
Constant / Constant := (c,d) -> numeric d / numeric d
Constant / RingElement :=
Constant / InexactNumber := (c,x) -> numeric(precision x,c) / x
RingElement / Constant := (x,c) -> (1/numeric(precision x,c)) * x
InexactNumber / Constant := (x,c) -> x / numeric(precision x,c)
Constant ^ Constant := (c,d) -> (numeric c) ^ (numeric d)
Constant ^ InexactNumber := (c,x) -> (numeric(precision x,c)) ^ x
InexactNumber ^ Constant := (x,c) -> x ^ (numeric(precision x,c))

Constant == Constant := (c,d) -> numeric d == numeric d
Constant == RingElement :=
Constant == InexactNumber := (c,x) -> numeric(precision x,c) == x
RingElement == Constant :=
InexactNumber == Constant := (x,c) -> x == numeric(precision x,c)

Constant _ Ring := (c,R) -> (
     prec := precision R;
     if prec === infinity
     then error "cannot promote constant to a ring with exact arithmetic"
     else (numeric (prec, c))_R)
Constant _ InexactFieldFamily := (x,RR) -> x_(default RR)

Constant + Number := (c,x) -> numeric c + x
Number + Constant := (x,c) -> x + numeric c
- Constant := c -> - numeric c
Constant - Number := (c,x) -> numeric c - x
Number - Constant := (x,c) -> x - numeric c
Constant * Number := (c,x) -> numeric c * x
Number * Constant := (x,c) -> x * numeric c
Constant / Number := (c,x) -> numeric c / x
Number / Constant := (x,c) -> x / numeric c
Constant ^ Number := (c,x) -> (numeric c) ^ x
Number ^ Constant := (x,c) -> x ^ (numeric c)

Constant + InfiniteNumber := (c,x) -> x
InfiniteNumber + Constant := (x,c) -> x
Constant - InfiniteNumber := (c,x) -> -x
InfiniteNumber - Constant := (x,c) -> x
Constant * InfiniteNumber := (c,x) -> (numeric c)*x
InfiniteNumber * Constant := (x,c) -> x*(numeric c)
Constant // InfiniteNumber := Constant / InfiniteNumber := (c,x) -> 0
InfiniteNumber // Constant := (x,c) -> x // (numeric c)
InfiniteNumber / Constant := (x,c) -> x / (numeric c)
Constant ! := c -> (numeric c)!

-- printing

toString RealField := R -> concatenate("RR_",toString R.precision)
toString RealIntervalField := R -> concatenate("RRi_",toString R.precision)
toString ComplexField := R -> concatenate("CC_",toString R.precision)

expression RealField := R -> new Subscript from {symbol RR, R.precision}
expression RealIntervalField := R -> new Subscript from {symbol RRi, R.precision}
expression ComplexField := R -> new Subscript from {symbol CC, R.precision}
expression RR := x -> (
     if x < 0 
     then (
	  if x == -1 
	  then - expression 1
	  else new Minus from { -x }
	  )
     else (
	  if x == 1 then expression 1 
	  else new Holder from {x}
	  )
     )
expression CC := z -> (
     x := realPart z;
     y := imaginaryPart z;
     if y == 0 then expression x
     else if x == 0 
     then if y == 1 then hold ii
     else if y == -1 then - hold ii
     else y * hold ii
     else if y == -1 then x - hold ii
     else if y == 1 then x + hold ii
     else x + y * hold ii)
net InexactField := R -> net expression R
net CC := z -> simpleToString z
toExternalString RR := toExternalString0
toExternalString CC := toExternalString0
texMath CC := x -> texMath expression x
withFullPrecision = f -> (
     prec := printingPrecision;
     acc := printingAccuracy;
     printingPrecision = 0;
     printingAccuracy = -1;
     f();
     printingPrecision = prec;
     printingAccuracy = acc;				    -- sigh, what if an interrupt or an error occurred?
     )
InexactNumber#{Standard,Print} = x ->  withFullPrecision ( () -> Thing#{Standard,Print} x )
InexactNumber#{Standard,AfterPrint} = x -> (
     << endl;                             -- double space
     << concatenate(interpreterDepth:"o") << lineNumber;
     y := class x;
     << " : " << y;
     prec := precision x;
     -- if prec =!= defaultPrecision then
     << " (of precision " << prec << ")";
     -*
     while parent y =!= Thing do (
	  y = parent y;
	  << " < " << y;
	  );
     *-
     << endl;
     )

isReal = method()
isReal RRi := isReal RR := isReal QQ := isReal ZZ := x -> true
isReal CC := z -> imaginaryPart z == 0

acosh = method()
acosh Number := z -> log(z+sqrt(z^2-1))
asinh = method()
asinh Number := z -> log(z+sqrt(z^2+1))
atanh = method()
atanh Number := z -> log((1+z)/(1-z))/2
acoth = method()
acoth Number := z -> atanh(1/z)
acot = method()
acot Number := z -> atan(1/z)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
