--		Copyright 1993-2008 by Daniel R. Grayson

needs "enginering.m2"

-- ImmutableType

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType

-- these types are pre-defined

RR.synonym = "real number"
RRi.synonym = "real interval"
CC.synonym = "complex number"
CCi.synonym = "complex interval"
RR.texMath = ///{\mathbb R}///
RRi.texMath = ///{\square\mathbb R}///
CC.texMath = ///{\mathbb C}///
CCi.texMath = ///{\square\mathbb C}///
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
CCi.InexactField = ComplexIntervalField = new Type of InexactField; ComplexIntervalField.synonym = "complex interval field"

RingFamily_* := RR -> RR#(symbol _*)
RingFamily_* := RRi -> RRi#(symbol _*)
RingFamily_* := CC -> CC#(symbol _*)
RingFamily_* := CCi -> CCi#(symbol _*)
RingFamily_* = (RR,e) -> RR#(symbol _*) = e
RingFamily_* = (RRi,e) -> RRi#(symbol _*) = e
RingFamily_* = (CC,e) -> CC#(symbol _*) = e
RingFamily_* = (CCi,e) -> CCi#(symbol _*) = e
InexactNumber' = new Type of Number
RR_* = RR' = new Type of InexactNumber'
RRi_* = RRi' = new Type of InexactNumber'
CC_* = CC' = new Type of InexactNumber'
CCi_* = CCi' = new Type of InexactNumber'

RR'.texMath = ///{\mathbb R}_*///
RRi'.texMath = ///{\square\mathbb R}_*///
CC'.texMath = ///{\mathbb C}_*///
CCi'.texMath = ///{\square\mathbb C}_*///

setAttribute(CC',PrintNet,"CC" | "*"^-1)
setAttribute(CCi',PrintNet,"CCi" | "*"^-1)
setAttribute(RR',PrintNet,"RR" | "*"^-1)
setAttribute(RRi',PrintNet,"RRi" | "*"^-1)
setAttribute(CC',PrintNames,"CC_*")
setAttribute(CCi',PrintNames,"CCi_*")
setAttribute(RR',PrintNames,"RR_*")
setAttribute(RRi',PrintNames,"RRi_*")
setAttribute(InexactNumber',PrintNet,"InexactNumber" | "*"^-1)

protect back
RR'.back = RR
RRi'.back = RRi
CC'.back = CC
CCi'.back = CCi
new RealField of Number from ZZ := memoize (
     (RealField,Number,prec) -> newClass(RealField,Number,
	  hashTable { 
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {ZZ,QQ},
	       symbol isBasic => true,
	       symbol RawRing => rawRR prec
	       }))
new ComplexField of Number from ZZ := memoize(
     (ComplexField,Number,prec) -> newClass(ComplexField,Number,
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol isBasic => true,
	       symbol baseRings => {ZZ,QQ,RR_prec},
	       symbol RawRing => rawCC prec
	       }))
new RealIntervalField of Number from ZZ := memoize (
     (RealIntervalField,Number,prec) -> newClass(RealIntervalField,Number,
	  hashTable {
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {ZZ,QQ,RR_prec},
	       symbol isBasic => true,
	       symbol RawRing => rawRRi prec
	       }))
new ComplexIntervalField of Number from ZZ := memoize (
     (ComplexIntervalField,Number,prec) -> newClass(ComplexIntervalField,Number,
      hashTable {
           symbol precision => prec,
           symbol Engine => true,
           symbol baseRings => {ZZ,QQ,RR_prec,CC_prec,RRi_prec},
           symbol isBasic => true,
           symbol RawRing => rawCCi prec
           }))
precision InexactField := R -> R.precision
InexactFieldFamily _ ZZ := (T,prec) -> new T.InexactField of T#(symbol _*) from prec -- oops...
default InexactFieldFamily := R -> R_defaultPrecision

diameter' = diameter
diameter = method()
diameter RRi := diameter'
diameter CCi := diameter'

-- lift and promote between real or complex rings
promote(RawRingElement,RR') := (x,R) -> new RR from x
promote(RawRingElement,RRi') := (x,R) -> new RRi from x
promote(RawRingElement,CC') := (x,R) -> new CC from x
promote(RawRingElement,CCi') := (x,R) -> new CCi from x
promote(RawRingElement,Number) := (x,R) -> new R from x
promote(RawRingElement,RingElement) := (x,R) -> new R from x
promote(QQ,RR) :=
promote(ZZ,RR) := (x,RR) -> promote(x,default RR)
promote(Number,CC) := (x,CC) -> promote(x,default CC)
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
promote(ZZ,CCi') :=
promote(QQ,CCi') := (i,K) -> toCCi(toRRi(i),interval 0)
promote(RR,CCi') := (i,K) -> toCCi(toRRi(precision i,i,i),toRRi(precision i, 0,0))
promote(RRi,CCi') := (i,K) -> toCCi(i, interval 0)
promote(CC,CCi') := (i,K) -> toCCi(toRRi(precision i,realPart i,realPart i),toRRi(precision i, imaginaryPart i, imaginaryPart i))
promote(CCi,CCi') := (i,K) -> toCCi(realPart i, imaginaryPart i) -- this should be fixed
lift(RingElement, InexactNumber) :=
lift(Number,      InexactNumber) := opts -> (x, K) -> lift(x, default K, opts)

liftable(Number,InexactNumber) := (x,RR) -> liftable(x,default RR)
liftable(CC,  RR')  :=
liftable(CC,  RRi') :=
liftable(CCi, RRi') := (x, K) -> imaginaryPart x == 0
liftable(RRi, RR')  :=
liftable(CCi, CC')  := (x, K) -> diameter x == 0
liftable(CCi, RR') := (z, K) -> (
    imaginaryPart z == 0 and diameter realPart z == 0)

lift(RR, RR') := opts -> (x, K) -> numeric(precision K, x)
lift(RRi, RR') := opts -> (x, K) -> (
    if liftable(x, K) then lift(midpoint x, K)
    else if opts.Verify then error "lift: interval has positive diameter")
lift(CC, RR') := opts -> (x, K) -> (
    if liftable(x, K) then lift(realPart x, K)
    else if opts.Verify then error "lift: complex number is not real")
lift(CCi,RR') := opts -> (x, K) -> (
    if liftable(x, K) then lift(midpoint realPart x, K)
    else if opts.Verify then error "lift: complex interval not a real number")

lift(CC, CC') := opts -> (x, K) -> (
    toCC(precision K, realPart x, imaginaryPart x))
lift(CCi, CC') := opts -> (x, K) -> (
    if liftable(x, K)
    then toCC(precision K, midpoint realPart x, midpoint imaginaryPart x)
    else if opts.Verify then error "lift: interval has positive diameter")

lift(RRi, RRi') := opts -> (x, K) -> toRRi(precision K, left x, right x)
lift(CC, RRi') := opts -> (x, K) -> (
    if liftable(x, K) then toRRi(precision K, realPart x, realPart x)
    else if opts.Verify then error "lift: complex number is not real")
lift(CCi, RRi') := opts -> (x, K) -> (
    if liftable(x, K) then toRRi(precision K, left realPart x, right realPart x)
    else if opts.Verify then error "lift: complex interval is not real")

lift(CCi, CCi') := opts -> (x, K) -> (
    toCCi(precision K, realPart x, imaginaryPart x))

-- lift and promote to and from other rings

numeric VisibleList := x -> apply(x,numeric)
numeric(ZZ,VisibleList) := (prec,x) -> apply(x, t -> numeric(prec,t))
numeric Number := x -> numeric(defaultPrecision, x)
numeric CC := identity
numeric RR := identity
numeric RRi := identity
numeric CCi := identity
numeric(ZZ,Number) := toRR
numeric(ZZ,RRi) := (prec,x) -> toRRi(prec,left(x),right(x))
numeric(ZZ,CCi) := (prec,x) -> toCCi(prec,realPart(x),imaginaryPart(x))
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
ZZ _ ComplexIntervalField :=
QQ _ ComplexIntervalField :=
RR _ ComplexIntervalField :=
RRi _ ComplexIntervalField := (x,R) -> toCCi(R.precision,x, interval 0)
CC _ ComplexIntervalField := (x,R) -> toCCi(R.precision,x)
CCi _ ComplexIntervalField := (x,R) -> toCCi(R.precision,x)

internalRepresentation = z -> if z === 0. then 0/1 else if isFinite z then (
     (prec,sgn,expt,m,numbits) := partsRR z;
     sgn * m / 2^(numbits - expt)
     )
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
	  if r' == 0 or abs(n*d) > p2 then return internalRepresentation r;
	  r' = 1/r' ;
	  ))
lift(RR,ZZ) := opts -> (r,ZZ) -> (
     i := floor r; 
     if r == i then i 
     else if opts.Verify then error "lift: real number is not integer")
lift(CC,QQ) := lift(CC,ZZ) := opts -> (z,R) -> (
     if imaginaryPart z == 0 then lift(realPart z, R, opts)
     else if opts.Verify then error "lift: complex number not real"
     )
liftable(RRi,QQ) := (z,RR) -> diameter(z) == 0
lift(RRi,QQ) := opts -> (r,QQ) -> (
     if diameter(r) == 0 then lift(midpoint(r),QQ)
     else if opts.Verify then error "lift: interval has positive diameter"
)
lift(RRi,ZZ) := opts -> (r,ZZ) -> (
     if diameter(r) == 0 then lift(midpoint(r),ZZ, opts)
     else if opts.Verify then error "lift: interval has positive diameter"
)

liftable(CCi, QQ) := (x, K) -> imaginaryPart x == 0 and diameter x == 0
lift(CCi, QQ) := opts -> (x, K) -> (
    if liftable(x, K) then lift(realPart x, K)
    else if opts.Verify then error "lift: complex interval not rational number")
lift(CCi, ZZ) := opts -> (x, R) -> (
    if liftable(x, QQ) then lift(midpoint realPart x, ZZ, opts)
    else if opts.Verify then error "lift: complex interval not an integer")

lift(Constant, Number) := opts -> (x, R) -> (
    lift(numeric(precision R, x), R, opts))
lift(Constant, InexactNumber) := opts -> (x, R) -> (
    lift(numeric(precision default R, x), default R, opts))

ring RR := x -> new RealField of RR' from precision x
ring RRi := x -> new RealIntervalField of RRi' from precision x
ring CC := x -> new ComplexField of CC' from precision x
ring CCi := x -> new ComplexIntervalField of CCi' from precision x

new RR from RawRingElement := (RRR,x) -> ( assert( RRR === RR ); rawToRR x)
new RRi from RawRingElement := (RRRi,x) -> ( assert( RRRi === RRi ); rawToRRi x)
new CC from RawRingElement := (CCC,x) -> ( assert( CCC === CC ); rawToCC x)
new CCi from RawRingElement := (CCCi,x) -> ( assert( CCCi === CCi ); rawToCCi x)

-- arithmetic operations

CC.InverseMethod = y -> conjugate y / y^2

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
realPart ZZ :=
realPart QQ :=
realPart InexactNumber := realPart0
realPart Number := realPart0 @@ numeric

imaginaryPart ZZ :=
imaginaryPart QQ :=
imaginaryPart InexactNumber := imaginaryPart0
imaginaryPart Number := imaginaryPart0 @@ numeric

left Number := identity
left RRi := left0
left CCi := x -> error "use lowerLeft or upperLeft"

right Number := identity
right RRi := right0
right CCi := x -> error "use lowerRight or upperRight"

lowerLeft Number := identity
lowerLeft RRi := left
lowerLeft CCi := z -> (left realPart z) + (left imaginaryPart z)*ii

lowerRight Number := identity
lowerRight RRi := right
lowerRight CCi := z -> (right realPart z) + (left imaginaryPart z)*ii

upperLeft Number := identity
upperLeft RRi := left
upperLeft CCi := z -> (left realPart z) + (right imaginaryPart z)*ii

upperRight Number := identity
upperRight RRi := right
upperRight CCi := z -> (right realPart z) + (right imaginaryPart z)*ii

conjugate CC := z -> toCC(precision z, realPart z, - imaginaryPart z)
conjugate CCi := z -> toCCi(precision z, realPart z, - imaginaryPart z)
conjugate Constant := conjugate @@ numeric

isConstant Number := i -> true

round Number := round0 @@ numeric
round(ZZ,RR) := (n,x) -> (
     prec := precision x;
     p := (toRR(prec,10))^n;
     toRR(prec,round(x*p)/p))
round(ZZ, CC) := (n, x) -> toCC(round(n, realPart x), round(n, imaginaryPart x))
round(ZZ, RRi) := (n, x) -> toRRi(round(n, left x), round(n, right x))
round(ZZ, Number) := (n, x) -> round(n, numeric x)

truncate Number := {} >> o -> x -> (
    if x >= 0 then floor x
    else if x < 0 then ceiling x
    else 0) -- e.g., RRi's containing 0 as interior pt

random RR := RR => opts -> x -> x * rawRandomRRUniform precision x
random(RR,RR) := opts -> (x,y) -> x + random(y-x)
RR'.random = opts -> R -> rawRandomRRUniform R.precision
CC'.random = opts -> C -> rawRandomCC C.precision
RRi'.random = opts -> R -> rawRandomRRi R.precision
CCi'.random = opts -> C -> rawRandomCCi C.precision
random RingFamily := opts -> R -> random(default R,opts)

random QQ := QQ => opts -> x -> rawFareyApproximation(
    random numeric x, opts.Height)

-- algebraic operations and functions

RR.isBasic = CC.isBasic = RRi.isBasic = CCi.isBasic = true

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
CatalanConstant = new Constant from { symbol CatalanConstant, mpfrConstantCatalan, cRRi0}
ii = new Constant from { symbol ii, ConstantII, p -> toCCi(p, 0, 1)}

ring Constant := ring @@ numeric
promote(Constant, InexactNumber') :=
promote(Constant, RingElement)    := (x, R) -> (
    promote(numeric(precision R, x), R))

isFinite Constant := x -> true

lngamma = method()
lngamma RR := x -> (
     (y,s) := lgamma x;
     if s == -1 then y + ii * numeric_(precision y) pi else y
     )
lngamma Number := lngamma @@ numeric
lngamma RRi := lngamma CC := lngamma CCi := lgamma

expression Constant := hold
toString Constant := net Constant := c -> toString c#0
toExternalString Constant := c -> toString c#0
numeric Constant := c -> c#1 defaultPrecision
numeric(ZZ,Constant) := (prec,c) -> c#1 prec
numeric(InfiniteNumber, Constant) := (prec, c) -> numeric c
numericInterval Constant := c -> if #c < 3 then interval(0,-1,Precision=>defaultPrecision) else c#2 defaultPrecision
numericInterval(ZZ,Constant) := (prec,c) -> if #c < 3 then interval(0,-1,Precision=>prec) else c#2 prec

constantTexMath := new HashTable from {
    symbol pi => "\\pi",
    symbol EulerConstant => "\\gamma",
    symbol CatalanConstant => "\\mathrm{G}",
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

Constant == Constant := (c,d) -> numeric c == numeric d
Constant == RingElement :=
Constant == InexactNumber := (c,x) -> numeric(precision x,c) == x
RingElement == Constant :=
InexactNumber == Constant := (x,c) -> x == numeric(precision x,c)
Constant ? Constant := (c,d) -> numeric c ? numeric d
InexactNumber ? Constant := (x,c) -> x ? numeric(precision x,c)
Constant ? InexactNumber := (c,x) -> numeric(precision x,c) ? x

Constant _ Ring := (c,R) -> (
     prec := precision R;
     if prec === infinity
     then error "cannot promote constant to a ring with exact arithmetic"
     else (numeric (prec, c))_R)
-- e.g. 1_CC or ii_CC
Number   _ RingFamily :=
Constant _ RingFamily := (x, R) -> x_(default R)
-- TODO: find examples, or remove
Number      ^ RingFamily :=
RingElement ^ RingFamily := (x, R) -> lift(x, default R) -- TODO: set Verify => false?

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

Constant == Number := (c,x) -> numeric c == x
Number == Constant := (x,c) -> x == numeric c
Constant ? Number := (c,x) -> numeric c ? x
Number ? Constant := (x,c) -> x ? numeric c

Constant ! := c -> (numeric c)!

-- printing

toString InexactField := R -> concatenate(
    toString (parent R).back, "_", toString R.precision)
expression InexactField := R -> Subscript((parent R).back, R.precision)

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
     if x == 0 then x=0;
     if y == 0 or abs y < abs x * 2^(-precision z) then y=0;
     x + y * hold ii)
net InexactField := R -> net expression R
net CC := z -> simpleToString z
toExternalString RR := toExternalString0
toExternalString CC := toExternalString0
texMath CC := x -> texMath expression x
texMath RR := x -> (
    if not isANumber x then texMath toString x else
    if    isInfinite x then texMath(if x > 0 then infinity else -infinity)
    else (
	s := simpleToString x;
	r := regex("(-?\\d*)(?:\\.(\\d*)|)(?:"|regexQuote printingSeparator|"(-?\\d+)|)",s);
	if r === null then return s; -- shouldn't happen
	ss := substring(r#1,s);
	if ss=="1" and r#2#1==0 and r#3#1>0 then "10^{"|substring(r#3,s)|"}"
	else if ss=="-1" and r#2#1==0 and r#3#1>0 then "-10^{"|substring(r#3,s)|"}"
	else concatenate (
	    "{",
	    (lookup(texMath,ZZ)) ss,
	    if r#2#1>0 then "."|substring(r#2,s),
	    "}",
	    if r#3#1>0 then "\\cdot 10^{"|substring(r#3,s)|"}"
	    )
	)
    )
texMath RRi := x -> concatenate("\\big[",texMath left x,",",texMath right x,"\\big]",if isEmpty x then "\\text{ (an empty interval)}")
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
InexactNumber#AfterPrint = x ->  (class x," (of precision ",precision x,")")

isReal = method()
isReal RRi := isReal RR := isReal QQ := isReal ZZ := x -> true
isReal CC := z -> imaginaryPart z == 0
isReal CCi := z -> imaginaryPart z == 0
isReal Constant := isReal @@ numeric
isReal InfiniteNumber := x -> false

isInfinite' = isInfinite
isInfinite = method()
isInfinite Number := isInfinite'
isInfinite Constant := isInfinite @@ numeric
isInfinite InfiniteNumber := x -> true

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

BesselJ' = BesselJ
BesselJ = method()
BesselJ(ZZ, Number) := (n, x) -> BesselJ'(n, numeric x)
BesselJ(Number, Number) := (n, x) -> BesselJ'(numeric n, numeric x)

BesselY' = BesselY
BesselY = method()
BesselY(ZZ, Number) := (n, x) -> BesselY'(n, numeric x)
BesselY(Number, Number) := (n, x) -> BesselY'(numeric n, numeric x)

ring ComplexField := R -> CC
ring RealField := R -> RR
ring RealIntervalField := R -> RRi

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
