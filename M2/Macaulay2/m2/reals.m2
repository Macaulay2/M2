--		Copyright 1993-2002 by Daniel R. Grayson

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

round = x -> floor(x + 0.5)

RR#0 = 0.
RR#1 = 1.
RR.isBasic = true
RR.InverseMethod = x -> 1/x
isConstant RR := i -> true

-----------------------------------------------------------------------------
-- ImmutableType

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType

-----------------------------------------------------------------------------

BigNumberType.synonym = "big number ring"
BigNumberType _ ZZ := (T,prec) -> new T.NumberRing of T from prec
Thing ** BigNumberType := (X,T) -> X ** T_53		    -- default precision (??)
dim BigNumberType := R -> 0
char BigNumberType := R -> 0
degreeLength BigNumberType := R -> 0
isField BigNumberType := R -> true
frac BigNumberType := R -> R

BigNumber.synonym = "big number"

BigNumberRing = new Type of Ring
BigNumberRing.synonym = "big number ring"

RealNumberRing = new Type of {* ImmutableType *} BigNumberRing
RealNumberRing.synonym = "real number ring"
new RealNumberRing of RR from ZZ := memoize (
     (RealNumberRing, RR,prec) -> newClass(RealNumberRing,RR,
	  hashTable { 
	       symbol precision => prec,
	       symbol Engine => true,
	       symbol baseRings => {},
	       symbol RawRing => rawRR prec
	       }))
raw RealNumberRing := R -> R.RawRing
isField RealNumberRing := R -> true
degreeLength RealNumberRing := R -> 0
liftable(ZZ,RealNumberRing) := 
liftable(RR,RealNumberRing) := 
liftable(QQ,RealNumberRing) := R -> true
ZZ _ RealNumberRing :=
QQ _ RealNumberRing :=
RR _ RealNumberRing :=
lift(ZZ,RealNumberRing) := 
lift(RR,RealNumberRing) := 
lift(QQ,RealNumberRing) := 
promote(ZZ,RealNumberRing) := 
promote(RR,RealNumberRing) := 
promote(QQ,RealNumberRing) := (x,R) -> toRR(R.precision,x)
frac RealNumberRing := identity
numgens RealNumberRing := R -> 0
dim RealNumberRing := R -> 0
char RealNumberRing := R -> 0
generators RealNumberRing := R -> {}
expression RealNumberRing := R -> new Subscript from {symbol RR, R.precision}
RR.NumberRing = RealNumberRing
net RealNumberRing := R -> net expression R
toString RealNumberRing := R -> concatenate("RR_",toString R.precision)
ring RR := x -> new RealNumberRing of RR from precision x

new RR from RawRingElement := (RR,x) -> rawToRR x

promote(RR,RR) := (i,RR) -> i
lift(RR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
liftable'(RR,ZZ) := (r,ZZ) -> r == floor r

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

lift(RR,QQ) := (r,QQ) -> approx(r,abs r / 2^(precision r - 16))
lift(RR,ZZ) := (r,ZZ) -> (i := floor r; if r == i then i else error "can't lift to ZZ")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
