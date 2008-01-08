--		Copyright 1993-2002 by Daniel R. Grayson

RR.isBasic = true
RR#0 = 0.
RR#1 = 1.
RR.char = 0
RR.InverseMethod = x -> 1/x
RR.degreeLength = 0
RR.isField = true
RR.RawRing = rawRR()
RR.frac = RR
RR.baseRings = {}
RR.dim = 0
RR.char = 0
RR.Engine = true

scan((QQ,RR,CC,RRR,CCC), F -> (
	  F // F := (x,y) -> if y == 0 then 0_F else x/y;
	  F % F := (x,y) -> if y == 0 then x else 0_F;
	  F // ZZ := (x,y) -> x // y_F;
	  F % ZZ := (x,y) -> x % y_F;
	  ))

scan((RR,CC,RRR,CCC), F -> (
	  F // QQ := (x,y) -> x // y_F;
	  F % QQ := (x,y) -> x % y_F;
	  ))

scan((CC,RRR,CCC), F -> (
	  F // RR := (x,y) -> x // y_F;
	  F % RR := (x,y) -> x % y_F;
	  ))

	  CCC // RRR := (x,y) -> x // y_CCC;
	  CCC % RRR := (x,y) -> x % y_CCC;
	  CCC // CC := (x,y) -> x // y_CCC;
	  CCC % CC := (x,y) -> x % y_CCC;

new RR from RawRingElement := (RR,x) -> rawToReal x

RR == ZZ := (x,y) -> x === y+0.
ZZ == RR := (y,x) -> x === y+0.

RR == QQ := (x,r) -> x === r+0.
QQ == RR := (r,x) -> x === r+0.

isConstant RR := i -> true

round = x -> floor(x + 0.5)

promote(RR,RR) := (i,RR) -> i
promote(QQ,RR) := 
promote(ZZ,RR) := (i,RR) -> i + 0.

lift(RR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
liftable'(RR,ZZ) := (r,ZZ) -> r == floor r

-- big reals: RRR

RRR.isBasic = true
RRR#0 = 0						    -- deceptive, since it's an integer
RRR#1 = 1
RRR.char = 0
RRR.InverseMethod = x -> 1/x
RRR.degreeLength = 0
RRR.isField = true
-- RRR.RawRing = rawRRR()
RRR.frac = RRR
RRR.baseRings = {}
RRR.dim = 0
RRR.char = 0
RRR.Engine = true
isConstant RRR := i -> true

-----------------------------------------------------------------------------
-- ImmutableType

ImmutableType = new Type of HashTable
ImmutableType.synonym = "immutable type"
globalAssignment ImmutableType

-----------------------------------------------------------------------------

BigNumberRing.synonym = "big number ring"
BigNumber.synonym = "big number"
RealNumberRing = new Type of ImmutableType
RealNumberRing.synonym = "real number ring"
new RealNumberRing {* of RRR *} from ZZ := (RealNumberRing {* ,RRR *} ,prec) -> hashTable { 
     symbol precision => prec,
     symbol RawRing => rawRRR prec
     }
raw RealNumberRing := R -> R.RawRing
isField RealNumberRing := R -> true
degreeLength RealNumberRing := R -> 0
liftable(ZZ,RealNumberRing) := 
liftable(RR,RealNumberRing) := 
liftable(RRR,RealNumberRing) := 
liftable(QQ,RealNumberRing) := R -> true
ZZ _ RealNumberRing :=
QQ _ RealNumberRing :=
RR _ RealNumberRing :=
RRR _ RealNumberRing :=
lift(ZZ,RealNumberRing) := 
lift(RR,RealNumberRing) := 
lift(RRR,RealNumberRing) := 
lift(QQ,RealNumberRing) := 
promote(ZZ,RealNumberRing) := 
promote(RR,RealNumberRing) := 
promote(RRR,RealNumberRing) := 
promote(QQ,RealNumberRing) := (x,R) -> toRRR(R.precision,x)
frac RealNumberRing := identity
numgens RealNumberRing := R -> 0
dim RealNumberRing := R -> 0
char RealNumberRing := R -> 0
generators RealNumberRing := R -> {}
expression RealNumberRing := R -> new Subscript from {symbol RRR, R.precision}
BigNumberRing _ ZZ := (T,prec) -> new T.NumberRing of T from prec
RRR.NumberRing = RealNumberRing
net RealNumberRing := R -> net expression R
toString RealNumberRing := R -> concatenate("RRR ",toString R.precision)
ring RRR := x -> new RealNumberRing {* of RRR *} from precision x

new RRR from RawRingElement := (RRR,x) -> rawToRRR x

promote(RRR,RRR) := (i,RRR) -> i

-- promote(QQ,RRR) := promote(ZZ,RRR) := (i,RRR) -> toRRR i
promote(RRR,RR) := (i,RR) -> toRR i	    -- which should it be?
-- lift(RR,RRR) := (i,RRR) -> toRRR i

lift(RRR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
liftable'(RRR,ZZ) := (r,ZZ) -> r == floor r

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

lift(RR,QQ) := (r,QQ) -> approx(r,abs r * 10.^-14)
lift(RRR,QQ) := (r,QQ) -> approx(r,abs r / 2^(precision r - 16))

lift(RRR,RR) := (r,RR) -> notImplemented()
lift(RRR,ZZ) := (r,ZZ) -> (i := floor r; if r == i then i else error "can't lift to ZZ")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
