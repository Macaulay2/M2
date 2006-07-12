--		Copyright 1993-2002 by Daniel R. Grayson

RR.isBasic = true
RR#0 = 0.
RR#1 = 1.
RR.char = 0
RR.InverseMethod = x -> 1/x
RR.degreeLength = 0
RR.isField = true
RR.RawRing = rawRR 10.^-10
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

lift(RR,QQ) := (r,QQ) -> notImplemented() -- we should do a good rational approximation here by continued fractions

-- big reals: RRR

RRR.isBasic = true
RRR#0 = toRRR 0						    -- deceptive, since the precision gets fixed!
RRR#1 = toRRR 1
RRR.char = 0
RRR.InverseMethod = x -> 1/x
RRR.degreeLength = 0
RRR.isField = true
RRR.RawRing = rawRRR()
RRR.frac = RRR
RRR.baseRings = {}
RRR.dim = 0
RRR.char = 0
RRR.Engine = true
isConstant RRR := i -> true

promote(RRR,RRR) := (i,RRR) -> i
promote(RR,RRR) := 
promote(QQ,RRR) := 
promote(ZZ,RRR) := (i,RRR) -> toRRR i

lift(RRR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
liftable'(RRR,ZZ) := (r,ZZ) -> r == floor r

lift(RRR,RR) := (r,RR) -> notImplemented()
lift(RRR,QQ) := (r,QQ) -> notImplemented()
lift(RRR,ZZ) := (r,ZZ) -> notImplemented()

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
