--		Copyright 1993-2002 by Daniel R. Grayson

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
-- new RR := RR -> RR.pop()
degree RR := i -> {}

RR == ZZ := (x,y) -> x === y+0.
ZZ == RR := (y,x) -> x === y+0.

RR == QQ := (x,r) -> x === r+0.
QQ == RR := (r,x) -> x === r+0.

isConstant RR := i -> true

BigReal#0 = 0.
BigReal#1 = 1.
BigReal.char = 0
BigReal.InverseMethod = x -> 1/x
BigReal.degreeLength = 0
BigReal.isField = true
BigReal.RawRing = rawBigRR()
BigReal.frac = BigReal
BigReal.baseRings = {}
BigReal.dim = 0
BigReal.char = 0
BigReal.Engine = true
degree BigReal := i -> {}
isConstant BigReal := i -> true

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
