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

BigRR#0 = 0.
BigRR#1 = 1.
BigRR.char = 0
BigRR.InverseMethod = x -> 1/x
BigRR.degreeLength = 0
BigRR.isField = true
BigRR.RawRing = rawBigRR()
BigRR.frac = BigRR
BigRR.baseRings = {}
BigRR.dim = 0
BigRR.char = 0
BigRR.Engine = true
degree BigRR := i -> {}
isConstant BigRR := i -> true

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
