--		Copyright 1993-2002 by Daniel R. Grayson


QQ.RawRing = rawQQ()
QQ.isBasic = true
-- from raw QQ to toplevel QQ
--MES removed --new QQ from RawRingElement := (QQ,x) -> rawToInteger rawNumerator x / rawToInteger rawDenominator x
new QQ from RawRingElement := (QQ,x) -> rawToRational x
-- from toplevel QQ to raw QQ
raw QQ := x -> rawFraction(
     QQ.RawRing,
     rawFromNumber(rawZZ(), numerator x),
     rawFromNumber(rawZZ(), denominator x))

ZZ.frac = QQ
QQ#1 = 1/1
QQ#0 = 0/1
QQ.char = 0
QQ.isField = true
toString QQ := x -> toString numerator x | "/" | toString denominator x
QQ.baseRings = {ZZ}
QQ.frac = QQ

smallrats := {-10/1, -5/1, -10/3, -5/2, -2/1, -5/3, -10/7, -5/4, -10/9, -9/1, -9/2, -3/1, -9/4, -9/5, -3/2,
      -9/7, -9/8, -1/1, -8/1, -4/1, -8/3, -8/5, -4/3, -8/7, -8/9, -7/1, -7/2, -7/3, -7/4, -7/5,
      -7/6, -7/8, -7/9, -6/1, -6/5, -6/7, -3/4, -2/3, -5/6, -5/7, -5/8, -5/9, -4/5, -4/7, -1/2,
      -4/9, -3/5, -3/7, -3/8, -1/3, -2/5, -2/7, -1/4, -2/9, -1/5, -1/6, -1/7, -1/8, -1/9, 0/1, 1/1,
      1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 2/1, 2/3, 2/5, 2/7, 2/9, 3/1, 3/2, 3/4, 3/5, 3/7, 3/8,
      4/1, 4/3, 4/5, 4/7, 4/9, 5/1, 5/2, 5/3, 5/4, 5/6, 5/7, 5/8, 5/9, 6/1, 6/5, 6/7, 7/1, 7/2, 7/3,
      7/4, 7/5, 7/6, 7/8, 7/9, 8/1, 8/3, 8/5, 8/7, 8/9, 9/1, 9/2, 9/4, 9/5, 9/7, 9/8, 10/1, 10/3,
      10/7, 10/9}

QQ.random = () -> smallrats#(random (#smallrats))
expression QQ := r -> (
     n := numerator r;
     d := denominator r;
     if n < 0 
     then -((expression (-n))/(expression d))
     else (expression n)/(expression d)
     )
net QQ := r -> net expression r
QQ.InverseMethod = x -> 1/x
QQ.dim = 0
QQ == ZZ := (r,i) -> r == i/1
ZZ == QQ := (i,r) -> r == i/1

QQ.Engine = true
assert (hash ZZ < hash QQ)

lift(QQ,ZZ) := (r,o) -> if denominator r === 1 then numerator r else error "rational number is not an integer"
liftable(QQ,ZZ) := (r,o) -> denominator r === 1
lift(QQ,QQ) := (r,QQ) -> r
liftable(QQ,QQ) := (QQ,QQ) -> true

QQ.degreeLength = 0
isUnit Number := x -> x != 0

isConstant QQ := i -> true

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
