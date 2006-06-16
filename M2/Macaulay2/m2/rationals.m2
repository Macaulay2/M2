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

ring QQ := class

ZZ.frac = QQ
QQ#1 = 1/1
QQ#0 = 0/1
QQ.char = 0
QQ.isField = true
toString QQ := x -> toString numerator x | "/" | toString denominator x
QQ.baseRings = {ZZ}
QQ.mathML = "<mi>&Qopf;</mi>"
QQ.frac = QQ

QQ.random = () -> (random 21 - 10) / (random 9 + 1)
degree QQ := i -> {}
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
promote(ZZ,QQ) := (n,QQ) -> n/1
lift(QQ,ZZ) := (r,o) -> (
     if denominator r === 1 then numerator r 
     else error "rational number is not an integer"
     )
promote(QQ,QQ) := (r,QQ) -> r
lift(QQ,QQ) := (r,QQ) -> r
QQ.degreeLength = 0
isUnit QQ := x -> x != 0

isConstant QQ := i -> true

promote(QQ,QQ) := (i,o) -> i
promote(ZZ,QQ) := (i,o) -> i/1
promote(QQ,Ring) := (r,S) -> promote(r,S#0)


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
