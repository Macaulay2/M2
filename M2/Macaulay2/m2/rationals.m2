--		Copyright 1993-2002 by Daniel R. Grayson

needs "expressions.m2"
needs "integers.m2"

-----------------------------------------------------------------------------
-- QQ
-----------------------------------------------------------------------------

QQ.synonym = "rational number"
QQ.texMath = ///{\mathbb Q}///

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
QQ.baseRings = {ZZ}
QQ.frac = QQ
round QQ := x -> floor(x + 1/2)
QQ.random = opts -> QQ -> rawRandomQQ opts.Height
promote(ZZ,QQ) := (x,QQ) -> x/1
promote(QQ,QQ) := (x,QQ) -> x
expression QQ := r -> (
     n := numerator r;
     d := denominator r;
     if n < 0 
     then -((expression (-n))/(expression d))
     else (expression n)/(expression d)
     )
toString QQ := r -> if denominator r === 1 then simpleToString numerator r else simpleToString r
net QQ := r -> net expression r
texMath QQ := r -> texMath expression r
QQ.InverseMethod = x -> 1/x
QQ.dim = 0
QQ.Engine = true
assert (hash ZZ < hash QQ)

lift(QQ,ZZ) := opts -> (r,o) -> if denominator r === 1 then numerator r else if opts.Verify then error "rational number is not an integer"
liftable(QQ,ZZ) := (r,o) -> denominator r === 1
lift(QQ,QQ) := opts -> (r,QQ) -> r

QQ.degreeLength = 0
isUnit Number := x -> x != 0

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
