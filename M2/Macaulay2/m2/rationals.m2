--		Copyright 1993-1999 by Daniel R. Grayson

ZZ.frac = QQ
QQ#1 = 1/1
QQ#0 = 0/1
QQ.char = 0
QQ.isField = true
toString QQ := x -> toString numerator x | "/" | toString denominator x
QQ.ConversionFormat = ConvertApply((x,y) -> x/y, ConvertInteger, ConvertInteger)
QQ.ConvertToExpression = ConvertApply((x,y)->expression(x/y),ConvertInteger,ConvertInteger)
QQ.pop = () -> eePop QQ.ConversionFormat
QQ.baseRings = {ZZ}
QQ.frac = QQ
QQ.random = () -> (random 21 - 10) / (random 9 + 1)
degree QQ := i -> {}
ggPush QQ := i -> (
     ggPush QQ, ggINT, gg numerator i, ggfromint, 
     ggPush QQ, ggINT, gg denominator i, ggfromint, 
     ggdiv)
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
QQ.handle = newHandle (ggZ, ggfractionfield)
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
