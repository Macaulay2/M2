--		Copyright 1994 by Daniel R. Grayson

ZZ.frac = QQ
QQ#1 = 1/1
QQ#0 = 0/1
QQ.char = 0
name QQ := x -> string numerator x | "/" | string denominator x
QQ.ConversionFormat = ConvertApply((x,y) -> x/y, ConvertInteger, ConvertInteger)
QQ.ConvertToExpression = ConvertApply((x,y)->expression(x/y),ConvertInteger,ConvertInteger)
QQ.pop = () -> eePop QQ.ConversionFormat
QQ.baseRings = {ZZ}
QQ.frac = QQ
QQ.random = () -> (random 21 - 10) / (random 9 + 1)
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
InverseMethod QQ := x -> 1/x
QQ.dim = 0
QQ == ZZ := (r,i) -> r == i/1
ZZ == QQ := (i,r) -> r == i/1

QQ.Engine = true
QQ.handle = newHandle (ggZ, ggfractionfield)
assert (hash ZZ < hash QQ)
promote(ZZ,QQ) := (n,QQ) -> n/1
lift(QQ,ZZ) := r -> (
     if denominator r === 1 then numerator r 
     else error "rational number is not an integer"
     )
promote(QQ,QQ) := (r,QQ) -> r
lift(QQ,QQ) := (r,QQ) -> r
QQ.degreeLength = 0

document { quote numerator,
     TT "numerator x", " -- provides the numerator of a fraction.",
     PARA,
     EXAMPLE "numerator (4/6)"
     }

document { quote denominator,
     TT "denominator x", " -- provides the denominator of a fraction.",
     PARA,
     EXAMPLE "denominator (4/6)"
     }

document { quote QQ,
     TT "QQ", " -- denotes the class of all rational numbers.",
     PARA,
     EXAMPLE "1/2 + 3/5",
     PARA,
     "Functions:",
     MENU {
	  TO "denominator",
	  TO "numerator"
	  },
     PARA,
     SEEALSO{"numbers", "arithmetic functions"}
     }

