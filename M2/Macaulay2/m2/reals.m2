--		Copyright 1994 by Daniel R. Grayson

RR#0 = 0.
RR#1 = 1.
RR.char = 0
RR.ConversionFormat = ConvertMissing
InverseMethod RR := x -> 1/x
net RR := string
RR.degreeLength = 0

RR == ZZ := (x,y) -> x === y+0.
ZZ == RR := (y,x) -> x === y+0.

RR == QQ := (x,r) -> x === r+0.
QQ == RR := (r,x) -> x === r+0.

document { quote RR,
     TT "RR", " -- the class of all real numbers.  It is a field.",
     PARA,
     "A real number is entered as a sequence of decimal digits with a point.",
     EXAMPLE "3.14159",
     PARA,
     SEEALSO {"basictype", "numbers"}
     }
