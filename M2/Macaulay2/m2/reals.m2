--		Copyright 1993-1999 by Daniel R. Grayson

RR#0 = 0.
RR#1 = 1.
RR.char = 0
RR.ConversionFormat = ConvertMissing
InverseMethod RR := x -> 1/x
RR.degreeLength = 0

RR == ZZ := (x,y) -> x === y+0.
ZZ == RR := (y,x) -> x === y+0.

RR == QQ := (x,r) -> x === r+0.
QQ == RR := (r,x) -> x === r+0.

