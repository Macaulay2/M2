--		Copyright 1993-2002 by Daniel R. Grayson

RR#0 = 0.
RR#1 = 1.
RR.char = 0
RR.InverseMethod = x -> 1/x
RR.degreeLength = 0
RR.isField = true

RR.frac = RR
RR.baseRings = {}
RR.dim = 1
RR.char = 0
RR.Engine = true
new RR := RR -> RR.pop()
degree RR := i -> {}

RR == ZZ := (x,y) -> x === y+0.
ZZ == RR := (y,x) -> x === y+0.

RR == QQ := (x,r) -> x === r+0.
QQ == RR := (r,x) -> x === r+0.

isConstant RR := i -> true
