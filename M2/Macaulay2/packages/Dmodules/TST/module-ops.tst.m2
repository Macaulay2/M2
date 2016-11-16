restart
needsPackage "Dmodules"
W = makeWA(QQ[x,y])
M1 = x*W^1 -- makes no sense?
M2 = W^1*x 
f = map(W^1,W^1,x)
M3 = image f
ann M1
N = W^1*dy*x+W^1*dy*y
MN = (W^1*x+W^1*y)/N
isWellDefined MN
mingens ann MN

-- isWeylAlgebra

assert isWeylAlgebra W
assert not isWeylAlgebra ZZ
assert not isWeylAlgebra QQ
assert not isWeylAlgebra (ZZ/101)
assert not isWeylAlgebra (QQ[x]/x^23)
assert not isWeylAlgebra (
    S = QQ[x,y,SkewCommutative=>true]
    )
WH = QQ[x,dx,h,WeylAlgebra=>{x=>dx, h}]
assert isWeylAlgebra WH

-- Does "prune", "ker", work all the time? 
-- Relation to "Dprune"?
