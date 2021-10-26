
restart
setRandomSeed 0
needsPackage "NumericalAlgebraicGeometry"
printingPrecision=30
R = CC_200[x]
F = x^7-2*x^3-x-1
S = {F}
pts = solveSystem(S, Software=>BERTINI)
pts = refine(S, pts, Bits=>180, Software=>BERTINI)
-- pts = refine(S, pts, Bits=>180) -- this one is fine 
pts/coordinates
flatten entries  matrix oo
oo/(a -> sub(F, {x=>a}))
netList oo
-- notice the lack of precision in one of the rows, is this a BUG?
-- looks like it is a bug in... bertini (see main_data)