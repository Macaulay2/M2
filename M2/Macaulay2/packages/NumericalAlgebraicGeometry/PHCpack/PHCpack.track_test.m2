restart
needsPackage "NumericalAlgebraicGeometry"
-- debug NumericalAlgebraicGeometry; DBG = 1; printingPrecision = 20; 
PHCpack#"exported symbols"
NumericalAlgebraicGeometry#"exported symbols"

R = CC[x,y];
T = {x^2-2,y^2-x^2}
(S,solsS) = totalDegreeStartSystem T
solsT = solveSystem(T,Software=>PHCPACK)
-- solsT = track(S,T,solsS/toList, gamma=>0.6+0.8*ii,Software=>PHCPACK)
rsols = sortSolutions refine(T,solsT,Software=>PHCPACK,Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130)
assert(abs(first coordinates first rsols + sqrt(2p400)) < 2^(-398))
 