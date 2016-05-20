restart
debug needsPackage "NumericalAlgebraicGeometry"
n = 2; d = 2;
R=QQ[x_0..x_(n-1)]
eps = 10^-3
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii)

sols = trackHomotopy(H,solsS,tStepMin=>minimalStepSize 53,CorrectorTolerance=>1e-8,Precision=>infinity,EndZoneFactor=>0)
peek sols 

sols' = track(S,T,solsS,tStepMin=>minimalStepSize 53,CorrectorTolerance=>1e-8,EndZoneFactor=>0.)
peek sols'

-- NOTE THE DISCREPANCY in the number of steps reported