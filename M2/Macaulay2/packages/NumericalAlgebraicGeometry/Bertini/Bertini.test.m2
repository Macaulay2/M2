-- this file is created to test Bertini interface separately from NAG package
restart
NAG = loadPackage "NumericalAlgebraicGeometry"
NAG.DebuggingMode = true
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};

/// -- larger example (commented out)
loadPackage "NumericalAlgebraicGeometry"
load "../benchmarks.m2"
T = (katsuraBench 11)_*; -- #sols=1024, M2:4, H:7, B:15, P:37                                                 
(S,solsS) = totalDegreeStartSystem T; 
///

sols = solveSystem(T, Software=>BERTINI)
sols = track(S,T,solsS,Software=>BERTINI)

setDefault (Software=>BERTINI) 
V = numericalVariety F


