restart
needsPackage "NumericalAlgebraicGeometry"
CC[x,y];
f = x^2-1;
g = y^2+1;
F = {f,g};
M = solveSystem F
B = solveSystem(F,Software=>BERTINI)
P = solveSystem(F,Software=>PHCPACK)
areEqual(sortSolutions M, sortSolutions B)
areEqual(sortSolutions M, sortSolutions P)
