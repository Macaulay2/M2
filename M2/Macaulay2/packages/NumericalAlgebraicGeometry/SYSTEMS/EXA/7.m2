restart
needsPackage "NumericalAlgebraicGeometry"

f3 = x_1*x_2*x_3 + x_2*x_3*x_4 + x_3*x_4*x_5 + x_4*x_5*x_1 + x_5*x_1*x_2; 
f4 = x_1*x_2*x_3*x_4 + x_2*x_3*x_4*x_5 + x_3*x_4*x_5*x_1 + x_4*x_5*x_1*x_2 + x_5*x_1*x_2*x_3; 
f5 = x_1*x_2*x_3*x_4*x_5 - 1; 
F = {f1,f2,f3,f4,f5};
M = solveSystem F
B = solveSystem(F,Software=>BERTINI)
P = solveSystem(F,Software=>PHCPACK);
areEqual(sortSolutions M, sortSolutions B)
areEqual(sortSolutions M, sortSolutions P)
