needsPackage "NumericalAlgebraicGeometry"
-- 0-dimensional example: cyclic5
CC[x_1..x_5];
f1 = x_1+x_2+x_3+x_4+x_5; 
f2 = x_1*x_2 + x_2*x_3 + x_3*x_4 + x_4*x_5 + x_5*x_1; 
f3 = x_1*x_2*x_3 + x_2*x_3*x_4 + x_3*x_4*x_5 + x_4*x_5*x_1 + x_5*x_1*x_2; 
f4 = x_1*x_2*x_3*x_4 + x_2*x_3*x_4*x_5 + x_3*x_4*x_5*x_1 + x_4*x_5*x_1*x_2 + x_5*x_1*x_2*x_3; 
f5 = x_1*x_2*x_3*x_4*x_5 - 1; 
F = {f1,f2,f3,f4,f5};
M = solveSystem F
B = solveSystem(F,Software=>BERTINI)
P = solveSystem(F,Software=>PHCPACK);
areEqual(sortSolutions M, sortSolutions B)
areEqual(sortSolutions M, sortSolutions P)
p := first M
coordinates p
status p
peek p
-- positive-dimensional example: adjacent 2x2 minors of 3x4 matrix 
R = QQ[x_1..x_12]
A = genericMatrix(R,4,3)
I = ideal flatten apply(3,i->apply(2,j->det submatrix(A,{i,i+1},{j,j+1})))
VM := numericalIrreducibleDecomposition I
VB := numericalIrreducibleDecomposition(I,Software=>BERTINI)
-- VP := numericalIrreducibleDecomposition(I,Software=>PHCPACK) -- takes a long time
cs = components VM
c := last cs
equations c
slice c
points c
peek c
transpose clean_0.001 matrix first c.Points --M2 voodoo
decompose I -- Whose witness is that point?

