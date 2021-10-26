restart
needsPackage "NumericalAlgebraicGeometry"
R = CC[x_1..x_12]
A = genericMatrix(R,4,3)
I = ideal flatten apply(3,i->apply(2,j->det submatrix(A,{i,i+1},{j,j+1})))
VM := numericalIrreducibleDecomposition I
VB := numericalIrreducibleDecomposition(I,Software=>BERTINI)
VP := numericalIrreducibleDecomposition(I,Software=>PHCPACK)

